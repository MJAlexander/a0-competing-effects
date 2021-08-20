## Simulating a0 based on varying mortality risks and prematurity
## Creates Figure 2
## Author: MA

library(tidyverse)
library(here)
library(survival)
source("code/functions/simulate_death_times.R")



# Get hazard estimates ----------------------------------------------------


# cut points

TAU <- c(0, 1,2,3,4,5,6,7,14, 21,28, 60,90,120,150,180, 210, 240, 270, 300, 330,366)


dfm_all <- read_rds(here("data/dfm_all.RDS"))
births <- read_rds(here("data/births.RDS"))

d <- dfm_all %>% 
  rename(year = birth_year) %>% 
  filter(mother_race==6|mother_race==7, year==2012) %>%
  mutate(race = ifelse(mother_race==6, "NHW", "NHB")) %>% 
  rename(aged = death_age)

d$prematurity <-  cut(x = d$gest_age, breaks = c(0,27,31, 36, 48))
levels(d$prematurity) <- c("extremely preterm","very preterm","later preterm","full-term")


deaths <- d %>% 
  filter(!is.na(prematurity)) %>% 
  group_by(race, prematurity, aged) %>% 
  summarise(deaths = n())


survivors <- deaths %>% 
  group_by(race, prematurity) %>% 
  mutate(cumulative_deaths = cumsum(deaths)) %>% 
  left_join(births) %>% 
  mutate(survivors = lag(births - cumulative_deaths, default = births[1])) %>% 
  mutate(prob_death = deaths/survivors,
         prob_surv = 1- prob_death) %>%
  mutate(Sx = lag(cumprod(1-prob_death), default = 1)) %>%
  #mutate(Sx = survivors/births[1]) %>% 
  mutate(var_term = deaths/(survivors*(survivors-deaths)),
         se = sqrt(Sx^2*cumsum(var_term)))

# set up for pch

d <- d %>% 
  mutate(aged = aged+0.5, event = 1)

# get survival object for deaths
dsurv <- survival::survSplit(formula = Surv(time = aged, event = event) ~ ., data = d, cut = TAU) %>%
  mutate(interval = factor(tstart),
         interval_length = aged - tstart) %>%
  as_tibble()

# for simulation, let's just try fitting to deaths, because we just need death densities anyway

dcph_prem <- dsurv %>% 
  mutate(preterm = ifelse(prematurity == "full-term", "full-term", "preterm")) %>% 
  group_by(preterm, interval, tstart) %>%
  summarise(D = sum(event), 
            E_deaths = sum(interval_length)) %>% 
  mutate(E = E_deaths)


# estimate hazards
mod_pre <- glm(D ~ offset(log(E))-1 + interval, family = "poisson", data = dcph_prem %>% filter(preterm == "preterm"))
mod_full <- glm(D ~ offset(log(E))-1 + interval, family = "poisson", data = dcph_prem %>% filter(preterm == "full-term"))

# get the (log) hazards
haz_pre <- coef(mod_pre)
haz_full <- coef(mod_full)

# Simulation --------------------------------------------------------------


risks <- seq(1, 3.5, by = .1)
fracs <- seq(0, 0.5, by = 0.02)

nsims <- 1000
sims_df <- tibble()
for(i in 1:nsims){
  for(j in 1:length(fracs)){
    for(k in 1:length(risks)){
      this_sim <- tibble(t = simulate_death_times(1000*(1-fracs[j]), 
                                                  tau = TAU, haz_full[1], log(exp(haz_full[2:length(haz_full)])*seq(1, risks[k], length.out = length(haz_full)-1))), prem = "full-term")
      this_sim <- bind_rows(this_sim, tibble(t  = simulate_death_times(1000*fracs[j], 
                                                                       tau = TAU, haz_pre[1], haz_pre[2:length(haz_pre)]), prem = "pre-term"))
      this_sim$frac = fracs[j]
      this_sim$risk = risks[k]
      this_sim$sim <- i
      sims_df <- bind_rows(sims_df, this_sim)
    }

  }
}


a0s <- sims_df %>% 
  group_by(risk, frac, sim) %>% 
  summarise(a0 = mean(t)) %>% 
  group_by(risk, frac) %>% 
  summarise(a0_mean = mean(a0),
            a0_lower = quantile(a0, 0.025),
            a0_upper = quantile(a0, 0.975)) %>% 
  ungroup() %>% 
  mutate(risk = (risk))



# Plot and save -----------------------------------------------------------


## FIGURE 2 PLOTS

# plot risk
ggplot(a0s %>% filter(frac == 0.10), aes(risk, a0_mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = a0_lower, ymax = a0_upper)) + theme_bw(base_size = 14) + ylab("a0 (days)") 

# plot frac
ggplot(a0s %>% filter(risk==1), aes(frac, a0_mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = a0_lower, ymax = a0_upper)) + theme_bw(base_size = 14) + ylab("a0 (days)") 


# plot grid

a0s %>% 
  ggplot(aes(frac, risk, fill = a0_mean-30)) + 
  geom_tile() + scale_fill_viridis_c(name = "mean age at death (days)", option = "magma") + 
  theme_minimal() + ylab("mortality risk multiplier") + 
  xlab("fraction of births that are premature")


# save

write_csv(a0s, "output/sims.csv")




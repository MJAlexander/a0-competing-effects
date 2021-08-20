## Relationship between ratio of IMR/U5MR and prematurity based on US data
## Makes Figure 3
## Author: LR 

library(tidyverse)
library(here)
library(janitor)

# Reading in data from CDC WONDER. I downloaded a "Multiple Cause of Death, 1999-2019" data request txt file, 
# grouped by year, gender, race, Hispanic origin, and 5-year age group, containing death counts, 
# rates, and standard errors for rates (just in case we want the rates later; 
# here we're using counts). 
# I limited it to non-Hispanic Black and White, the years 2007-2014, and ages <1 and 1-4.

deaths <- read_csv(here("data/US_WONDER_deaths.csv"))

deathsu1 <- deaths %>%
  select(c(Year,`Gender Code`,Race,`Five-Year Age Groups`,Deaths)) %>%
  filter(`Five-Year Age Groups` == "< 1 year") %>%
  group_by(Year,`Gender Code`,Race) %>%
  summarise(Deaths=sum(Deaths))

deaths <- deaths %>%
  select(c(Year,`Gender Code`,Race,`Five-Year Age Groups`,Deaths)) %>%
  group_by(Year,`Gender Code`,Race) %>%
  summarise(Deaths=sum(Deaths))

# Same deal with births, CDC WONDER, same years

births <- read_csv(here("data/US_WONDER_births.csv"))

births <- births %>%
  select(c(Year,`Gender Code`,`Mother's Bridged Race`,Births)) %>%
  rename(Race = `Mother's Bridged Race`)

imr <- deathsu1 %>% 
  left_join(births) %>% 
  mutate(imr = (Deaths/Births)*1000) %>%
  select(Year,`Gender Code`,Race,imr)

u5mr <- deaths %>% 
  left_join(births) %>% 
  mutate(u5mr = (Deaths/Births)*1000) %>%
  select(Year,`Gender Code`,Race,u5mr)

ratio <- u5mr %>%
  left_join(imr) %>%
  mutate(ratio = (imr/u5mr)) %>%
  rename(Sex = `Gender Code`) %>%
  mutate(Race = ifelse(Race == "White","NHW","NHB"))

write_rds(ratio, path = "output/US_race_mortality_ratio.rds")

ratios <- read_rds("output/US_race_mortality_ratio.rds")
births <- read_csv("data/births07-13_bysex.csv")

preterm <- births %>% 
  mutate(preterm = gest<37) %>% 
  group_by(year, sex, race) %>% 
  summarize(preterm_prop = sum(births[preterm==TRUE])/sum(births))


d <- ratios %>% 
  group_by(Year, Race, Sex) %>% 
  summarize(ratio = mean(ratio)) %>% 
  janitor::clean_names() %>% 
  left_join(preterm)

d %>% 
  ungroup() %>% 
  summarise(cor(ratio, preterm_prop))

mod <- lm(preterm_prop ~ ratio, data = d)

summary(mod)

ratios %>% 
  group_by(Year, Race, Sex) %>% 
  janitor::clean_names() %>% 
  left_join(preterm) %>% 
  ggplot(aes(ratio, preterm_prop, color = race, pch = sex)) + geom_point() +
  theme_bw(base_size = 14)+
  geom_abline(slope = coef(mod)[[2]], intercept = coef(mod)[[1]])+
  coord_cartesian(ylim = c(0.09, 0.19)) +
  labs(x = "ratio of IMR to U5MR", y = "proportion of births that are pre-term")

ggsave("fig/ratio_preterm_scatter.pdf", width = 6, height = 5)



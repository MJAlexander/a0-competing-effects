### Figure 1 in paper
## Author: MA

library(tidyverse)
library(here)
library(patchwork)

# remove 2013 as not complete
dfm_all <- readRDS(here("data","dfm_all.RDS"))
d <- dfm_all %>% 
  rename(year = birth_year) %>% 
  filter(mother_race==6|mother_race==7, year!=2013) %>%
  mutate(race = ifelse(mother_race==6, "NHW", "NHB"),
         preterm = ifelse(gest_age<37, "pre-term", "full-term"))

p1 <- d %>% 
  filter(year==2012, preterm=="full-term") %>% 
  ggplot(aes(death_age)) + 
  geom_histogram(fill = "firebrick4", aes(y=..count../sum(..count..)), binwidth = 5) + 
  facet_wrap(~preterm, scales = "free_y") +
  theme_bw(base_size = 14) + 
  xlab("age at death (days)") + 
  ylab("proportion")
p2 <- d %>% 
  filter(year==2012, preterm=="pre-term") %>% 
  ggplot(aes(death_age)) + 
  geom_histogram(fill = "firebrick4", aes(y=..count../sum(..count..)), binwidth = 5) + 
  facet_wrap(~preterm, scales = "free_y") +
  theme_bw(base_size = 14) + 
  xlab("age at death (days)") + ylab("")
p1+p2
ggsave(here("fig", "histogram_preterm.pdf"), width = 8, height = 4)
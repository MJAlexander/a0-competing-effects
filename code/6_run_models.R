## Estimate piecewise and ratio models
## Makes Table 4-7 output
## Author: MA and LR

library(tidyverse)
library(here)
library(rstan)
library(brms)
library(tidybayes)
library(janitor)
library(patchwork)


# Data --------------------------------------------------------------------



# load in data and remove troublesome points 

d <- read_rds(here("data/hmd_data.rds"))

d$filter <- ifelse(d$PopName=="BGR" & d$cohort < 2009,"filter",NA)
d$filter <- ifelse(d$PopName=="CHE" & d$cohort <1880,"filter",d$filter)
d$filter <- ifelse(d$PopName=="EST" & d$cohort <1992,"filter",d$filter)
d$filter <- ifelse(d$PopName=="SVK" & d$cohort <1965,"filter",d$filter)
d$filter <- ifelse(d$PopName=="NLD" & d$cohort <1950,"filter",d$filter)
d$filter <- ifelse(d$PopName=="DEUTE" & d$cohort >1990,"filter",d$filter)
d$filter <- ifelse(d$PopName=="DEUTW" & d$cohort >1990,"filter",d$filter)

d_red <- d %>%
  filter(!PopName %in% c("FRACNP","NZL_MA","RUS","TWN","UKR","ISL","LUX")) %>%
  filter(is.na(filter)) %>%
  select(-filter)

# leave out 20% of data as a test set

dm <- d_red %>% filter(Sex=="m" & imr < .08) %>% clean_names()
df <- d_red %>% filter(Sex=="f" & imr < .08) %>% clean_names()

set.seed(127)
n_obs_m <- nrow(dm)
n_train_m <- round(n_obs_m*0.9)
train_index_m <- sample(1:n_obs_m, n_train_m)
n_obs_f <- nrow(df)
n_train_f <- round(n_obs_f*0.9)
train_index_f <- sample(1:n_obs_f, n_train_f)
dm_train <- dm[train_index_m,]
dm_test <- dm[!(1:n_obs_m %in% train_index_m),]
df_train <- df[train_index_f,]
df_test <- df[!(1:n_obs_f %in% train_index_f),]

## both sexes
db <- bind_rows(df, dm)
db_train <- bind_rows(df_train, dm_train)
db_test <- bind_rows(df_test, dm_test)

# we also need AK data

andreev <- d %>%
  filter(PopName == "AUT" & cohort %in% 1971:2007 | PopName == "BEL" & cohort %in% 1941:2008 | PopName == "CAN" & cohort %in% 1950:2006 | PopName == "CZE" & cohort %in% 1965:2007 |  PopName == "DNK" & cohort %in% 1921:2007 | PopName == "EST" & cohort %in% 1992:2008 | PopName == "FIN" & cohort %in% 1917:2008 | PopName == "FRATNP" & cohort %in% 1975:2008 | PopName == "DEUTNP" & cohort %in% 1991:2007 | PopName == "HUN" & cohort %in% 1950:2005 | PopName == "ITA" & cohort %in% 1929:2005 | PopName == "JPN" & cohort %in% 1950:2008 | PopName == "NZL_NP" & cohort %in% 1980:2007 | PopName == "NOR" & cohort %in% 1993:2007 | PopName == "PRT" & cohort %in% 1980:2008 | PopName == "SVK" & cohort %in% 1965:2007 | PopName == "SVN" & cohort %in% 1983:2008 |  PopName == "ESP" & cohort %in% 1975:2005 | PopName == "SWE" & cohort %in% 1901:2007 | PopName == "USA" & cohort %in% 1959:2006)

df_ak <- andreev %>% clean_names() %>% filter(sex=="f")
dm_ak <- andreev %>% clean_names() %>% filter(sex=="m")
# Models ------------------------------------------------------------------

## PIECEWISE

stan_data <- list(y = df_train %>% select(a0) %>% pull(),
                  x = df_train %>% select(imr) %>% pull(),
                  N = nrow(df_train))

f_piecewise_fit <- stan(file = here("code/models/one_cut.stan"), 
                        init = 0,
                        data = stan_data)

stan_data <- list(y = dm_train %>% select(a0) %>% pull(),
                  x = dm_train %>% select(imr) %>% pull(),
                  N = nrow(dm_train))

m_piecewise_fit <- stan(file = here("code/models/one_cut.stan"), 
                        init = 0,
                        data = stan_data)

stan_data <- list(y = db_train %>% select(a0) %>% pull(),
                  x = db_train %>% select(imr) %>% pull(),
                  N = nrow(db_train))

b_piecewise_fit <- stan(file = here("code/models/one_cut.stan"), 
                        init = 0,
                        data = stan_data)

## AK

stan_data <- list(y = df_ak %>% select(a0) %>% pull(),
                  x = df_ak %>% select(imr) %>% pull(),
                  N = nrow(df_ak))

f_ak_piecewise_fit <- stan(file = here("code/models/one_cut.stan"),
                           init = 0,
                           data = stan_data)

stan_data <- list(y = dm_ak %>% select(a0) %>% pull(),
                  x = dm_ak %>% select(imr) %>% pull(),
                  N = nrow(dm_ak))

m_ak_piecewise_fit <- stan(file = here("code/models/one_cut.stan"),
                           init = 0,
                           data = stan_data)


# get posterior samples of parameters and estimates

f_piecewise_pars <- f_piecewise_fit %>% 
  gather_draws(alpha, beta1, beta2, cutpoint1) %>% 
  median_qi()

m_piecewise_pars <- m_piecewise_fit %>% 
  gather_draws(alpha, beta1, beta2, cutpoint1) %>% 
  median_qi()

b_piecewise_pars <- b_piecewise_fit %>% 
  gather_draws(alpha, beta1, beta2, cutpoint1) %>% 
  median_qi()

f_piecewise_a0_hat <- f_piecewise_fit %>% 
  gather_draws(mu[i]) %>% 
  median_qi() %>% 
  bind_cols(a0 = df_train %>% select(a0) %>% pull()) %>% 
  rename(fit = .value, data = a0) %>% 
  ungroup() %>% 
  select(data, fit)

m_piecewise_a0_hat <- m_piecewise_fit %>% 
  gather_draws(mu[i]) %>% 
  median_qi() %>% 
  bind_cols(a0 = dm_train %>% select(a0) %>% pull()) %>% 
  rename(fit = .value, data = a0) %>% 
  ungroup() %>% 
  select(data, fit)

b_piecewise_a0_hat <- b_piecewise_fit %>% 
  gather_draws(mu[i]) %>% 
  median_qi() %>% 
  bind_cols(a0 = db_train %>% select(a0) %>% pull()) %>% 
  rename(fit = .value, data = a0) %>% 
  ungroup() %>% 
  select(data, fit)

# ak

f_ak_piecewise_pars <- f_ak_piecewise_fit %>%
  gather_draws(alpha, beta1, beta2, cutpoint1) %>%
  median_qi()

m_ak_piecewise_pars <- m_ak_piecewise_fit %>%
  gather_draws(alpha, beta1, beta2, cutpoint1) %>%
  median_qi()

f_ak_piecewise_a0_hat <- f_ak_piecewise_fit %>%
  gather_draws(mu[i]) %>%
  median_qi() %>%
  bind_cols(a0 = df_ak %>% select(a0) %>% pull()) %>%
  rename(fit = .value, data = a0) %>%
  ungroup() %>%
  select(data, fit)

m_ak_piecewise_a0_hat <- m_ak_piecewise_fit %>%
  gather_draws(mu[i]) %>%
  median_qi() %>%
  bind_cols(a0 = dm_ak %>% select(a0) %>% pull()) %>%
  rename(fit = .value, data = a0) %>%
  ungroup() %>%
  select(data, fit)

## Leave one out CV

f_piecewise_loo <- loo(f_piecewise_fit)
m_piecewise_loo <- loo(m_piecewise_fit)
b_piecewise_loo <- loo(b_piecewise_fit)

## RATIO MODEL

stan_data <- list(y = df_train %>% select(a0) %>% pull(),
                  imr = df_train %>% select(imr) %>% pull(),
                  ratio = df_train %>% select(ratio) %>% pull(),
                  N = nrow(df_train))

f_ratio_fit <- stan(file = here("code/models/ratio.stan"), 
                    init = 0,
                    data = stan_data)

stan_data <- list(y = dm_train %>% select(a0) %>% pull(),
                  imr = dm_train %>% select(imr) %>% pull(),
                  ratio = dm_train %>% select(ratio) %>% pull(),
                  N = nrow(dm_train))

m_ratio_fit <- stan(file = here("code/models/ratio.stan"), 
                    init = 0,
                    data = stan_data)

stan_data <- list(y = db_train %>% select(a0) %>% pull(),
                  imr = db_train %>% select(imr) %>% pull(),
                  ratio = db_train %>% select(ratio) %>% pull(),
                  N = nrow(db_train))

b_ratio_fit <- stan(file = here("code/models/ratio.stan"), 
                    init = 0,
                    data = stan_data)

## AK 

stan_data <- list(y = df_ak %>% select(a0) %>% pull(),
                  imr = df_ak %>% select(imr) %>% pull(),
                  ratio = df_ak %>% select(ratio) %>% pull(),
                  N = nrow(df_ak))

f_ak_ratio_fit <- stan(file = here("code/models/ratio.stan"),
                       init = 0,
                       data = stan_data)

stan_data <- list(y = dm_ak %>% select(a0) %>% pull(),
                  imr = dm_ak %>% select(imr) %>% pull(),
                  ratio = dm_ak %>% select(ratio) %>% pull(),
                  N = nrow(dm_ak))

m_ak_ratio_fit <- stan(file = here("code/models/ratio.stan"),
                       init = 0,
                       data = stan_data)

# get estimates etc

f_ratio_pars <- f_ratio_fit %>% 
  gather_draws(alpha, beta1, beta2) %>% 
  median_qi()

m_ratio_pars <- m_ratio_fit %>% 
  gather_draws(alpha, beta1, beta2) %>% 
  median_qi()

b_ratio_pars <- b_ratio_fit %>% 
  gather_draws(alpha, beta1, beta2) %>% 
  median_qi()


f_ratio_a0_hat <- f_ratio_fit %>% 
  gather_draws(mu[i]) %>% 
  median_qi() %>% 
  bind_cols(a0 = df_train %>% select(a0) %>% pull()) %>% 
  rename(fit = .value, data = a0) %>% 
  ungroup() %>% 
  select(data, fit)

m_ratio_a0_hat <- m_ratio_fit %>% 
  gather_draws(mu[i]) %>% 
  median_qi() %>% 
  bind_cols(a0 = dm_train %>% select(a0) %>% pull()) %>% 
  rename(fit = .value, data = a0) %>% 
  ungroup() %>% 
  select(data, fit)

b_ratio_a0_hat <- b_ratio_fit %>% 
  gather_draws(mu[i]) %>% 
  median_qi() %>% 
  bind_cols(a0 = db_train %>% select(a0) %>% pull()) %>% 
  rename(fit = .value, data = a0) %>% 
  ungroup() %>% 
  select(data, fit)

## AK

f_ak_ratio_pars <- f_ak_ratio_fit %>%
  gather_draws(alpha, beta1, beta2) %>%
  median_qi()

m_ak_ratio_pars <- m_ak_ratio_fit %>%
  gather_draws(alpha, beta1, beta2) %>%
  median_qi()

f_ak_ratio_a0_hat <- f_ak_ratio_fit %>%
  gather_draws(mu[i]) %>%
  median_qi() %>%
  bind_cols(a0 = df_ak %>% select(a0) %>% pull()) %>%
  rename(fit = .value, data = a0) %>%
  ungroup() %>%
  select(data, fit)

m_ak_ratio_a0_hat <- m_ak_ratio_fit %>%
  gather_draws(mu[i]) %>%
  median_qi() %>%
  bind_cols(a0 = dm_ak %>% select(a0) %>% pull()) %>%
  rename(fit = .value, data = a0) %>%
  ungroup() %>%
  select(data, fit)

# LOO

f_ratio_loo <- loo(f_ratio_fit)
m_ratio_loo <- loo(m_ratio_fit)
b_ratio_loo <- loo(b_ratio_fit)


# Compare models ----------------------------------------------------------

## LOO

f_loo <- loo_compare(f_piecewise_loo, f_ratio_loo) %>% 
  as_tibble(rownames = "model") %>% 
  select(model, elpd_diff, se_diff) %>% 
  mutate(sex = "f")
m_loo <- loo_compare(m_piecewise_loo, m_ratio_loo) %>% 
  as_tibble(rownames = "model") %>% 
  select(model, elpd_diff, se_diff)%>% 
  mutate(sex = "m")
b_loo <- loo_compare(b_piecewise_loo, b_ratio_loo) %>% 
  as_tibble(rownames = "model") %>% 
  select(model, elpd_diff, se_diff)%>% 
  mutate(sex = "b")

## In sample MSE

f_ak_a0_hat <- tibble(data = df_train %>% select(a0) %>% pull(),
                      x = df_train %>% select(imr) %>% pull()) %>% 
  mutate(AK = ifelse(x < 0.0170, 0.1490 - 2.0867*x, 0)) %>%
  mutate(AK = ifelse(x >= 0.0170, 0.0438 + 4.1075*x, AK)) %>%
  mutate(AK = ifelse(x > .0658, 0.3141, AK)) %>% 
  rename(fit = AK)

m_ak_a0_hat <- tibble(data = dm_train %>% select(a0) %>% pull(),
                      x = dm_train %>% select(imr) %>% pull()) %>% 
  mutate(AK = ifelse(x < 0.0226, 0.1493 - 2.0367*x, 0)) %>%
  mutate(AK = ifelse(x >= 0.0226 ,0.0244 + 3.4994*x, AK)) %>%
  mutate(AK = ifelse(x > .0785, 0.2991, AK)) %>% 
  rename(fit = AK)

f_insample_mse <- bind_rows(
  f_ak_a0_hat %>% 
    mutate(diff_sq = (fit - data)^2) %>% 
    summarise(mse = mean(diff_sq)) %>% 
    mutate(model = "AK"),
  f_piecewise_a0_hat %>% 
    mutate(diff_sq = (fit - data)^2) %>% 
    summarise(mse = mean(diff_sq)) %>% 
    mutate(model = "piecewise"),
  f_ratio_a0_hat %>% 
    mutate(diff_sq = (fit - data)^2) %>% 
    summarise(mse = mean(diff_sq)) %>% 
    mutate(model = "ratio")
) %>% 
  mutate(sex="f", type = "insample")

m_insample_mse <- bind_rows(
  m_ak_a0_hat %>% 
    mutate(diff_sq = (fit - data)^2) %>% 
    summarise(mse = mean(diff_sq)) %>% 
    mutate(model = "AK"),
  m_piecewise_a0_hat %>% 
    mutate(diff_sq = (fit - data)^2) %>% 
    summarise(mse = mean(diff_sq)) %>% 
    mutate(model = "piecewise"),
  m_ratio_a0_hat %>% 
    mutate(diff_sq = (fit - data)^2) %>% 
    summarise(mse = mean(diff_sq)) %>% 
    mutate(model = "ratio")
) %>% 
  mutate(sex="m", type = "insample")

b_insample_mse <- bind_rows(
  b_piecewise_a0_hat %>% 
    mutate(diff_sq = (fit - data)^2) %>% 
    summarise(mse = mean(diff_sq)) %>% 
    mutate(model = "piecewise"),
  b_ratio_a0_hat %>% 
    mutate(diff_sq = (fit - data)^2) %>% 
    summarise(mse = mean(diff_sq)) %>% 
    mutate(model = "ratio")
) %>% 
  mutate(sex="b", type = "insample")

## In sample on AK data

f_insample_mse_ak <- df_ak %>% 
  mutate(x2 = imr>f_ak_piecewise_pars$.value[f_ak_piecewise_pars$.variable=="cutpoint1"]) %>% 
  mutate(piecewise = f_ak_piecewise_pars$.value[f_ak_piecewise_pars$.variable=="alpha"] +  f_ak_piecewise_pars$.value[f_ak_piecewise_pars$.variable=="beta1"] * imr + f_ak_piecewise_pars$.value[f_ak_piecewise_pars$.variable=="beta2"] * (imr - f_ak_piecewise_pars$.value[f_ak_piecewise_pars$.variable=="cutpoint1"]) * x2) %>% 
  mutate(AK = ifelse(imr < 0.0170, 0.1490 - 2.0867*imr, 0)) %>%
  mutate(AK = ifelse(imr >= 0.0170, 0.0438 + 4.1075*imr, AK)) %>%
  mutate(AK = ifelse(imr > .0658, 0.3141, AK)) %>% 
  rename(ak = AK) %>% 
  mutate(ratio = f_ak_ratio_pars$.value[f_ak_ratio_pars$.variable=="alpha"]+f_ak_ratio_pars$.value[f_ak_ratio_pars$.variable=="beta1"]*imr + f_ak_ratio_pars$.value[f_ak_ratio_pars$.variable=="beta2"]*ratio) %>% 
  select(a0, piecewise, ak, ratio) %>% 
  pivot_longer(piecewise:ratio, names_to = "model", values_to = "fit") %>% 
  mutate(diff_sq = (fit - a0)^2) %>% 
  group_by(model) %>%
  summarise(mse = mean(diff_sq)) %>% 
  mutate(sex="f", type = "insample_ak") 

# male

m_insample_mse_ak <- dm_ak %>% 
  mutate(x2 = imr>m_ak_piecewise_pars$.value[m_ak_piecewise_pars$.variable=="cutpoint1"]) %>% 
  mutate(piecewise = m_ak_piecewise_pars$.value[m_ak_piecewise_pars$.variable=="alpha"] +  m_ak_piecewise_pars$.value[m_ak_piecewise_pars$.variable=="beta1"] * imr + m_ak_piecewise_pars$.value[m_ak_piecewise_pars$.variable=="beta2"] * (imr - m_ak_piecewise_pars$.value[m_ak_piecewise_pars$.variable=="cutpoint1"]) * x2) %>% 
  mutate(AK = ifelse(imr < 0.0170, 0.1490 - 2.0867*imr, 0)) %>%
  mutate(AK = ifelse(imr >= 0.0170, 0.0438 + 4.1075*imr, AK)) %>%
  mutate(AK = ifelse(imr > .0658, 0.3141, AK)) %>% 
  rename(ak = AK) %>% 
  mutate(ratio = m_ak_ratio_pars$.value[m_ak_ratio_pars$.variable=="alpha"]+m_ak_ratio_pars$.value[m_ak_ratio_pars$.variable=="beta1"]*imr + m_ak_ratio_pars$.value[m_ak_ratio_pars$.variable=="beta2"]*ratio) %>% 
  select(a0, piecewise, ak, ratio) %>% 
  pivot_longer(piecewise:ratio, names_to = "model", values_to = "fit") %>% 
  mutate(difm_sq = (fit - a0)^2) %>% 
  group_by(model) %>%
  summarise(mse = mean(difm_sq)) %>% 
  mutate(sex="m", type = "insample_ak")

## Out of sample MSE

f_outsample_mse <- df_test %>% 
  mutate(x2 = imr>f_piecewise_pars$.value[f_piecewise_pars$.variable=="cutpoint1"]) %>% 
  mutate(piecewise = f_piecewise_pars$.value[f_piecewise_pars$.variable=="alpha"] +  f_piecewise_pars$.value[f_piecewise_pars$.variable=="beta1"] * imr + f_piecewise_pars$.value[f_piecewise_pars$.variable=="beta2"] * (imr - f_piecewise_pars$.value[f_piecewise_pars$.variable=="cutpoint1"]) * x2) %>% 
  mutate(AK = ifelse(imr < 0.0170, 0.1490 - 2.0867*imr, 0)) %>%
  mutate(AK = ifelse(imr >= 0.0170, 0.0438 + 4.1075*imr, AK)) %>%
  mutate(AK = ifelse(imr > .0658, 0.3141, AK)) %>% 
  rename(ak = AK) %>% 
  mutate(ratio = f_ratio_pars$.value[f_ratio_pars$.variable=="alpha"]+f_ratio_pars$.value[f_ratio_pars$.variable=="beta1"]*imr + f_ratio_pars$.value[f_ratio_pars$.variable=="beta2"]*ratio) %>% 
  select(a0, piecewise, ak, ratio) %>% 
  pivot_longer(piecewise:ratio, names_to = "model", values_to = "fit") %>% 
  mutate(diff_sq = (fit - a0)^2) %>% 
  group_by(model) %>%
  summarise(mse = mean(diff_sq)) %>% 
  mutate(sex="f", type = "out of sample")

m_outsample_mse <- dm_test %>% 
  mutate(x2 = imr>m_piecewise_pars$.value[m_piecewise_pars$.variable=="cutpoint1"]) %>% 
  mutate(piecewise = m_piecewise_pars$.value[m_piecewise_pars$.variable=="alpha"] +  m_piecewise_pars$.value[m_piecewise_pars$.variable=="beta1"] * imr + m_piecewise_pars$.value[m_piecewise_pars$.variable=="beta2"] * (imr - m_piecewise_pars$.value[m_piecewise_pars$.variable=="cutpoint1"]) * x2) %>% 
  mutate(AK = ifelse(imr < 0.0170, 0.1490 - 2.0867*imr, 0)) %>%
  mutate(AK = ifelse(imr >= 0.0170, 0.0438 + 4.1075*imr, AK)) %>%
  mutate(AK = ifelse(imr > .0658, 0.3141, AK)) %>% 
  rename(ak = AK) %>% 
  mutate(ratio = m_ratio_pars$.value[m_ratio_pars$.variable=="alpha"]+m_ratio_pars$.value[m_ratio_pars$.variable=="beta1"]*imr + m_ak_ratio_pars$.value[m_ratio_pars$.variable=="beta2"]*ratio) %>% 
  select(a0, piecewise, ak, ratio) %>% 
  pivot_longer(piecewise:ratio, names_to = "model", values_to = "fit") %>% 
  mutate(difm_sq = (fit - a0)^2) %>% 
  group_by(model) %>%
  summarise(mse = mean(difm_sq)) %>% 
  mutate(sex="m", type = "out of sample")

b_outsample_mse <- db_test %>% 
  mutate(x2 = imr>b_piecewise_pars$.value[b_piecewise_pars$.variable=="cutpoint1"]) %>% 
  mutate(piecewise = b_piecewise_pars$.value[b_piecewise_pars$.variable=="alpha"] +  b_piecewise_pars$.value[b_piecewise_pars$.variable=="beta1"] * imr + b_piecewise_pars$.value[b_piecewise_pars$.variable=="beta2"] * (imr - b_piecewise_pars$.value[m_piecewise_pars$.variable=="cutpoint1"]) * x2) %>% 
  mutate(ratio = b_ratio_pars$.value[b_ratio_pars$.variable=="alpha"]+b_ratio_pars$.value[b_ratio_pars$.variable=="beta1"]*imr + b_ratio_pars$.value[b_ratio_pars$.variable=="beta2"]*ratio) %>% 
  select(a0, piecewise, ratio) %>% 
  pivot_longer(piecewise:ratio, names_to = "model", values_to = "fit") %>% 
  mutate(difm_sq = (fit - a0)^2) %>% 
  group_by(model) %>%
  summarise(mse = mean(difm_sq)) %>% 
  mutate(sex="b", type = "out of sample")

## Consolidate results


all_mse <- bind_rows(f_insample_mse, f_outsample_mse, f_insample_mse_ak,m_insample_mse, m_outsample_mse, m_insample_mse_ak, b_insample_mse, b_outsample_mse) %>% 
  mutate(model = ifelse(model=="AK", "ak", model)) %>% 
  select(sex, type, model, mse) %>% 
  mutate(mse = sqrt(mse)) %>% 
  pivot_wider(names_from = "model", values_from = "mse")

write_csv(all_mse, path = here("output/mse.csv"))

loo <- bind_rows(f_loo,m_loo, b_loo)

write_csv(loo, path = here("output/loo.csv"))

## Compare to the truth

coefs <- bind_rows(f_piecewise_pars %>% 
                     mutate(sex="f", model = "piecewise"),
                   m_piecewise_pars %>% 
                     mutate(sex="m", model = "piecewise"),
                   b_piecewise_pars %>% 
                     mutate(sex="b", model = "piecewise"),
                   f_ak_piecewise_pars %>% 
                     mutate(sex="f", model = "piecewise (AK data)"),
                   m_ak_piecewise_pars %>% 
                     mutate(sex="m", model = "piecewise (AK data)"),
                   f_ratio_pars %>% 
                     mutate(sex="f", model = "ratio"),
                   m_ratio_pars %>% 
                     mutate(sex="m", model = "ratio"),
                   b_ratio_pars %>% 
                     mutate(sex="b", model = "ratio")) %>% 
  clean_names() %>% 
  select(variable, value, sex, model) %>% 
  filter(model!="piecewise (AK data)")


# Compare to the truth ----------------------------------------------------

dfm_all <- readRDS(here("data","dfm_all.RDS"))
dd <- dfm_all %>% 
  rename(year = birth_year) %>% 
  filter(mother_race==6|mother_race==7, year!=2013) %>%
  mutate(race = ifelse(mother_race==6, "NHW", "NHB"),
         preterm = ifelse(gest_age<37, "pre-term", "full-term"))
a0 <- dd %>% 
  group_by(year, race, sex) %>% 
  summarise(a0 = mean(death_age)/365)

ratios <- read_rds("output/US_race_mortality_ratio.rds")

us_all <- ratios %>% 
  clean_names() %>% 
  left_join(a0)

## Females

coef_f_ratio <- coefs %>% filter(sex=="f"&model=="ratio")
coef_f_piecewise <- coefs %>% filter(sex=="f"&model=="piecewise")
coef_b_ratio <- coefs %>% filter(sex=="b"&model=="ratio")

rmse_f <- us_all %>% 
  filter(sex=="F") %>% 
  mutate(ratio_fit = coef_f_ratio$value[coef_f_ratio$variable == "alpha"]+
           coef_f_ratio$value[coef_f_ratio$variable == "beta1"]*imr/1000 + 
           coef_f_ratio$value[coef_f_ratio$variable == "beta2"]*ratio) %>% 
  mutate(piecewise_fit = coef_f_piecewise$value[coef_f_piecewise$variable == "alpha"]+
           coef_f_piecewise$value[coef_f_piecewise$variable == "beta1"]*imr/1000+
           +
           coef_f_piecewise$value[coef_f_piecewise$variable == "beta2"]*(imr/1000 - coef_f_piecewise$value[coef_f_piecewise$variable == "cutpoint1"])) %>% 
  mutate(ratio_fit_b = coef_b_ratio$value[coef_b_ratio$variable == "alpha"]+
           coef_b_ratio$value[coef_b_ratio$variable == "beta1"]*imr/1000 + 
           coef_b_ratio$value[coef_b_ratio$variable == "beta2"]*ratio) %>% 
  mutate(ratio_diff = (ratio_fit - a0)^2,
         ratio_diff_b = (ratio_fit_b - a0)^2,
         piece_diff = (piecewise_fit - a0)^2
  ) %>% 
  group_by(race) %>% 
  summarise(mse_ratio = sqrt(mean(ratio_diff, na.rm = TRUE)),
            mse_ratio_b = sqrt(mean(ratio_diff_b, na.rm = TRUE)),
            mse_piece = sqrt(mean(piece_diff, na.rm = TRUE))) %>% 
  mutate(sex = "F")

## Males

coef_f_ratio <- coefs %>% filter(sex=="m"&model=="ratio")
coef_f_piecewise <- coefs %>% filter(sex=="m"&model=="piecewise")

rmse_m <- us_all %>% 
  filter(sex=="M") %>% 
  mutate(ratio_fit = coef_f_ratio$value[coef_f_ratio$variable == "alpha"]+
           coef_f_ratio$value[coef_f_ratio$variable == "beta1"]*imr/1000 + 
           coef_f_ratio$value[coef_f_ratio$variable == "beta2"]*ratio) %>% 
  mutate(ratio_fit_b = coef_b_ratio$value[coef_b_ratio$variable == "alpha"]+
           coef_b_ratio$value[coef_b_ratio$variable == "beta1"]*imr/1000 + 
           coef_b_ratio$value[coef_b_ratio$variable == "beta2"]*ratio) %>% 
  mutate(piecewise_fit = coef_f_piecewise$value[coef_f_piecewise$variable == "alpha"]+
           coef_f_piecewise$value[coef_f_piecewise$variable == "beta1"]*imr/1000+
           +
           coef_f_piecewise$value[coef_f_piecewise$variable == "beta2"]*(imr/1000 - coef_f_piecewise$value[coef_f_piecewise$variable == "cutpoint1"])) %>% 
  mutate(ratio_diff = (ratio_fit - a0)^2,
         ratio_diff_b = (ratio_fit_b - a0)^2,
         piece_diff = (piecewise_fit - a0)^2) %>% 
  group_by(race) %>% 
  summarise(mse_ratio = sqrt(mean(ratio_diff, na.rm = TRUE)),
            mse_ratio_b = sqrt(mean(ratio_diff_b, na.rm = TRUE)),
            mse_piece = sqrt(mean(piece_diff, na.rm = TRUE))) %>% 
  mutate(sex = "M")

bind_rows(rmse_f, rmse_m) %>% 
  select(-mse_ratio_b) %>% 
  pivot_longer(mse_ratio:mse_piece, values_to = "RMSE", names_to = "model") %>% 
  mutate(model = ifelse(model=="mse_ratio", "ratio", "piecewise")) %>% 
  mutate(RMSE = round(RMSE, 4)) %>% 
  write_csv("output/rmse_us.csv")


EV71_Lit <- readRDS("data/EV71_Lit.rds") %>% 
  filter(!(Age_LL_value == 0 & Age_UL_value %in% c(5,6))) %>% 
  filter(index %in% c("109",
                      "588")) %>% 
  group_by(index) %>% 
  group_split()

tab %>% 
  left_join(pop_all %>%
              dplyr::filter(ag_LL < 6) %>% 
              mutate(ag_LL = ag_LL,
                     year = as.numeric(year)),
            by = c("CNTY_CODE", "code_prv", "year")) %>%
  group_by(CNTY_CODE, year) %>% 
  mutate(APC = sum(pop),
         pop_r = pop/APC,
         d2_r = d2*pop_r,
         birth_cohort = year-ag_LL) %>% 
  dplyr::select(CNTY_CODE, code_prv, year, birth_cohort, ag_LL, d2, d2_r, pop, pop_r) %>% 
  group_by(CNTY_CODE, birth_cohort) %>% 
  mutate(index = max(1:n())) %>%
  group_split() %>% 
  map(mutate, 
      pop_lag = lag(pop,1),
      pop_change = pop/pop_lag,
      pop_change = if_else(is.na(pop_change),
                           1,
                           pop_change),
      d2_r_cs = cumsum(d2_r),
      d2_r_cs_adjusted = pop_change*cumsum(d2_r)) %>% 
  bind_rows() %>% 
  mutate(cov = d2_r_cs_adjusted/ pop) -> tmp

tmp %>% 
  dplyr::filter(cov > 1) %>% 
  pull(CNTY_CODE) %>% 
  unique() -> to_remove_CNTY_CODE

tmp %>% 
  dplyr::filter(cov > 1) %>% 
  pull(code_prv) %>% 
  unique() -> to_remove_prv

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2011:2016),
                year == 2016) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*pop_r)) -> cov_2016

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2012:2017),
                year == 2017) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*pop_r)) -> cov_2017

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2013:2018),
                year == 2018) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*pop_r)) -> cov_2018

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2014:2019),
                year == 2019) %>% 
  group_by(CNTY_CODE) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*pop_r)) -> cov_2019


cov <- list(cov_2016, cov_2017, cov_2018, cov_2019)

cov %>% 
  map(mutate, 
      code_prf = substr(CNTY_CODE, 1, 4)) %>% 
  map(group_by, code_prf) %>% 
  map(mutate,
      cov_weighted_max_prf = max(cov_weighted),
      cov_weighted_min_prf = min(cov_weighted)) %>% 
  map(group_by, code_prv) %>% 
  map(mutate,
      cov_weighted_max_prv = max(cov_weighted),
      cov_weighted_min_prv = min(cov_weighted)) %>% 
  map(dplyr::select, CNTY_CODE, code_prv, code_prf, year, starts_with("cov_weighted")) %>% 
  map(distinct) %>% bind_rows() -> cov_baseline

source("code/plot_fig2.R")

#### importing HE ####
pop_all %>%
  filter(CNTY_CODE == 510723,
         year == 2017,
         ag_LL %in% paste0(0:5)) %>%
  mutate(all = sum(pop),
         pop_r = pop/all,
         cov = case_when(ag_LL == 0 ~ 64.5/420,
                         ag_LL == 1 ~ 129/420,
                         ag_LL == 2 ~ 78/420,
                         ag_LL == 3 ~ 33/420,
                         ag_LL == 4 ~ 33/420,
                         ag_LL == 5 ~ 33/420),
         covered = cov*pop,
         doses_all = sum(covered),
         weights = covered/doses_all,
         author = "HE") %>% 
  ungroup %>% 
  dplyr::select(ag_LL, weights, author) -> distribution_weights_HE

tab %>% 
  left_join(pop_all %>%
              dplyr::filter(ag_LL < 6) %>% 
              mutate(ag_LL = ag_LL,
                     year = as.numeric(year)),
            by = c("CNTY_CODE", "code_prv", "year")) %>%
  group_by(CNTY_CODE, year) %>% 
  left_join(distribution_weights_HE, by = "ag_LL") %>% 
  mutate(APC = sum(pop),
         d2_r = d2*weights,
         birth_cohort = year-ag_LL) %>% 
  dplyr::select(CNTY_CODE, code_prv, year, birth_cohort, ag_LL, d2, d2_r, pop, weights) %>% 
  group_by(CNTY_CODE, birth_cohort) %>% 
  mutate(index = max(1:n())) %>%
  group_split() %>% 
  map(mutate, 
      pop_lag = lag(pop,1),
      pop_change = pop/pop_lag,
      pop_change = if_else(is.na(pop_change),
                           1,
                           pop_change),
      d2_r_cs = cumsum(d2_r),
      d2_r_cs_adjusted = pop_change*cumsum(d2_r)) %>% 
  bind_rows() %>% 
  mutate(cov = d2_r_cs_adjusted/ pop) -> tmp

tmp %>% 
  dplyr::filter(cov > 1) %>% 
  pull(CNTY_CODE) %>% 
  unique() -> to_remove_CNTY_CODE

tmp %>% 
  dplyr::filter(cov > 1) %>% 
  pull(code_prv) %>% 
  unique() -> to_remove_prv

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2011:2016),
                year == 2016) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*weights)) -> cov_2016

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2012:2017),
                year == 2017) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*weights)) -> cov_2017

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2013:2018),
                year == 2018) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*weights)) -> cov_2018

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2014:2019),
                year == 2019) %>% 
  group_by(CNTY_CODE) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*weights)) -> cov_2019


cov <- list(cov_2016, cov_2017, cov_2018, cov_2019)

cov %>% 
  map(mutate, 
      code_prf = substr(CNTY_CODE, 1, 4)) %>% 
  map(group_by, code_prf) %>% 
  map(mutate,
      cov_weighted_max_prf = max(cov_weighted),
      cov_weighted_min_prf = min(cov_weighted)) %>% 
  map(group_by, code_prv) %>% 
  map(mutate,
      cov_weighted_max_prv = max(cov_weighted),
      cov_weighted_min_prv = min(cov_weighted)) %>% 
  map(dplyr::select, CNTY_CODE, code_prv, code_prf, year, starts_with("cov_weighted")) %>% 
  map(distinct) %>% bind_rows() -> cov_HE

#### importing WU ####
pop_all %>%
  filter(CNTY_CODE == 330205,
         year == 2017,
         ag_LL %in% paste0(0:5)) %>%
  mutate(all = sum(pop),
         pop_r = pop/all,
         cov = case_when(ag_LL == 0 ~ 8/80,
                         ag_LL == 1 ~ 21/85,
                         ag_LL == 2 ~ 21/85,
                         ag_LL == 3 ~ 20/77,
                         ag_LL == 4 ~ 20/77,
                         ag_LL == 5 ~ 20/77),
         covered = cov*pop,
         doses_all = sum(covered),
         weights = covered/doses_all,
         author = "WU") %>% 
  ungroup %>% 
  dplyr::select(ag_LL, weights, author) -> distribution_weights_WU

tab %>% 
  left_join(pop_all %>%
              dplyr::filter(ag_LL < 6) %>% 
              mutate(ag_LL = ag_LL,
                     year = as.numeric(year)),
            by = c("CNTY_CODE", "code_prv", "year")) %>%
  group_by(CNTY_CODE, year) %>% 
  left_join(distribution_weights_WU, by = "ag_LL") %>% 
  mutate(APC = sum(pop),
         d2_r = d2*weights,
         birth_cohort = year-ag_LL) %>% 
  dplyr::select(CNTY_CODE, code_prv, year, birth_cohort, ag_LL, d2, d2_r, pop, weights) %>% 
  group_by(CNTY_CODE, birth_cohort) %>% 
  mutate(index = max(1:n())) %>%
  group_split() %>% 
  map(mutate, 
      pop_lag = lag(pop,1),
      pop_change = pop/pop_lag,
      pop_change = if_else(is.na(pop_change),
                           1,
                           pop_change),
      d2_r_cs = cumsum(d2_r),
      d2_r_cs_adjusted = pop_change*cumsum(d2_r)) %>% 
  bind_rows() %>% 
  mutate(cov = d2_r_cs_adjusted/ pop) -> tmp

tmp %>% 
  dplyr::filter(cov > 1) %>% 
  pull(CNTY_CODE) %>% 
  unique() -> to_remove_CNTY_CODE

tmp %>% 
  dplyr::filter(cov > 1) %>% 
  pull(code_prv) %>% 
  unique() -> to_remove_prv

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2011:2016),
                year == 2016) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*weights)) -> cov_2016

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2012:2017),
                year == 2017) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*weights)) -> cov_2017

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2013:2018),
                year == 2018) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*weights)) -> cov_2018

tmp %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove,
                birth_cohort %in% c(2014:2019),
                year == 2019) %>% 
  group_by(CNTY_CODE) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(cov_weighted = sum(cov*weights)) -> cov_2019


cov <- list(cov_2016, cov_2017, cov_2018, cov_2019)

cov %>% 
  map(mutate, 
      code_prf = substr(CNTY_CODE, 1, 4)) %>% 
  map(group_by, code_prf) %>% 
  map(mutate,
      cov_weighted_max_prf = max(cov_weighted),
      cov_weighted_min_prf = min(cov_weighted)) %>% 
  map(group_by, code_prv) %>% 
  map(mutate,
      cov_weighted_max_prv = max(cov_weighted),
      cov_weighted_min_prv = min(cov_weighted)) %>% 
  map(dplyr::select, CNTY_CODE, code_prv, code_prf, year, starts_with("cov_weighted")) %>% 
  map(distinct) %>% bind_rows() -> cov_WU

#### sanity check ####
# cov_WU %>% 
#   mutate(scenario = "Wu et al.") %>% 
#   bind_rows(cov_HE %>% 
#               mutate(scenario = "He et al.")) %>% 
#   bind_rows(cov_baseline %>% 
#               mutate(scenario = "Proportional")) %>%
#   dplyr::filter(code_prv == 11) %>% 
#   ggplot(., aes(x = year, y = cov_weighted, group = interaction(CNTY_CODE, scenario), color = scenario)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~CNTY_CODE)



EV71_Lit <- readRDS("data/EV71_Lit.rds") %>% 
  filter(!(Age_LL_value == 0 & Age_UL_value %in% c(5,6))) %>% 
  filter(index %in% c("109",
                      "588")) %>% 
  group_by(index) %>% 
  group_split()

to_remove <- read_rds(paste0(path_dropbox_github, "to_remove.rds"))

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

#### putting vaccine doses and weights together ####
vac_all %>%
  left_join(pop_all %>%
              dplyr::filter(ag_LL < 6) %>%
              mutate(ag_LL = ag_LL,
                     year = as.numeric(year)) %>%
              pivot_wider(names_from = "ag_LL",
                          values_from = pop) %>%
              mutate(APC = 0.5*`0` + `1` + `2` + `3` + `4` + `5`),
            # left_join(pop_all %>%
            #             dplyr::filter(ag_LL < 6) %>%
            #             mutate(ag_LL = ag_LL,
            #                    year = as.numeric(year)),
            by = c("CNTY_CODE", "code_prv", "year")) %>%
  dplyr::filter(!is.na(APC)) %>% 
  pivot_longer(cols = c(`0`, `1`, `2`, `3`, `4`, `5`),
               names_to = "ag_LL",
               values_to = "pop") %>% 
  group_by(CNTY_CODE, year) %>%
  left_join(distribution_weights_HE %>% mutate(ag_LL = as.character(ag_LL)) %>% rename(weights_HE = weights) %>% dplyr::select(-author), by = "ag_LL") %>% 
  left_join(distribution_weights_WU %>% mutate(ag_LL = as.character(ag_LL)) %>% rename(weights_WU = weights) %>% dplyr::select(-author), by = "ag_LL") %>% 
    mutate(#APC = sum(pop),
    ag_LL = as.numeric(ag_LL), 
    weights_pop = if_else(ag_LL == 0, (pop*0.5)/APC, pop/APC),
    d2_weighted_pop = d2*weights_pop,
    d2_weighted_HE = d2*weights_HE,
    d2_weighted_WU = d2*weights_WU,
        birth_cohort = year-ag_LL) %>% 
  dplyr::select(CNTY_CODE, code_prv, year, birth_cohort, ag_LL, 
                d2, d2_weighted_pop,d2_weighted_HE, d2_weighted_WU, 
                pop, weights_pop, weights_HE, weights_WU) %>%
  rename(pop_by_age_group = pop) %>% 
  group_by(CNTY_CODE, birth_cohort) %>% 
  mutate(index = max(1:n())) %>%
  group_split() %>% 
  map(arrange, year) %>% 
  map(mutate, 
      pop_lag = lag(pop_by_age_group,1),
      pop_change = pop_by_age_group/pop_lag,
      pop_change = if_else(is.na(pop_change),
                           1,
                           pop_change),
      d2_weighted_pop_cs = cumsum(d2_weighted_pop),
      d2_weighted_HE_cs = cumsum(d2_weighted_HE),
      d2_weighted_WU_cs = cumsum(d2_weighted_WU),
      d2_weighted_pop_cs_adjusted = pop_change*cumsum(d2_weighted_pop),
      d2_weighted_HE_cs_adjusted = pop_change*cumsum(d2_weighted_HE),
      d2_weighted_WU_cs_adjusted = pop_change*cumsum(d2_weighted_WU)) %>% 
  bind_rows() %>% 
  mutate(cov_by_age_pop = d2_weighted_pop_cs_adjusted/ pop_by_age_group,
         cov_by_age_HE = d2_weighted_HE_cs_adjusted/ pop_by_age_group,
         cov_by_age_WU = d2_weighted_WU_cs_adjusted/ pop_by_age_group) %>% 
  # we need to make sure we only include eligible children here
  dplyr::filter(#!CNTY_CODE %in% to_remove,
                (birth_cohort %in% c(2011:2016) & year == 2016) |
                  (birth_cohort %in% c(2012:2017) & year == 2017) | 
                  (birth_cohort %in% c(2013:2018) & year == 2018) |
                  (birth_cohort %in% c(2014:2019) & year == 2019))  %>% 
  group_by(CNTY_CODE, year) %>% 
  mutate(covered_weighted_pop = sum(d2_weighted_pop_cs),
         covered_weighted_HE = sum(d2_weighted_HE_cs),
         covered_weighted_WU = sum(d2_weighted_WU_cs),
         # we need to make sure that when we calculate eligible children, only
         # 50% of the 0 year old age group is included while calculating 
         marker_0 = if_else(ag_LL == 0, 0.5, 1),
         pop_eligible = sum(marker_0*pop_by_age_group),
         coverage_weighted_pop = covered_weighted_pop/pop_eligible,
         coverage_weighted_HE = covered_weighted_HE/pop_eligible,
         coverage_weighted_WU = covered_weighted_WU/pop_eligible, 
         code_prf = substr(CNTY_CODE, 1, 4)) %>% 
  group_by(code_prf, year) %>% 
  mutate(coverage_weighted_pop_prf_max = max(coverage_weighted_pop, na.rm = T),
         coverage_weighted_HE_prf_max = max(coverage_weighted_HE, na.rm = T),
         coverage_weighted_WU_prf_max = max(coverage_weighted_WU, na.rm = T),
         coverage_weighted_pop_prf_min = min(coverage_weighted_pop, na.rm = T),
         coverage_weighted_HE_prf_min = min(coverage_weighted_HE, na.rm = T),
         coverage_weighted_WU_prf_min = min(coverage_weighted_WU, na.rm = T)) %>% 
  group_by(code_prv, year) %>% 
  mutate(coverage_weighted_pop_prv_max = max(coverage_weighted_pop, na.rm = T),
         coverage_weighted_HE_prv_max = max(coverage_weighted_HE, na.rm = T),
         coverage_weighted_WU_prv_max = max(coverage_weighted_WU, na.rm = T),
         coverage_weighted_pop_prv_min = min(coverage_weighted_pop, na.rm = T),
         coverage_weighted_HE_prv_min = min(coverage_weighted_HE, na.rm = T),
         coverage_weighted_WU_prv_min = min(coverage_weighted_WU, na.rm = T)) %>% 
  ungroup() -> tmp
          
write_rds(tmp, paste0(path_dropbox_github, "coverage_results_20250129.rds"))

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
# tab %>% 
#   left_join(pop_all %>%
#               dplyr::filter(ag_LL < 6) %>% 
#               mutate(ag_LL = ag_LL,
#                      year = as.numeric(year)),
#             by = c("CNTY_CODE", "code_prv", "year")) %>%
#   group_by(CNTY_CODE, year) %>% 
#   mutate(APC = sum(pop),
#          pop_r = pop/APC,
#          d2_r = d2*pop_r,
#          birth_cohort = year-ag_LL) %>% 
#   dplyr::select(CNTY_CODE, year, ag_LL, pop_r) %>% 
#   left_join(distribution_weights_HE, by = "ag_LL") %>% 
#   dplyr::select(-author) %>% 
#   rename(weights_HE = weights) %>% 
#   left_join(distribution_weights_WU, by = "ag_LL") %>% 
#   dplyr::select(-author) %>% 
#   rename(weights_WU = weights) %>% 
#   dplyr::filter(year == 2017) %>% 
#   mutate(ag_LL = factor(ag_LL)) -> weights_save

# write_rds(weights_save, paste0(path_dropbox_github, "weights_compare_20240305.rds"))

# pivot_longer(cols = c("pop_r", "weights_HE", "weights_WU")) %>% 
#   ggplot(., aes(x = ag_LL, y = value, group = interaction(CNTY_CODE, name), color = name)) +
#   geom_line() +
#   geom_point()


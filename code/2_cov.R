EV71_Lit <- readRDS("data/EV71_Lit.rds") %>% 
  filter(!(Age_LL_value == 0 & Age_UL_value %in% c(5,6))) %>% 
  filter(index %in% c("109",
                      "588")) %>% 
  group_by(index) %>% 
  group_split()

data %>% 
  left_join(POP_R_5 %>% 
              mutate(year = paste0("perc_5_",year)) %>% 
              pivot_wider(names_from = year,
                          values_from = perc_5),
            by = c("CNTY_CODE")) %>% 
  mutate(active_dose_2017 = d2_2016*(1-perc_5_2016) + d2_2017,
         active_dose_2018 = active_dose_2017*(1-perc_5_2017) + d2_2018,
         cov_APC_2016 = d2_2016/APC_2016,
         cov_APC_2017 = active_dose_2017/APC_2017,
         cov_APC_2018_lm = active_dose_2018/APC_2018_lm,
         cov_APC_2018_con = active_dose_2018/APC_2018_con,
         cov_YB_2016 = d2_2016/YB_2016,
         cov_YB_2017 = active_dose_2017/YB_2017,
         cov_YB_2018 = active_dose_2018/YB_2018) %>% 
  dplyr::select(CNTY_CODE,
                starts_with("cov")) %>% 
  pivot_longer(starts_with("cov")) %>% 
  separate(name, into = c("seg", "denom_source", "year", "ext")) %>% 
  dplyr::select(-seg) -> coverage

#
EV71_Lit[[1]] %>% 
  dplyr::select(Age_LL_value, denominator, numerator, CNTY_CODE) %>% 
  add_row(Age_LL_value = c(0,4, 5),
          denominator = 420,
          CNTY_CODE = "510722",
          numerator = c(0, 53, 53)) %>% 
  mutate(cov = numerator/denominator) %>% 
  arrange(Age_LL_value) %>% 
  left_join(pop %>% dplyr::filter(CNTY_CODE == "510722", year == 2018, ag_LL %in% 0:5),
            by = c("CNTY_CODE",
                   "Age_LL_value" = "ag_LL")) %>% 
  mutate(year = c(NA, NA, NA, 2018, 2017, 2016),
         doses = tot*cov,
         perc_5 = doses/sum(doses))  %>% 
  dplyr::filter(!is.na(year)) %>% 
  dplyr::select(year, perc_5) %>% 
  mutate(year = paste0("perc_5_", year)) %>% 
  pivot_wider(names_from = year, values_from = perc_5) -> perc5_alt_1

data %>% 
  bind_cols(perc5_alt_1) %>% 
  mutate(active_dose_2017 = d2_2016*(1-perc_5_2016) + d2_2017,
         active_dose_2018 = active_dose_2017*(1-perc_5_2017) + d2_2018,
         cov_APC_2016 = d2_2016/APC_2016,
         cov_APC_2017 = active_dose_2017/APC_2017,
         cov_APC_2018_lm = active_dose_2018/APC_2018_lm,
         cov_APC_2018_con = active_dose_2018/APC_2018_con,
         cov_YB_2016 = d2_2016/YB_2016,
         cov_YB_2017 = active_dose_2017/YB_2017,
         cov_YB_2018 = active_dose_2018/YB_2018) %>% 
  dplyr::select(CNTY_CODE,
                starts_with("cov")) %>% 
  pivot_longer(starts_with("cov")) %>% 
  separate(name, into = c("seg", "denom_source", "year", "ext")) %>% 
  dplyr::select(-seg) -> coverage_alt_1

EV71_Lit[[2]] %>% 
  dplyr::select(Age_LL_value, denominator, numerator, CNTY_CODE) %>% 
  mutate(perc_5 = numerator/denominator) %>%
  add_row(.[2,] %>% mutate(Age_LL_value = 2)) %>% 
  mutate(denominator = if_else(Age_LL_value %in% 1:2, 0.5*denominator, denominator),
         numerator = if_else(Age_LL_value %in% 1:2, 0.5*numerator, numerator)) %>% 
  add_row(.[3,] %>% mutate(Age_LL_value = 4)) %>% 
  add_row(.[3,] %>% mutate(Age_LL_value = 5))%>% 
  mutate(denominator = if_else(Age_LL_value %in% 3:5, 0.33*denominator, denominator),
         numerator = if_else(Age_LL_value %in% 3:5, 0.33*numerator, numerator)) %>% 
  mutate(cov = numerator/denominator) %>% 
  arrange(Age_LL_value) %>%
  left_join(pop %>% dplyr::filter(CNTY_CODE == "330205", year == 2018, ag_LL %in% 0:5),
            by = c("CNTY_CODE",
                   "Age_LL_value" = "ag_LL")) %>% 
  mutate(doses = tot*cov,
         perc_5 = doses/sum(doses)) %>% 
  mutate(year = c(NA, NA, NA, 2018, 2017, 2016)) %>% 
  dplyr::filter(!is.na(year)) %>% 
  dplyr::select(year, perc_5) %>% 
  mutate(year = paste0("perc_5_", year)) %>% 
  pivot_wider(names_from = year, values_from = perc_5) -> perc5_alt_2

data %>% 
  bind_cols(perc5_alt_2) %>% 
  mutate(active_dose_2017 = d2_2016*(1-perc_5_2016) + d2_2017,
         active_dose_2018 = active_dose_2017*(1-perc_5_2017) + d2_2018,
         cov_APC_2016 = d2_2016/APC_2016,
         cov_APC_2017 = active_dose_2017/APC_2017,
         cov_APC_2018_lm = active_dose_2018/APC_2018_lm,
         cov_APC_2018_con = active_dose_2018/APC_2018_con,
         cov_YB_2016 = d2_2016/YB_2016,
         cov_YB_2017 = active_dose_2017/YB_2017,
         cov_YB_2018 = active_dose_2018/YB_2018) %>% 
  dplyr::select(CNTY_CODE,
                starts_with("cov")) %>% 
  pivot_longer(starts_with("cov")) %>% 
  separate(name, into = c("seg", "denom_source", "year", "ext")) %>% 
  dplyr::select(-seg) -> coverage_alt_2

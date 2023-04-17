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



data %>%
  dplyr::select(-starts_with("APC")) %>%
  pivot_longer(starts_with("d2")) %>%
  separate(name, into = c("dose", "year")) %>%
  dplyr::select(-dose) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(CNTY_CODE) %>% group_split() %>%
  map(filter, !is.na(value)) %>%
  bind_rows() %>%
  group_by(CNTY_CODE, year) %>%
  summarise(value = sum(value)) %>%
  group_by(CNTY_CODE) %>%
  mutate(rk = rank(value),
         rk_max = max(rk)) -> data_tmp

data_tmp %>%
  group_by(CNTY_CODE) %>% group_split() %>% map(arrange, year) %>%
  map(mutate, doses_cum = cumsum(value)) %>%
  map(rename, doses_annual = value) %>%
  bind_rows() -> data_tmp

data_tmp %>%
  filter(rk == rk_max) %>%
  dplyr::select(-starts_with("rk")) %>%
  ungroup %>%
  left_join(POP_R_5 %>%
              dplyr::filter(year == 2017) %>%
              dplyr::select(-year),
            by = c("CNTY_CODE")) %>%
  left_join(APC_YB, by = c("year", "CNTY_CODE")) %>%
  mutate(cov = (1-perc_5)*doses_cum/APC,
         prv_no = substr(CNTY_CODE,1,2)) -> coverage

# shape %>%
#   filter(lvl == "cty") %>%
#   left_join(coverage,
#             by = "CNTY_CODE") %>%
#   ggplot(., aes(fill = cov)) +
#   geom_sf() +
#   scale_fill_viridis()



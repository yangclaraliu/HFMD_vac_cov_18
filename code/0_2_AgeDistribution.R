lit <- read_rds("data/EV71_Lit.rds")

lit %>% 
  filter(!(Age_LL_value == 0 & Age_UL_value %in% c(5,6))) %>% 
  ggplot(., aes(x = Age_LL_value, y = cov)) +
  geom_point() +
  geom_segment(aes(xend = Age_UL_value, yend = cov)) +
  facet_wrap(~index)

data %>% 
  filter(CNTY_CODE == "510723")

unique(lit$第一作者)
lit %>% dplyr::filter("第一作者" == "何小君")

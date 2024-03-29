pop_all %>% 
  mutate(birth_cohort = as.numeric(year) - ag_LL) %>% 
  dplyr::filter(ag_LL <= 5) %>% 
  dplyr::select(-ag_LL) %>% 
  group_by(birth_cohort) %>% group_split() %>% 
  map(pivot_wider,
      names_from = year,
      values_from = pop) %>% 
  bind_rows() %>% 
  mutate(r_diff_2017 = `2017`/`2016` - 1,
         r_diff_2018 = `2018`/`2017` - 1,
         r_diff_2019 = `2019`/`2018` - 1) %>% 
  group_by(code_prv, birth_cohort) %>% 
  mutate(sum_2016 = sum(`2016`),
         sum_2017 = sum(`2017`),
         sum_2018 = sum(`2018`),
         sum_2019 = sum(`2019`),
         r_diff_prv_2017 = sum_2017/sum_2016,
         r_diff_prv_2018 = sum_2018/sum_2017,
         r_diff_prv_2019 = sum_2019/sum_2018) %>% 
  .[complete.cases(.),] -> pop_to_plot

pop_to_plot %>% 
  dplyr::select(CNTY_CODE, code_prv, starts_with("r_diff_20")) %>% 
  pivot_longer(cols = starts_with("r_diff_20")) %>% 
  mutate(year = parse_number(name),
         birth_cohort = factor(birth_cohort)) %>%
  group_by(year, birth_cohort) %>% 
  mutate(median_all = median(value)) %>% 
  group_by(year, birth_cohort, code_prv) %>% 
  mutate(median_prv = median(value)) -> p_tab

ggplot(p_tab, aes(x = value, group = birth_cohort)) +
  geom_density(fill = "black", alpha = 0.4, linetype = 2, adjust = 2) +
  geom_vline(aes(xintercept = median_all), linetype = 2) +
  geom_density(data = p_tab %>% dplyr::filter(code_prv == "11"),
               aes(x = value, group = birth_cohort), adjust = 2) +
  geom_vline(aes(xintercept = median_prv), color = "black", data = p_tab %>% dplyr::filter(code_prv == "11")) +
  facet_grid(birth_cohort~year) +
  custom_theme +
  labs(x = "Between-year relative changes in population sizes",
       y = "Density") +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Birth cohort", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Calendar year", breaks = NULL, labels = NULL)) +
  geom_vline(xintercept = 0, color = "red") -> p_save

data.frame(loc = c("Beijing only", "All data available"),
           value = c(1,2),
           colors = factor(c("Baseline - No Change", "Baseline - No change"))) %>% 
  ggplot(., aes(x = loc, y = value)) +
  geom_vline(aes(xintercept = value, linetype = loc, color = colors)) +
  scale_linetype_manual(values = c(2,1)) +
  # scale_color_manual(values = c("red","blue")) +
  labs(linetype = "",
       color = "") +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) -> fake_plot_to_get_legend

get_legend(fake_plot_to_get_legend) -> p_legend

plot_grid(p_save, p_legend,
          ncol = 1, rel_heights = c(30,1)) 

ggsave("figs/fig2_v2.png",
       width = 10,
       height = 8)

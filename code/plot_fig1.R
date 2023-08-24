pop_all %>% group_by(CNTY_CODE, code_prv, year) %>% summarise(pop = sum(pop)) %>% 
  group_by(year) %>% group_split() %>% 
  map(right_join, shape_cty %>% 
        data.table() %>% 
        dplyr::select(CNTY_CODE, code_prv),
      by = c("CNTY_CODE", "code_prv")) %>% 
  map(left_join, tab %>% 
        dplyr::select(CNTY_CODE, code_prv, year, d2) %>% 
        mutate(year = as.character(year)),
      by = c("CNTY_CODE", "code_prv", "year")) %>% 
  map(mutate, data_absent = is.na(d2)) %>%
  map(mutate, label_region = substr(CNTY_CODE, 1, 1)) %>% 
  map(group_by, label_region, data_absent) %>% 
  map(summarise, pop = sum(pop, na.rm = T), county_counts = n()) %>% 
  map(group_by, label_region) %>% 
  map(mutate, pop_tot = sum(pop), county_tot = sum(county_counts),
      pop_cov = pop/pop_tot, county_cov = county_counts/county_tot) %>% 
  bind_rows(.id = "year") %>% 
  mutate(year = as.numeric(year) + 2015) -> data_coverage
  
data_coverage %<>% 
  mutate(before_2019 = if_else(year < 2019,
                               "Before 2019",
                               "2019"),
         before_2019 = factor(before_2019,
                              levels = c("Before 2019", "2019"))) %>% 
  group_by(before_2019, data_absent, label_region) %>% 
  summarise(county_counts = mean(county_counts),
            county_tot = mean(county_tot))

lm(p_risk ~ label_region, data = tab) |> summary()

tab %>% 
  dplyr::select(CNTY_CODE, code_prv, year, d2) %>% 
  mutate(year = as.character(year),
         label_region = substr(CNTY_CODE, 1, 1)) %>% 
  dplyr::filter(label_region == 2,
                year == 2016) 

APC %>%
  mutate(label_region = factor(label_region,
                               levels = 1:6,
                               labels = labels_region$names_region)) %>% 
  ggplot(., aes(group = label_region, y = p_risk, color = label_region, x = label_region)) +
  geom_boxplot() +
  scale_color_manual(values = colors_region) +
  geom_hline(aes(yintercept = mean(p_risk_national)), linetype = 2, size = 1.5, color = "black") +
  custom_theme +
  theme(legend.position = "none") +
  labs(x = "Region",
       y = "Proportion of population\nunder 5 years of age") -> p1

data_coverage %>% 
  dplyr::select(before_2019, label_region, data_absent, county_counts, county_tot) %>% 
  dplyr::filter(data_absent == F) %>% 
  mutate(label_region = factor(label_region,
                               levels = 6:1,
                               labels = rev(labels_region$names_region))) %>% 
  ggplot(aes(x = label_region)) +
  geom_bar(aes(y = county_tot, color = label_region), fill = NA,stat = "identity") +
  geom_bar(aes(y = county_counts, color = label_region, fill = label_region), stat = "identity") +
  scale_color_manual(values = rev(colors_region)) +
  scale_fill_manual(values = rev(colors_region)) +
  custom_theme +
  theme(legend.position = "none") +
  labs(x = "Region",
       y = "Total number of counties\nand counties with available data") +
  facet_wrap(~before_2019) +
  coord_flip() -> p2

(get_legend(p1 + 
              theme(legend.position = "top",
                    legend.text = element_text(size = 16)) + 
              labs(color = "", fill = "") + 
              guides(color=guide_legend(nrow=1,byrow=T)))) -> p_legend

plot_grid(p_legend, plot_grid(p1, 
                              p2, 
                              nrow = 2,
                              align = "hv",
                              axis = "l"), rel_heights = c(1,10), ncol = 1) -> p_save

ggsave("figs/fig1_v2.png", p_save, width = 10, height = 12)


tmp <- labels_region

cov %>% 
  mutate(labels_region = substr(CNTY_CODE, 1, 1) %>% as.numeric) %>% 
  left_join(labels_region %>% 
              mutate(names_region = factor(names_region)), 
            by = "labels_region") %>% 
  ungroup %>% 
  dplyr::select(CNTY_CODE, code_prv, code_prf, year, starts_with("coverage"), names_region) %>% 
  distinct() %>% 
  mutate(year = factor(year),
         names_region = factor(names_region,
                               levels = tmp$names_region)) %>% 
  dplyr::filter(coverage_weighted_pop <= 1,
                coverage_weighted_HE <= 1,
                coverage_weighted_WU <= 1) -> p_tab

p_tab %>% 
  group_by(year) %>% 
  summarise(min = min(coverage_weighted_pop),
            median = median(coverage_weighted_pop),
            max = max(coverage_weighted_pop),
            national_Q1 = quantile(coverage_weighted_pop, 0.25, na.rm = T),
            national_Q2 = quantile(coverage_weighted_pop, 0.5, na.rm = T),
            national_Q3 = quantile(coverage_weighted_pop, 0.75, na.rm = T)) 

p_tab %>% 
  group_by(year, names_region) %>% 
  summarise(min = min(coverage_weighted_pop),
            md = median(coverage_weighted_pop),
            max = max(coverage_weighted_pop),
            national_Q1 = quantile(coverage_weighted_pop, 0.25, na.rm = T),
            national_Q2 = quantile(coverage_weighted_pop, 0.5, na.rm = T),
            national_Q3 = quantile(coverage_weighted_pop, 0.75, na.rm = T)) %>% 
  dplyr::filter(year == 2018) %>% View()

ggplot(p_tab) +
  # geom_rect(xmin = 0.5, xmax = 3.5, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.1) +
  geom_boxplot(aes(x = year, y = coverage_weighted_pop, color = names_region),
               outlier.shape = NA) +
  # coord_cartesian(ylim = c(0, 1)) +
  scale_color_manual(values = colors_region) +
  custom_theme+ 
  theme(legend.text = element_text(size = 16),
        legend.position = "none",
        axis.text.x = element_text(angle = 90),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.background = element_rect(colour = "black", fill = NA)) + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(color = "", fill = "", x = "Year", y = "Coverage of EV-71\nMono-valent vaccine") + 
  facet_wrap(~names_region, nrow = 2) +
  guides(color=guide_legend(nrow = 1,
                            byrow=T)) -> p1

p_legend <- get_legend(p1 + theme(legend.position = "top",
                                  legend.background = element_rect(fill = "white",
                                                                   colour = "white"),
                                  legend.justification = "centre",
                                  legend.box.just = "centre"))


p_save <- plot_grid(p_legend, p1, ncol = 1,
  align = "v", axis = "lr",
  rel_heights = c(2, 10)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white"))

ggsave("figs/fig2_v4.png", p_save, width = 10,  height = 10)

p_tab %>% 
  mutate(diff_coverage_weighted_pop_prf = coverage_weighted_pop_prf_max - coverage_weighted_pop_prf_min,
         diff_coverage_weighted_pop_prv = coverage_weighted_pop_prv_max - coverage_weighted_pop_prv_min) %>% 
  group_by(year, code_prf) %>% 
  mutate(var_coverage_weighted_pop_prf = var(coverage_weighted_pop, na.rm = T)) %>% 
  group_by(year, code_prv) %>% 
  mutate(var_coverage_weighted_pop_prv = var(coverage_weighted_pop, na.rm = T),
         diff_coverage_weighted_pop_prv_median = median(diff_coverage_weighted_pop_prv, na.rm = T)) %>% 
  group_by(year, names_region) %>% 
  mutate(diff_coverage_weighted_pop_region_median = median(diff_coverage_weighted_pop_prf, na.rm = T),
         var_coverage_weighted_pop_region_median = median(var_coverage_weighted_pop_prf, na.rm = T)) %>% 
  left_join(shape_prv, by = "code_prv") -> p_tab2 
  
p_tab %>% 
  dplyr::filter(names_region %in% c("Huabei\n(~North)", "Huadong\n(~East)"),
                year == 2019) %>% 
  group_by(names_region) %>% 
  summarise(Q1 = quantile(coverage_weighted_pop,  0.5))


p_tab2 %>% 
  dplyr::filter(year == 2019) %>% 
  ggplot(., aes(x = NAME_EN, y = var_coverage_weighted_pop_prf, color = names_region)) +
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(breaks = labels_region$names_region, values = colors_region) +
  facet_grid(~names_region, space = "free", scales = "free") +
  geom_hline(aes(yintercept = var_coverage_weighted_pop_region_median, color = names_region),
             linetype = 2) +
  custom_theme + 
  coord_cartesian(y = c(0, 0.04)) +
  theme(legend.text = element_text(size = 16),
        legend.position = "none",
        axis.text.x = element_text(angle = 90),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.text = element_blank(),
        strip.background = element_rect(colour = "black", fill = NA)) +
  labs(y = "Within prefecture variance in vaccine\ncoverage, presented by province (2018)",
       x = "Province or provincial level cities") + 
  guides(color=guide_legend(nrow=1,byrow=T)) -> p2

p_tab2 %>% 
  dplyr::select(year, names_region, 
                var_coverage_weighted_pop_region_median) %>% 
  distinct() %>% 
  dplyr::filter(year == 2019)

p_tab2 %>% 
  dplyr::select(year, names_region, 
                diff_coverage_weighted_pop_region_median) %>% 
  distinct() %>% 
  dplyr::filter(year == 2019) %>% View()

p_tab2 %>% 
  dplyr::filter(year == 2018) %>% 
  ggplot(., aes(x = NAME_EN, y = diff_coverage_weighted_pop_prf, color = names_region)) +
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(breaks = labels_region$names_region, values = colors_region) +
  facet_grid(~names_region, space = "free", scales = "free") +
  geom_hline(aes(yintercept = diff_coverage_weighted_pop_region_median, color = names_region),
             linetype = 2) +
  custom_theme + 
  theme(legend.text = element_text(size = 16),
        legend.position = "none",
        axis.text.x = element_text(angle = 90),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.text = element_blank(),
        strip.background = element_rect(colour = "black", fill = NA)) +
  labs(y = "Within prefecture absolute difference (i.e. max - min)\nin vaccine coverage, presented by province (2018)",
       x = "Province or provincial level cities") + 
  guides(color=guide_legend(nrow=1,byrow=T)) -> p3

p_save <- plot_grid(p_legend, 
                    p2, p3, ncol = 1, 
                    align = "v", axis = "lr",
                    rel_heights = c(2,
                                    10,10)) +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white"))
ggdraw() +
  draw_plot(p_save) +
  draw_label(label = "(A)", x = 0.05, y = 0.95, size = 14, fontface = "bold") +
  draw_label(label = "(B)", x = 0.05, y = 0.5, size = 14, fontface = "bold") -> p_save_w_labels


ggsave("figs/fig3_v4.png", p_save_w_labels, width = 15, height = 12)

p_tab %>% 
  mutate(diff_prf = cov_weighted_max_prf - cov_weighted_min_prf,
         diff_prv = cov_weighted_max_prv - cov_weighted_min_prv) %>% 
  dplyr::filter(year == 2018) %>% 
  dplyr::select(CNTY_CODE, code_prv, code_prv, names_region, diff_prf, diff_prv, year, labels_region) %>% 
  distinct() %>% 
  group_by(code_prv) %>% 
  mutate(diff_prv_md = median(diff_prf)) %>% 
  group_by(labels_region) %>% 
  mutate(diff_region_md = mean(diff_prv_md),
         names_region = factor(names_region)) %>% 
  left_join(shape_prv, by = "code_prv") %>% 
  dplyr::select(-geometry) %>% 
  mutate(code_prf = substr(CNTY_CODE, 1,4)) %>% 
  dplyr::select(code_prf, diff_prf) %>% 
  distinct() %>% 
  group_by(labels_region) %>% 
  summarise(Q1 = quantile(diff_prf, 0.25),
            Q2 = quantile(diff_prf, 0.5),
            Q3 = quantile(diff_prf, 0.75))

p_tab %>% 
  dplyr::filter(year == 2018) %>% 
  mutate(diff_prf = cov_weighted_max_prf - cov_weighted_min_prf,
         diff_prv = cov_weighted_max_prv - cov_weighted_min_prv) %>% 
  dplyr::select(CNTY_CODE, code_prv, code_prv, names_region, diff_prf, diff_prv, year, labels_region) %>% 
  distinct() %>% 
  group_by(code_prv) %>% 
  mutate(diff_prv_md = median(diff_prf)) %>% 
  group_by(labels_region) %>% 
  mutate(diff_region_md = mean(diff_prv_md),
         names_region = factor(names_region)) %>% 
  left_join(shape_prv, by = "code_prv") %>% 
  dplyr::select(names_region, diff_region_md) %>% 
  distinct

tmp <- labels_region

cov$baseline %>% 
  mutate(labels_region = substr(CNTY_CODE, 1, 1) %>% as.numeric) %>% 
  left_join(labels_region %>% 
              mutate(names_region = factor(names_region)), 
            by = "labels_region") %>% 
  ungroup %>% 
  mutate(year = factor(year),
         names_region = factor(names_region,
                               levels = tmp$names_region)) -> p_tab

ggplot(p_tab) +
  # geom_rect(xmin = 0.5, xmax = 3.5, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.1) +
  facet_wrap(~names_region, nrow = 1) +
  geom_boxplot(aes(x = year, y = cov_weighted, color = names_region),
               outlier.shape = NA) +
  coord_cartesian(ylim = c(0,0.6)) +
  scale_color_manual(values = colors_region) +
  custom_theme+ 
  theme(legend.text = element_text(size = 16),
        legend.position = "none",
        axis.text.x = element_text(angle = 90),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.background = element_rect(colour = "black", fill = NA)) + 
  labs(color = "", fill = "", x = "Year", y = "Coverage of EV-71\nMono-valent vaccine") + 
  guides(color=guide_legend(nrow=1,byrow=T)) -> p1

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
  ggplot(., aes(x = NAME_EN, y = diff_prf, color = names_region)) +
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(breaks = labels_region$names_region, values = colors_region) +
  facet_grid(~names_region, space = "free", scales = "free") +
  geom_hline(aes(yintercept = diff_region_md, color = names_region),
             linetype = 2) +
  custom_theme + 
  theme(legend.text = element_text(size = 16),
        legend.position = "none",
        axis.text.x = element_text(angle = 90),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.text = element_blank(),
        strip.background = element_rect(colour = "black", fill = NA)) +
  labs(y = "Within prefecture\nabsolutedifferences in\nvaccine coverage (2018)",
       x = "Province or provincial level cities") + 
  guides(color=guide_legend(nrow=1,byrow=T)) -> p2

p_save <- plot_grid(p1, p2, ncol = 1, align = "v", axis = "lr")

ggsave("figs/fig3_v2.png", p_save, width = 15, height = 8)

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
  dplyr::filter(NAME_EN == "Guangdong") %>% 
  dplyr::select(-geometry) %>% 
  mutate(code_prf = substr(CNTY_CODE, 1,4)) %>% 
  dplyr::select(code_prf, diff_prf) %>% 
  distinct() %>% 
  pull(diff_prf) %>% 
  summary()
  

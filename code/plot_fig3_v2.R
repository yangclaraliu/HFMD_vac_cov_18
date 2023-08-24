x <- labels_region$names_region

cov_baseline %>% 
  mutate(labels_region = substr(CNTY_CODE, 1, 1) %>% as.numeric) %>% 
  left_join(labels_region %>% 
              mutate(names_region = factor(names_region, levels = x)), 
            by = "labels_region") %>% 
  ungroup %>% 
  mutate(year = factor(year)) -> p_tab

ggplot(p_tab) +
  geom_boxplot(aes(x = year, y = cov_weighted, color = names_region),
               outlier.shape = NA) +
  facet_wrap(~names_region, nrow = 1) +
  coord_cartesian(ylim = c(0,1)) +
  scale_color_manual(values = colors_region) +
  custom_theme+ 
  theme(legend.text = element_text(size = 16),
        legend.position = "none",
        axis.text.x = element_text(angle = 90),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.background = element_rect(colour = "black", fill = NA)) + 
  labs(color = "", fill = "", x = "Year", y = "Coverage of EV-71 Mono-valent vaccine") + 
  guides(color=guide_legend(nrow=1,byrow=T)) -> p_save

ggsave("figs/fig3_v2.png", p_save, width = 10, height = 7)

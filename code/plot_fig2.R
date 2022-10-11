data[,1:4] |> 
  pivot_longer(starts_with("d2")) |> 
  mutate(name = gsub("d2_","",name),
         prv = substr(CNTY_CODE, 1, 2)) |> 
  left_join(prv_list, by = "prv") |> 
  left_join(labels_region, by = "region") %>%
  ggplot(.) +
  geom_boxplot(aes(x = name, y = value, color = region_label),
               outlier.shape = NA) +
  facet_wrap(~region_label, nrow = 1) +
  coord_cartesian(ylim = c(0,12000)) +
  scale_color_manual(values = colors_region) +
  custom_theme+ 
  theme(legend.text = element_text(size = 16),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.background = element_rect(colour = "black", fill = NA)) + 
  labs(color = "", fill = "", x = "Year", y = "Number of 2nd dose administered") + 
  guides(color=guide_legend(nrow=1,byrow=T)) -> p_save

ggsave("figs/fig2.png", p_save, width = 10, height = 7)

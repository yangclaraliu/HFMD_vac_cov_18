data %>% 
  dplyr::select(CNTY_CODE, starts_with("d2")) %>% 
  filter(!is.na(d2_2018) & !is.na(d2_2017) & !is.na(d2_2016)) %>% 
  mutate(tot = d2_2016 + d2_2017 + d2_2018,
         d2_2016 = d2_2016/tot,
         d2_2017 = d2_2017/tot,
         d2_2018 = d2_2018/tot,
         diff_stable = abs(d2_2017 - d2_2018),
         k = abs(d2_2016 - 0.33) + abs(d2_2017 - 0.33) + abs(d2_2018 - 0.33),
         
         type = case_when(((d2_2017 >= d2_2016) | (d2_2017 < d2_2016 & abs(d2_2016 - d2_2017) < 0.1)) & 
                            ((d2_2018 >= d2_2017) | (d2_2018 < d2_2017 & abs(d2_2018 - d2_2017) < 0.1)) ~ "Increasing/ Stablising Trends",
                          TRUE ~ "Other Trends")
         ) %>% 
  filter(tot > 0) -> tmp

tmp %>% 
  group_by(type) %>%
  tally() %>% 
  mutate(tot = sum(n),
         p = n/tot)
  summarise(s1 = quantile(k, 0.25),
            s1 = quantile(k, 0.5))
# wss <- map_dbl(1:20, ~{kmeans(select(tmp, -CNTY_CODE), ., nstart=25,iter.max = 50)$tot.withinss})
# n_clust <- 1:20
# elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))
# ggplot(elbow_df) +
#   geom_line(aes(y = wss, x = n_clust), colour = "#82518c") 
# clusters <- kmeans(select(tmp, -CNTY_CODE), centers = 5)
# tmp[,"cluster"] <- clusters$cluster

tmp %>% 
  dplyr::select(-tot) %>% 
  pivot_longer(cols = starts_with("d2")) %>% 
  separate(name, into = c("dose", "year")) %>% 
  mutate(year = factor(year),
         l = if_else(k < 0.75, "stable", "steep")) %>% 
  ggplot(., aes(x = year, y = value, group = CNTY_CODE, color = k)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~type) +
  custom_theme +
  labs(x = "Year", y = "Proportions", color = "Magnitude of Deviation from Uniform Distribution (k)") +
  theme(legend.key.width = unit(3, 'cm')) +
  scale_color_viridis(option = "plasma") -> p1

tmp %>% 
  dplyr::select(-tot) %>% 
  pivot_longer(cols = starts_with("d2")) %>% 
  separate(name, into = c("dose", "year")) %>% 
  mutate(year = factor(year),
         l = if_else(k < 0.75, "stable", "steep")) %>% 
  ggplot(., aes(x = k)) +
  geom_histogram(color = "black", fill = "grey90") +
  facet_wrap(~type) +
  custom_theme +
  labs(x = "Magnitude of Deviation from Uniform Distribution (k)",
       y = "Counts") -> p2

p <- plot_grid(p1, p2, ncol = 1)
ggsave("figs/pfigures_1.png", width = 12, height = 10)

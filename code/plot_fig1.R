pop |> 
  data.table() |> 
  dplyr::filter(age_group %in% paste0(0:4,"-"),
                year == 2018) |> 
  group_by(CNTY_CODE) |> 
  summarise(under5 = sum(tot)) |> 
  left_join(pop |> 
              data.table() |> 
              dplyr::filter(year == 2018) |> 
              group_by(CNTY_CODE) |> 
              summarise(tot = sum(tot)),
            by = "CNTY_CODE"
  ) |> 
  mutate(p_under5 = under5/tot,
         prv = substr(CNTY_CODE,1,2)) |> 
  left_join(prv_list, by = "prv") |> 
  left_join(labels_region, by = "region") |> 
  group_by(region_label) |> 
  filter(!is.na(p_under5) & !is.na(region)) |> 
  ungroup() |> 
  mutate(country_pop = sum(tot),
         country_under5 = sum(under5),
         p_under5_national = country_under5/country_pop) -> risky_pop

pop |> 
  data.table() |> 
  dplyr::filter(year == 2018) |> 
  group_by(CNTY_CODE) |> 
  summarise(tot = sum(tot))|> 
  mutate(prv = substr(CNTY_CODE, 1, 2)) |> 
  left_join(prv_list, by = "prv") |> 
  left_join(labels_region, by = "region") |> 
  mutate(data_presence = CNTY_CODE %in% data$CNTY_CODE) |> 
  group_by(region_label, data_presence) |> 
  summarise(tot = sum(tot)) |> 
  filter(!is.na(region_label)) |> 
  group_by(region_label) |> 
  mutate(all = sum(tot)) |> 
  filter(data_presence == T) |> 
  mutate(p_presence = tot/all) -> data_covered_pop


data[,c("CNTY_CODE", "d2_2018")] |> 
  right_join(shape, by = "CNTY_CODE") |> 
  filter(prv %in% prv_list$prv) |> 
  left_join(prv_list, by = "prv") |> 
  left_join(labels_region, by = "region") |> 
  filter(lvl == "cty") |> 
  mutate(data_presence = !is.na(d2_2018),
         location_presence = TRUE) |> 
  group_by(region_label) |> 
  summarise(data_presence = sum(data_presence),
            location_presence = sum(location_presence)) -> data_covered_county
  
data[,c("CNTY_CODE", "d2_2018")] |> 
  mutate(prv = substr(CNTY_CODE, 1, 2)) |> 
  dplyr::select(prv) |> distinct() |> mutate(prv_presence = T) |> 
  right_join(shape, by = "prv") |>
  filter(lvl == "prv", prv %in% prv_list$prv) |>  
  right_join(prv_list, by = "prv") |> mutate(prv_presence = if_else(is.na(prv_presence), F, prv_presence)) |> 
  left_join(labels_region, by = "region") |> 
  group_by(region_label) |> 
  summarise(data_presence = sum(prv_presence),
            location_presence = n()) -> data_covered_province

lm(p_under5 ~ region_label, data = risky_pop) |> summary()

risky_pop %>%
  ggplot(., aes(group = region_label, y = p_under5, color = region_label, x = region_label)) +
  geom_boxplot() +
  scale_color_manual(values = colors_region) +
  geom_hline(aes(yintercept = p_under5_national), linetype = 2, size = 1) +
  custom_theme +
  theme(legend.position = "none") +
  labs(x = "Region",
       y = "Proportion of population\nunder 5 years of age") -> p1

data_covered_county |> mutate(p_covered_county = data_presence /location_presence) |> 
  ggplot(aes(x = region_label)) +
  geom_bar(aes(y = location_presence, color = region_label), fill = NA,stat = "identity") +
  geom_bar(aes(y = data_presence, color = region_label, fill = region_label), stat = "identity") +
  scale_color_manual(values = colors_region) +
  scale_fill_manual(values = colors_region) +
  custom_theme +
  theme(legend.position = "none") +
  labs(x = "Region",
       y = "Total number of counties\nand counties with available data") -> p2

(get_legend(p2 + 
              theme(legend.position = "top",
                    legend.text = element_text(size = 16)) + 
              labs(color = "", fill = "") + 
              guides(color=guide_legend(nrow=1,byrow=T)))) -> p_legend

plot_grid(p_legend, plot_grid(p1 + theme(axis.text.x = element_text(angle = 90)), 
                              p2 + theme(axis.text.x = element_text(angle = 90)), 
                              ncol = 2), rel_heights = c(1,10), ncol = 1) -> p_save

ggsave("figs/fig1.png", p_save, width = 10, height = 7)

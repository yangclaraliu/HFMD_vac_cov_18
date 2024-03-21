require(gamlss)
require(car)

pop_all %>% 
  dplyr::filter(ag_LL < 5) %>% 
  group_by(CNTY_CODE, code_prv, year) %>% 
  summarise(APC = sum(pop)) %>% 
  left_join(pop_all %>% 
              group_by(CNTY_CODE, code_prv, year) %>% 
              summarise(tot = sum(pop)),
            by = c("CNTY_CODE", "code_prv", "year")) %>% 
  ungroup %>% 
  mutate(p_risk = APC/tot,
         year = as.numeric(year)) %>% 
  mutate(p_risk_s = (p_risk - mean(p_risk))/sd(p_risk),
         tot_s = (tot - mean(tot))/sd(tot)) -> pop_risk

cov %>%
  bind_rows(.id = "scenario") %>% 
  left_join(pt_imputed %>% 
              mutate(edu_s = (edu-mean(edu))/sd(edu),
                     GDPpc_s = (GDPpc - mean(GDPpc))/sd(GDPpc),
                     urban_s = (urban_prop-mean(urban_prop))/sd(urban_prop),
                     temp_s = (temp-mean(temp))/sd(temp)), 
            by = "CNTY_CODE") %>% 
  dplyr::filter(year == 2019) %>% 
  left_join(pop_risk,
            by = c("CNTY_CODE", "code_prv", "year")) %>% 
  .[complete.cases(.),] %>% 
  mutate(region = substr(CNTY_CODE, 1, 1)) %>% 
  dplyr::filter(!code_prv %in% 52) -> reg_tab

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

# model <- gamlss(cov_weighted ~ edu + GDPpc + urban_prop + p_risk + temp + tot + code_prv,
#                 family = BEZI,
#                 data = reg_tab %>% dplyr::filter(scenario == "baseline"),
#                 trace = T) 

reg_tab %>% 
  group_by(scenario) %>% 
  group_split() %>% 
  setNames(c("HE", "WU", "baseline")) %>% 
  map(~gamlss(formula = cov_weighted ~ GDPpc_s + urban_s + code_prv + p_risk_s + tot_s + temp_s + edu_s,
              family = ZALG,
              data = .,
              trace = T)) -> models

reg_tab %>% 
  dplyr::filter(cov_weighted <= 1) %>% 
  group_by(scenario) %>%  
  group_split() %>% 
  setNames(c("HE", "WU", "baseline")) %>% 
  map(~gamlss(formula = cov_weighted ~ GDPpc_s + urban_s + code_prv + p_risk_s + tot_s + temp_s + edu_s,
              family = BEZI,
              data = .,
              trace = T)) -> models_filtered

reg_tab %>% 
  dplyr::filter(cov_weighted <= 1) %>% 
  group_by(scenario) %>% 
  group_split() %>% map(dplyr::filter) %>% 
  setNames(c("HE", "WU", "baseline")) %>% 
  map(~gamlss(formula = cov_weighted ~ GDPpc_s + urban_s + p_risk_s + tot_s + temp_s + edu_s,
              family = BEZI,
              data = .,
              trace = T)) -> models_filtered_no_prov

reg_tab %>% 
  group_by(scenario) %>% 
  group_split() %>% 
  setNames(c("HE", "WU", "baseline")) %>% 
  map(~glm(formula = cov_weighted ~ GDPpc_s + urban_s 
           + p_risk_s + tot_s + temp_s + edu_s,
           data = .)) -> models_glm

vif(models_glm[[1]])
summary(models_glm[[1]])


models_filtered_no_prov %>% 
  map(summary) -> model_summary

# 
# reg_tab %>% 
#   group_by(scenario) %>% 
#   group_split() %>% map(dplyr::filter, cov_weighted < 1) %>% 
#   setNames(c("HE", "WU", "baseline")) %>% 
#   map(~gamlss(formula = cov_weighted ~ GDPpc_s + urban_s + code_prv + p_risk_s + tot_s + temp_s + edu_s,
#       family = BEZI,
#       data = .,
#       trace = T)) -> models
# 
# reg_tab %>% 
#   group_by(scenario) %>% 
#   group_split() %>% map(dplyr::filter, cov_weighted < 1) %>% 
#   setNames(c("HE", "WU", "baseline")) %>% 
#   map(~gamlss(formula = cov_weighted ~ GDPpc_s + urban_s +  p_risk_s + tot_s + temp_s + edu_s,
#               family = GA,
#               data = .,
#               trace = T)) -> models_no_prv

# model_s <- gamlss(cov_weighted ~ GDPpc_s + urban_s + code_prv + p_risk_s + tot_s + temp_s + edu_s,
#                   family = BEZI,
#                   data = reg_tab %>% dplyr::filter(scenario == "HE", cov_) ,
#                   trace = T) 

# model_s <- lm(cov_weighted ~ GDPpc_s + urban_s + edu_s + code_prv + p_risk_s + tot_s + temp_s,
#               data = reg_tab %>% dplyr::filter(scenario == "baseline")) 
# vif(model_s)
# model_summary <- summary(model)
# model_s_summary <- summary(model_s)
# se <- sqrt(diag(vcov(model))) %>% tail(-3)

# coef(model) %>% 
#   tail(-3) %>% 
#   enframe %>% 
#   mutate(prv_no = as.character(parse_number(name)),
#          se = se,
#          LL = value - 1.96*se,
#          UL = value + 1.96*se,
#   ) %>% 
#   mutate(nt = ntile(value, 4)) %>% arrange(value) 

# coef(model) %>% 
  # tail(-3) %>% 
  # enframe %>%
# model_summary[[3]] %>% 
#   .[,1:2] %>% 
#   data.frame() |> 
#   rownames_to_column() |> 
#   filter(grepl("code_prv",rowname)) |> 
#   mutate(code_prv = (parse_number(rowname)),
#          LL = Estimate  - 1.96*`Std..Error`,
#          UL = Estimate  + 1.96*`Std..Error`,
#   ) %>% 
#   add_row(Estimate  = 0, code_prv = 11, LL = 0, UL = 0) %>% 
#   arrange(code_prv) %>% 
#   left_join(shape_prv %>% mutate(code_prv = as.numeric(code_prv)), by = c("code_prv")) %>% 
#   mutate(region = substr(code_prv, 1, 1)) %>% 
#   filter(!is.na(code_prv)) %>% 
#   left_join(labels_region %>% rename(region = labels_region) %>% mutate(region = as.character(region)), by = "region") |> 
#   rename(value = Estimate) %>% 
#   arrange(code_prv) -> p_table

# shape_prv |> 
#   rename(names_region = region) %>% 
#   left_join(labels_region, by = "names_region") %>% 
#   group_by(labels_region) |> tally() |> pull(n) -> rep_times
# 
# rep(colors_region,
#     times = rep_times) -> colors_label
# 
# p_table %>% 
#   arrange(code_prv) %>% 
#   mutate(names_region = factor(names_region,
#                                levels = labels_region$names_region)) %>% 
#   mutate(NAME_EN = factor(NAME_EN),
#          code_prv = factor(code_prv,
#                            levels = shape_prv$code_prv,
#                            labels = shape_prv$NAME_EN)) %>% 
#   ggplot(.,
#          aes(x = code_prv, color = names_region)) +
#   geom_rect(aes(xmin= c(1)-0.5,
#                 xmax = c(5)+0.5,
#                 ymin = -Inf,
#                 ymax = Inf), 
#             fill = "grey90", 
#             alpha = 0.04,
#             color = NA) +
#   geom_rect(aes(xmin= c(9)-0.5,
#                 xmax = c(15)+0.5,
#                 ymin = -Inf,
#                 ymax = Inf), fill = "grey90", alpha = 0.04, color = NA) +
#   geom_rect(aes(xmin= c(22)-0.5,
#                 xmax = c(26)+0.5,
#                 ymin = -Inf,
#                 ymax = Inf), fill = "grey90", alpha = 0.04, color = NA) +
#   geom_point(aes(y = value), size = 2) +
#   geom_segment(aes(xend = code_prv, y = LL, yend = UL)) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   scale_x_discrete(drop = F,
#                    guide = guide_axis(n.dodge = 3)) +
#   custom_theme +
#   theme(legend.position = "top",
#         legend.text = element_text(size = 16)) + 
#   guides(color=guide_legend(nrow=1,byrow=T)) +
#   scale_color_manual(breaks = labels_region$names_region, values = colors_region) +
#   labs(x = "", y = "Province Specific Intercepts", color = "") -> p_prv

model_summary %>%
  map(data.frame) %>% 
  map(rownames_to_column) %>% 
  bind_rows(.id = "scenario") %>% 
  filter(!grepl("code_prv",rowname)) |> 
  mutate(LL = Estimate  - 1.96*`Std..Error`,
         UL = Estimate  + 1.96*`Std..Error`,
  ) %>%
  dplyr::filter(!grepl("Intercept", rowname)) %>% 
  #.[2:7,] %>% 
  dplyr::filter(scenario == "baseline") %>% 
  ggplot(., aes(y = rowname, x = Estimate)) +
  geom_point(size = 3) +
  geom_segment(aes(y = rowname, yend = rowname,
                   x = LL, xend = UL)) +
  geom_vline(xintercept = 0, linetype = 2) +
  custom_theme +
  theme(legend.position = "top",
        legend.text = element_text(size = 16)) +
  labs(y = "Independent variables", 
       x = "Estimated Effects", color = "") +
  # scale_color_manual(values = c("baseline" = "black", 
  #                               "WU" = "#c51b8a", 
  #                               "HE" = "#756bb1")) 
  scale_y_discrete(labels = c("Average level\nof education",
                              "GDP per capita",
                              "Proportion of\npopulation under 5",
                              "Average temperature",
                              "Total population size",
                              "Proportion of population\nin urban setting")) -> p_effect

reg_tab %>% 
  mutate(tier1 = code_prv %in% c(11, 12, 31, 50)) %>% 
  dplyr::select(CNTY_CODE, code_prv, code_prf, year, cov_weighted, p_risk, p_risk_s, tier1) %>% 
  ggplot(., aes(x = p_risk, y = cov_weighted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "#c51b8a",
              fill = "#fa9fb5",
              method = "loess") +
  custom_theme +
  theme(legend.position = "top",
        legend.text = element_text(size = 16)) +
  labs(y = "Vaccine coverage", 
       x = "Proportion of population\nunder 5 years of age", color = "") +
  geom_hline(yintercept = 0) -> p_under5

reg_tab %>% 
  mutate(tier1 = code_prv %in% c(11, 12, 31, 50)) %>% 
  # dplyr::select(CNTY_CODE, code_prv, code_prf, year, cov_weighted, p_risk, p_risk_s, tier1) %>% 
  ggplot(., aes(x = temp, y = cov_weighted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "#c51b8a",
              fill = "#fa9fb5",
              method = "loess") +
  custom_theme +
  theme(legend.position = "top",
        legend.text = element_text(size = 16)) +
  labs(y = "Vaccine coverage", 
       x = "Average Temperature", color = "") +
  geom_hline(yintercept = 0) -> p_temp


# p_save <- plot_grid(p_prv, plot_grid(p_effect, p_under5, nrow = 1, axis = "l", align = "h"), ncol = 1)
p_save <- plot_grid(p_effect, plot_grid(p_under5, p_temp, nrow = 1, axis = "l", align = "h"), nrow = 2, axis = "l", align = "h", rel_heights = c(5, 10)) + 
  theme(plot.background = element_rect(fill = "white"))
ggsave("figs/fig5_v3.png", p_save, width = 12, height = 12)

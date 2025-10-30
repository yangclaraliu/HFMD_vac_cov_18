require(gamlss)
require(car)

pop_all %>% 
  dplyr::filter(ag_LL <= 5) %>% 
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
  left_join(epi %>% 
              dplyr::select(CNTY_CODE, burden, burden_s),
            by = "CNTY_CODE") %>% 
  dplyr::filter(year == 2018) %>% 
  left_join(pop_risk,
            by = c("CNTY_CODE", "code_prv", "year")) %>% 
  .[complete.cases(.),] %>% 
  mutate(region = substr(CNTY_CODE, 1, 1)) %>% 
  dplyr::filter(coverage_weighted_pop == 0) %>% View()

cov %>%
  bind_rows(.id = "scenario") %>% 
  left_join(pt_imputed %>% 
              mutate(edu_s = (edu-mean(edu))/sd(edu),
                     GDPpc_s = (GDPpc - mean(GDPpc))/sd(GDPpc),
                     urban_s = (urban_prop-mean(urban_prop))/sd(urban_prop),
                     temp_s = (temp-mean(temp))/sd(temp)), 
            by = "CNTY_CODE") %>% 
  left_join(epi %>% 
              dplyr::select(CNTY_CODE, burden, burden_s),
            by = "CNTY_CODE") %>% 
  dplyr::filter(year == 2018) %>% 
  left_join(pop_risk,
            by = c("CNTY_CODE", "code_prv", "year")) %>% 
  .[complete.cases(.),] %>% 
  mutate(region = substr(CNTY_CODE, 1, 1)) %>% 
  dplyr::filter(!code_prv %in% 52,
                coverage_weighted_pop <= 1,
                coverage_weighted_HE <= 1,
                coverage_weighted_WU <= 1) %>% 
  dplyr::select(CNTY_CODE, year, starts_with("edu"), code_prv,
                starts_with("urban"), starts_with("GDPpc"),
                starts_with("temp"), starts_with("p_risk"),
                starts_with("burden"), starts_with("tot"),
                starts_with("coverage_weighted")) %>% 
  distinct() -> reg_tab

# model <- gamlss(cov_weighted ~ edu + GDPpc + urban_prop + p_risk + temp + tot + code_prv,
#                 family = BEZI,
#                 data = reg_tab %>% dplyr::filter(scenario == "baseline"),
#                 trace = T) 

#### check correlations ####
library(corrplot)
library(RColorBrewer)

M <- cor(reg_tab[,c("edu_s", "GDPpc_s", "urban_s", "temp_s", "burden_s", "p_risk_s", "tot_s")])
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

M <- cor(reg_tab[,c("edu", "GDPpc", "urban_prop", "temp", "burden", "p_risk", "tot")])
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

chisq.test(x = reg_tab$code_prv,
           y = reg_tab$edu)

summary(lm(p_risk ~ code_prv, data = reg_tab))

#### check vif ####
model_s <- lm(coverage_weighted_pop ~ edu_s + GDPpc_s + urban_s + 
                temp_s + 
                burden_s + p_risk_s + tot_s + code_prv,
              data = reg_tab)

model <- lm(coverage_weighted_pop ~ edu + GDPpc + urban_prop + temp + 
                burden + p_risk + tot + code_prv,
              data = reg_tab)

print(car::vif(model_s))
print(car::vif(model))

#### full model
models <- list()
# s = standardised; ns = non-standardised

models[["pop_full_s"]] <- gamlss(formula = coverage_weighted_pop ~ p_risk_s + tot_s + temp_s + edu_s + burden_s + urban_s + GDPpc_s + code_prv,
                                            family = BEZI,
                                            data = reg_tab,
                                            trace = T)


models[["pop_full_ns"]] <- gamlss(formula = coverage_weighted_pop ~ p_risk + tot + temp + edu + burden + urban_prop + GDPpc + code_prv,
                                 family = BEZI,
                                 data = reg_tab,
                                 trace = T)

#### test what difference would it make if we just revert to using GLM
models[["pop_full_s_glm"]] <- glm(formula = coverage_weighted_pop ~ p_risk_s + tot_s + temp_s + edu_s + burden_s + urban_s + GDPpc_s + code_prv,
                              data = reg_tab)

#### remove urban prop due to multicollinarity ####
models[["pop_s_MC"]] <- gamlss(formula = coverage_weighted_pop ~ p_risk_s + tot_s + temp_s + edu_s + burden_s + GDPpc_s + code_prv,
                                 family = BEZI,
                                 data = reg_tab,
                                 trace = T)

models[["pop_ns_MC"]] <- gamlss(formula = coverage_weighted_pop ~ p_risk + tot + temp + edu + burden + GDPpc + code_prv,
                                  family = BEZI,
                                  data = reg_tab,
                                  trace = T)

LR.test(models$pop_s_MC, models$pop_full_s)

#### test if province specific intercept should be included ####
models[["pop_s_noPrv"]] <- gamlss(formula = coverage_weighted_pop ~ p_risk_s + tot_s + temp_s + edu_s + burden_s + GDPpc_s + urban_s,
                                  family = BEZI,
                                  data = reg_tab,
                                  trace = T)
LR.test(models$pop_s_noPrv, models$pop_full_s)

# seems like after changing the age group definition we should indeed include province specific intercept

#### univariate model #### 
##### (1) #####
models[["pop_univariate_s_p_risk"]] <- gamlss(formula = coverage_weighted_pop ~ p_risk_s,
                                 family = BEZI,
                                 data = reg_tab,
                                 trace = T)

##### (2) #####
models[["pop_univariate_s_tot"]] <- gamlss(formula = coverage_weighted_pop ~ tot_s,
                                              family = BEZI,
                                              data = reg_tab,
                                              trace = T)


##### (3) #####
models[["pop_univariate_s_temp"]] <- gamlss(formula = coverage_weighted_pop ~ temp_s,
                                              
                                              family = BEZI,
                                              data = reg_tab,
                                              trace = T)

##### (4) #####
models[["pop_univariate_s_edu"]] <- gamlss(formula = coverage_weighted_pop ~ edu_s,
                                              
                                              family = BEZI,
                                              data = reg_tab,
                                              trace = T)

##### (5) #####
models[["pop_univariate_s_burden"]] <- gamlss(formula = coverage_weighted_pop ~ burden_s,
                                              
                                              family = BEZI,
                                              data = reg_tab,
                                              trace = T)

##### (6) #####
models[["pop_univariate_s_urban"]] <- gamlss(formula = coverage_weighted_pop ~ urban_s,
                                             family = BEZI,
                                             data = reg_tab,
                                             trace = T)

##### (7) #####
models[["pop_univariate_s_GDPpc"]] <- gamlss(formula = coverage_weighted_pop ~ GDPpc_s,
                                              
                                              family = BEZI,
                                            data = reg_tab,
                                              trace = T)

##### (8) #####
models[["pop_univariate_s_prv"]] <- gamlss(formula = coverage_weighted_pop ~ code_prv,
                                              family = BEZI,
                                              data = reg_tab,
                                              trace = T)

#### summary: univariate ####
model_summary_univariate <- list("p_risk" = summary(models$pop_univariate_s_p_risk),
                                 "tot" = summary(models$pop_univariate_s_tot),
                                 "temp" = summary(models$pop_univariate_s_temp),
                                 "edu" = summary(models$pop_univariate_s_edu),
                                 "burden" = summary(models$pop_univariate_s_burden),
                                 "urban" = summary(models$pop_univariate_s_urban),
                                 "GDPpc" = summary(models$pop_univariate_s_GDPpc),
                                 "prv" = summary(models$pop_univariate_s_prv))

model_summary_univariate

#### sensitivity analysis regression ####
# given that the final model is the full model, we will run the same thing
# through again but using the other datasets

models[["HE_full_s"]] <- gamlss(formula = coverage_weighted_HE ~ p_risk_s + tot_s + temp_s + edu_s + burden_s + urban_s + GDPpc_s + code_prv,
                                family = BEZI,
                                data = reg_tab,
                                trace = T)

models[["WU_full_s"]] <- gamlss(formula = coverage_weighted_WU ~ p_risk_s + tot_s + temp_s + edu_s + burden_s + urban_s + GDPpc_s + code_prv,
                                family = BEZI,
                                data = reg_tab,
                                trace = T)

model_summary_SA <- list("pop" = summary(models$pop_full_s),
                         "HE" = summary(models$HE_full_s),
                         "WU" = summary(models$WU_full_s))

model_summary_main <- list()
model_summary_main[["pop"]] <- data.frame(effect = exp(coef(models$pop_full_s, what = "mu")))
ci_tmp <- exp(confint(models$pop_full_s, what = "mu"))
model_summary_main[["pop"]] <- cbind(model_summary_main[["pop"]], ci_tmp)

model_summary_main[["HE"]] <- data.frame(effect = exp(coef(models$HE_full_s, what = "mu")))
ci_tmp <- exp(confint(models$HE_full_s, what = "mu"))
model_summary_main[["HE"]] <- cbind(model_summary_main[["HE"]], ci_tmp)

model_summary_main[["WU"]] <- data.frame(effect = exp(coef(models$WU_full_s, what = "mu")))
ci_tmp <- exp(confint(models$WU_full_s, what = "mu"))
model_summary_main[["WU"]] <- cbind(model_summary_main[["WU"]], ci_tmp)

model_summary_main %>%
  bind_rows(.id = "scenario") %>% 
  rownames_to_column() %>% 
  dplyr::filter(!grepl("code_prv",rowname)) %>%
  dplyr::filter(!grepl("Intercept", rowname)) %>% 
  dplyr::filter(scenario == "HE") %>% 
  mutate(rowname = gsub(c("[0-9]"), "", rowname),
         rowname = str_remove_all(rowname, "\\."),
         rowname = factor(rowname,
                          levels = c("p_risk_s",
                                     "tot_s",
                                     "temp_s",
                                     "edu_s",
                                     "burden_s",
                                     "urban_s",
                                     "GDPpc_s"),
                          labels = c("Proportion of\npopulation under 5",
                                     "Total population size",
                                     "Average temperature",
                                     "Average level\nof education",
                                     "Historical bruden of HFMD",
                                     "Proportion of population\nin urban setting",
                                     "GDP per capita"
                                     ))) %>% 
         # scenario = factor(scenario,
         #                   levels = c("pop", "HE", "WU"),
         #                   labels = c("Population age distribution by county",
         #                              "HE",
         #                              "WU et al."))) %>% 
  ggplot(., aes(y = rowname, x = effect)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.3)) +
  geom_linerange(aes(xmin = `2.5 %`, xmax = `97.5 %`),
                position = position_dodge(width = 0.3)) +
  geom_vline(xintercept = 1, 
             linetype = 2) +
  custom_theme +
  theme(legend.position = "top",
        legend.text = element_text(size = 16)) +
  labs(y = "", 
       x = "Estimated Effects\nRelative change in mean", color = "") -> p_effect
  # scale_color_manual(values = c("baseline" = "black", 
  #                               "WU" = "#c51b8a", 
  #                               "HE" = "#756bb1"))

reg_tab %>% 
  # mutate(tier1 = code_prv %in% c(11, 12, 31, 50)) %>% 
  #dplyr::select(CNTY_CODE, code_prv, code_prf, year, cov_weighted, p_risk, p_risk_s, tier1) %>% 
  ggplot(., aes(x = p_risk, y = coverage_weighted_pop)) +
  #geom_point() +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="grey") +
  # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette="BuPu", direction=1) +
  # scale_x_log10() +
  # geom_point(alpha = 0.5) +
  # geom_smooth(color = "#c51b8a",
  #             fill = "#fa9fb5",
  #             method = "lm") +
  custom_theme +
  theme(legend.position = "top",
        legend.text = element_text(size = 16),
        legend.key.width = unit(2, "cm")) +
  # geom_hline(yintercept = 0) +
  labs(y = "Vaccine coverage", 
       x = "Proportion of population\nunder 5 years of age", 
       color = "",
       fill = "Density") -> p_under5

model_summary_main %>%
  bind_rows(.id = "scenario") %>% 
  rownames_to_column() %>% 
  dplyr::filter(grepl("code_prv",rowname)) %>%
  separate(rowname, into = c("seg1", "code_prv", "seg3")) %>% 
  dplyr::select(-seg1, -seg3) %>% 
  bind_rows(data.frame(code_prv = c('11', "11", "11"),
                       scenario = c("pop", "HE", "WU"))) %>% 
  replace(., is.na(.), 1) %>% 
  # dplyr::filter(scenario == "pop") %>% 
  mutate(code_prv = as.character(parse_number(code_prv)),
         scenario = factor(scenario,
                           levels = c("pop", "HE", "WU"),
                           labels = c("Population age distribution by county",
                                      "HE",
                                      "WU et al."))) %>% 
  left_join(shape_prv, by = "code_prv") %>% 
  arrange(NAME_EN) %>% 
  mutate(region = factor(region, 
                         levels = c("Huabei\n(~North)",
                                    "Dongbei\n(~Northeast)",
                                    "Huadong\n(~East)",
                                    "Zhongnan\n(~South)",
                                    "Xinan\n(~Southwest)",
                                    "Xibei\n(~Northwest)")),
         NAME_EN = fct_reorder2(NAME_EN, desc(NAME_EN), region)) %>% 
  dplyr::filter(scenario == "Population age distribution by county") %>% 
  # dplyr::filter(scenario == "HE") %>% 
  # dplyr::filter(scenario == "WU et al.") %>% 
  ggplot(., aes(x = NAME_EN, y = effect, color = region)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_linerange(aes(ymin = `2.5 %`, ymax = `97.5 %`),
                 position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_color_manual(values = colors_region) +
  custom_theme +
  theme(legend.position = "top",
        legend.text = element_text(size = 16),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "",
       y = "Relative mean difference", 
       color = "") +
  guides(color = guide_legend(nrow = 1)) + 
  # scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  facet_grid(~region, drop = T, space = "free", scales = "free") -> p_prv

# summary(tmp$effect)

# reg_tab %>% 
#   dplyr::filter(cov_weighted <= 1) %>% 
#   mutate(tier1 = code_prv %in% c(11, 12, 31, 50)) %>% 
#   # dplyr::select(CNTY_CODE, code_prv, code_prf, year, cov_weighted, p_risk, p_risk_s, tier1) %>% 
#   ggplot(., aes(x = temp, y = cov_weighted)) +
#   stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="grey") +
#   # stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
#   scale_fill_distiller(palette="BuPu", direction=1) +
#   # geom_point(alpha = 0.5) +
#   # geom_smooth(color = "#c51b8a",
#   #             fill = "#fa9fb5",
#   #             method = "lm") +
#   custom_theme +
#   theme(legend.position = "bottom",
#         legend.text = element_text(size = 16),
#         legend.key.width = unit(2, "cm")) +
#   # geom_hline(yintercept = 0) +
#   labs(y = "Vaccine coverage", 
#        x = "Average Temperature (°C)", 
#        color = "",
#        fill = "Density") -> p_temp


# p_save <- plot_grid(p_prv, plot_grid(p_effect, p_under5, nrow = 1, axis = "l", align = "h"), ncol = 1)
#p_save <-

plot_grid(
  p_effect,
  p_under5,
  rel_widths = c(6,4),
  nrow = 1,
  axis = "l",
  align = "h",
  labels = c("(A)", "(B)")
) -> p_top
  
plot_grid(
    p_top,
    p_prv,
    nrow = 2,
    axis = "l",
    align = "h",
    rel_heights = c(5, 5),
    labels = c("", "(C)")
  ) +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "white")) -> p_save

ggsave("figs/fig5_v5_HE.png", p_save, width = 12, height = 10)
 
# response to review exercise: 2025/10/29
reg_tab %>% 
  dplyr::select(CNTY_CODE, code_prv, year, coverage_weighted_pop, p_risk) %>% 
  mutate(rank_p_risk = ecdf(reg_tab$p_risk)(reg_tab$p_risk),
         rank_cov = ecdf(reg_tab$coverage_weighted_pop)(reg_tab$coverage_weighted_pop)) %>% 
  dplyr::filter(rank_p_risk >= 0.8 & rank_cov <= 0.2) %>% 
  left_join(provinces %>% mutate(code_prv = substr(ZONECODE, 1, 2)), by = "code_prv") %>% 
  mutate(PYNAME_short = word(PYNAME, 1)) %>% 
  mutate(PYNAME_short = factor(PYNAME_short, 
                               levels = names(sort(table(PYNAME_short), decreasing = TRUE)))) %>% 
  mutate(lvl_of_extreme = if_else(rank_p_risk >= 0.9 & rank_cov <= 0.1, "Top 10%ile at risk + Bottom 10%ile coverage", "Top 20%ile at risk + Bottom 20%ile coverage")) -> p_tab

p_tab %>% 
  dplyr::filter(lvl_of_extreme == "Top 10%ile at risk + Bottom 10%ile coverage") %>% 
  group_by(PYNAME_short) %>% tally() %>% arrange(n)

ggplot(p_tab, aes(x = PYNAME_short, fill = lvl_of_extreme)) +
  geom_bar(stat = "count") + 
  labs(fill = "",
       x = "",
       y = "Count") +
  theme_bw() + 
  theme(axis.text =element_text(angle = 90),
        legend.position = "top") -> p_save

ggsave("figs/fig5_R2R.png", p_save, width = 12, height = 10)

p_tab %>% 
  left_join(counties %>% mutate(CNTY_CODE = substr(CNTY_CODE, 1, 6)),
            by = "CNTY_CODE") %>% 
  dplyr::select(`NAME.y`,
                `PYNAME.y`,
                `PYNAME.x`,
                lvl_of_extreme) %>% 
  rename(province = `PYNAME.x`,
         county = `PYNAME.y`,
         county_CN = `NAME.y`) %>% 
  mutate(province = word(province, 1)) %>% 
  arrange(lvl_of_extreme) -> tmp

write_csv(tmp, "p_risk_extreme.csv")

# response to review exercise: 2025/10/30
models[["pop_full_s_ziv"]] <- gamlss(formula = coverage_weighted_pop ~ p_risk_s + tot_s + temp_s + edu_s + burden_s + urban_s + GDPpc_s + code_prv,
                                     family = BEZI,
                                     sigma.formula = ~edu_s + GDPpc_s,
                                     nu.formula = ~edu_s + GDPpc_s,
                                     data = reg_tab,
                                     trace = T)


summary(models$pop_full_s)
summary(models$pop_full_s_ziv)

model_resid <- residuals(models$pop_full_s, what = "z-score")
qqnorm(model_resid, main = "QQ plot of z-score residuals")
qqline(model_resid)

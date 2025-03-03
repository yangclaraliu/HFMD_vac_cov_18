cov <- read_rds(paste0(path_dropbox_github, "coverage_results_20250129.rds")) 

cov %>% 
  dplyr::select(CNTY_CODE,
                code_prf,
                code_prv,
                year,
                ag_LL,
                birth_cohort,
                weights_pop,
                weights_WU,
                weights_HE) %>% 
  pivot_longer(cols = starts_with("weights")) %>% 
  dplyr::filter(year == 2018) %>% 
  mutate(name = factor(name,
                       levels = c("weights_HE",
                                  "weights_WU",
                                  "weights_pop"),
                       labels = c("He",
                                  "Wu et al.",
                                  "Population age distribution\nby county"))) -> p_tab
p_tab %>%   
  ggplot(.) +
  geom_line(aes(x = ag_LL, 
                y = value, 
                group = interaction(name, CNTY_CODE), 
                color = name, 
                alpha = name, 
                linewidth = name)) +
  geom_point(aes(x = ag_LL, y = value, group = name, color = name, size = name, alpha = name)) +
  scale_color_manual(values = c("#c51b8a", "#756bb1", "black"), drop = F) +
  scale_alpha_manual(values = c(1, 1, 0.01)) +
  scale_linewidth_manual(values = c(1, 1, 0.2)) +
  scale_size_manual(values = c(2, 2, 0.1)) +
  custom_theme + 
  theme(legend.text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.background = element_rect(colour = "black", fill = NA)) +
  labs(y = "% of vaccines distributed\nto a specific age group",
       x = "Age",
       color = "") + 
  guides(color=guide_legend(nrow=1,byrow=T),
         shape = F,
         alpha = F,
         linewidth = F, 
         size = F) -> p1
  
cov %>%
  dplyr::filter(year == 2018, code_prv != 52) %>% 
  dplyr::select(CNTY_CODE, year, coverage_weighted_pop, coverage_weighted_HE, coverage_weighted_WU, code_prv) %>% 
  distinct() %>% 
  pivot_longer(cols = c("coverage_weighted_HE",
                        "coverage_weighted_WU")) %>% 
  mutate(name = factor(name,
                       levels = c("coverage_weighted_HE",
                                  "coverage_weighted_WU"),
                       labels =  c("He",
                                   "Wu et al."))) %>% 
  ggplot(., aes(x = coverage_weighted_pop, y = value, color = name)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_manual(values = c("#c51b8a", "#756bb1")) +
  custom_theme + 
  theme(legend.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = NA)) +
  labs(y = "Estimates with alternative assumptions",
       x = "Baseline estimate \n(based on population age distribution)",
       color = "") + 
  geom_smooth(method = "lm") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3, fill = NA),
                               nrow=1,
                               byrow=T))  -> p2


cov %>%
  dplyr::filter(year == 2018, code_prv != 52) %>% 
  dplyr::select(CNTY_CODE, year, coverage_weighted_pop, coverage_weighted_HE, coverage_weighted_WU, code_prv) %>% 
  distinct() %>% 
  pivot_longer(cols = c("coverage_weighted_HE",
                        "coverage_weighted_WU")) %>% 
  mutate(name = factor(name,
                       levels = c("coverage_weighted_HE",
                                  "coverage_weighted_WU"),
                       labels =  c("He",
                                   "Wu et al."))) -> reg_tab 

lm(data = reg_tab %>% dplyr::filter(name == "He"),
   formula = value ~ coverage_weighted_pop) -> model_HE

lm(data = reg_tab %>% dplyr::filter(name != "He"),
   formula = value ~ coverage_weighted_pop) -> model_WU

summary(model_HE)
summary(model_WU)


  plot_grid(p1, p2, ncol = 1, align = "v", axis = "lr") -> p_save

  ggdraw() +
  draw_plot(p_save) +
  draw_label(label = "(A)", x = 0.05, y = 0.95, size = 14, fontface = "bold") +
  draw_label(label = "(B)", x = 0.05, y = 0.47, size = 14, fontface = "bold") -> p_save_w_labels

ggsave("figs/fig4_v2.png", p_save_w_labels, width = 7, height = 13)

 # EV71_Lit <- readRDS("data/EV71_Lit.rds") %>%
#   filter(!(Age_LL_value == 0 & Age_UL_value %in% c(5,6)))

# EV71_Lit %>% 
#   ggplot(., aes(x = Age_LL_value, y = cov)) +
#   geom_point() +
#   geom_segment(aes(xend = Age_UL_value, yend = cov)) +
#   facet_wrap(~index)
# 
# pop %>% 
#   filter(CNTY_CODE == 510723,
#          year == 2017,
#          age_group %in% paste0(0:5, "-")) %>% 
#   mutate(all = sum(tot),
#          tot_p = tot/all,
#          cov = case_when(age_group == "0-" ~ 64.5/420,
#                          age_group == "1-" ~ 129/420,
#                          age_group == "2-" ~ 78/420, 
#                          age_group == "3-" ~ 33/420,
#                          age_group == "4-" ~ 33/420,
#                          age_group == "5-" ~ 33/420)) %>% 
#   dplyr::select(age_group, cov) -> tmp
# 
# 
# pop %>% 
#   filter(age_group %in% paste0(0:5, "-"),
#          year >= 2016 & year < 2018) %>% 
#   group_by(CNTY_CODE, year) %>% 
#   mutate(tot_sum = sum(tot)) %>% 
#   mutate(tot_p = tot/tot_sum) %>% 
#   left_join(tmp, by = "age_group") %>% 
#   group_by(CNTY_CODE, year) %>% 
#   mutate(doses = tot_p*cov,
#          doses_tot = sum(doses),
#          doses_p = doses/doses_tot) %>% 
#   filter(ag_LL == 5) %>% 
#   dplyr::select(CNTY_CODE, year, doses_p) -> tmp
# 
# # Age out proportions by year
# POP_R_5 %>% 
#   rename(YB = perc_5) %>% 
#   left_join(tmp, by = c("CNTY_CODE", "year")) %>% 
#   # scaled based on Santai Survey
#   rename(ST = doses_p) %>% 
#   pivot_longer(c("YB", "ST")) %>% 
#   unite("tmp", name:year) %>% 
#   filter(!(CNTY_CODE %in% c(441325, 450122, 441325))) %>% 
#   pivot_wider(names_from = tmp, values_from = value) -> AO
# 
# data %>% 
#   dplyr::select(CNTY_CODE, starts_with("d2")) %>% 
#   pivot_longer(starts_with("d2")) %>% 
#   separate(name, into = c("dose", "ag")) %>% 
#   group_by(CNTY_CODE) %>% group_split() %>% 
#   map(arrange, ag) %>% map(mutate, counts = cumsum(value)) %>% 
#   bind_rows() %>% 
#   dplyr::select(-value) %>% 
#   pivot_wider(names_from = ag, values_from = counts) -> outstanding_doses
# 
# outstanding_doses %>% 
#   left_join(AO, by = "CNTY_CODE") %>% 
#   mutate(ST_doses_2016_LL = `2016`*(1-ST_2016),
#          ST_doses_2016_UL = `2016`,
#          ST_doses_2017_LL = `2017`*(1-ST_2016),
#          ST_doses_2017_UL = `2016`*(1-ST_2016) + (`2017`-`2016`),
#          ST_doses_2018_LL = `2018`*(1-ST_2017),
#          ST_doses_2018_UL = `2017`*(1-ST_2017) + (`2018`-`2017`),
#          YB_doses_2016_LL = `2016`*(1-YB_2016),
#          YB_doses_2016_UL = `2016`,
#          YB_doses_2017_LL = `2017`*(1-YB_2016),
#          YB_doses_2017_UL = `2016`*(1-YB_2016) + (`2017`-`2016`),
#          YB_doses_2018_LL = `2018`*(1-YB_2017),
#          YB_doses_2018_UL = `2017`*(1-YB_2017) + (`2018`-`2017`)) %>% 
#   dplyr::select(CNTY_CODE, ends_with(c("LL", "UL"))) %>% 
#   # OD = outstanding doses
#   pivot_longer(ends_with(c("LL", "UL")), 
#                names_to = "metric", 
#                values_to = "OD") %>% 
#   separate(metric, into = c("age_structure_source", "dp", "year", "metric")) %>% 
#   dplyr::select(-dp) -> outstanding_doses_aged
# 
# data %>% 
#   dplyr::select(-starts_with("d2")) %>% 
#   rename(APC_YB_2016_ob = YB_2016_ob,
#          APC_YB_2017_ob = YB_2017_ob,
#          APC_YB_2018_lm = YB_2018_lm) %>% 
#   mutate(APC_YB_2018_con = APC_YB_2017_ob) %>% 
#   pivot_longer(starts_with("APC")) %>% 
#   # in process, ob: observation, lm: linear model, con: assumed to be consistent with last year
#   separate(name, into = c("dp", "pop_source", "year", "process")) %>% 
#   dplyr::select(-dp) -> outstanding_pop
# 
# outstanding_pop %>% 
#   left_join(outstanding_doses_aged,
#             by = c("CNTY_CODE", "year")) %>% 
#   mutate(cov = OD/value) -> tmp
# 
# tmp %>% 
#   unite("all_id", pop_source, process, age_structure_source, metric) %>% 
#   mutate(prv_no = substr(CNTY_CODE, 1, 2), year = factor(year)) %>% 
#   filter(cov <0 | cov > 1) %>%
#   pull(CNTY_CODE) %>% unique  -> to_remove
# 
# tmp |> 
#   mutate(prv = substr(CNTY_CODE, 1, 2)) |> 
#   left_join(prv_list, by = "prv") |> 
#   left_join(labels_region, by = "region") -> tmp
# 
# #### sensitivity analysis by data source ####
# 
# p <- list()
# 
# tmp %>% 
#   filter(!(CNTY_CODE %in% to_remove),
#          year == 2018) %>% 
#   dplyr::select(-OD, -value)  %>% 
#   pivot_wider(names_from = age_structure_source ,
#               values_from = cov) %>% 
#   filter(process == "lm", pop_source == "YB", metric == "LL") %>% 
#   ggplot(., aes(x = YB, y = ST, color = region_label)) +
#   geom_point(alpha = 0.5) +
#   geom_abline(slope = 1, intercept = 0, linetype = 2) +
#   custom_theme+
#   theme(legend.position = "none") +
#   scale_color_manual(values = colors_region) +
#   coord_cartesian(xlim = c(0, 1), ylim = c(0,1)) +
#   labs(x = "Vaccine Coverage Assuming\nProportional Vaccination Process",
#        y = "Vaccine Coverage Assuming\nTargeted Vaccination Process") -> p[[1]]
# 
# tmp %>% 
#   filter(!(CNTY_CODE %in% to_remove),
#          year == 2018) %>% 
#   dplyr::select(-OD, -value)  %>% 
#   pivot_wider(names_from = pop_source  ,
#               values_from = cov) %>% 
#   filter(process == "lm", age_structure_source == "YB", metric == "LL") %>% 
#   mutate(region = substr(CNTY_CODE,1,1),
#          prv_no = substr(CNTY_CODE,1,2)) %>% #,
#   #        diff = sur/YB - 1) %>% 
#   # ggplot(., aes(x = diff)) +
#   # geom_density() +
#   # lims(x = c(-1,5))
#   ggplot(., aes(x = YB, y = sur, color = region_label)) +
#   geom_point(alpha = 0.5) +
#   geom_abline(slope = 1, intercept = 0, linetype = 2) +
#   stat_ellipse(size = 1) +
#   custom_theme +
#   theme(legend.position = "none") +
#   coord_cartesian(xlim = c(0, 1), ylim = c(0,1)) +
#   scale_color_manual(values = colors_region) +
#   labs(x = "Vaccine CoverageBased on\nYear Book Population",
#        y = "Vaccine CoverageBased on Population\nEstimates in the Surveillance System") -> p[[2]]
# 
# tmp %>% 
#   filter(!(CNTY_CODE %in% to_remove),
#          year == 2018) %>% 
#   dplyr::select(-OD, -value)  %>% 
#   pivot_wider(names_from = process,
#               values_from = cov) %>% 
#   filter(pop_source  == "YB", age_structure_source == "YB", metric == "LL") %>% 
#   mutate(region = substr(CNTY_CODE,1,1),
#          prv_no = substr(CNTY_CODE,1,2)) %>%
#   ggplot(., aes(x = lm, y = con, color = region_label)) +
#   geom_point(alpha = 0.5) +
#   geom_abline(slope = 1, intercept = 0, linetype = 2) +
#   custom_theme +
#   theme(legend.position = "none") +
#   coord_cartesian(xlim = c(0, 1), ylim = c(0,1)) +
#   scale_color_manual(values = colors_region) +
#   labs(x = "Vaccination Coverage Using\nLinear Population Growth Projection",
#        y = "Vaccination Coverage Using\nPlateaued Population Growth") -> p[[3]]
# 
# (get_legend(p[[1]]+ 
#                   theme(legend.position = "top",
#                         legend.text = element_text(size = 16)) + 
#                   labs(color = "", fill = "") + 
#                   guides(color=guide_legend(nrow=1,byrow=T)))) -> p_legend
# 
# plot_grid(p_legend,
#           p[[2]],
#           plot_grid(p[[1]],p[[3]], nrow = 1, labels = c("(b)", "(c)")),
#           ncol = 1,
#           rel_heights = c(1,10,10),
#           labels = c("", "(a)","")) -> p_save
# 
# 
# 
# ggsave("figs/fig3.png", p_save, height = 15, width = 15)

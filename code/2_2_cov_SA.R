
EV71_Lit %>% 
  ggplot(., aes(x = Age_LL_value, y = cov)) +
  geom_point() +
  geom_segment(aes(xend = Age_UL_value, yend = cov)) +
  facet_wrap(~index)

pop %>% 
  filter(CNTY_CODE == 510722,
         year == 2017,
         ag_LL %in% paste0(0:4)) %>% 
  mutate(all = sum(tot),
         tot_p = tot/all,
         cov = case_when(ag_LL == "0" ~ 0/420,
                         ag_LL == "1" ~ 161/420,
                         ag_LL == "2" ~ 109/420, 
                         ag_LL == "3" ~ 53/420,
                         ag_LL == "4" ~ 33/420)) %>% 
  dplyr::select(ag_LL, cov) -> tmp


pop %>% 
  filter(age_group %in% paste0(0:5, "-"),
         year >= 2016 & year < 2018) %>% 
  group_by(CNTY_CODE, year) %>% 
  mutate(tot_sum = sum(tot)) %>% 
  mutate(tot_p = tot/tot_sum) %>% 
  left_join(tmp, by = "age_group") %>% 
  group_by(CNTY_CODE, year) %>% 
  mutate(doses = tot_p*cov,
         doses_tot = sum(doses),
         doses_p = doses/doses_tot) %>% 
  filter(ag_LL == 5) %>% 
  dplyr::select(CNTY_CODE, year, doses_p) -> tmp

# Age out proportions by year
POP_R_5 %>% 
  rename(YB = perc_5) %>% 
  left_join(tmp, by = c("CNTY_CODE", "year")) %>% 
  # scaled based on Santai Survey
  rename(ST = doses_p) %>% 
  pivot_longer(c("YB", "ST")) %>% 
  unite("tmp", name:year) %>% 
  filter(!(CNTY_CODE %in% c(441325, 450122, 441325))) %>% 
  pivot_wider(names_from = tmp, values_from = value) -> AO

data %>% 
  dplyr::select(CNTY_CODE, starts_with("d2")) %>% 
  pivot_longer(starts_with("d2")) %>% 
  separate(name, into = c("dose", "ag")) %>% 
  group_by(CNTY_CODE) %>% group_split() %>% 
  map(arrange, ag) %>% map(mutate, counts = cumsum(value)) %>% 
  bind_rows() %>% 
  dplyr::select(-value) %>% 
  pivot_wider(names_from = ag, values_from = counts) -> outstanding_doses

outstanding_doses %>% 
  left_join(AO, by = "CNTY_CODE") %>% 
  mutate(ST_doses_2016_LL = `2016`*(1-ST_2016),
         ST_doses_2016_UL = `2016`,
         ST_doses_2017_LL = `2017`*(1-ST_2016),
         ST_doses_2017_UL = `2016`*(1-ST_2016) + (`2017`-`2016`),
         ST_doses_2018_LL = `2018`*(1-ST_2017),
         ST_doses_2018_UL = `2017`*(1-ST_2017) + (`2018`-`2017`),
         YB_doses_2016_LL = `2016`*(1-YB_2016),
         YB_doses_2016_UL = `2016`,
         YB_doses_2017_LL = `2017`*(1-YB_2016),
         YB_doses_2017_UL = `2016`*(1-YB_2016) + (`2017`-`2016`),
         YB_doses_2018_LL = `2018`*(1-YB_2017),
         YB_doses_2018_UL = `2017`*(1-YB_2017) + (`2018`-`2017`)) %>% 
  dplyr::select(CNTY_CODE, ends_with(c("LL", "UL"))) %>% 
  # OD = outstanding doses
  pivot_longer(ends_with(c("LL", "UL")), 
               names_to = "metric", 
               values_to = "OD") %>% 
  separate(metric, into = c("age_structure_source", "dp", "year", "metric")) %>% 
  dplyr::select(-dp) -> outstanding_doses_aged

data %>% 
  dplyr::select(-starts_with("d2")) %>% 
  rename(APC_YB_2016_ob = YB_2016_ob,
         APC_YB_2017_ob = YB_2017_ob,
         APC_YB_2018_lm = YB_2018_lm) %>% 
  mutate(APC_YB_2018_con = APC_YB_2017_ob) %>% 
  pivot_longer(starts_with("APC")) %>% 
  # in process, ob: observation, lm: linear model, con: assumed to be consistent with last year
  separate(name, into = c("dp", "pop_source", "year", "process")) %>% 
  dplyr::select(-dp) -> outstanding_pop

outstanding_pop %>% 
  left_join(outstanding_doses_aged,
            by = c("CNTY_CODE", "year")) %>% 
  mutate(cov = OD/value) -> tmp

tmp %>% 
  unite("all_id", pop_source, process, age_structure_source, metric) %>% 
  mutate(prv_no = substr(CNTY_CODE, 1, 2), year = factor(year)) %>% 
  filter(cov <0 | cov > 1) %>%
  pull(CNTY_CODE) %>% unique  -> to_remove
  
tmp %>% 
  unite("APC", year, process) %>% 
  dplyr::select(-OD, -value) %>% 
  pivot_wider(names_from = APC, values_from = cov) %>% 
  pivot_longer(cols = starts_with("2018")) %>% 
  separate(name, into = c("year", "process")) %>% 
  rename("2018_val" = value) %>% 
  select(-year) %>% 
  filter(CNTY_CODE == "110101",
         pop_source == "YB",
         age_structure_source == "YB") %>% View()


#### provincial level distribution ####
p <- list()
for(i in 1:6){
  tmp %>% 
    filter(pop_source == "YB", age_structure_source == "YB", process == "lm",
           year == 2018, !(CNTY_CODE %in% to_remove)) %>% 
    mutate(prv_no = substr(CNTY_CODE, 1,2),
           region = substr(CNTY_CODE,1,1)) %>% 
    mutate(prv_no = factor(prv_no,
                           levels = prv_list[prv_list$region == i,]$prv,
                           labels = prv_list[prv_list$region == i,]$NAME_EN)) %>% 
    filter(region == i) %>% 
    left_join(labels_region, by = "region") %>% 
    ggplot(., aes(y = cov, x = prv_no, color = metric)) +
    geom_boxplot() +
    coord_flip() +
    scale_x_discrete(drop = F,
                     limits = rev) +
    lims(y = c(0,1)) +
    theme_bw() +
    labs(x = "", y = "Vaccine Coverage in 2018") +
    facet_grid(~region_label) +
    theme(legend.position = "none",
          strip.text = element_text(size = 13, face = "bold")) +
    ggsci::scale_color_lancet(labels = c("Lower Limits",
                                         "Upper Limits")) -> p[[i]]
}

p1 <- plot_grid(p[[1]], p[[2]], p[[3]], ncol = 1,
                rel_heights = c(5,3,7), align = "v")
p2 <- plot_grid(p[[4]], p[[5]], p[[6]], ncol = 1,
                rel_heights = c(5,5,6), align = "v")
p_legend <- get_legend(p[[1]] + 
                         labs(color = "") +
                         theme(legend.position = "top"))

p_save <- plot_grid(p_legend, NA, p1,p2, ncol = 2, rel_heights = c(1,30))  

ggsave(plot = p_save, filename = "figs/pfigures_2.png",
       height = 8, width = 8)

#### withinin provinces #### 
tmp %>% 
  filter(pop_source == "YB", age_structure_source == "YB", process == "lm",
         year == 2018, !(CNTY_CODE %in% to_remove), metric == "LL") %>% 
  mutate(prv_no = substr(CNTY_CODE,1,2)) %>% 
  group_by(prv_no) %>% 
  summarise(median = median(cov),
            sd = sd(cov)) %>% 
  arrange((sd))

p <- list()
for(i in c(22, 31, 44, 62)){
  tmp %>% 
    filter(pop_source == "YB", age_structure_source == "YB", process == "lm",
           year == 2018, !(CNTY_CODE %in% to_remove)) %>% 
    mutate(prv_no = substr(CNTY_CODE,1,2)) %>% 
    filter(prv_no == i) %>% 
    dplyr::select(-OD) %>% 
    pivot_wider(names_from = metric, values_from = cov) %>% 
    right_join(shape %>% 
                 filter(prv == i,
                        lvl == "cty"),
               by = "CNTY_CODE") %>% 
    st_as_sf() %>% 
    ggplot(.) +
    geom_sf(aes(fill = LL)) +
    scale_fill_gradientn(limits = c(0,0.8),
                         colours=c("navyblue", "darkmagenta", "darkorange1"),
                         breaks = c(0, 0.8),
                         na.value = "grey90") +
    geom_sf(data = shape %>% 
              filter(prv == i,
                     prf == "01",
                     lvl == "prf"),
            fill = NA,
            color = "darkorange1") +
    theme_cowplot() +
    labs(title = prv_list %>% 
           left_join(labels_region,
                     by = "region") %>% 
           filter(prv == i) %>% 
           mutate(NAME_EN = as.character(NAME_EN)) %>% 
           .[,c("region_label", "NAME_EN")] %>% 
           unlist %>% 
           paste(., collapse = "-")) +
    theme(legend.position = "none") -> p[[i]]
}

p_legend <- get_legend(p[[31]] + theme(legend.position = "top") + labs(fill = "Vaccine Coverage in 2018:   "))
p_save <- plot_grid(p_legend, NA,
                    p[[31]], p[[44]], rel_widths = c(1, 1.62),
                    rel_heights = c(1,20))

ggsave("figs/pfigure_3.png", width = 15, height = 8)


#### sensitivity analysis by data source ####
p <- list()

tmp %>% 
  filter(!(CNTY_CODE %in% to_remove),
         year == 2018) %>% 
  dplyr::select(-OD, -value)  %>% 
  pivot_wider(names_from = age_structure_source ,
              values_from = cov) %>% 
  filter(process == "lm", pop_source == "YB", metric == "LL") %>% 
  ggplot(., aes(x = YB, y = ST)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0) +
  custom_theme +
  labs(x = "Vaccine Coverage Assuming\nProportional Vaccination Process",
       y = "Vaccine Coverage Assuming\nTargeted Vaccination Process") -> p[[1]]

tmp %>% 
  filter(!(CNTY_CODE %in% to_remove),
         year == 2018) %>% 
  dplyr::select(-OD, -value)  %>% 
  pivot_wider(names_from = pop_source  ,
              values_from = cov) %>% 
  filter(process == "lm", age_structure_source == "YB", metric == "LL") %>% 
  mutate(region = substr(CNTY_CODE,1,1),
         prv_no = substr(CNTY_CODE,1,2)) %>% #,
  #        diff = sur/YB - 1) %>% 
  # ggplot(., aes(x = diff)) +
  # geom_density() +
  # lims(x = c(-1,5))
  ggplot(., aes(x = YB, y = sur)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0) +
  custom_theme +
  # facet_wrap(~prv_no) +
  labs(x = "Vaccine CoverageBased on\nYear Book Population",
       y = "Vaccine CoverageBased on Population\nEstimates in the Surveillance System") -> p[[2]]
  
tmp %>% 
  filter(!(CNTY_CODE %in% to_remove),
         year == 2018) %>% 
  dplyr::select(-OD, -value)  %>% 
  pivot_wider(names_from = process,
              values_from = cov) %>% 
  filter(pop_source  == "YB", age_structure_source == "YB", metric == "LL") %>% 
  mutate(region = substr(CNTY_CODE,1,1),
         prv_no = substr(CNTY_CODE,1,2)) %>%
  ggplot(., aes(x = lm, y = con)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0) +
  custom_theme +
  labs(x = "Vaccination Coverage Using\nLinear Population Growth Projection",
       y = "Vaccination Coverage Using\nPlateaued Population Growth") -> p[[3]]

plot_grid(plotlist = p, nrow = 1,
          labels = c("(a)", "(b)", "(c)")) -> p_save

ggsave("figs/pfigures_4.png", p_save, height = 6, width = 15)

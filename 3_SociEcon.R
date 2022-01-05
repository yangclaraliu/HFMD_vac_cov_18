

pt_imputed <- readRDS("C:/Users/eideyliu/Documents/GitHub/vac2_cov_review/data/pt_imputed.rds") %>% 
  filter(lvl == "cty") %>% 
  dplyr::select(CNTY_CODE, edu, GDPpc, p_child, urban_prop, temp) %>% 
  .[complete.cases(.),]

tmp %>% 
  filter(pop_source == "YB", age_structure_source == "YB", process == "lm",
         year == 2018, !(CNTY_CODE %in% to_remove), metric == "LL") %>% 
  mutate(prv_no = substr(CNTY_CODE,1,2)) %>% 
  select(-OD, -value) %>% 
  left_join(pt_imputed, by = "CNTY_CODE") %>% 
  .[complete.cases(.),] %>% 
  mutate(region = substr(CNTY_CODE, 1, 1)) -> reg_tab

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

reg_tab %>% 
  ggplot(., aes(x = GDPpc, y = cov)) +
  geom_point() +
  scale_x_log10(label=scientific_10) +
  scale_y_log10(label=scientific_10) +
  custom_theme +
  labs(y = "Vaccine Coverage in 2018",
       x = "GDP per Capita") -> p_GDP

reg_tab %>% 
  ggplot(., aes(x = edu, y = cov)) +
  geom_point() +
  scale_x_log10(label=scientific_10) +
  scale_y_log10(label=scientific_10) +
  custom_theme +
  labs(y = "Vaccine Coverage in 2018",
       x = "Average Years of Education") -> p_edu

p_top <- plot_grid(p_edu, p_GDP, align = "hv")

model <- lm(cov ~ GDPpc + edu + prv_no, data = reg_tab) 
se <- sqrt(diag(vcov(model))) %>% tail(-3)


coef(model) %>% 
  tail(-3) %>% 
  enframe %>% 
  mutate(prv_no = as.character(parse_number(name)),
         se = se,
         LL = value - 1.96*se,
         UL = value + 1.96*se,
  ) %>% 
  mutate(nt = ntile(value, 4)) %>% arrange(value) 

coef(model) %>% 
  tail(-3) %>% 
  enframe %>% 
  mutate(prv_no = as.character(parse_number(name)),
         se = se,
         LL = value - 1.96*se,
         UL = value + 1.96*se,
         ) %>% 
  add_row(value = 0, prv_no = "11", se = 0, LL = 0, UL = 0,
          name = "prv_no11") %>% 
  left_join(prv_list, by = c("prv_no" = "prv")) %>% arrange(prv_no) %>% 
  mutate(prv_no = factor(prv_no,
                         levels = prv_list$prv,
                         labels = prv_list$NAME_EN)) %>% 
  ggplot(., aes(x = prv_no)) +
  geom_rect(aes(xmin=("Beijing"),
                xmax = ("Neimenggu"),
                ymin = -Inf,
                ymax = Inf), fill = 'grey90', alpha = 0.04) +
  geom_rect(aes(xmin=("Shanghai"),
                xmax = ("Shandong"),
                ymin = -Inf,
                ymax = Inf), fill = 'grey90', alpha = 0.04) +
  geom_rect(aes(xmin=("Chongqing"),
                xmax = ("Xizang"),
                ymin = -Inf,
                ymax = Inf), fill = 'grey90', alpha = 0.04) +
  geom_point(aes(y = value), size = 2) +
  geom_segment(aes(xend = prv_no, y = LL, yend = UL)) +
  geom_hline(yintercept = 0.137, linetype = 2, color = "red") +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_discrete(drop = F,
                   guide = guide_axis(n.dodge=2)) +
  geom_text(data =   labels_region %>% 
              mutate(co_x = c("Hebei", "Jilin", "Anhui", "Guangdong",
                              "Guizhou", "Qinghai"),
                     co_y = 0.4),
            aes(x = co_x, y = co_y, label = region_label),
            fontface = "bold") +
  custom_theme +
  labs(x = "", y = "Province Specific Intercepts") -> p_bottom

p_save <- plot_grid(p_top, p_bottom, ncol = 1)
ggsave("figs/p_figures_5.pdf", width = 15, height = 8)

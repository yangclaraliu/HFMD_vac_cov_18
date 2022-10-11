

pt_imputed <- readRDS("C:/Users/eideyliu/Documents/GitHub/vac2_cov_review/data/pt_imputed.rds") %>% 
  filter(lvl == "cty") %>% 
  dplyr::select(CNTY_CODE, edu, GDPpc, urban_prop, temp) %>% 
  .[complete.cases(.),]

tmp %>% 
  filter(pop_source == "YB", age_structure_source == "YB", process == "lm",
         year == 2018, !(CNTY_CODE %in% to_remove), metric == "LL") %>% 
  mutate(prv_no = substr(CNTY_CODE,1,2)) %>% 
  dplyr::select(-OD, -value) %>% 
  left_join(pt_imputed, by = "CNTY_CODE") %>% 
  .[complete.cases(.),] %>% 
  left_join(risky_pop[,c("CNTY_CODE",
                         "p_under5",
                         "tot")],
            by = "CNTY_CODE") |> 
  complete() |> 
  mutate(region = substr(CNTY_CODE, 1, 1),
         edu_s = (edu-mean(edu))/sd(edu),
         GDPpc_s = (GDPpc - mean(GDPpc))/sd(GDPpc),
         p_under5_s = (p_under5 - mean(p_under5))/sd(p_under5),
         tot_s = (tot-mean(tot))/sd(tot)) -> reg_tab



scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

model <- gamlss(cov ~ GDPpc + edu + prv_no + p_under5 + tot,
                family = BEZI,
                data = reg_tab,
                trace = T) 

model_s <- gamlss(cov ~ GDPpc_s + edu_s + prv_no + p_under5_s + tot_s,
                family = BEZI,
                data = reg_tab,
                trace = T) 

model_summary <- summary(model)
model_s_summary <- summary(model_s)
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
model_summary[,1:2] |> 
  data.frame() |> 
  rownames_to_column() |> 
  filter(grepl("prv",rowname)) |> 
  mutate(prv_no = as.character(parse_number(rowname)),
         LL = Estimate  - 1.96*`Std..Error`,
         UL = Estimate  + 1.96*`Std..Error`,
  ) %>% 
  add_row(Estimate  = 0, prv_no = "11", LL = 0, UL = 0) %>% 
  left_join(prv_list, by = c("prv_no" = "prv")) %>% arrange(prv_no) %>% 
  mutate(prv_no = factor(prv_no,
                         levels = prv_list$prv,
                         labels = prv_list$NAME_EN)) %>% 
  filter(!is.na(prv_no)) |> 
  left_join(labels_region, by = "region") |> 
  rename(value = Estimate) -> p_table
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

prv_list |> 
  left_join(labels_region) |> 
  group_by(region_label) |> tally() |> pull(n) -> rep_times

rep(colors_region,
    times = rep_times) -> colors_label
require(scales)
ggplot(p_table,
       aes(x = prv_no, color = region_label)) +

  geom_rect(aes(xmin= c(0)-0.5,
                xmax = c(5)+0.5,
                ymin = -Inf,
                ymax = Inf), fill = "grey90", alpha = 0.04, color = NA) +
  geom_rect(aes(xmin= c(9)-0.5,
                xmax = c(15)+0.5,
                ymin = -Inf,
                ymax = Inf), fill = "grey90", alpha = 0.04, color = NA) +
  geom_rect(aes(xmin= c(22)-0.5,
                xmax = c(26)+0.5,
                ymin = -Inf,
                ymax = Inf), fill = "grey90", alpha = 0.04, color = NA) +
  geom_point(aes(y = value), size = 2) +
  geom_segment(aes(xend = prv_no, y = LL, yend = UL)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_discrete(drop = F,
                   guide = guide_axis(n.dodge = 2)) +
  custom_theme +
  theme(legend.position = "top",
        legend.text = element_text(size = 16)) + 
  guides(color=guide_legend(nrow=1,byrow=T)) +
  scale_color_manual(values = colors_region) +
  labs(x = "", y = "Province Specific Intercepts", color = "") -> p_bottom

scientific_10 <- function(x) {
  parse(text=gsub("e", "%*%10^", scales::scientific_format()(x)))
}
reg_tab |>  
  dplyr::select(CNTY_CODE, cov, GDPpc, edu, p_under5, tot, region) |> 
  pivot_longer(cols = c("GDPpc","edu", "p_under5", "tot")) |>
  mutate(name = factor(name,
                       levels = c("GDPpc","edu", "p_under5", "tot"),
                       labels = c("GDP per capita",
                                  "Education level",
                                  "Proportion of population\nunder 5 years",
                                  "Total population size"))) -> p_table
p_top <- list()

p_table |> 
  filter(name == "GDP per capita") |> 
  ggplot((aes(x = value, y = cov, color = region))) +
  geom_point(alpha = 0.4) +
  facet_wrap(~name, scales = "free", nrow = 1) +
  custom_theme +
  labs(y = "") +
  lims(y = c(0,1)) +
  theme(legend.position = "none",
        # axis.text.x = element_text(size = 12),
        strip.background = element_rect(fill = NA, colour = "black"),
        legend.text = element_text(size = 16)) + 
  guides(color=guide_legend(nrow=1,byrow=T)) +
  scale_x_log10(label = comma,
                breaks = c(10^4,10^5)) +
  labs(x = "") +
  scale_color_manual(values = colors_region)  -> p_top[[1]]

p_table |> 
  filter(name == "Education level") |> 
  ggplot((aes(x = value, y = cov, color = region))) +
  geom_point(alpha = 0.4) +
  labs(y = "") +
  facet_wrap(~name, scales = "free", nrow = 1) +
  custom_theme +
  lims(y = c(0,1)) +
  theme(legend.position = "none",
        # axis.text.x = element_text(size = 12),
        strip.background = element_rect(fill = NA, colour = "black"),
        legend.text = element_text(size = 16)) + 
  guides(color=guide_legend(nrow=1,byrow=T)) +
  scale_x_log10(label = comma,
                breaks = c(5,10)) +
  labs(x = "") +
  scale_color_manual(values = colors_region) -> p_top[[2]]

p_table |> 
  filter(name == "Proportion of population\nunder 5 years") |> 
  ggplot((aes(x = value, y = cov, color = region))) +
  geom_point(alpha = 0.4) +
  facet_wrap(~name, scales = "free", nrow = 1) +
  custom_theme +
  labs(y = "") +
  lims(y = c(0,1)) +
  theme(legend.position = "none",
        # axis.text.x = element_text(size = 12),
        strip.background = element_rect(fill = NA, colour = "black"),
        legend.text = element_text(size = 16)) + 
  guides(color=guide_legend(nrow=1,byrow=T)) +
  scale_x_log10(label = comma,
                breaks = c(0.02, 0.05,0.1)) +
  labs(x = "") +
  scale_color_manual(values = colors_region) -> p_top[[3]]

p_table |> 
  filter(name == "Total population size") |> 
  ggplot((aes(x = value, y = cov, color = region))) +
  geom_point(alpha = 0.4) +
  facet_wrap(~name, scales = "free", nrow = 1) +
  custom_theme +
  labs(y = "") +
  lims(y = c(0,1)) +
  theme(legend.position = "none",
        # axis.text.x = element_text(size = 12),
        strip.background = element_rect(fill = NA, colour = "black"),
        legend.text = element_text(size = 16)) + 
  guides(color=guide_legend(nrow=1,byrow=T)) +
  scale_x_log10(label = comma,
                breaks = c(10^5,10^6)) +
  labs(x = "") +
  scale_color_manual(values = colors_region) -> p_top[[4]]


p_save <- plot_grid(p_bottom, plot_grid(plotlist = (p_top), nrow = 1, axis = "l", align = "h")+
                      draw_label("Coverage", x=  0, y=0.5, vjust= 1.5, angle=90), ncol = 1)
ggsave("figs/fig5.png", width = 16, height = 8)

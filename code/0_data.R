if(!require(pacman)) install.packages("pacman")
p_load(tidyverse, readxl, sf, data.table, magrittr, cowplot, viridis)
path_dropbox <- "~/Dropbox/EPIC/[Data] China/"
path_dropbox_github <- "~/Dropbox/Github_Data/HFMD_cov_2019/"
#### CNTY_CODE ####
CNTY_CODE <- paste0(path_dropbox, "Urbanicity/2017_CNTY_CODE.xlsx") %>% read_excel() %>% 
  .[,c(1:3,6:8)] %>% 
  distinct() %>% 
  mutate(CNTY_CODE = paste0(`省级代码`,
                            `市级代码`,
                            `县级代码`))

#### shape file####
prefectures <- paste0(path_dropbox,"Different Versions of Administrative Boundary/CCDC2022/dishi.shp") %>% sf::read_sf()
counties <- paste0(path_dropbox,"Different Versions of Administrative Boundary/CCDC2022/quxian.shp") %>% sf::read_sf()
provinces <-  paste0(path_dropbox,"Different Versions of Administrative Boundary/CCDC2022/sheng.shp") %>% sf::read_sf()

provinces %>% 
  mutate(code_prv = substr(ZONECODE,1,2)) %>% 
  dplyr::filter(!code_prv %in% c(71, 81, 82)) %>% 
  dplyr::select(code_prv, NAME, PYNAME) %>% 
  rename(NAME_EN = PYNAME) %>% 
  mutate(region = substr(code_prv, 1, 1)) %>% 
  separate(NAME_EN, into = c("seg1", "seg2", "seg3")) %>% 
  dplyr::select(-seg2, -seg3) %>% 
  rename(NAME_EN = seg1) %>% 
  mutate(region = factor(region,
                         levels = 1:6,
                         labels = c("Huabei\n(~North)",
                                    "Dongbei\n(~Northeast)",
                                    "Huadong\n(~East)",
                                    "Zhongnan\n(~South)",
                                    "Xinan\n(~Southwest)",
                                    "Xibei\n(~Northwest)")),
         NAME_EN = if_else(code_prv == "61", "Shaanxi", NAME_EN)) %>% 
  arrange(code_prv) -> shape_prv

counties %>% 
  dplyr::select(NAME, PYNAME, CNTY_CODE) %>% 
  mutate(CNTY_CODE = substr(CNTY_CODE, 1,6),
         code_prv = substr(CNTY_CODE, 1, 2),
         code_prf = substr(CNTY_CODE, 3, 4),
         code_cty = substr(CNTY_CODE, 5,6)) %>% 
  dplyr::filter(code_cty != "00") -> shape_cty

target_province <- read_rds(paste0(path_dropbox_github, "prv_data_exist.rds"))

labels_region <- data.frame(labels_region = 1:6,
                            names_region = c(
                              "Huabei\n(~North)",
                              "Dongbei\n(~Northeast)",
                              "Huadong\n(~East)",
                              "Zhongnan\n(~South)",
                              "Xinan\n(~Southwest)",
                              "Xibei\n(~Northwest)"
                            ))


#### population age structure ####
# this document only contains counties of the 23 provinces with inoculation data
pop_all <- read_rds(paste0(path_dropbox_github, "pop_tar.rds")) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "year",
               values_to = "pop")

APC <- pop_all %>% 
  dplyr::filter(ag_LL < 6) %>% 
  pivot_wider(names_from = "ag_LL",
              values_from = "pop") %>% 
  mutate(APC = 0.5*`0` + `1` + `2` +`3` + `4` + `5`,
         year = as.numeric(year)) %>% 
  dplyr::select(CNTY_CODE, code_prv, year, APC)

# APC <- pop_all %>% 
#   dplyr::filter(ag_LL <= 5) %>% 
#   group_by(CNTY_CODE, code_prv, year) %>% 
#   summarise(APC = sum(pop)) %>% 
#   mutate(year = as.numeric(year))

pop_all %>% 
  group_by(CNTY_CODE, code_prv, year) %>% 
  summarise(tot = sum(pop)) %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(APC,
            by = c("CNTY_CODE",
                   "code_prv",
                   "year")) %>% 
  dplyr::filter(!is.na(APC)) %>% 
  mutate(p_risk = APC/tot,
         label_region = substr(CNTY_CODE, 1, 1)) %>% 
  group_by(year) %>% 
  mutate(p_risk_national = sum(APC)/sum(tot)) -> APC

# pop_all[,"CNTY_CODE"] %>% 
#   distinct() %>% 
#   dplyr::filter(CNTY_CODE %in% shape_cty$CNTY_CODE)
# 93% locations have been found in the shape file

#### inoculation #####
vac_all <- read_rds(paste(path_dropbox_github, "vac_all.rds")) %>% 
  rename(CNTY_CODE = CNTY_CODE_shape,
         code_prv = code_prf)

# merge tables 
vac_all %>% 
  left_join(APC, by = c("CNTY_CODE", "year", "code_prv")) %>% 
  ungroup %>% 
  dplyr::filter(!is.na(APC)) %>% 
  group_by(year) %>% 
  mutate(p_risk = APC/tot,
         APC_national = sum(APC),
         tot_national = sum(tot),
         p_risk_national = APC_national/tot_national) %>% 
  dplyr::select(-APC_national,
                -tot_national) -> tab

custom_theme <-
  theme_cowplot() +
  theme(strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(size = 14),
        legend.position = "top",
        legend.justification = "left",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.background = element_rect(fill = NA, color = "black")) 

colors_region <- ggsci::pal_lancet()(6)
cov <- read_rds(paste0(path_dropbox_github, "coverage_results_20250129.rds"))
# source("code/plot_fig1.R")
# source("code/plot_fig2_pop.R")

pt_imputed <- readRDS("/Users/yangliu/Library/CloudStorage/Dropbox/Github_Data/vac2_cov_review/pt_imputed.rds") %>% 
  filter(lvl == "cty") %>% 
  dplyr::select(CNTY_CODE, edu, GDPpc, urban_prop, temp) %>% 
  .[complete.cases(.),]

#### read in epi data ####
epi <- read_excel(paste0(path_dropbox_github, "Copy of HFMDcountydata.xlsx")) %>% 
  mutate(CNTY_CODE = substr(`地区编码`, 1,6)) %>% 
  dplyr::select(-`地区名称`, -`地区编码`) %>% 
  pivot_wider(names_from = `年份`,
              values_from = `发病数`) %>% 
  .[complete.cases(.),] %>% 
  mutate(cases = `2009` + `2010` + `2011` + `2012` + `2013` + `2014` + `2015` + `2016`) %>% 
  left_join(pop_all %>% 
              dplyr::filter(year == 2016,
                            ag_LL <= 5) %>% 
              group_by(CNTY_CODE) %>% 
              summarise(pop = sum(pop)),
            by = "CNTY_CODE") %>% 
  dplyr::filter(!is.na(pop)) %>% 
  mutate(burden = (cases/8)/pop,
         burden_s = (burden-mean(burden))/sd(burden))

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}
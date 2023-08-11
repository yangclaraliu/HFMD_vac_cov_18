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
                         labels = c("North", "North East", "South East","South",
                                    "South West", "North West"))) -> shape_prv

counties %>% 
  dplyr::select(NAME, PYNAME, CNTY_CODE) %>% 
  mutate(CNTY_CODE = substr(CNTY_CODE, 1,6),
         code_prv = substr(CNTY_CODE, 1, 2),
         code_prf = substr(CNTY_CODE, 3, 4),
         code_cty = substr(CNTY_CODE, 5,6)) %>% 
  dplyr::filter(code_cty != "00") -> shape_cty

target_province <- read_rds(paste0(path_dropbox_github, "prv_data_exist.rds"))

#### population age structure ####
# this document only contains counties of the 23 provinces with inoculation data
pop_all <- read_rds(paste0(path_dropbox_github, "pop_tar.rds")) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "year",
               values_to = "pop")

APC <- pop_all %>% 
  dplyr::filter(ag_LL < 5) %>% 
  group_by(CNTY_CODE, code_prv, year) %>% 
  summarise(APC = sum(pop)) %>% 
  mutate(year = as.numeric(year))

pop_all %>% 
  group_by(CNTY_CODE, code_prv, year) %>% 
  summarise(tot = sum(pop)) %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(APC,
            by = c("CNTY_CODE",
                   "code_prv",
                   "year")) %>% 
  dplyr::filter(!is.na(APC)) -> APC

pop_all[,"CNTY_CODE"] %>% 
  distinct() %>% 
  dplyr::filter(CNTY_CODE %in% shape_cty$CNTY_CODE)
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
        axis.text = element_text(size = 14),
        legend.position = "top",
        legend.justification = "left",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) 

colors_region <- ggsci::pal_lancet()(6)

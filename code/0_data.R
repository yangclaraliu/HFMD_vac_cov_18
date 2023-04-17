if(!require(pacman)) install.packages("pacman")
p_load(tidyverse, readxl, sf, data.table, magrittr, cowplot, viridis)
path_dropbox <- "~/Dropbox/EPIC/[Data] China/"

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

#### population age structure ####
pop <- read_rds(paste0(path_dropbox, "Pop_all/cdc_pop_res_cty.rds")) %>% 
  .[,c(1,5,6,9)] %>% 
  setNames(c("CNTY_CODE", "year", "age_group", "tot")) %>% 
  mutate(CNTY_CODE = substr(CNTY_CODE,1,6),
         ag_LL = parse_number(as.character(age_group))) %>% 
  dplyr::filter(year != 2018)

pop_2018 <- read_xlsx(paste0(path_dropbox, "China_POP_2018.xlsx")) %>% 
  setNames(c("year", "CNTY_CODE", "location", "age_group", "male", "female","tot")) %>% 
  mutate(CNTY_CODE = as.character(CNTY_CODE),
         n_CNTY_CODE = nchar(CNTY_CODE)) %>% 
  dplyr::filter(n_CNTY_CODE >= 6) %>% 
  mutate(CNTY_CODE = substr(CNTY_CODE, 1,  6),
         code_prv = substr(CNTY_CODE, 1, 2),
         code_prf = substr(CNTY_CODE, 3, 4),
         code_cty = substr(CNTY_CODE, 5, 6),
         age_group = as.character(age_group),
         ag_LL = parse_number(age_group)) %>% 
  dplyr::filter(code_prf != "00",
                code_cty != "00") %>% 
  dplyr::select(CNTY_CODE, year, age_group, tot, ag_LL)

bind_rows(pop, pop_2018) %>% 
  mutate(code_prv = substr(CNTY_CODE, 1, 2),
         code_prf = substr(CNTY_CODE, 3, 4),
         code_cty = substr(CNTY_CODE, 5, 6)) -> pop

pop_missing <- data.table(CNTY_CODE_missing = sort(unique(pop$CNTY_CODE))[which(!(sort(unique(pop$CNTY_CODE)) %in% shape_cty$CNTY_CODE))])

pop_missing %<>% 
  left_join(CNTY_CODE, by = c("CNTY_CODE_missing" = "CNTY_CODE")) %>% 
  filter(!is.na(`省级代码`)) %>% 
  mutate(sn = substr(`县（区、市、旗）`,1,2),
         CNTY_CODE_new = NA)

for(i in 1:nrow(pop_missing)){
  tmp <- shape_cty[grep(pop_missing$sn[i], shape_cty$NAME),]
  if(nrow(tmp) == 1) pop_missing$CNTY_CODE_new[i] <- tmp$CNTY_CODE
}

pop_missing %<>% filter(!is.na(CNTY_CODE_new))

for(i in 1:nrow(pop_missing)){
  pop %<>% 
    mutate(CNTY_CODE = if_else(CNTY_CODE == pop_missing$CNTY_CODE_missing[i],
                               pop_missing$CNTY_CODE_new[i],
                               CNTY_CODE))
}

which((pop$CNTY_CODE %>% unique) %in% shape_cty$CNTY_CODE) %>% length -> n_find
((pop$CNTY_CODE %>% unique)) %>% length -> n_tot
# location that cannot be found is roughly 15.6%
1 - n_find/n_tot

pop %>% 
  group_by(CNTY_CODE, year, ag_LL) %>% 
  summarise(tot = sum(tot)) -> pop

pop %>% 
  filter(ag_LL <= 4, year >= 2016) %>% 
  group_by(year, CNTY_CODE) %>% 
  summarise(pop = sum(tot)) -> POP_05

pop %>% 
  data.table %>% 
  filter(year >= 2016,
         ag_LL == 4) %>% 
  left_join(POP_05, by = c("year", "CNTY_CODE")) %>% 
  dplyr::select(-ag_LL) %>% 
  mutate(perc_5 = tot/pop) %>% 
  dplyr::select(CNTY_CODE, year, perc_5) -> POP_R_5
  
### inoculation #####
raw <- list()
for(i in 1:2){
  paste0(path_dropbox, "[Data] HFMD/2016-2018 data of vaccine inoculation/2016-2018 data of vaccine inoculation 0712.xlsx") %>% readxl::read_excel(sheet = i, col_types = "text") -> raw[[i]]
}
raw[[1]] <- raw[[1]][-1,]
raw_save <- raw

# APC = age appropriate children
# 441901, 442001, 441900
raw[[1]] %<>% 
  .[,c(2,3,6,7,9,10,12)] %>% 
  setNames(c("NAME_prf","NAME_cty", "CNTY_CODE",
             "APC_2016","d2_2016",
             "APC_2017","d2_2017")) %>% 
  mutate(prv = substr(CNTY_CODE,1,2),
         CNTY_CODE = substr(CNTY_CODE,1,6),
         APC_2016 = if_else(APC_2016 == "-",
                            as.character(NA),
                            APC_2016)) %>% 
  left_join(shape_prv %>% 
              data.frame %>% 
              .[,1:3], 
            by = c("prv" = "code_prv")) %>% 
  rename(NAME_prv = NAME_EN) %>% 
  dplyr::filter(!is.na(CNTY_CODE) & CNTY_CODE != "0" & CNTY_CODE != "无编码")

raw[[1]] %<>% 
  group_by(CNTY_CODE) %>% 
  mutate_at(vars(c(APC_2016,APC_2017,d2_2016, d2_2017)), as.numeric) %>% 
  summarise_at(vars(c("APC_2016","APC_2017",d2_2016, d2_2017)), sum) 

raw[[2]] %<>% 
  .[,c(2,3,4,5,7,9)] %>% 
  setNames(c("month",
             "NAME_prv", "NAME_prf", "NAME_cty", "CNTY_CODE","d2_2018")) %>%
  mutate(NAME_prf = gsub("[[^\u0001-\u007F]+", "", NAME_prf),
         NAME_cty = gsub("[[^\u0001-\u007F]+", "", NAME_cty),
         d2_2018 = as.numeric(d2_2018),
         d2_2018 = if_else(is.na(d2_2018), 0, d2_2018),
         CNTY_CODE = substr(CNTY_CODE,1,6)) %>%
  group_by(month, CNTY_CODE) %>% summarise(d2_2018 = sum(d2_2018, na.rm = T)) %>% 
  group_by(CNTY_CODE) %>% group_split() %>% 
  map(mutate, n_month = length(month)) %>% bind_rows() %>% 
  group_by(CNTY_CODE) %>% 
  summarise(d2_2018 = sum(as.numeric(d2_2018)),
            n_month = mean(as.numeric(n_month))) %>% 
  mutate(d2_2018 = d2_2018*12/n_month) %>% 
  dplyr::select(CNTY_CODE, d2_2018)

raw[[1]] %>% 
  left_join(raw[[2]], by = "CNTY_CODE") -> data

data.table(CNTY_CODE_missing = data[which(!(data$CNTY_CODE %in% shape_cty$CNTY_CODE)),]$CNTY_CODE) %>% 
  mutate(NAME_prv = as.character(NA),
         NAME_prf = as.character(NA),
         NAME_cty = as.character(NA)) -> missing

for(i in 1:nrow(missing)){
  tmp <- raw_save[[2]] %>% filter(`County Code` == missing$CNTY_CODE_missing[i]) %>% 
    .[1,3:5]
  missing[i,]$NAME_prv <- tmp[1,1]
  missing[i,]$NAME_prf <- tmp[1,2]
  missing[i,]$NAME_cty <- tmp[1,3]
  
  if(is.na(missing[i,]$NAME_prv)){
    raw_save[[1]] %>% filter(`County Code` == missing$CNTY_CODE_missing[i]) %>% 
      .[1,2:3] -> tmp
    missing[i,]$NAME_prf <- tmp[1,1]
    missing[i,]$NAME_cty <- tmp[1,2]
  }
}
 
missing %<>% 
  mutate(sn = substr(NAME_cty,1,2), CNTY_CODE_new = NA) %>% 
  left_join(shape_prv, by = c("NAME_prv" = "NAME"))  %>% 
  mutate(code_prv = if_else(NAME_prv == "广西省",
                       "45",
                       code_prv),
         code_prv = if_else(NAME_prv == "宁夏",
                       "62",
                       code_prv),
         code_prv = if_else(NAME_prv == "吉林",
                       "22",
                       code_prv),
         code_prv = if_else(NAME_prf == "唐山市",
                       "13",
                       code_prv),
         code_prv = if_else(NAME_prf == "常德市",
                       "43",
                       code_prv)
  ) 

for(i in 1:nrow(missing)){
  tmp <- shape_cty[grep(missing$sn[i], shape_cty$NAME),c("CNTY_CODE","code_prv")] %>% 
    dplyr::filter(code_prv  == missing$code_prv[i])
  if(nrow(tmp) == 1) missing$CNTY_CODE_new[i] <- tmp$CNTY_CODE
}

missing %<>% 
  mutate(CNTY_CODE_new = if_else(CNTY_CODE_new == "450000",
                                 as.character(NA),
                                 CNTY_CODE_new),
         CNTY_CODE_new = if_else(CNTY_CODE_new %in% c("371700","330300"),
                                 as.character(NA),
                                 CNTY_CODE_new))

for(i in 1:nrow(missing)){
  data %<>% 
    mutate(CNTY_CODE = if_else(!is.na(missing$CNTY_CODE_missing[i]) & 
                                 CNTY_CODE == missing$CNTY_CODE_missing[i],
                               missing$CNTY_CODE_new[i],
                               CNTY_CODE))
}

data %>% 
  filter(is.na(CNTY_CODE)) %>% 
  summarise(d2_2016 = sum(d2_2016, na.rm = T),
            d2_2017 = sum(d2_2017, na.rm = T),
            d2_2018 = sum(d2_2018, na.rm = T)) %>% unlist %>% sum -> doses_missed

data %>% 
  summarise(d2_2016 = sum(d2_2016, na.rm = T),
            d2_2017 = sum(d2_2017, na.rm = T),
            d2_2018 = sum(d2_2018, na.rm = T)) %>% unlist %>% sum -> doses_tot

# approximately 3.5% doses cannot be geocoded
doses_missed/doses_tot

data %<>% 
  group_by(CNTY_CODE) %>% 
  summarise(APC_2016 = sum(APC_2016),
            APC_2017 = sum(APC_2017),
            d2_2016 = sum(d2_2016),
            d2_2017 = sum(d2_2017),
            d2_2018 = sum(d2_2018)) %>% 
  .[complete.cases(.),]


# data %>% 
#   mutate(CNTY_CODE = case_when(CNTY_CODE == "441900" ~ "441901",
#                                CNTY_CODE == "442000" ~ "442001",
#                                TRUE ~ CNTY_CODE)) %>% 
#   filter(!is.na(CNTY_CODE)) %>% 
#   left_join(POP_05 %>% 
#               pivot_wider(names_from = year,
#                           values_from = pop),
#             by = "CNTY_CODE") %>%
#   filter(!is.na(d2_2016) & !is.na(d2_2017) & !is.na(d2_2018) & !is.na(APC_2016) & !is.na(APC_2017) &
#            !is.na(`2017`) & !is.na(`2018`) & !is.na(`2016`)) %>%
#   left_join(shape_cty, by = "CNTY_CODE") %>%
#   pull(lvl) %>% table

# attach registered children

data %>% 
  dplyr::select(-starts_with("APC", ignore.case = F)) %>% 
  pivot_longer(cols = starts_with("d2", ignore.case = F)) %>% 
  separate(name, into = c("dose", "year")) %>% 
  dplyr::select(-dose) %>% 

  mutate(year = as.numeric(year)) %>% 
  left_join(POP_05, by = c("CNTY_CODE", "year"))

APC_YB <- pop %>% 
  filter(ag_LL %in% 0:5) %>% 
  dplyr::select(-ag_LL) %>%   
  filter(year >= 2016) %>% 
  group_by(CNTY_CODE, year) %>% 
  summarise(APC = sum(tot)) %>% 
  pivot_wider(names_from = year,
              values_from = APC) %>% 
  setNames(c("CNTY_CODE", "YB_2016", "YB_2017", "YB_2018"))

APC_sur <- qs::qread("data/APC_sur.qs")

data %<>% 
  left_join(APC_YB,
            by = "CNTY_CODE") %>% 
  # we believe it's errors when d2_2016 and d2_2017 are not 0s but d2_2018 is
  filter(!(d2_2016 != 0 & d2_2017 != 0 & d2_2018 == 0)) %>% 
  filter(!is.na(YB_2016) & !is.na(YB_2017) & !is.na(YB_2018))

# data %<>% 
#   right_join(APC_sur, by = "CNTY_CODE") %>%
#   .[complete.cases(.),] %>% 
#   rename(YB_2018_lm = YB_2018,
#          YB_2018_con = YB_2017,
#          YB_2017_ob = YB_2017,
#          YB_2016_ob = YB_2016)

data %<>% 
  left_join(APC_sur[,c(1,2,5)],
            by = "CNTY_CODE") %>% 
  .[complete.cases(.),] %>% 
  rename(APC_2018_lm = APC_sur_2018_lm,
         APC_2018_con = APC_sur_2018_con)

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

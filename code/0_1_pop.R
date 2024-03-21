pop <- read_excel(paste0(path_dropbox, "Pop_all/CDC_pop_res_cty.xlsx")) 
pop <- setNames(pop,
                c("CNTY_CODE", "name", "lvl", "lvl_name", "year", "age_group", "pop_male", "pop_female", "pop"))
pop %<>% 
  mutate(CNTY_CODE = substr(CNTY_CODE, 1, 6),
         age_group = gsub("-","",age_group),
         ag_LL = parse_number(age_group)) %>% 
  dplyr::select(-lvl, -lvl_name, -pop_male, -pop_female) #%>% 
#  dplyr::filter(year > 2015 & year < 2018) 

# pop %<>% 
#   mutate(CNTY_CODE = substr(CNTY_CODE, 1, 6),
#          age_group = gsub("-","",age_group),
#          ag_LL = parse_number(age_group)) %>% 
#   dplyr::select(-lvl, -lvl_name, -pop_male, -pop_female) %>% 
#   dplyr::filter(year > 2015 & year < 2018) 


pop_2018 <- read_excel(paste0(path_dropbox, "[Data] HFMD/20230802_2018_pop.xlsx"),
                       col_types = "text") %>% 
  setNames(c("year", "CNTY_CODE", "name", "age_group", "pop_male", "pop_female", "tot")) %>% 
  dplyr::filter(CNTY_CODE != "0") %>% 
  mutate(CNTY_CODE = substr(CNTY_CODE,1,6),
         code_prv = substr(CNTY_CODE,1,2),
         code_prf = substr(CNTY_CODE,3,4),
         code_cty = substr(CNTY_CODE,5,6),
         ag_LL = parse_number(as.character(age_group)),
         pop = as.numeric(tot)) %>% 
  dplyr::filter(code_prf != "00",
                code_cty != "00") %>% 
  dplyr::select(colnames(pop))

pop_2019 <- 
  read_excel(paste0(path_dropbox, "[Data] HFMD/20230802_2019_pop.xlsx"),
             col_types = "text") %>% 
  setNames(c("year", "CNTY_CODE", "name", "age_group", "pop_male", "pop_female", "tot")) %>% 
  dplyr::filter(CNTY_CODE != "0") %>% 
  mutate(CNTY_CODE = substr(CNTY_CODE,1,6),
         code_prv = substr(CNTY_CODE,1,2),
         code_prf = substr(CNTY_CODE,3,4),
         code_cty = substr(CNTY_CODE,5,6),
         ag_LL = parse_number(as.character(age_group)),
         pop = as.numeric(tot)) %>% 
  dplyr::filter(code_prf != "00",
                code_cty != "00",
                year == "2019") %>% 
  dplyr::select(colnames(pop))

# pop_2018 <- read_xlsx(paste0(path_dropbox, "China_POP_2018.xlsx"), col_types = "text") %>% 
#   setNames(c("year", "CNTY_CODE", "location", "age_group", "male", "female","tot")) %>% 
#   mutate(CNTY_CODE = as.character(CNTY_CODE),
#          n_CNTY_CODE = nchar(CNTY_CODE)) %>% 
#   dplyr::filter(n_CNTY_CODE >= 6) %>% 
#   mutate(CNTY_CODE = substr(CNTY_CODE, 1,  6),
#          code_prv = substr(CNTY_CODE, 1, 2),
#          code_prf = substr(CNTY_CODE, 3, 4),
#          code_cty = substr(CNTY_CODE, 5, 6),
#          age_group = as.character(age_group),
#          ag_LL = parse_number(age_group),
#          tot = parse_number(tot)) %>% 
#   dplyr::filter(code_prf != "00",
#                 code_cty != "00") %>% 
#   dplyr::select(CNTY_CODE, year, age_group, tot, ag_LL)

bind_rows(pop,
          pop_2018 %>% mutate(year = as.numeric(year)), 
          pop_2019 %>% mutate(year = as.numeric(year))) %>% 
  mutate(code_prv = substr(CNTY_CODE, 1, 2),
         code_prf = substr(CNTY_CODE, 3, 4),
         code_cty = substr(CNTY_CODE, 5, 6)) %>% 
  dplyr::filter(code_prv %in% target_province$code_prv) -> pop_all

shape_cty %>% 
  dplyr::filter(code_prv %in% target_province$code_prv) -> target_county

# pop_missing <- data.table(CNTY_CODE_missing = sort(unique(pop$CNTY_CODE))[which(!(sort(unique(pop$CNTY_CODE)) %in% shape_cty$CNTY_CODE))])
# 
# pop_missing %<>% 
#   left_join(CNTY_CODE, by = c("CNTY_CODE_missing" = "CNTY_CODE")) %>% 
#   filter(!is.na(`省级代码`)) %>% 
#   mutate(sn = substr(`县（区、市、旗）`,1,2),
#          CNTY_CODE_new = NA)
# 
# for(i in 1:nrow(pop_missing)){
#   tmp <- shape_cty[grep(pop_missing$sn[i], shape_cty$NAME),]
#   if(nrow(tmp) == 1) pop_missing$CNTY_CODE_new[i] <- tmp$CNTY_CODE
# }
# 
# pop_missing %<>% filter(!is.na(CNTY_CODE_new))
# 
# for(i in 1:nrow(pop_missing)){
#   pop %<>% 
#     mutate(CNTY_CODE = if_else(CNTY_CODE == pop_missing$CNTY_CODE_missing[i],
#                                pop_missing$CNTY_CODE_new[i],
#                                CNTY_CODE))
# }
# 
# which((pop$CNTY_CODE %>% unique) %in% shape_cty$CNTY_CODE) %>% length -> n_find
# ((pop$CNTY_CODE %>% unique)) %>% length -> n_tot
# # location that cannot be found is roughly 9.5%, only focusing on the 23 provinces
# # we have data for
# 1 - n_find/n_tot
# 
# pop %>% 
#   group_by(CNTY_CODE, year, ag_LL) %>% 
#   summarise(tot = sum(tot)) -> pop

pop_all %>% 
  group_by(year, name, CNTY_CODE, ag_LL) %>% 
  summarise(pop = sum(pop)) %>% 
  mutate(year = as.character(year),
         code_prv = substr(CNTY_CODE,1,2)) %>% 
  pivot_wider(names_from = year,
              values_from = pop) %>% 
  .[!complete.cases(.),] -> missing

pop_all %>% 
  group_by(year, CNTY_CODE, ag_LL) %>% 
  summarise(pop = sum(pop)) %>% 
  mutate(year = as.character(year),
         code_prv = substr(CNTY_CODE,1,2)) %>% 
  pivot_wider(names_from = year,
              values_from = pop) %>% 
  .[complete.cases(.),] -> complete

fix_pairs <- read_excel(paste0(path_dropbox_github, "pop_names_fix_pairs.xlsx"))

for(i in 1:nrow(fix_pairs)){
 marker_1 <- fix_pairs[i,]$ver1 %in% shape_cty$CNTY_CODE
 marker_2 <- fix_pairs[i,]$ver2 %in% shape_cty$CNTY_CODE
 
 if(marker_1 + marker_2 == 1){
   if(marker_2){
     missing %<>% 
       mutate(CNTY_CODE = if_else(CNTY_CODE == as.character(fix_pairs[i,]$ver1),
                                  as.character(fix_pairs[i,]$ver2),
                                  CNTY_CODE))
   }
   
   if(marker_1){
     missing %<>% 
       mutate(CNTY_CODE = if_else(CNTY_CODE == as.character(fix_pairs[i,]$ver2),
                                  as.character(fix_pairs[i,]$ver1),
                                  CNTY_CODE))
   }
 } 
}

missing %>% 
  rename(loc = name) %>% 
  pivot_longer(cols = starts_with("20")) %>% 
  dplyr::filter(!is.na(value)) %>% 
  arrange(CNTY_CODE) %>%
  group_by(CNTY_CODE, code_prv, name, ag_LL) %>% 
  summarise(value = sum(value)) %>% 
  pivot_wider(names_from = name,
              values_from = value) %>% 
  .[complete.cases(.),] -> missing_found

bind_rows(missing_found,
          complete) %>% 
  group_by(CNTY_CODE, code_prv, ag_LL) %>% 
  summarise(`2016` = sum(`2016`),
            `2017` = sum(`2017`),
            `2018` = sum(`2018`),
            `2019` = sum(`2019`)) -> pop_all

# xlsx::write(tmp, paste0(path_dropbox_github, "pop_names_fix.xlsx"))

# check year to year changes
pop_all %>% 
  group_by(CNTY_CODE) %>% 
  summarise(`2016` = sum(`2016`),
            `2017` = sum(`2017`),
            `2018` = sum(`2018`),
            `2019` = sum(`2019`)) %>% 
  mutate(r1 = `2017`/`2016`,
         r2 = `2018`/`2017`,
         r3 = `2019`/`2018`) %>% 
  filter(r1 > 1.25 | r2 > 1.25 | r3 > 1.25 | r1 < 0.75 | r2 < 0.75 | r3 < 0.75) %>% 
  pull(CNTY_CODE) -> to_remove

pop_all %>% 
  dplyr::filter(!CNTY_CODE %in% to_remove) -> to_save

write_rds(to_save,
          file = paste0(path_dropbox_github, "pop_tar.rds"))

write_rds(to_remove,
          file = paste0(path_dropbox_github, "to_remove.rds"))


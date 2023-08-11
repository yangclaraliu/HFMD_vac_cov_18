# these locations have missing data
read_excel(paste0(path_dropbox, "[Data] HFMD/20230802_Henan_vaccination.xlsx")) %>% 
  setNames(c("year", "month", "name_province", "name_prefecture", "name_county", "CNTY_CODE", "d1", "d2")) %>% 
  dplyr::select(year, starts_with("name"), CNTY_CODE, month, d2) %>%
  group_by(year, name_province, name_prefecture, CNTY_CODE, month) %>% 
  dplyr::filter(!is.na(d2)) %>% 
  summarise(d2 = sum(d2)) %>%
  group_by(CNTY_CODE) %>% 
  group_split() %>% 
  map(rownames_to_column) %>% 
  map(mutate, rowname = max(as.numeric(rowname))) %>% 
  bind_rows() %>% 
  dplyr::filter(rowname > 3) %>%
  pivot_wider(names_from = month, 
              values_from = d2) %>% 
  pivot_longer(cols = as.character(c(1:12)),
               names_to = "month") %>% 
  mutate(month = factor(month, levels = 1:12)) %>% 
  group_by(CNTY_CODE) %>% 
  group_split() %>% 
  map(mutate,
      value = imputeTS::na_interpolation(value, option = "spline")) %>% 
  bind_rows() %>% 
  group_by(year, name_province, name_prefecture, CNTY_CODE) %>% 
  summarise(d2 = sum(value)) -> raw_2019_henan

read_excel(paste0(path_dropbox, "[Data] HFMD/20230802_13prov_vaccination.xlsx")) %>% 
  setNames(c("index", "year", "name_province", "month", "name_prefecture", "name_county", "CNTY_CODE", "APC", "d1", "d2")) %>% 
  dplyr::select(year, name_province, name_prefecture, CNTY_CODE, month, d2) %>% 
  group_by(year) %>% group_split() -> raw_13

# only available for Ningxia in 2016 and 2017 on an annual level, 
# can be left alone, okay as it is

# only available for Ningxia in 2018 on a monthly level
# summarise over month level
raw_13[[3]] %<>% 
  group_by(year, name_province, name_prefecture, CNTY_CODE) %>% 
  summarise(d2 = sum(d2))

raw_13[[4]] %<>% 
  dplyr::select(year, starts_with("name"), CNTY_CODE, month, d2) %>%
  group_by(year, name_province, name_prefecture, CNTY_CODE, month) %>% 
  dplyr::filter(!is.na(d2)) %>% 
  summarise(d2 = sum(d2)) %>%
  group_by(CNTY_CODE) %>% 
  group_split() %>% 
  map(rownames_to_column) %>% 
  map(mutate, rowname = max(as.numeric(rowname))) %>% 
  bind_rows() %>% 
  dplyr::filter(rowname > 3) %>%
  pivot_wider(names_from = month, 
              values_from = d2) %>% 
  pivot_longer(cols = as.character(c(1:12)),
               names_to = "month") %>% 
  mutate(month = factor(month, levels = 1:12)) %>% 
  group_by(CNTY_CODE) %>% 
  group_split() %>% 
  map(mutate,
      value = imputeTS::na_interpolation(value, option = "linear")) %>% 
  bind_rows() %>% 
  group_by(year, name_province, name_prefecture, CNTY_CODE) %>% 
  summarise(d2 = sum(value))

raw_13 %<>% 
  bind_rows() %>% 
  dplyr::filter(!is.na(CNTY_CODE))

raw_13 %>% 
  group_by(CNTY_CODE, year) %>% tally %>% arrange(desc(n)) %>% 
  dplyr::filter(n > 1) %>% 
  pull(CNTY_CODE) -> tmp

raw_13 %>% 
  dplyr::filter(CNTY_CODE %in% tmp) %>% 
  arrange(CNTY_CODE, year) %>% 
  mutate(code_prv = substr(CNTY_CODE, 1, 2)) %>% 
  dplyr::filter(code_prv != 64) %>% 
  pull(CNTY_CODE) -> tmp

raw_13 %<>% 
  dplyr::filter(!CNTY_CODE %in% tmp)

raw_13  %<>% 
  group_by(year, name_province, name_prefecture, CNTY_CODE) %>% 
  summarise(d2 = sum(d2))

raw <- list()
for(i in 1:2){
  paste0(path_dropbox, "[Data] HFMD/2016-2018 data of vaccine inoculation/2016-2018 data of vaccine inoculation 0712.xlsx") %>% readxl::read_excel(sheet = i, col_types = "text") -> raw[[i]]
}
raw[[1]] <- raw[[1]][-1,]
raw_save <- raw

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
  left_join(raw[[2]], by = "CNTY_CODE") %>% 
  dplyr::select(-APC_2016, -APC_2017) %>% 
  pivot_longer(cols = starts_with("d2"),
               names_to = "year",
               values_to = "d2") %>% 
  mutate(year = as.numeric(gsub("d2_","", year))) %>% 
  bind_rows(raw_2019_henan %>% 
              ungroup %>% 
              dplyr::select(CNTY_CODE, year, d2) %>% 
              mutate(CNTY_CODE = as.character(CNTY_CODE))) %>% 
  bind_rows(raw_13 %>% 
              ungroup %>% 
              dplyr::select(CNTY_CODE, year, d2) %>% 
              mutate(CNTY_CODE = as.character(CNTY_CODE))) %>% 
  mutate(code_prv = substr(CNTY_CODE, 1, 2)) %>% 
  dplyr::filter(!is.na(CNTY_CODE),
                !is.na(d2)) %>% 
  distinct() %>% 
  group_by(CNTY_CODE, year, code_prv) %>% 
  summarise(d2 = mean(d2)) -> data

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
  data[data$CNTY_CODE == missing$CNTY_CODE_missing[i],"CNTY_CODE_shape"] <- missing$CNTY_CODE_new[i]
}

data %<>% 
  mutate(CNTY_CODE_shape = if_else(is.na(CNTY_CODE_shape),
                                   CNTY_CODE,
                                   CNTY_CODE_shape))

data %<>% 
  group_by(CNTY_CODE_shape, year) %>% 
  summarise(d2 = sum(d2)) %>% 
  mutate(code_prf = substr(CNTY_CODE_shape, 1, 2))

data %>% 
  dplyr::filter(CNTY_CODE_shape %in% shape_cty$CNTY_CODE) %>% 
  pull(d2) %>% sum -> doses_located

data %>% 
  pull(d2) %>% sum -> doses_tot

# approximately 3.5% doses cannot be geocoded
1 - doses_located/doses_tot

write_rds(data, paste(path_dropbox_github, "vac_all.rds"))

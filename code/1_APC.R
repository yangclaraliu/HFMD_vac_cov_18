# # population simulation
# pop %>% 
#   data.table %>% 
#   filter(# year > 2015 & 
#     # year != 2018 & 
#     age_group %in% paste0(0:5,"-")) %>% 
#   dplyr::select(-ag_LL) %>% 
#   # group_by(CNTY_CODE, year, age_group) %>% tally %>% filter(n != 1) #%>% 
#   filter(!(CNTY_CODE %in% c(450122, 441325, 330322, 500227))) %>% 
#   mutate(tot_log = log(tot, base = 10)) %>% 
#   group_by(CNTY_CODE, age_group) %>% group_split() %>% 
#   map(arrange, year) %>% map(mutate, diff = c(NA, diff(tot_log))) %>% 
#   bind_rows() -> tmp
# 
# # remove between year changes that are too big
# tmp %>% 
#   mutate(diff_abs = abs(diff)) %>% 
#   filter(diff_abs > 1) %>% 
#   pull(CNTY_CODE) %>% 
#   unique -> to_remove
# 
# tmp %<>% 
#   filter(!(CNTY_CODE %in% to_remove))
# 
# # remove locations with fewer than 10 children
# # likely reporting error
# tmp %>% 
#   filter(tot <= 10) %>%
#   pull(CNTY_CODE) %>% 
#   unique -> to_remove
# 
# tmp %<>% 
#   filter(!(CNTY_CODE %in% to_remove))
# 
# tmp %>% 
#   ggplot(., aes(x = year, y = tot, group = interaction(age_group, CNTY_CODE))) +
#   geom_line() +
#   scale_y_log10()
# 
# #### modelling new borns ####
# tmp %>% 
#   filter(age_group == "0-") %>% 
#   mutate(prv_no = substr(CNTY_CODE, 1, 2)) %>% 
#   group_by(CNTY_CODE) %>% group_split() %>% 
#   map(mutate, rk = rank(year)) %>% 
#   map(mutate, rk_max = max(rk)) %>% 
#   bind_rows() %>% 
#   filter(year != 2018) %>% 
#   filter(rk_max >= 6) %>% 
#   group_by(prv_no) %>% group_split() -> reg_tab
# 
# model <- list()
# for(i in 1:length(reg_tab)){
#   model[[i]] <- lm(tot ~ splines::ns(year) + CNTY_CODE, data = reg_tab[[i]])
# }
# 
# # model %>% map(summary) -> x
# model %>% map(summary) %>% lapply(., "[[", "dev.expl") %>% unlist() %>% hist
# model %>% map(summary) %>% lapply(., "[[", "r.squared") %>% unlist() %>% hist
# 
# for(i in 1:32){
#   reg_tab[[i]]$CNTY_CODE %>% unique %>% 
#     map(~predict(model[[i]],newdata = CJ(CNTY_CODE = ., year = c(2018:2019)))) %>% 
#     setNames(reg_tab[[i]]$CNTY_CODE %>% unique) %>% 
#     map(data.table) %>% map(rownames_to_column) %>% 
#     bind_rows(.id = "CNTY_CODE") %>% 
#     rename(tot = V1, year = rowname) %>% 
#     mutate(year = if_else(year == 1, 2018, 2019)) %>% 
#     bind_rows(reg_tab[[i]][,colnames(.)]) %>% 
#     mutate(status = if_else(year >= 2018, "predicted", "observed")) %>% 
#     ggplot(., aes(x = year, y = tot, group = CNTY_CODE, color = status)) +
#     geom_line() +
#     geom_point() +
#     facet_wrap(~CNTY_CODE, scales = "free") -> p_tmp
#   
#   ggsave(paste0("figs/0y_projection/",unique(reg_tab[[i]]$prv_no),".png"),
#          p_tmp,
#          width = 15, height = 10)
# }
# 
# 
# res <- list()
# for(i in 1:length(reg_tab)){
#   reg_tab[[i]]$CNTY_CODE %>% unique %>% 
#     map(~predict(model[[i]],newdata = CJ(CNTY_CODE = ., year = c(2016:2019)))) %>% 
#     setNames(reg_tab[[i]]$CNTY_CODE %>% unique) %>% 
#     map(data.table) %>% map(rownames_to_column) %>% 
#     bind_rows(.id = "CNTY_CODE") %>% 
#     rename(tot = V1, year = rowname) %>% 
#     mutate(year = as.numeric(year) + 2015, status = "predicted") %>% 
#     bind_rows(reg_tab[[i]][,c("CNTY_CODE", "year", "tot")] %>% 
#                 mutate(status = "observed")) %>% 
#     filter(year >= 2016) %>% 
#     pivot_wider(names_from = status, values_from = tot) %>% 
#     mutate(value = if_else(!is.na(observed), observed, predicted)) %>% 
#     dplyr::select(-observed, -predicted) -> res[[i]]
# }
# 
# res %>% 
#   bind_rows() -> APC_0
# 
# APC_0 %>% 
#   filter(value < 100) %>% 
#   pull(CNTY_CODE) -> to_remove
# 
# APC_0 %<>% 
#   filter(!CNTY_CODE %in% to_remove)
# 
# tmp %>% 
#   filter(CNTY_CODE %in% APC_0$CNTY_CODE) %>% 
#   dplyr::select(CNTY_CODE, year, tot, age_group) %>% 
#   filter(year >= 2016 & year < 2018) -> APC_tmp
# 
# APC_tmp %>% 
#   pivot_wider(names_from = age_group,
#               values_from = tot) %>% 
#   mutate(APC = `1-` + `2-` + `3-` + `4-` + `5-` + `0-`) -> APC_tmp
# 
# APC_tmp %>% 
#   filter(year == 2017) %>% 
#   left_join(APC_0 %>% 
#               filter(year == 2018) %>% 
#               rename(newborn_2018 = value) %>% 
#               dplyr::select(-year),
#             by = "CNTY_CODE") %>% 
#   mutate(year = 2018,
#          APC_new = APC - `5-` + newborn_2018) %>% 
#   dplyr::select(CNTY_CODE, year, APC_new) %>% 
#   rename(APC = APC_new) -> APC_2018
# 
# APC_tmp %>% 
#   filter(year == 2017) %>% 
#   left_join(APC_0 %>% 
#               filter(year == 2018) %>% 
#               rename(newborn_2018 = value) %>% 
#               dplyr::select(-year),
#             by = "CNTY_CODE") %>% 
#   left_join(APC_0 %>% 
#               filter(year == 2019) %>% 
#               rename(newborn_2019 = value) %>% 
#               dplyr::select(-year),
#             by = "CNTY_CODE") %>% 
#   mutate(year = 2019,
#          APC_new = APC - `5-` - `4-` + newborn_2018 + newborn_2019) %>% 
#   dplyr::select(CNTY_CODE, year, APC_new) %>% 
#   rename(APC = APC_new) -> APC_2019
# 
# APC_tmp %>% 
#   dplyr::select(CNTY_CODE, year, APC) %>% 
#   bind_rows(APC_2019, APC_2018) %>% 
#   arrange(CNTY_CODE, year) -> APC
# 
# qs::qsave(APC, "data/APC_YB.qs")

# APC_YB <- qs::qread("Data/APC_YB.qs")
# 
# 
# data %<>% 
#   left_join(APC_YB %>% 
#               pivot_wider(names_from = year,
#                           values_from = APC) %>% 
#               dplyr::select(-`2019`) %>% 
#               setNames(c("CNTY_CODE", "YB_2016", "YB_2017", "YB_2018")),
#             by = "CNTY_CODE")

data %>%
  dplyr::select(CNTY_CODE, starts_with("APC")) %>%
  filter(APC_2016 > 100 & APC_2017 > 100,
         !(CNTY_CODE %in% c(130109, 330111, 370312))) %>%
  pivot_longer(starts_with("APC")) %>%
  separate(name, into = c("metric", "year")) %>%
  mutate(year = as.numeric(year),
          prv_no = substr(CNTY_CODE, 1, 2)) %>%
  group_by(CNTY_CODE) %>% group_split() -> reg_tab
  # ggplot(., aes(x = year, y = value, group = CNTY_CODE)) +
  # geom_line() +
  # facet_wrap(~prv_no)

  model <- list()
  for(i in 1:length(reg_tab)){
    model[[i]] <- lm(value ~ year, data = reg_tab[[i]])
  }

  res <- list()
  for(i in 1:length(reg_tab)){
    reg_tab[[i]]$CNTY_CODE %>% unique %>%
      map(~predict(model[[i]],newdata = CJ(CNTY_CODE = ., year = c(2018)))) %>%
      setNames(reg_tab[[i]]$CNTY_CODE %>% unique) %>%
      map(data.table) %>% map(rownames_to_column) %>%
      bind_rows(.id = "CNTY_CODE") %>%
      rename(value = V1, year = rowname) %>%
      mutate(year = 2018, status = "predicted") %>%
      bind_rows(reg_tab[[i]][,c("CNTY_CODE", "year", "value")] %>%
                  mutate(status = "observed")) -> res[[i]]
  }
  
  res %>%
    bind_rows() -> APC_surveillance
# 
#   prv_index <- reg_tab %>% bind_rows() %>% pull(prv_no) %>% unique %>% sort
# 
#   for(i in 1:length(prv_index)){
#     APC_surveillance %>% 
#       mutate(prv_no = substr(CNTY_CODE, 1, 2)) %>% 
#       filter(prv_no == prv_index[i]) %>% 
#       ggplot(., aes(x = year, y = value)) +
#       geom_line() +
#       geom_point(aes(color = status)) +
#       facet_wrap(~CNTY_CODE, scales = "free") + 
#       labs(title = shape %>% 
#              filter(lvl == "prv", prv == prv_index[i]) %>% pull(NAME)) -> p_tmp
#     
#     ggsave(paste0("figs/APC_surveillance_projection/",
#                   shape %>% 
#                     filter(lvl == "prv", prv == prv_index[i]) %>% pull(PYNAME),
#                   ".png"),
#            p_tmp,
#            height = 10,
#            width = 10)
#   }  

  APC_surveillance %>% 
    dplyr::select(-status) %>% 
    pivot_wider(names_from = year,
                values_from = value) %>%
    mutate(APC_sur_2018_con = `2017`) %>% 
    rename(APC_sur_2016_ob = `2016`,
           APC_sur_2017_ob = `2017`,
           APC_sur_2018_lm = `2018`) -> to_save

  qs::qsave(to_save, "data/APC_sur.qs")
      
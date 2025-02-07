# n = 1831 for regression

in_reg <- unique(reg_tab$CNTY_CODE)

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
                code_cty != "00") 

to_compare_pop <- pop_2018 %>% 
  dplyr::filter(ag_LL <= 5) %>% 
  group_by(CNTY_CODE, code_prv, year) %>% 
  summarise(APC = sum(pop)) %>% 
  left_join(pop_2018 %>% 
              group_by(CNTY_CODE, code_prv, year) %>% 
              summarise(tot = sum(pop)),
            by = c("CNTY_CODE", "code_prv", "year")) %>% 
  ungroup %>% 
  mutate(p_risk = APC/tot,
         year = as.numeric(year)) %>% 
  .[complete.cases(.),] %>% 
  mutate(p_risk_s = (p_risk - mean(p_risk))/sd(p_risk),
         tot_s = (tot - mean(tot))/sd(tot),
         in_reg = CNTY_CODE %in% in_reg) %>% 
  dplyr::select(CNTY_CODE, code_prv, tot, p_risk, in_reg) %>% 
  as.data.table()

to_compare_pop %>% 
  pivot_longer(cols = c("tot", "p_risk")) %>% 
  group_by(name, in_reg) %>% 
  summarise(
    Q1 = quantile(value, 0.25),
    median = quantile(value, 0.5),
    mu = mean(value), 
    Q3 = quantile(value, 0.75),
    sd = sd(value)) %>% 
  arrange(name, desc(in_reg))

p_load("effsize")
cohen.d(to_compare_pop[in_reg == T]$tot,
        to_compare_pop[in_reg == F]$tot)

cohen.d(to_compare_pop[in_reg == T]$p_risk,
        to_compare_pop[in_reg == F]$p_risk)

to_compare_ses <- pt_imputed %>% 
  mutate(in_reg = CNTY_CODE %in% in_reg) %>% 
  as.data.table

to_compare_ses%>% 
  pivot_longer(cols = c("edu", "GDPpc", "urban_prop", "temp")) %>%
  group_by(name, in_reg) %>% 
  summarise(
    Q1 = quantile(value, 0.25),
    median = quantile(value, 0.5),
    mu = mean(value), 
    Q3 = quantile(value, 0.75),
    sd = sd(value)) %>% 
  arrange(name, desc(in_reg)) %>% View()

cohen.d(to_compare_ses[in_reg == T]$GDPpc,
        to_compare_ses[in_reg == F]$GDPpc)

cohen.d(to_compare_ses[in_reg == T]$edu,
        to_compare_ses[in_reg == F]$edu)

cohen.d(to_compare_ses[in_reg == T]$temp,
        to_compare_ses[in_reg == F]$temp)

cohen.d(to_compare_ses[in_reg == T]$urban_prop,
        to_compare_ses[in_reg == F]$urban_prop)


to_compare_epi <- epi %>% 
  mutate(in_reg = CNTY_CODE %in% in_reg) %>% 
  as.data.table

to_compare_epi%>% 
  group_by(in_reg) %>% 
  summarise(
    Q1 = quantile(burden, 0.25),
    median = quantile(burden, 0.5),
    mu = mean(burden), 
    Q3 = quantile(burden, 0.75),
    sd = sd(burden)) %>% 
  arrange(desc(in_reg)) %>% View()

cohen.d(to_compare_epi[in_reg == T]$burden,
        to_compare_epi[in_reg == F]$burden)

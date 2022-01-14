# APC_CDC %>% 
#   pivot_wider(names_from = year, values_from = pop) %>% 
#   left_join(data, by = "CNTY_CODE") %>% 
#   dplyr::select(-starts_with("d2")) %>% 
#   mutate(prv = substr(CNTY_CODE,1,2)) %>% 
#   left_join(prv_list, by = "prv") %>% 
#   filter(!is.na(prv) & NAME != "NA") %>%
#   filter(!(is.na(`2017`) | is.na(APC_2017))) %>% 
#   mutate(NAME = droplevels(as.factor(NAME))) %>% 
#   filter(`2017` > 0,
#          APC_2017 > 0,
#          `2016` > 0,
#          APC_2016 > 0,
#          `2018` > 0) -> tmp

APC_YB_observed <- qs::qread("data/APC_YB.qs") %>% 
    pivot_wider(names_from = year, 
              values_from = APC) %>% 
  rename(YB_2016 = `2016`,
         YB_2017 = `2017`) %>% 
  dplyr::select(-`2018`, -`2019`) 

# data %>% 
#   left_join(APC_YB_observed, by = "CNTY_CODE") %>% 
#   rename(RP_2016 = APC_2016,
#          RP_2017 = APC_2017) %>% 
#   mutate(r1_a = d2_2016/RP_2016,
#          r1_b = d2_2016/YB_2016,
#          r2_a = d2_2017/RP_2017,
#          r2_b = d2_2017/YB_2017) %>% 
#   dplyr::select(CNTY_CODE, starts_with("r", ignore.case = F)) %>% 
#   pivot_longer(starts_with("r", ignore.case = F)) %>% 
#   separate(name, into = c("year", "type")) %>% 
#   mutate(year = factor(year, labels = c(2016, 2017))) %>%
#   filter(!is.na(CNTY_CODE)) %>% 
#   pivot_wider(names_from = type, values_from = value, values_fn = mean) %>% 
#   ggplot(., aes(x = a, y = b)) +
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   lims(x = c(0,1), y = c(0,1))

tmp  %>% 
  ggplot(., aes(x = `2017`, y = get("APC_2017"))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~NAME) +
  labs(title = "Year = 2017",
       x = "CDC Population Counts (0-5 yo)",
       y = "Age Appropriate Children Reported with Vaccine Inoculation Information") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        title = element_text(size = 24),
        strip.text = element_text(size = 16))

tmp %>% 
  dplyr::select(-`2018`) %>% 
  pivot_longer(cols = c("APC_2016", "APC_2017"),
               names_to = "year",
               values_to = "APC") %>% 
  pivot_longer(cols = c("2016", "2017"),
               values_to = "POP") %>% 
  mutate(year = parse_number(year)) %>% 
  filter(as.numeric(name) == year) -> tmp2

model <- lm(log(APC) ~ log(POP) + prv, data = tmp2)

data.table(observed = tmp2$APC,
           fitted = exp(model$fitted.values),
           prv = tmp2$prv,
           year = as.character(tmp2$year),
           NAME = tmp2$NAME,
           CNTY_CODE = tmp2$CNTY_CODE) %>% 
  ggplot(., aes(x = observed, y = fitted)) +
  geom_point(aes(color = year)) +
  # geom_line() +
  facet_wrap(~NAME) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Observed",
       y = "Fitted") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        title = element_text(size = 24),
        strip.text = element_text(size = 16))

tmp %>% 
  dplyr::select(CNTY_CODE, starts_with("APC"), prv, NAME) %>% 
  pivot_longer(cols = starts_with("APC"), names_to = "year") %>% 
  mutate(year = parse_number(year) %>% as.character) %>% 
  ggplot(.,aes(x = year, y = value, group = CNTY_CODE)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.1) +
  facet_wrap(~NAME, scales = "free") +
  labs(x = "Year",
       title = "By-year Changes in Age Appropriate Children \nas Reported by Provinces with Inoculation Data",
       y = "Age Appropriate Children as Reported")+
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        title = element_text(size = 24),
        strip.text = element_text(size = 16))

tmp %>% 
  dplyr::select(CNTY_CODE, starts_with("201"), prv, NAME) %>% 
  pivot_longer(cols = starts_with("201"), names_to = "year") %>% 
  mutate(year = parse_number(year) %>% as.character) %>% 
  ggplot(.,aes(x = year, y = value, group = CNTY_CODE)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.1) +
  facet_wrap(~NAME, scales = "free") +
  scale_y_log10() +
  labs(x = "Year",
       title = "By-year Changes in Population (0-5 yo)",
       y = "Population (0-5yo)")+
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        title = element_text(size = 24),
        strip.text = element_text(size = 16))



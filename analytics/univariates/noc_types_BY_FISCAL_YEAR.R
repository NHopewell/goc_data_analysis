#### NOC_Type by fiscal year

# 2016/2017 noc type
employer_data_comp %>%
  filter(!is.na(NOC_Type), 
         Fiscal_Year == '16/17') %>%
  tally -> n_noc_one

# figure
employer_data_comp %>%
  filter(!is.na(NOC_Type), 
         Fiscal_Year == '16/17') %>%
  ggplot(aes(x = fct_infreq(NOC_Type))) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat = 'count', aes(label=..count..), 
            vjust = -0.4) +
  labs(title = "NOC Type Breakdown", 
       subtitle = str_c("2016/2017 (",n_noc_one$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")


employer_data_comp %>% 
  filter(!is.na(NOC_Type), 
         Fiscal_Year == '16/17') %>%
  group_by(fct_infreq(NOC_Type)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('NOC_Type (2016/2017)' = 'fct_infreq(NOC_Type)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 5))


# 2017/2018 noc type
employer_data_comp %>%
  filter(!is.na(NOC_Type), 
         Fiscal_Year == '17/18') %>%
  tally() -> n_noc_two

# figure
employer_data_comp %>%
  filter(!is.na(NOC_Type), 
         Fiscal_Year == '17/18') %>%
  ggplot(aes(x = fct_infreq(NOC_Type))) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat = 'count', aes(label=..count..), 
            vjust = -0.4) +
  labs(title = "NOC Type Breakdown", 
       subtitle = str_c("2017/2018 (",n_noc_two$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")


employer_data_comp %>% 
  filter(!is.na(NOC_Type), 
         Fiscal_Year == '17/18') %>%
  group_by(fct_infreq(NOC_Type)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('NOC_Type (2017/2018)' = 'fct_infreq(NOC_Type)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 5))


# 2018/2019 noc type
employer_data_comp %>%
  filter(!is.na(NOC_Type), 
         Fiscal_Year == '18/19') %>%
  tally() -> n_noc_three

# figure
employer_data_comp %>%
  filter(!is.na(NOC_Type), 
         Fiscal_Year == '18/19') %>%
  ggplot(aes(x = fct_infreq(NOC_Type))) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat='count', aes(label=..count..), 
            vjust = -0.4) +
  labs(title = "NOC Type Breakdown", 
       subtitle = str_c("2018/2019 (",n_noc_three$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")


employer_data_comp %>% 
  filter(!is.na(NOC_Type), 
         Fiscal_Year == '18/19') %>%
  group_by(fct_infreq(NOC_Type)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('NOC_Type (2018/2019)' = 'fct_infreq(NOC_Type)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 5))
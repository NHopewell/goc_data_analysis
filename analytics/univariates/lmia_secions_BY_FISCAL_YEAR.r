##### LMIA Section Faceted by fiscal year 

# 2016/2017 lmia regulation section
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '16/17') %>%
  tally() -> n_lmia_one

# figure
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '16/17') %>%
  mutate(LMIA_Regulation_Section = fct_recode(
    LMIA_Regulation_Section, 
    "Permanent residence\napplicants in Canada" = 
      "Permanent residence applicants in Canada")
  ) %>%
  ggplot(aes(x = LMIA_Regulation_Section)) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat = 'count', aes(label=..count..), 
            vjust = -0.5) +
  labs(title = "LMIA Regulation Section Breakdown", 
       subtitle = str_c("2016/2017 (",n_lmia_one$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")

employer_data_comp %>% 
  filter(!is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '16/17') %>%
  group_by(LMIA_Regulation_Section) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('LMIA_Regulation_Section (2016/2017)' = 'LMIA_Regulation_Section',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 3))


# 2017/2018 lmia regulation section
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '17/18') %>%
  tally() -> n_lmia_two

# figure
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '17/18') %>%
  mutate(LMIA_Regulation_Section = fct_recode(
    LMIA_Regulation_Section, 
    "Permanent residence\napplicants in Canada" = 
      "Permanent residence applicants in Canada")
  ) %>%
  ggplot(aes(x = LMIA_Regulation_Section)) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat = 'count', aes(label=..count..), 
            vjust = -0.5) +
  labs(title = "LMIA Regulation Section Breakdown", 
       subtitle = str_c("2017/2018 (",n_lmia_two$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")


employer_data_comp %>% 
  filter(!is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '17/18') %>%
  group_by(LMIA_Regulation_Section) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('LMIA_Regulation_Section (2017/2018)' = 'LMIA_Regulation_Section',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 3))



# 2018/2019 lmia regulation section
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '18/19') %>%
  tally() -> n_lmia_three

# figure
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '18/19') %>%
  mutate(LMIA_Regulation_Section = fct_recode(
    LMIA_Regulation_Section, 
    "Permanent residence\napplicants in Canada" = 
      "Permanent residence applicants in Canada")
  ) %>%
  ggplot(aes(x = LMIA_Regulation_Section)) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.5) +
  labs(title = "LMIA Regulation Section Breakdown", 
       subtitle = str_c("2018/2019 (",n_lmia_three$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")


employer_data_comp %>% 
  filter(!is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '18/19') %>%
  group_by(LMIA_Regulation_Section) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('LMIA_Regulation_Section 2018/2019)' = 'LMIA_Regulation_Section',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 3))
##### LMIA subsection Faceted by fiscal year


# 2016/2017 lmia regulation subsection
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection), 
         Fiscal_Year == '16/17') %>%
  tally() -> n_sub_one

# figure
employer_data_temp %>%
  filter(Fiscal_Year == '16/17') %>%
  ggplot(aes(x = fct_rev(fct_infreq(LMIA_Regulation_Subsection)))) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  coord_flip() +
  geom_text(stat = 'count', aes(label = ..count..), 
            hjust = -0.06) +
  labs(title = "LMIA Regulation Sub-Section Breakdown", 
       subtitle = str_c("2016/2017 (",n_sub_one$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")

employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection), 
         Fiscal_Year == '16/17') %>%
  group_by(fct_infreq(LMIA_Regulation_Subsection)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('LMIA_Regulation_Subsection (2016/2017)' = 'fct_infreq(LMIA_Regulation_Subsection)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 9))


# 2017/2018 lmia regulation subsection
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection), 
         Fiscal_Year == '17/18') %>%
  tally() -> n_sub_two

# figure
employer_data_temp %>%
  filter(Fiscal_Year == '17/18') %>%
  ggplot(aes(x = fct_rev(fct_infreq(LMIA_Regulation_Subsection)))) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  coord_flip() +
  geom_text(stat='count', aes(label = ..count..), 
            hjust = -0.06) +
  labs(title = "LMIA Regulation Sub-Section Breakdown", 
       subtitle = str_c("2017/2018 (",n_sub_two$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")


employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection), 
         Fiscal_Year == '17/18') %>%
  group_by(fct_infreq(LMIA_Regulation_Subsection)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('LMIA_Regulation_Subsection (2017/2018)' = 'fct_infreq(LMIA_Regulation_Subsection)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 9))

# 2018/2019 lmia regulation subsection
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection), 
         Fiscal_Year == '18/19') %>%
  tally() -> n_sub_three

# figure    
employer_data_temp %>%
  filter(Fiscal_Year == '18/19') %>%
  ggplot(aes(x = fct_rev(fct_infreq(LMIA_Regulation_Subsection)))) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  coord_flip() +
  geom_text(stat='count', aes(label=..count..), 
            hjust = -0.06) +
  labs(title = "LMIA Regulation Sub-Section Breakdown", 
       subtitle = str_c("2018/2019 (",n_sub_three$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")

employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection), 
         Fiscal_Year == '18/19') %>%
  group_by(fct_infreq(LMIA_Regulation_Subsection)) %>%
  tally()  %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('LMIA_Regulation_Subsection (2018/2019)' = 'fct_infreq(LMIA_Regulation_Subsection)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 9))
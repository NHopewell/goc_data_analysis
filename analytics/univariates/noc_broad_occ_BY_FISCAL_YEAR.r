#### NOC_Broad Occupation faceted by fiscal year 

# 2016/2017 noc broad occ
employer_data_temp %>%
  filter(Fiscal_Year == '16/17') %>%
  tally() -> n_nb_one

# figure
employer_data_temp %>%
  filter(Fiscal_Year == '16/17') %>%
  ggplot(aes(x = fct_rev(fct_infreq(NOC_Broad_Occupation)))) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  coord_flip() +
  geom_text(stat = 'count', aes(label = ..count..), 
            hjust = -0.05) +
  labs(title = "NOC Broad Occupation Breakdown", 
       subtitle = str_c("2016/2017 (",n_nb_one$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")


employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation), 
         Fiscal_Year == '16/17') %>%
  group_by(fct_infreq(NOC_Broad_Occupation)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('NOC_Broad_Occupation (2016/2017)' = 'fct_infreq(NOC_Broad_Occupation)',
         count = n) %>%
  kable()  %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 9))


# 2017/2018 noc broadd occ
employer_data_temp %>%
  filter(Fiscal_Year == '17/18') %>%
  tally() -> n_nb_two

# figure   
employer_data_temp %>%
  filter(Fiscal_Year == '17/18') %>%
  ggplot(aes(x = fct_rev(fct_infreq(NOC_Broad_Occupation)))) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  coord_flip() +
  geom_text(stat = 'count', aes(label=..count..), 
            hjust = -0.05) +
  labs(title = "NOC Broad Occupation Breakdown", 
       subtitle = str_c("2017/2018 (",n_nb_two$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")


employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation), 
         Fiscal_Year == '17/18') %>%
  group_by(fct_infreq(NOC_Broad_Occupation)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('NOC_Broad_Occupation (2017/2018)' = 'fct_infreq(NOC_Broad_Occupation)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 9))


# 2018/2019 noc broadd occ
employer_data_temp %>%
  filter(Fiscal_Year == '18/19') %>%
  tally() -> n_nb_three

# figure
employer_data_temp %>%
  filter(Fiscal_Year == '18/19') %>%
  ggplot(aes(x = fct_rev(fct_infreq(NOC_Broad_Occupation)))) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  coord_flip() +
  geom_text(stat='count', aes(label=..count..), 
            hjust = -0.05) +
  labs(title = "NOC Broad Occupation Breakdown", 
       subtitle = str_c("2018/2019 (",n_nb_three$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")


employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation), 
         Fiscal_Year == '18/19') %>%
  group_by(fct_infreq(NOC_Broad_Occupation)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('NOC_Broad_Occupation (2018/2019)' = 'fct_infreq(NOC_Broad_Occupation)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 9))
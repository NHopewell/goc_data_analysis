### NAICS, config A, by fiscal year

# 2016/2017 naics config a
naics_sliced %>%
  filter(!is.na(NAICS), !is.na(target), 
         Fiscal_Year == '16/17') %>%
  tally() -> naics_a_one 

# figure
ggplot(naics_sliced %>%
         filter(!is.na(NAICS), !is.na(target), 
                Fiscal_Year == '16/17'), 
       aes(x=NAICS, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.2, position = position_dodge(width = .9))+
  labs(title="Decision Breakdown by NAICS", 
       subtitle = str_c("2016/2017 (",naics_a_one $n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "bottom")


employer_data_comp %>% 
  filter(!is.na(target), !is.na(NAICS), 
         Fiscal_Year == '16/17',
         NAICS %in% naics_names) -> naics_sub

( summary_table(dplyr::group_by(naics_sub, NAICS), naics_summary1) )



# 2017/2018 naics config a
naics_sliced %>%
  filter(!is.na(NAICS), !is.na(target), 
         Fiscal_Year == '17/18') %>%
  tally() -> naics_a_two

# figure
ggplot(naics_sliced %>%
         filter(!is.na(NAICS), !is.na(target), 
                Fiscal_Year == '17/18'), 
       aes(x=NAICS, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.2, position = position_dodge(width = .9))+
  labs(title="Decision Breakdown by NAICS", 
       subtitle = str_c("2017/2018 (",naics_a_two$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "bottom")


employer_data_comp %>% 
  filter(!is.na(target), !is.na(NAICS), 
         Fiscal_Year == '17/18',
         NAICS %in% naics_names) -> naics_sub

( summary_table(dplyr::group_by(naics_sub, NAICS), naics_summary1) )


# 2018/2019 naics config a
naics_sliced %>%
  filter(!is.na(NAICS), !is.na(target), 
         Fiscal_Year == '18/19') %>%
  tally() -> naics_a_three

# figure
ggplot(naics_sliced %>%
         filter(!is.na(NAICS), !is.na(target), 
                Fiscal_Year == '18/19'), 
       aes(x = NAICS, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.2, position = position_dodge(width = .9))+
  labs(title="Decision Breakdown by NAICS", 
       subtitle = str_c("2018/2019 (",naics_a_three$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "bottom")


employer_data_comp %>% 
  filter(!is.na(target), !is.na(NAICS), 
         Fiscal_Year == '18/19',
         NAICS %in% naics_names) -> naics_sub

( summary_table(dplyr::group_by(naics_sub, NAICS), naics_summary1) )
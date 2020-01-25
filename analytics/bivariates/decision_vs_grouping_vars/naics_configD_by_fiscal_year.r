## nacis config d, by fiscal year

# 2016/2017 naics config d
ggplot(naics_sliced %>%
         filter(!is.na(NAICS), !is.na(target_down), 
                Fiscal_Year == '16/17'), 
       aes(x = NAICS, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.2, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by NAICS", 
       subtitle = str_c("2016/2017 (",naics_a_one $n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "bottom")

naics_sub <- employer_data_comp %>% 
  filter(!is.na(target_down), !is.na(NAICS), 
         Fiscal_Year == '16/17',
         NAICS %in% naics_names) -> naics_sub

( summary_table(dplyr::group_by(naics_sub, NAICS), naics_summary4) )


ggplot(naics_sliced %>%
         filter(!is.na(NAICS), !is.na(target_down), 
                Fiscal_Year == '17/18'), 
       aes(x = NAICS, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.2, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by NAICS", 
       subtitle = str_c("2017/2018 (",naics_a_two$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "bottom")

employer_data_comp %>% 
  filter(!is.na(target_down), !is.na(NAICS), 
         Fiscal_Year == '17/18',
         NAICS %in% naics_names) -> naics_sub 

( summary_table(dplyr::group_by(naics_sub, NAICS), naics_summary4) ) 


# 2018/2019 naics config d
ggplot(naics_sliced %>%
         filter(!is.na(NAICS), !is.na(target_down), 
                Fiscal_Year == '18/19'), 
       aes(x = NAICS, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust=-0.2, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by NAICS", 
       subtitle = str_c("2018/2019 (",naics_a_three$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "bottom")


employer_data_comp %>% 
  filter(!is.na(target_down), !is.na(NAICS), 
         Fiscal_Year == '18/19',
         NAICS %in% naics_names) -> naics_sub 

( summary_table(dplyr::group_by(naics_sub, NAICS), naics_summary4) )

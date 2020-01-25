#### LMIA Section, Descision config A, by fiscal year

lmia_data_temp %>%
  filter(!is.na(LMIA_Regulation_Section), !is.na(target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(LMIA_Regulation_Section, target), 
                  fill = target), na.rm=TRUE, color = "grey40") +
  facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                     "17/18" = " 2017/2018",
                                                     "18/19" = "2018/2019"))) +
  labs(title = "Decision Breakdown by LMIA_Regulation_Section  - \nfaceted by Fiscal Year", 
       subtitle = str_c("All years (",lmia_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")


# 2016/2017 lmia section
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), !is.na(target), 
         Fiscal_Year == '16/17') %>%
  tally() -> lmia_a_one

# figure
ggplot(lmia_data_temp %>%
         filter(!is.na(LMIA_Regulation_Section), !is.na(target), 
                Fiscal_Year == '16/17'), 
       aes(x = LMIA_Regulation_Section, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, 
            position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by LMIA_Regulation_Section", 
       subtitle = str_c("2016/2017 (",lmia_a_one$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()


employer_data_comp %>% 
  filter(!is.na(target), !is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '16/17') -> lmias

( summary_table(dplyr::group_by(lmias, LMIA_Regulation_Section), lmia_summary1) )


# 2017/2018 lmia section
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), !is.na(target), 
         Fiscal_Year == '17/18') %>%
  tally() -> lmia_a_two

# figure
ggplot(lmia_data_temp %>%
         filter(!is.na(LMIA_Regulation_Section), !is.na(target), 
                Fiscal_Year == '17/18'), 
       aes(x = LMIA_Regulation_Section, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), vjust = -0.5, 
            position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by LMIA_Regulation_Section", 
       subtitle = str_c("2017/2018 (",lmia_a_two$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()


employer_data_comp %>% 
  filter(!is.na(target), !is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '17/18') -> lmias

( summary_table(dplyr::group_by(lmias, LMIA_Regulation_Section), lmia_summary1) )


# 2018/2019 lmia section
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), !is.na(target), 
         Fiscal_Year == '18/19') %>%
  tally() -> lmia_a_three

# figure
ggplot(lmia_data_temp %>%
         filter(!is.na(LMIA_Regulation_Section), !is.na(target), 
                Fiscal_Year == '18/19'), 
       aes(x = LMIA_Regulation_Section, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, 
            position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by LMIA_Regulation_Section", 
       subtitle = str_c("2018/2019 (",lmia_a_three$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()


employer_data_comp %>% 
  filter(!is.na(target), !is.na(LMIA_Regulation_Section), 
         Fiscal_Year == '18/19') -> lmias

( summary_table(dplyr::group_by(lmias, LMIA_Regulation_Section), lmia_summary1) )
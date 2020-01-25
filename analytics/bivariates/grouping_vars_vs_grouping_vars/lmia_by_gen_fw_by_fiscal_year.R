##### Lmia section By Gender of Foriegn Worker, by fiscal year

# 2016/2017 lmia by gender
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), !is.na(Gender_of_FW), 
         Fiscal_Year == '16/17') %>%
  tally() -> lmiag_a_one

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(LMIA_Regulation_Section), !is.na(Gender_of_FW), 
                Fiscal_Year == '16/17'), 
       aes(x = LMIA_Regulation_Section, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label = ..count..), vjust = -0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Section by Gender of Foreign Worker",
       subtitle = str_c("2016/2017 (",lmiag_a_one$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal()

employer_data_comp %>% 
  filter(!is.na(LMIA_Regulation_Section), !is.na(Gender_of_FW), 
         Fiscal_Year == '16/17') -> lmia_gen

( summary_table(dplyr::group_by(lmia_gen, Gender_of_FW), lmia_gen_summary1) )


# 2017/2018 lmia by gender
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), !is.na(Gender_of_FW), 
         Fiscal_Year == '17/18') %>%
  tally() -> lmiag_a_two

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(LMIA_Regulation_Section), !is.na(Gender_of_FW), 
                Fiscal_Year == '17/18'), 
       aes(x = LMIA_Regulation_Section, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust=-0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Section by Gender of Foreign Worker",
       subtitle = str_c("2017/2018 (",lmiag_a_two$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal()

employer_data_comp %>% 
  filter(!is.na(LMIA_Regulation_Section), !is.na(Gender_of_FW), 
         Fiscal_Year == '17/18') ->  lmia_gen

( summary_table(dplyr::group_by(lmia_gen, Gender_of_FW), lmia_gen_summary1) )

# 2018/2019 lmia by gender
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), !is.na(Gender_of_FW), 
         Fiscal_Year == '18/19') %>%
  tally() -> lmiag_a_three

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(LMIA_Regulation_Section), !is.na(Gender_of_FW), 
                Fiscal_Year == '18/19'), 
       aes(x = LMIA_Regulation_Section, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label=..count..), vjust=-0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Section by Gender of Foreign Worker",
       subtitle = str_c("2018/2019 (",lmiag_a_three$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal()

employer_data_comp %>% 
  filter(!is.na(LMIA_Regulation_Section), !is.na(Gender_of_FW), 
         Fiscal_Year == '18/19') -> lmia_gen

( summary_table(dplyr::group_by(lmia_gen, Gender_of_FW), lmia_gen_summary1) )


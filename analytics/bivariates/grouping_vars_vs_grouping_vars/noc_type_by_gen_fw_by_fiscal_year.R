#### NOC Type by gender of fw, by fiscal year

# 2016/2017 noc vs gen of fw 
employer_data_comp %>%
  filter(!is.na(NOC_Type), !is.na(Gender_of_FW), 
         Fiscal_Year == '16/17') %>%
  tally() -> noc_gen_a

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(NOC_Type), !is.na(Gender_of_FW), 
                Fiscal_Year == '16/17'), 
       aes(x=NOC_Type, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Type by Gender of Foreign Worker",
       subtitle = str_c("2016/2017 (",noc_gen_a$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal()

employer_data_comp %>% 
  filter(!is.na(NOC_Type), !is.na(Gender_of_FW), 
         Fiscal_Year == '16/17') -> noc_gen

( summary_table(dplyr::group_by(noc_gen, Gender_of_FW), noc_reg_summary2) )

# 2017/2018 noc vs gen of fw 
employer_data_comp %>%
  filter(!is.na(NOC_Type), !is.na(Gender_of_FW), 
         Fiscal_Year == '17/18') %>%
  tally() -> noc_gen_b

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(NOC_Type), !is.na(Gender_of_FW), 
                Fiscal_Year == '17/18'), 
       aes(x=NOC_Type, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Type by Gender of Foreign Worker",
       subtitle = str_c("2017/2018 (",noc_gen_b$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal()

employer_data_comp %>% 
  filter(!is.na(NOC_Type), !is.na(Gender_of_FW), 
         Fiscal_Year == '17/18') -> noc_gen 

( summary_table(dplyr::group_by(noc_gen, Gender_of_FW), noc_reg_summary2) )

# 2018/2019 noc vs gen of fw 
employer_data_comp %>%
  filter(!is.na(NOC_Type), !is.na(Gender_of_FW), 
         Fiscal_Year == '18/19') %>%
  tally() -> noc_gen_c

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(NOC_Type), !is.na(Gender_of_FW), 
                Fiscal_Year == '18/19'), 
       aes(x=NOC_Type, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Type by Gender of Foreign Worker",
       subtitle = str_c("2018/2019 (",noc_gen_c$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal()

employer_data_comp %>% 
  filter(!is.na(NOC_Type), !is.na(Gender_of_FW), 
         Fiscal_Year == '18/19') -> noc_gen

( summary_table(dplyr::group_by(noc_gen, Gender_of_FW), noc_reg_summary2) )
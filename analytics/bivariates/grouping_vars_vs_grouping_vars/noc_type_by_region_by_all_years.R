#### NOC Type,  By Region, by fiscal year

# 2016/2017 noc by region
employer_data_comp %>%
  filter(!is.na(NOC_Type), !is.na(Region), 
         Fiscal_Year == '16/17') %>%
  tally() -> noc_reg_a

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(NOC_Type), !is.na(Region), 
                Fiscal_Year == '16/17'), 
       aes(x=NOC_Type, fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.4, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Type by Region",
       subtitle = str_c("2016/2017 (",noc_reg_a$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(legend.position="bottom")


employer_data_comp %>% 
  filter(!is.na(NOC_Type), !is.na(Region), 
         Fiscal_Year == '16/17') -> noc_reg

( summary_table(dplyr::group_by(noc_reg, Region), noc_reg_summary1) )


# 2017/2018 noc by region
employer_data_comp %>%
  filter(!is.na(NOC_Type), !is.na(Region), 
         Fiscal_Year == '17/18') %>%
  tally() -> noc_reg_b

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(NOC_Type), !is.na(Region), 
                Fiscal_Year == '17/18'), 
       aes(x=NOC_Type, fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.4, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Type by Region",
       subtitle = str_c("2017/2018 (",noc_reg_b$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position="bottom")


employer_data_comp %>% 
  filter(!is.na(NOC_Type), !is.na(Region), 
         Fiscal_Year == '17/18') -> noc_reg

( summary_table(dplyr::group_by(noc_reg, Region), noc_reg_summary1) )


# 2018/2019 noc by region
employer_data_comp %>%
  filter(!is.na(NOC_Type), !is.na(Region), 
         Fiscal_Year == '18/19') %>%
  tally() -> noc_reg_c

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(NOC_Type), !is.na(Region), 
                Fiscal_Year == '18/19'), 
       aes(x = NOC_Type, fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.4, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Type by Region",
       subtitle = str_c("2018/2019 (",noc_reg_c$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(legend.position="bottom")


employer_data_comp %>% 
  filter(!is.na(NOC_Type), !is.na(Region), 
         Fiscal_Year == '18/19') -> noc_reg

( summary_table(dplyr::group_by(noc_reg, Region), noc_reg_summary1) ) 
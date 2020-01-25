####  noc broad occ config c, by fiscal year


# 2016/2017 noc broad occ
broad_data %>%
  dplyr::filter(Fiscal_Year == '16/17') %>%
  ggplot( 
    aes(x=NOC_Broad_Occupation, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.4, 
            position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by NOC_Broad_Occupation", 
       subtitle = str_c("2016/2017 (",broad_a_one$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(legend.position = "bottom")


employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(NOC_Broad_Occupation), 
         Fiscal_Year == '16/17', 
         NOC_Broad_Occupation %in% broad_slice) -> nocs_b

( summary_table(dplyr::group_by(nocs_b, NOC_Broad_Occupation), nocb_summary3) )


# 2017/2018 noc broad occ
broad_data %>%
  dplyr::filter(Fiscal_Year == '17/18') %>%
  ggplot( 
    aes(x = NOC_Broad_Occupation, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.4, 
            position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by NOC_Broad_Occupation", 
       subtitle = str_c("2017/2018 (",broad_a_two$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(legend.position = "bottom")

employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(NOC_Broad_Occupation), 
         Fiscal_Year == '17/18', 
         NOC_Broad_Occupation %in% broad_slice) -> nocs_b

( summary_table(dplyr::group_by(nocs_b, NOC_Broad_Occupation), nocb_summary3) )

# 2018/2019 noc broad occ
broad_data %>%
  dplyr::filter(Fiscal_Year == '18/19') %>%
  ggplot( 
    aes(x=NOC_Broad_Occupation, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust=-0.4, 
            position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by NOC_Broad_Occupation", 
       subtitle = str_c("2018/2019 (",broad_a_three$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(legend.position="bottom")

employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(NOC_Broad_Occupation), 
         Fiscal_Year == '18/19', 
         NOC_Broad_Occupation %in% broad_slice) -> nocs_b

( summary_table(dplyr::group_by(nocs_b, NOC_Broad_Occupation), nocb_summary3) )
# 2016/2017 noc broad occ by gender
employer_data_comp %>%
  filter(!is.na(NOC_Broad_Occupation), !is.na(Gender_of_FW),
         Fiscal_Year == '16/17') %>%
  tally() -> nb_a_one

# figure
ggplot(bd_temp %>%
         filter(!is.na(NOC_Broad_Occupation), !is.na(Gender_of_FW),
                Fiscal_Year == '16/17'), 
       aes(x=NOC_Broad_Occupation, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), hjust=-0.1, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Broad_Occupation by Gender of Foreign Worker",
       subtitle = str_c("2016/2017 (",nb_a_one$n," observations)\n"),
       x = "",
       y = "") +
  coord_flip() +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "top") + 
  guides(fill = guide_legend(reverse=T))


employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation), !is.na(Gender_of_FW),
         Fiscal_Year == '16/17') -> nocb_gen

( summary_table(dplyr::group_by(nocb_gen, fct_rev(Gender_of_FW)), nocb_gen_summary1) )

# 2017/2018 noc broad occ by gender
employer_data_comp %>%
  filter(!is.na(NOC_Broad_Occupation), !is.na(Gender_of_FW),
         Fiscal_Year == '17/18') %>%
  tally() -> nb_a_two

# figure
ggplot(bd_temp %>%
         filter(!is.na(NOC_Broad_Occupation), !is.na(Gender_of_FW),
                Fiscal_Year == '17/18'), 
       aes(x=NOC_Broad_Occupation, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), hjust=-0.1, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Broad_Occupation by Gender of Foreign Worker",
       subtitle = str_c("2017/2018 (",nb_a_two$n," observations)\n"),
       x = "",
       y = "") +
  coord_flip() +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "top") + 
  guides(fill = guide_legend(reverse=T))

employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation), !is.na(Gender_of_FW),
         Fiscal_Year == '17/18') -> nocb_gen

( summary_table(dplyr::group_by(nocb_gen, fct_rev(Gender_of_FW)), nocb_gen_summary1) )


# 2018/2019 noc broad occ by gender
employer_data_comp %>%
  filter(!is.na(NOC_Broad_Occupation), !is.na(Gender_of_FW),
         Fiscal_Year == '18/19') %>%
  tally() -> nb_a_three

# figure
ggplot(bd_temp %>%
         filter(!is.na(NOC_Broad_Occupation), !is.na(Gender_of_FW),
                Fiscal_Year == '18/19'), 
       aes(x=NOC_Broad_Occupation, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), hjust=-0.1, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Broad_Occupation by Gender of Foreign Worker",
       subtitle = str_c("2018/2019 (",nb_a_three$n," observations)\n"),
       x = "",
       y = "") +
  coord_flip() +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "top") + 
  guides(fill = guide_legend(reverse=T))

employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation), !is.na(Gender_of_FW),
         Fiscal_Year == '18/19') -> nocb_gen

( summary_table(dplyr::group_by(nocb_gen, fct_rev(Gender_of_FW)), nocb_gen_summary1) )
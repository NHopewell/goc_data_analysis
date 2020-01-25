#### NOC_Broad Occupation By Region, by fiscal year

# 2016/2017 noc broad occ by region
bd_temp %>%
  filter(!is.na(NOC_Broad_Occupation), !is.na(Region), 
         Fiscal_Year == '16/17') %>%
  tally() -> bd_a_one

# figure
ggplot(bd_temp %>%
         filter(!is.na(NOC_Broad_Occupation), !is.na(Region), 
                Fiscal_Year == '16/17'), 
       aes(x=NOC_Broad_Occupation, fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label=..count..), hjust=-0.1, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Broad_Occupation by Region",
       subtitle = str_c("2016/2017 (",bd_a_one$n," observations)\n"),
       x = "",
       y = "") +
  coord_flip() +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "top") + 
  guides(fill = guide_legend(reverse=T))

employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation), !is.na(Region), 
         Fiscal_Year == '16/17') -> nocb_reg

( summary_table(dplyr::group_by(nocb_reg, fct_rev(Region)), nocb_reg_summary1) )



# 2017/2018 noc broad occ by region
bd_temp %>%
  filter(!is.na(NOC_Broad_Occupation), !is.na(Region), 
         Fiscal_Year == '17/18') %>%
  tally() -> bd_a_two

# figure
ggplot(bd_temp %>%
         filter(!is.na(NOC_Broad_Occupation), !is.na(Region), 
                Fiscal_Year == '17/18'), 
       aes(x=NOC_Broad_Occupation, fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), hjust=-0.1, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Broad_Occupation by Region",
       subtitle = str_c("2017/2018 (",bd_a_two$n," observations)\n"),
       x = "",
       y = "") +
  coord_flip() +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "top") + 
  guides(fill = guide_legend(reverse=T))

employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation), !is.na(Region), 
         Fiscal_Year == '17/18') -> nocb_reg

( summary_table(dplyr::group_by(nocb_reg, fct_rev(Region)), nocb_reg_summary1) )


# 2018/2019 noc broad occ by region
bd_temp %>%
  filter(!is.na(NOC_Broad_Occupation), !is.na(Region), 
         Fiscal_Year == '18/19') %>%
  tally() -> bd_a_three

# figure
ggplot(bd_temp %>%
         filter(!is.na(NOC_Broad_Occupation), !is.na(Region), 
                Fiscal_Year == '18/19'), 
       aes(x=NOC_Broad_Occupation, fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), hjust=-0.1, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Broad_Occupation by Region",
       subtitle = str_c("2018/2019 (",bd_a_three$n," observations)\n"),
       x = "",
       y = "") +
  coord_flip() +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "top") + 
  guides(fill = guide_legend(reverse=T))

employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation), !is.na(Region), 
         Fiscal_Year == '18/19') -> nocb_reg

( summary_table(dplyr::group_by(nocb_reg, fct_rev(Region)), nocb_reg_summary1) )
#### NOC Type,  Descision config A, by fiscal year

noc_data_temp %>%
  filter(!is.na(NOC_Type), !is.na(target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(NOC_Type, target), 
                  fill=target), na.rm=TRUE, color = "grey40") +
  facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                     "17/18" = " 2017/2018",
                                                     "18/19" = "2018/2019"))) +
  labs(title = "Decision Breakdown by NOC_Type - faceted by Fiscal Year", 
       subtitle = str_c("All years (",noc_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        legend.position="bottom")


# 2016/2017 noc config a
noc_data_temp %>%
  filter(!is.na(NOC_Type), !is.na(target), 
         Fiscal_Year == '16/17') %>%
  tally() -> noc_a_one

# figure
ggplot(noc_data_temp %>%
         filter(!is.na(NOC_Type), !is.na(target), 
                Fiscal_Year == '16/17'), 
       aes(x=NOC_Type, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label = ..count..), 
            vjust=-0.4, 
            position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by NOC_Type", 
       subtitle=str_c("2016/2017 (",noc_a_one$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position="bottom")


employer_data_comp %>% 
  filter(!is.na(target), !is.na(NOC_Type), 
         Fiscal_Year == '16/17') -> nocs

( summary_table(dplyr::group_by(nocs, NOC_Type), noc_summary1) )


# 2017/2018 noc config a
noc_data_temp %>%
  filter(!is.na(NOC_Type), !is.na(target), 
         Fiscal_Year == '17/18') %>%
  tally() -> noc_a_two

# figure
ggplot(noc_data_temp %>%
         filter(!is.na(NOC_Type), !is.na(target), 
                Fiscal_Year == '17/18'), 
       aes(x = NOC_Type, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.4, 
            position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by NOC_Type", 
       subtitle = str_c("2017/2018 (",noc_a_two$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "bottom")


employer_data_comp %>% 
  filter(!is.na(target), !is.na(NOC_Type), 
         Fiscal_Year == '17/18') -> nocs

( summary_table(dplyr::group_by(nocs, NOC_Type), noc_summary1) )



# 2018/2019 noc config a
noc_data_temp %>%
  filter(!is.na(NOC_Type), !is.na(target), 
         Fiscal_Year == '18/19') %>%
  tally() -> noc_a_three

# figure
ggplot(noc_data_temp %>%
         filter(!is.na(NOC_Type), !is.na(target), 
                Fiscal_Year == '18/19'), 
       aes(x = NOC_Type, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label=..count..), 
            vjust = -0.4, 
            position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by NOC_Type", 
       subtitle = str_c("2018/2019 (",noc_a_three$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position="bottom")


employer_data_comp %>% 
  filter(!is.na(target), !is.na(NOC_Type), 
         Fiscal_Year == '18/19') -> nocs

( summary_table(dplyr::group_by(nocs, NOC_Type), noc_summary1) )
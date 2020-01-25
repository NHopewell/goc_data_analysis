
###### noc config d, by fiscal year

noc_data_temp %>%
  filter(!is.na(NOC_Type), !is.na(target_down)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(NOC_Type, target_down), 
                  fill = target_down), na.rm=TRUE, color = "grey40") +
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
        axis.text.x = element_blank(),
        legend.position = "bottom")


# 2016/2017 noc config d
ggplot(noc_data_temp %>%
         filter(!is.na(NOC_Type), !is.na(target_down), 
                Fiscal_Year == '16/17'), 
       aes(x = NOC_Type, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.4, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by NOC_Type", 
       subtitle = str_c("2016/2017 (",noc_a_one$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(legend.position="bottom")


employer_data_comp %>% 
  filter(!is.na(target_down), !is.na(NOC_Type), 
         Fiscal_Year == '16/17') -> tdnoc

( summary_table(dplyr::group_by(tdnoc, NOC_Type), noc_summary4) )


# 2017/2018 noc config d
ggplot(noc_data_temp %>%
         filter(!is.na(NOC_Type), !is.na(target_down), 
                Fiscal_Year == '17/18'), 
       aes(x = NOC_Type, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.4, position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by NOC_Type", 
       subtitle = str_c("2017/2018 (",noc_a_two$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(legend.position="bottom")


employer_data_comp %>% 
  filter(!is.na(target_down), !is.na(NOC_Type), 
         Fiscal_Year == '17/18') -> tdnoc

( summary_table(dplyr::group_by(tdnoc, NOC_Type), noc_summary4) )



# 2018/2019 noc config d
ggplot(noc_data_temp %>%
         filter(!is.na(NOC_Type), !is.na(target_down), 
                Fiscal_Year == '18/19'), 
       aes(x = NOC_Type, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.4, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by NOC_Type", 
       subtitle = str_c("2018/2019 (",noc_a_three$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(legend.position = "bottom")


employer_data_comp %>% 
  filter(!is.na(target_down), !is.na(NOC_Type), 
         Fiscal_Year == '18/19') -> tdnoc

( summary_table(dplyr::group_by(tdnoc, NOC_Type), noc_summary4) )
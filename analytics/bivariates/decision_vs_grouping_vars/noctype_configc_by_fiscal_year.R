### noc config c, by fiscal year


noc_data_temp %>%
  filter(!is.na(NOC_Type), !is.na(target_three)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(NOC_Type, target_three), 
                  fill = target_three), na.rm = TRUE, color = "grey40") +
  facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                     "17/18" = " 2017/2018",
                                                     "18/19" = "2018/2019"))) +
  labs(title="Decision Breakdown by NOC_Type - faceted by Fiscal Year", 
       subtitle = str_c("All years (",noc_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")


# 2016/2017 noc config c
ggplot(noc_data_temp %>%
         filter(!is.na(NOC_Type), !is.na(target_three), 
                Fiscal_Year == '16/17'), 
       aes(x = NOC_Type, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.4, position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by NOC_Type", 
       subtitle = str_c("2016/2017 (",noc_a_one$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(legend.position = "bottom")


employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(NOC_Type), 
         Fiscal_Year == '16/17') -> t3noc

( summary_table(dplyr::group_by(t3noc, NOC_Type), noc_summary3) )


# 2017/2018 noc config c
ggplot(noc_data_temp %>%
         filter(!is.na(NOC_Type), !is.na(target_three), 
                Fiscal_Year == '17/18'), 
       aes(x = NOC_Type, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label=..count..), 
            vjust = -0.4, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by NOC_Type", 
       subtitle = str_c("2017/2018 (",noc_a_two$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(legend.position = "bottom")


employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(NOC_Type), 
         Fiscal_Year == '17/18') -> t3noc

( summary_table(dplyr::group_by(t3noc, NOC_Type), noc_summary3) )




# 2018/2019 noc config c
ggplot(noc_data_temp %>%
         filter(!is.na(NOC_Type), !is.na(target_three), 
                Fiscal_Year == '18/19'), 
       aes(x = NOC_Type, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label = ..count..), 
            vjust=-0.4, position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by NOC_Type", 
       subtitle = str_c("2018/2019 (",noc_a_three$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(legend.position = "bottom")

employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(NOC_Type),
         Fiscal_Year == '18/19') -> t3noc

( summary_table(dplyr::group_by(t3noc, NOC_Type), noc_summary3) )
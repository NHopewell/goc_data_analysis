##### Gender of FW Decision config C, by fiscal year

# gen config c
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), !is.na(target_three)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(Gender_of_FW, target_three), 
                  fill = target_three), na.rm=TRUE, color = "grey40") +
  facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                     "17/18" = " 2017/2018",
                                                     "18/19" = "2018/2019"))) +
  labs(title="Decision Breakdown by Gender of Foreign Worker - Faceted by Fiscal Year", 
       subtitle= str_c("All years (",gen_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")


# 2016/2017 gen config c
ggplot(employer_data_comp %>%
         filter(!is.na(Gender_of_FW), !is.na(target_three), 
                Fiscal_Year == '16/17'), 
       aes(x=Gender_of_FW, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.5, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by Gender of Foreign Worker", 
       subtitle = str_c("2016/2017 (",gen_a_one$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right")


employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(Gender_of_FW), 
         Fiscal_Year == '16/17') -> gens3

( summary_table(dplyr::group_by(gens3, Gender_of_FW), gens_summary3) )


# 2017/2018 gen config c
ggplot(employer_data_comp %>%
         filter(!is.na(Gender_of_FW), !is.na(target_three), 
                Fiscal_Year == '17/18'), 
       aes(x=Gender_of_FW, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust=-0.5, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by Gender of Foreign Worker", 
       subtitle = str_c("2017/2018 (",gen_a_two$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right")


employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(Gender_of_FW), 
         Fiscal_Year == "17/18") -> gens3

( summary_table(dplyr::group_by(gens3, Gender_of_FW), gens_summary3) ) 


# 2018/2019 gen config c
ggplot(employer_data_comp %>%
         filter(!is.na(Gender_of_FW), !is.na(target_three), 
                Fiscal_Year == '18/19'), 
       aes(x = Gender_of_FW, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust=-0.5, position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by Gender of Foreign Worker", 
       subtitle = str_c("2018/2019 (",gen_a_three$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right")


employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(Gender_of_FW), 
         Fiscal_Year == "18/19") -> gens3

( summary_table(dplyr::group_by(gens3, Gender_of_FW), gens_summary3) )
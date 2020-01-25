##### Gender of FW Decision config B, by year

# gen config b
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), !is.na(multi_target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(Gender_of_FW, multi_target), 
                  fill=multi_target), na.rm=TRUE, color = "grey40") +
  facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                     "17/18" = " 2017/2018",
                                                     "18/19" = "2018/2019"))) +
  labs(title="Decision Breakdown by Gender of Foreign Worker - Faceted by Fiscal Year", 
       subtitle= str_c("All years (",gen_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")


#2016/2017 gen config b
ggplot(employer_data_comp %>%
         filter(!is.na(Gender_of_FW), !is.na(multi_target), 
                Fiscal_Year == "16/17"), 
       aes(x=Gender_of_FW, fill = multi_target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.2, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by Gender of Foreign Worker", 
       subtitle = str_c("2016/2017 (",gen_a_one$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right",
        strip.background = element_rect(colour = "grey30", 
                                        fill = "white"))
employer_data_comp %>% 
  filter(!is.na(multi_target), !is.na(Gender_of_FW), 
         Fiscal_Year == "16/17") -> gens2

( summary_table(dplyr::group_by(gens2, Gender_of_FW), gens_summary2) )


# 2017/2018 gen config b
ggplot(employer_data_comp %>%
         filter(!is.na(Gender_of_FW), !is.na(multi_target), 
                Fiscal_Year == "17/18"), 
       aes(x = Gender_of_FW, fill = multi_target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.2, position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by Gender of Foreign Worker", 
       subtitle = str_c("2017/2018 (",gen_a_two$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right",
        strip.background = element_rect(colour = "grey30", 
                                        fill = "white"))

employer_data_comp %>% 
  filter(!is.na(multi_target), !is.na(Gender_of_FW), 
         Fiscal_Year == "17/18") -> gens2

( summary_table(dplyr::group_by(gens2, Gender_of_FW), gens_summary2) )


# 2018/2019 gen config b
ggplot(employer_data_comp %>%
         filter(!is.na(Gender_of_FW), !is.na(multi_target), 
                Fiscal_Year == "18/19"), 
       aes(x=Gender_of_FW, fill = multi_target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.2, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by Gender of Foreign Worker", 
       subtitle = str_c("2018/2019 (",gen_a_three$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right",
        strip.background = element_rect(colour = "grey30", fill = "white"))


employer_data_comp %>% 
  filter(!is.na(multi_target), !is.na(Gender_of_FW), 
         Fiscal_Year == "18/19") -> gens2

( summary_table(dplyr::group_by(gens2, Gender_of_FW), gens_summary2) )
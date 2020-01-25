#### Gender of FW, Descision config A, by fiscal year

# gen config a
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), !is.na(target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(Gender_of_FW, target), 
                  fill=target), na.rm=TRUE, color = "grey40") +
  facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                     "17/18" = " 2017/2018",
                                                     "18/19" = "2018/2019"))) +
  labs(title = "Decision Breakdown by Gender of Foriegn worker  - faceted by Fiscal Year", 
       subtitle = str_c("All years (",gen_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")


# 2016/2017 gender config a
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), !is.na(target), 
         Fiscal_Year == "16/17") %>%
  tally() -> gen_a_one

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(Gender_of_FW), !is.na(target), 
                Fiscal_Year == "16/17"), 
       aes(x = Gender_of_FW, fill = target)) +
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

gens <- employer_data_comp %>% 
  filter(!is.na(target), !is.na(Gender_of_FW), 
         Fiscal_Year == "16/17") -> gens

( summary_table(dplyr::group_by(gens, Gender_of_FW), gens_summary1) )

#2017/2018 gender config a
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), !is.na(target), 
         Fiscal_Year == "17/18") %>%
  tally() -> gen_a_two

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(Gender_of_FW), !is.na(target), 
                Fiscal_Year == "17/18"), 
       aes(x=Gender_of_FW, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label = ..count..), 
            vjust=-0.2, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by Gender of Foreign Worker", 
       subtitle = str_c("2017/2018 (",gen_a_two$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right",
        strip.background = element_rect(colour = "grey30", 
                                        fill = "white"))


gens <- employer_data_comp %>% 
  filter(!is.na(target), !is.na(Gender_of_FW), 
         Fiscal_Year == "17/18") -> gens

( summary_table(dplyr::group_by(gens, Gender_of_FW), gens_summary1) )



#2018/2019 gender config a
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), !is.na(target), 
         Fiscal_Year == "18/19") %>%
  tally() -> gen_a_three

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(Gender_of_FW), !is.na(target), 
                Fiscal_Year == "18/19"), 
       aes(x=Gender_of_FW, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label = ..count..), 
            vjust=-0.2, position = position_dodge(width = .9)) +
  labs(title="Decision Breakdown by Gender of Foreign Worker", 
       subtitle = str_c("2018/2019 (",gen_a_three$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right",
        strip.background = element_rect(colour = "grey30", 
                                        fill = "white"))


employer_data_comp %>% 
  filter(!is.na(target), !is.na(Gender_of_FW), 
         Fiscal_Year == "18/19") -> gens

( summary_table(dplyr::group_by(gens, Gender_of_FW), gens_summary1) )
                
                
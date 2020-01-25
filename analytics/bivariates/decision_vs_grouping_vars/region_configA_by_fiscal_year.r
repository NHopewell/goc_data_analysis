
#### Region Descision conf A, by fiscal year 

employer_data_comp %>%
  filter(!is.na(Region), !is.na(target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(Region, target), 
                  fill=target), na.rm=TRUE, color = "grey40") +
  facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                     "17/18" = " 2017/2018",
                                                     "18/19" = "2018/2019"))) +
  labs(title="Decision Breakdown by Province - faceted by Fiscal Year", 
       subtitle= str_c("All years (",reg_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position="bottom")



# 2016/2017 region config a
employer_data_comp %>%
  filter(!is.na(Region), !is.na(target), 
         Fiscal_Year == "16/17") %>% 
  tally() -> reg_a_one

# figure             
ggplot(employer_data_comp %>%
         filter(!is.na(Region), !is.na(target), 
                Fiscal_Year == "16/17"), 
       aes(x=Region, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label = ..count..), 
            vjust = -0.3, position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by Province",
       subtitle = str_c("2016/2017 (",reg_a_one$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right",
        strip.background = element_rect(colour = "grey30", 
                                        fill = "white"))

employer_data_comp %>% 
  filter(!is.na(target), !is.na(Region), 
         Fiscal_Year == "16/17") -> regs

( summary_table(dplyr::group_by(regs, Region), region_summary1) )



# 2017/2018 region config a
employer_data_comp %>%
  filter(!is.na(Region), !is.na(target), 
         Fiscal_Year == "17/18") %>%
  tally() -> reg_a_two

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(Region), !is.na(target), 
                Fiscal_Year == "17/18"), 
       aes(x=Region, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.2, position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by Province",
       subtitle = str_c("2017/2018 (",reg_a_two$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right",
        strip.background = element_rect(colour = "grey30", 
                                        fill = "white"))


employer_data_comp %>% 
  filter(!is.na(target), !is.na(Region), 
         Fiscal_Year == "17/18") -> regs

( summary_table(dplyr::group_by(regs, Region), region_summary1) )



# 2018/2019 region config a
employer_data_comp %>%
  filter(!is.na(Region), !is.na(target), 
         Fiscal_Year == "18/19") %>%
  tally() -> reg_a_three

# figure 
ggplot(employer_data_comp %>%
         filter(!is.na(Region), !is.na(target), 
                Fiscal_Year == "18/19"), 
       aes(x = Region, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.2, position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by Province",
       subtitle = str_c("2018/2019 (",reg_a_three$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right",
        strip.background = element_rect(colour = "grey30", 
                                        fill = "white"))

employer_data_comp %>% 
  filter(!is.na(target), !is.na(Region), 
         Fiscal_Year == "18/19") -> regs

( summary_table(dplyr::group_by(regs, Region), region_summary1) )
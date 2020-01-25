#### Region Descision conf D, by fiscal year

# region config d
employer_data_comp %>%
  filter(!is.na(Region), !is.na(target_down)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(Region, target_down), 
                  fill=target_down), na.rm=TRUE, color = "grey40") +
  facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                     "17/18" = " 2017/2018",
                                                     "18/19" = "2018/2019"))) +
  labs(title = "Decision Breakdown by Province - faceted by Fiscal Year", 
       subtitle= str_c("All years (",reg_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")


# 2016/2017 region config d
ggplot(employer_data_comp %>%
         filter(!is.na(Region), !is.na(target_down), 
                Fiscal_Year == "16/17"), 
       aes(x=Region, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.4, position = position_dodge(width = .9)) +
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
  filter(!is.na(target_down), !is.na(Region),
         Fiscal_Year == "16/17") -> tdregs

( summary_table(dplyr::group_by(tdregs, Region), region_summary4) )


# 2017/2018 region config d
ggplot(employer_data_comp %>%
         filter(!is.na(Region), !is.na(target_down), 
                Fiscal_Year == "17/18"), 
       aes(x = Region, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.4, position = position_dodge(width = .9)) +
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
  filter(!is.na(target_down), !is.na(Region),
         Fiscal_Year == "17/18") -> tdregs

( summary_table(dplyr::group_by(tdregs, Region), region_summary4) )



# 2018/2019 region config d
ggplot(employer_data_comp %>%
         filter(!is.na(Region), !is.na(target_down), 
                Fiscal_Year == "18/19"), 
       aes(x=Region, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.4, position = position_dodge(width = .9)) +
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
  filter(!is.na(target_down), !is.na(Region),
         Fiscal_Year == "18/19") -> tdregs

( summary_table(dplyr::group_by(tdregs, Region), region_summary4) )
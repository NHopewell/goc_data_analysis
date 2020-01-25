#### NOC Type,  By Region, All years 

# noc by region
employer_data_comp %>%
  filter(!is.na(NOC_Type), 
         !is.na(Region)) %>%
  tally() -> noc_reg_tot

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(NOC_Type), !is.na(Region)), 
       aes(x = NOC_Type, fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Type by Region",
       subtitle = str_c("All years (",noc_reg_tot$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(legend.position="bottom")


employer_data_comp %>% 
  filter(!is.na(NOC_Type), !is.na(Region)) -> noc_reg

noc_reg_summary1 <-
  list("NOC_Type" =
         list("Intermediate jobs" = ~ n_perc(.data$NOC_Type == "Intermediate jobs"),
              "Labour jobs" = ~ n_perc(.data$NOC_Type == "Labour jobs"),
              "Management jobs" = ~ n_perc(.data$NOC_Type == "Management jobs"),
              "Professional jobs"  = ~ n_perc(.data$NOC_Type == "Professional jobs"),
              "Technical jobs & skilled trade" = ~ n_perc(.data$NOC_Type == "Technical jobs & skilled trade")
         ))

( summary_table(dplyr::group_by(noc_reg, Region), noc_reg_summary1) )
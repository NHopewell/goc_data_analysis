#### NOC Type by gender of fw, All years 

employer_data_comp %>%
  filter(!is.na(NOC_Type), 
         !is.na(Gender_of_FW)) %>%
  tally() -> noc_gen_tot

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(NOC_Type), !is.na(Gender_of_FW)), 
       aes(x=NOC_Type, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Type by Gender of Foreign Worker",
       subtitle = str_c("All years (",noc_gen_tot$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal()

employer_data_comp %>% 
  filter(!is.na(NOC_Type), !is.na(Gender_of_FW)) -> noc_gen
noc_reg_summary2 <-
  list("NOC_Type" =
         list("Intermediate jobs" = ~ n_perc(.data$NOC_Type == "Intermediate jobs"),
              "Labour jobs" = ~ n_perc(.data$NOC_Type == "Labour jobs"),
              "Management jobs" = ~ n_perc(.data$NOC_Type == "Management jobs"),
              "Professional jobs"  = ~ n_perc(.data$NOC_Type == "Professional jobs"),
              "Technical jobs & skilled trade" = ~ n_perc(.data$NOC_Type == "Technical jobs & skilled trade")
         ))

( summary_table(dplyr::group_by(noc_gen, Gender_of_FW), noc_reg_summary2) )
####  noc broad occ config c, all years
ggplot(broad_data, 
       aes(x=NOC_Broad_Occupation, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.4, 
            position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by NOC_Broad_Occupation", 
       subtitle = str_c("All Years (",broad_n_a$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(legend.position = "bottom")


employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(NOC_Broad_Occupation), 
         NOC_Broad_Occupation %in% broad_slice) -> nocs_b

nocb_summary3 <-
  list("Decision config c" =
         list("compliant" = ~ n_perc(.data$target_three == "compliant"),
              "compliant w/ justification" = 
                ~ n_perc(.data$target_three == "compliant w/\njustification"),
              "non-compliant" = ~ n_perc(.data$target_three == "non-compliant")
         ))

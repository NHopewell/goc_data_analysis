##### Noc broad occ, Descision config B, All Years  

# noc broad occ, all years
ggplot(broad_data, 
       aes(x=NOC_Broad_Occupation, fill = multi_target)) +
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
  filter(!is.na(multi_target), !is.na(NOC_Broad_Occupation), 
         NOC_Broad_Occupation %in% broad_slice) -> nocs_b 

nocb_summary2 <-
  list("Decision config b" =
         list("compliant" = ~ n_perc(.data$multi_target == "compliant"),
              "compliant w/ justification" = 
                ~ n_perc(.data$multi_target == "compliant w/\njustification"),
              "compliant w/ justification and compensation" = 
                ~ n_perc(.data$multi_target == "compliant w/\njustification and\ncompensation"),
              "non-compliant" = ~ n_perc(.data$multi_target == "non-compliant")
         ))

( summary_table(dplyr::group_by(nocs_b, NOC_Broad_Occupation), nocb_summary2) )


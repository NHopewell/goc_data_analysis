#### noc broad occ config d, all years


ggplot(broad_data, 
       aes(x = NOC_Broad_Occupation, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.4, 
            position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by NOC_Broad_Occupation", 
       subtitle = "All Years (4969 observations)\n",
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(legend.position="bottom")


employer_data_comp %>% 
  filter(!is.na(target_down), !is.na(NOC_Broad_Occupation), 
         NOC_Broad_Occupation %in% broad_slice) -> nocs_b

nocb_summary4 <-
  list("Decision config a" =
         list("compliant" = ~ n_perc(.data$target_down == 'compliant'),
              "non-compliant" = ~ n_perc(.data$target_down == 'non-compliant')
         ))

( summary_table(dplyr::group_by(nocs_b, NOC_Broad_Occupation), nocb_summary4) )
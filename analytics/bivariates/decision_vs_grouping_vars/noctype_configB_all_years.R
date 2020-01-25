
##### NOC Type Decision config B, All years

# noc config b, all years
noc_data_temp %>%
  filter(!is.na(NOC_Type), !is.na(multi_target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(NOC_Type, multi_target), 
                  fill = multi_target), na.rm = TRUE, color = "grey40") +
  labs(title = "Decision Breakdown by NOC_Type", 
       subtitle=str_c("All years (",noc_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "right") -> p1

# figure
ggplot(noc_data_temp %>%
         filter(!is.na(NOC_Type), !is.na(multi_target)), 
       aes(x=NOC_Type, fill = multi_target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.4, position = position_dodge(width = .9)) +
  labs(y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right")  +
  theme(legend.position="bottom") -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)


employer_data_comp %>% 
  filter(!is.na(multi_target), !is.na(NOC_Type)) -> mnoc

noc_summary2 <-
  list("Decision config b" =
         list("compliant" = ~ n_perc(.data$multi_target == "compliant"),
              "compliant w/ justification" = 
                ~ n_perc(.data$multi_target == "compliant w/\njustification"),
              "compliant w/ justification and compensation" = 
                ~ n_perc(.data$multi_target == "compliant w/\njustification and\ncompensation"),
              "non-compliant" = ~ n_perc(.data$multi_target == "non-compliant")
         ))

( summary_table(dplyr::group_by(mnoc, NOC_Type), noc_summary2) )
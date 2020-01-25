## NAICS config b, all years 


# naics config b, all years
naics_sliced %>%
  filter(!is.na(NAICS), !is.na(multi_target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(NAICS, multi_target), 
                  fill=multi_target), na.rm=TRUE, color = "grey40") +
  labs(title = "Decision Breakdown by NAICS", 
       subtitle = str_c("All years (",naics_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        legend.position="right") -> p1

# figure
ggplot(naics_sliced %>%
         filter(!is.na(NAICS), !is.na(multi_target)), 
       aes(x = NAICS, fill = multi_target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label =..count..), 
            vjust=-0.2, position = position_dodge(width = .9)) +
  labs(y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "bottom") -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)


employer_data_comp %>% 
  filter(!is.na(multi_target), !is.na(NAICS),
         NAICS %in% naics_names) -> naics_sub

naics_summary2 <-
  list("Decision config b" =
         list("compliant" = ~ n_perc(.data$multi_target == "compliant"),
              "compliant w/ justification" = 
                ~ n_perc(.data$multi_target == "compliant w/\njustification"),
              "compliant w/ justification and compensation" = 
                ~ n_perc(.data$multi_target == "compliant w/\njustification and\ncompensation"),
              "non-compliant" = ~ n_perc(.data$multi_target == "non-compliant")
         ))

( summary_table(dplyr::group_by(naics_sub, NAICS), naics_summary2) )
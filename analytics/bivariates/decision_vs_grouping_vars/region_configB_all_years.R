#### Region Descision conf B, All years  

# Region config b
employer_data_comp %>%
  filter(!is.na(Region), !is.na(multi_target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(Region, multi_target), 
                  fill=multi_target), na.rm=TRUE, color = "grey40") +
  labs(title = "Decision Breakdown by Province", 
       subtitle= str_c("All years (",reg_n_a$n," observations)\n"),
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
ggplot(employer_data_comp %>%
         filter(!is.na(Region), !is.na(multi_target)), 
       aes(x=Region, fill = multi_target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.5, position = position_dodge(width = .9)) +
  labs(y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)


employer_data_comp %>% 
  filter(!is.na(multi_target), !is.na(Region)) -> mregs

region_summary2 <-
  list("Decision config b" =
         list("compliant" = ~ n_perc(.data$multi_target == "compliant"),
              "compliant w/ justification" = 
                ~ n_perc(.data$multi_target == "compliant w/\njustification"),
              "compliant w/ justification and compensation" = 
                ~ n_perc(.data$multi_target == "compliant w/\njustification and\ncompensation"),
              "non-compliant" = ~ n_perc(.data$multi_target == "non-compliant")
         ))

( summary_table(dplyr::group_by(mregs, Region), region_summary2) )
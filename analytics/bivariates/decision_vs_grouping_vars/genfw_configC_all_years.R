##### Gender of FW Decision config C, All years


# gen config c, all years
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), !is.na(target_three)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(Gender_of_FW, target_three),
                  fill = target_three), na.rm=TRUE, color = "grey40") +
  labs(title="Decision Breakdown by Gender of Foreign Worker", 
       subtitle= str_c("All years (",gen_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "right") -> p1

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(Gender_of_FW), !is.na(target_three)), 
       aes(x = Gender_of_FW, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.5, position = position_dodge(width = .9)) +
  labs(y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)

employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(Gender_of_FW)) -> gens3

gens_summary3 <-
  list("Decision config c" =
         list("compliant" = ~ n_perc(.data$target_three == "compliant"),
              "compliant w/ justification" = 
                ~ n_perc(.data$target_three == "compliant w/\njustification"),
              "non-compliant" = ~ n_perc(.data$target_three == "non-compliant")
         ))

( summary_table(dplyr::group_by(gens3, Gender_of_FW), gens_summary3) )
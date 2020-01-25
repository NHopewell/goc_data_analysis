#### Gender of FW, Descision config A, All years

# gender config a, all years
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), !is.na(target)) %>%
  tally() -> gen_n_a

# figure
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), !is.na(target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(Gender_of_FW, target), 
                  fill=target), na.rm=TRUE, color = "grey40") +
  labs(title="Decision Breakdown by Gender of Foreign Worker", 
       subtitle= str_c("All years (",gen_n_a$n," observations)\n"),
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
         filter(!is.na(Gender_of_FW), !is.na(target)), 
       aes(x=Gender_of_FW, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.4, position = position_dodge(width = .9)) +
  labs(y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)

employer_data_comp %>% 
  filter(!is.na(target), !is.na(Gender_of_FW)) -> gens

gens_summary1 <-
  list("Decision config a" =
         list("compliant" = ~ n_perc(.data$target == 'compliant'),
              "non-compliant" = ~ n_perc(.data$target == 'non-compliant')
         ))

( summary_table(dplyr::group_by(gens, Gender_of_FW), gens_summary1) )
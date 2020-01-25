##### lmia config b, all years

lmia_data_temp %>%
  filter(!is.na(LMIA_Regulation_Section), !is.na(multi_target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(LMIA_Regulation_Section, multi_target), 
                  fill = multi_target), na.rm=TRUE, color = "grey40") +
  labs(title = "Decision Breakdown by LMIA_Regulation_Section", 
       subtitle = str_c("All years (",lmia_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom") -> p1

# figure
ggplot(lmia_data_temp %>%
         filter(!is.na(LMIA_Regulation_Section), !is.na(multi_target)), 
       aes(x = LMIA_Regulation_Section, fill = multi_target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, 
            position = position_dodge(width = .9)) +
  labs(y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)


employer_data_comp %>% 
  filter(!is.na(multi_target), !is.na(LMIA_Regulation_Section)) -> lmias 

lmia_summary1 <-
  list("Decision config b" =
         list("compliant" = ~ n_perc(.data$multi_target == 'compliant'),
              "compliant w/ justification" = 
                ~ n_perc(.data$multi_target == "compliant w/\njustification"),
              "compliant w/ justification and compensation" = 
                ~ n_perc(.data$multi_target == "compliant w/\njustification and\ncompensation"),
              "non-compliant" = ~ n_perc(.data$multi_target == 'non-compliant')
         ))

( summary_table(dplyr::group_by(lmias, LMIA_Regulation_Section), lmia_summary1) )
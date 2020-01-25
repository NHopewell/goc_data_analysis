#### lmia section config d, all years


lmia_data_temp %>%
  filter(!is.na(LMIA_Regulation_Section), !is.na(target_down)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(LMIA_Regulation_Section, target_down), 
                  fill=target_down), na.rm=TRUE, color = "grey40") +
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
         filter(!is.na(LMIA_Regulation_Section), !is.na(target_down)), 
       aes(x = LMIA_Regulation_Section, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), vjust = -0.5, 
            position = position_dodge(width = .9)) +
  labs(y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)

employer_data_comp %>% 
  filter(!is.na(target_down), !is.na(LMIA_Regulation_Section)) -> lmias

lmia_summary4 <-
  list("Decision config d" =
         list("compliant" = ~ n_perc(.data$target_down == 'compliant'),
              "non-compliant" = ~ n_perc(.data$target_down == 'non-compliant')
         ))

( summary_table(dplyr::group_by(lmias, LMIA_Regulation_Section), lmia_summary4) )
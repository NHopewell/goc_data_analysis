#### LMIA Section By Region, all years 

# lmia section by region
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section), 
         !is.na(Region)) %>%
  tally() -> lmia_reg_tot

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(LMIA_Regulation_Section), !is.na(Region)), 
       aes(x=LMIA_Regulation_Section, fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), vjust = -0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Section by Region",
       subtitle = str_c("All years (",lmia_reg_tot$n," observations)\n"),
       x = "",
       y = "") +
  scale_fill_tableau() +
  theme_minimal()

employer_data_comp %>% 
  filter(!is.na(LMIA_Regulation_Section), !is.na(Region)) -> lmia_reg

lmia_reg_summary1 <-
  list("LMIA_Regulation_Section" =
         list("Canadian interests" = ~ n_perc(.data$LMIA_Regulation_Section == "Canadian interests" ),
              "International agreements"  = ~ n_perc(.data$LMIA_Regulation_Section == "International agreements" ),
              "Permanent residence applicants in Canada" = 
                ~ n_perc(.data$LMIA_Regulation_Section == "Permanent residence applicants in Canada")
         ))

( summary_table(dplyr::group_by(lmia_reg, Region), lmia_reg_summary1) )
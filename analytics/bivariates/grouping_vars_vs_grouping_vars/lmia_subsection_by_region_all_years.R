#### LMIA Sub-section By Region All years 

employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection), 
         !is.na(Region), 
         LMIA_Regulation_Subsection %in% slice) %>%
  tally() -> lmias_rg_tot

employer_data_comp %>%
  mutate(LMIA_Regulation_Subsection = fct_recode(LMIA_Regulation_Subsection,
                                                 "R204(a)" ="R204(a) Canada-international exemption codes",
                                                 "R204(c)" ="R204(c) Canada-provincial/territorial exemption codes",
                                                 "R205(a)" ="R205(a) Significant benefit exemption codes",
                                                 "R205(b)" ="R205(b) Reciprocal employment exemption codes",
                                                 "R205(d)" ="R205(d) Charitable or religious work exemption code"
  )) -> lsub_temp

slice <- c("R205(a)", 
           "R205(b)", 
           "R204(a)", 
           "R204(c)", 
           "R205(d)")

# figure
ggplot(lsub_temp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(Region), 
                LMIA_Regulation_Subsection %in% slice), 
       aes(x=fct_infreq(LMIA_Regulation_Subsection), fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.3, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Subsection by Region",
       subtitle = str_c("All years (",lmias_rg_tot$n," observations)\n"),
       x = "",
       y = "",
       caption = 
         "Subsections not shown due to low representation:\nR204(b), R205(c)(i), R205(c)(ii), R205(d)") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position="bottom")

lsub_temp %>% 
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Region), 
         LMIA_Regulation_Subsection %in% slice) -> lmias_reg

lmias_reg_summary1 <-
  list("LMIA_Regulation_Subsection" =
         list("R205(a)" = 
                ~ n_perc(.data$LMIA_Regulation_Subsection == "R205(a)"),
              "R205(b)" = 
                ~ n_perc(.data$LMIA_Regulation_Subsection ==  "R205(b)"),
              "R204(a)" = ~ n_perc(.data$LMIA_Regulation_Subsection == "R204(a)"),
              "R204(c)" = 
                ~ n_perc(.data$LMIA_Regulation_Subsection == "R204(c)"),
              "R205(d)" = 
                ~ n_perc(.data$LMIA_Regulation_Subsection ==  "R205(d)"))
  )

( summary_table(dplyr::group_by(lmias_reg, Region), lmias_reg_summary1) )
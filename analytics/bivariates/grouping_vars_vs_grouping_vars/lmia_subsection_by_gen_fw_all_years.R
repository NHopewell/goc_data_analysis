##### LMIA subsection By Gender of Foriegn Worker All years 

employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection), 
         !is.na(Gender_of_FW)) %>%
  tally() -> lmias_gen_tot

# figure
ggplot(lsub_temp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(Gender_of_FW), 
                LMIA_Regulation_Subsection %in% slice), 
       aes(x = LMIA_Regulation_Subsection, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Subsection by Gender of Foreign Worker",
       subtitle = str_c("All years (",lmias_gen_tot$n," observations)"),
       x = "",
       y = "",
       caption =
         "Subsections not shown due to low representation:\nR204(b), R205(c)(i), R205(c)(ii), R205(d)") +
  scale_fill_tableau() +
  theme_minimal()

lsub_temp %>% 
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Gender_of_FW)) -> lmias_gen

lmias_gen_summary1 <-
  list("LMIA_Regulation_Subsection" =
         list("R204(a)" = ~ n_perc(.data$LMIA_Regulation_Subsection == "R204(a)"),
              "R204(c)" = 
                ~ n_perc(.data$LMIA_Regulation_Subsection == "R204(c)"),
              "R205(a)" = 
                ~ n_perc(.data$LMIA_Regulation_Subsection == "R205(a)"),
              "R205(b)" = 
                ~ n_perc(.data$LMIA_Regulation_Subsection ==  "R205(b)"),
              "R205(d)" = 
                ~ n_perc(.data$LMIA_Regulation_Subsection ==  "R205(d)"))
  )

( summary_table(dplyr::group_by(lmias_gen, Gender_of_FW), lmias_gen_summary1) )

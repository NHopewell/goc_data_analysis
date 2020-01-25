#### lmia sub sectio config d, all years

sliced_data_comp %>%
  ggplot() +
  geom_mosaic(aes(x = product(LMIA_Regulation_Subsection, target_down), 
                  fill = target_down), na.rm = TRUE, color = "grey40") +
  labs(title="Decision Breakdown by LMIA_Regulation_Subsection", 
       subtitle = str_c("All years (",sub_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  scale_y_productlist(labels = c("R204(a) Canada-international exemption codes" = "R204(a)", 
                                 "R204(c) Canada-provincial/territorial exemption codes" = "R204(c)",
                                 "R205(a) Significant benefit exemption codes" = "R205(a)",
                                 "R205(b) Reciprocal employment exemption codes" = "R205(b)",
                                 "R205(d) Charitable or religious work exemption code" = "R205(d)")) + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "right") -> p1

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(target_down), 
                LMIA_Regulation_Subsection %in% slice),
       aes(x = LMIA_Regulation_Subsection, fill = target_down)) + 
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.20, 
            position = position_dodge(width = .9)) + 
  labs(x = "",
       y = "",
       fill = "Decision",
       caption = 
         "Subsections not shown due to low representation:\nR204(b), R205(c)(i), R205(c)(ii), R205(d)")  +
  scale_fill_tableau() +
  scale_x_discrete(labels = c("R204(a) Canada-international exemption codes" = "R204(a)", 
                              "R204(c) Canada-provincial/territorial exemption codes" = "R204(c)",
                              "R205(a) Significant benefit exemption codes" = "R205(a)",
                              "R205(b) Reciprocal employment exemption codes" = "R205(b)",
                              "R205(d) Charitable or religious work exemption code" = "R205(d)")) + 
  theme_minimal() -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)


employer_data_comp %>% 
  filter(!is.na(multi_target), !is.na(LMIA_Regulation_Subsection),
         LMIA_Regulation_Subsection %in% slice) %>%
  rename(Subsection = LMIA_Regulation_Subsection ) %>%
  mutate(Subsection= fct_recode(Subsection,
                                "R204(a)" = "R204(a) Canada-international exemption codes", 
                                "R204(c)" = "R204(c) Canada-provincial/territorial exemption codes",
                                "R205(a)" = "R205(a) Significant benefit exemption codes",
                                "R205(b)" = "R205(b) Reciprocal employment exemption codes",
                                "R205(d)" = "R205(d) Charitable or religious work exemption code")) -> lmias_sub

lmiaSub_summary4 <-
  list("Decision config d" =
         list("compliant" = ~ n_perc(.data$target_down == 'compliant'),
              "non-compliant" = ~ n_perc(.data$target_down == 'non-compliant')
         ))

( summary_table(dplyr::group_by(lmias_sub, Subsection), lmiaSub_summary4) )
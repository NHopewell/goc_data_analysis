##### LMIA subsection, Descision config C, by fiscal year

sliced_data_comp %>%
  ggplot() +
  geom_mosaic(aes(x = product(LMIA_Regulation_Subsection, target_three), 
                  fill=target_three), na.rm=TRUE, color = "grey40") +
  facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                     "17/18" = " 2017/2018",
                                                     "18/19" = "2018/2019"))) +
  labs(title="Decision Breakdown by LMIA_Regulation_Subsection", 
       subtitle= str_c("All years (",reg_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  scale_y_productlist(labels=c("R204(a) Canada-international exemption codes" = "R204(a)", 
                               "R204(c) Canada-provincial/territorial exemption codes" = "R204(c)",
                               "R205(a) Significant benefit exemption codes" = "R205(a)",
                               "R205(b) Reciprocal employment exemption codes" = "R205(b)",
                               "R205(d) Charitable or religious work exemption code" = "R205(d)")) + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        legend.position="bottom")

ggplot(employer_data_comp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(target_three), 
                Fiscal_Year == '16/17', 
                LMIA_Regulation_Subsection %in% slice),
       aes(x = LMIA_Regulation_Subsection, fill = target_three)) + 
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.20, 
            position = position_dodge(width = .9)) +
  labs(title="LMIA_Regulation_Subsection Breakdown", 
       subtitle = str_c("2016/2017 (",reg_a_one$n," observations)\n"),
       x = "",
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
  theme_minimal() +
  theme(legend.position="right")


employer_data_comp %>% 
  filter(!is.na(multi_target), !is.na(LMIA_Regulation_Subsection), 
         Fiscal_Year == '16/17',
         LMIA_Regulation_Subsection %in% slice) %>%
  rename(Subsection = LMIA_Regulation_Subsection ) %>%
  mutate(Subsection= fct_recode(Subsection,
                                "R204(a)" = "R204(a) Canada-international exemption codes", 
                                "R204(c)" = "R204(c) Canada-provincial/territorial exemption codes",
                                "R205(a)" = "R205(a) Significant benefit exemption codes",
                                "R205(b)" = "R205(b) Reciprocal employment exemption codes",
                                "R205(d)" = "R205(d) Charitable or religious work exemption code"))  -> lmias_sub

( summary_table(dplyr::group_by(lmias_sub, Subsection), lmiaSub_summary3) )


# 2017/2018 lmia sub section config c
ggplot(employer_data_comp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(target_three), 
                Fiscal_Year == '17/18', 
                LMIA_Regulation_Subsection %in% slice),
       aes(x = LMIA_Regulation_Subsection, fill = target_three)) + 
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label = ..count..), vjust = -0.20, 
            position = position_dodge(width = .9)) +
  labs(title = "LMIA_Regulation_Subsection Breakdown", 
       subtitle = str_c("2017/2018 (",reg_a_two$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision",
       caption = 
         "Subsections not shown due to low representation:\nR204(b), R205(c)(i), R205(c)(ii), R205(d)")  +
  scale_fill_tableau() +
  scale_x_discrete(labels=c("R204(a) Canada-international exemption codes" = "R204(a)", 
                            "R204(c) Canada-provincial/territorial exemption codes" = "R204(c)",
                            "R205(a) Significant benefit exemption codes" = "R205(a)",
                            "R205(b) Reciprocal employment exemption codes" = "R205(b)",
                            "R205(d) Charitable or religious work exemption code" = "R205(d)")) + 
  theme_minimal() +
  theme(legend.position="right")


employer_data_comp %>% 
  filter(!is.na(multi_target), !is.na(LMIA_Regulation_Subsection), 
         Fiscal_Year == '17/18',
         LMIA_Regulation_Subsection %in% slice) %>%
  rename(Subsection = LMIA_Regulation_Subsection ) %>%
  mutate(Subsection= fct_recode(Subsection,
                                "R204(a)" = "R204(a) Canada-international exemption codes", 
                                "R204(c)" = "R204(c) Canada-provincial/territorial exemption codes",
                                "R205(a)" = "R205(a) Significant benefit exemption codes",
                                "R205(b)" = "R205(b) Reciprocal employment exemption codes",
                                "R205(d)" = "R205(d) Charitable or religious work exemption code")) -> lmias_sub

( summary_table(dplyr::group_by(lmias_sub, Subsection), lmiaSub_summary3) )


# 2018/2019 lmia sub section config c
ggplot(employer_data_comp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(target_three), 
                Fiscal_Year == '18/19', 
                LMIA_Regulation_Subsection %in% slice),
       aes(x = LMIA_Regulation_Subsection, fill = target_three)) + 
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.20, 
            position = position_dodge(width = .9)) +
  labs(title="LMIA_Regulation_Subsection Breakdown", 
       subtitle = str_c("2018/2019 (",reg_a_three$n," observations)\n"),
       x = "",
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
  theme_minimal() +
  theme(legend.position="right")


employer_data_comp %>% 
  filter(!is.na(multi_target), !is.na(LMIA_Regulation_Subsection), 
         Fiscal_Year == '18/19',
         LMIA_Regulation_Subsection %in% slice) %>%
  rename(Subsection = LMIA_Regulation_Subsection ) %>%
  mutate(Subsection= fct_recode(Subsection,
                                "R204(a)" = "R204(a) Canada-international exemption codes", 
                                "R204(c)" = "R204(c) Canada-provincial/territorial exemption codes",
                                "R205(a)" = "R205(a) Significant benefit exemption codes",
                                "R205(b)" = "R205(b) Reciprocal employment exemption codes",
                                "R205(d)" = "R205(d) Charitable or religious work exemption code")) -> lmias_sub

( summary_table(dplyr::group_by(lmias_sub, Subsection), lmiaSub_summary3) )
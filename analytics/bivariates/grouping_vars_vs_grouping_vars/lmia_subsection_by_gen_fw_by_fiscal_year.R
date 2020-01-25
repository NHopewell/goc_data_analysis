##### LMIA subsection By Gender of Foriegn Worker by fiscal year

# 2016/2017 lmia subsection by gender
lsub_temp %>%
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Gender_of_FW),
         Fiscal_Year == '16/17', 
         LMIA_Regulation_Subsection %in% slice) %>%
  tally()-> lsub_a_one

# figure
ggplot(lsub_temp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(Gender_of_FW),
                Fiscal_Year == '16/17', 
                LMIA_Regulation_Subsection %in% slice), 
       aes(x=LMIA_Regulation_Subsection, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40") +
  geom_text(stat = 'count', aes(label = ..count..), vjust=-0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Subsection by Gender of Foreign Worker",
       subtitle = str_c("2016/2017 (",lsub_a_one$n," observations)"),
       x = "",
       y = "",
       caption = 
         "Subsections not shown due to low representation:\nR204(b), R205(c)(i), R205(c)(ii), R205(d)") +
  scale_fill_tableau() +
  theme_minimal()

lsub_temp %>% 
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Gender_of_FW), 
         Fiscal_Year == '16/17') -> lmias_gen

( summary_table(dplyr::group_by(lmias_gen, Gender_of_FW), lmias_gen_summary1) )


# 2017/2018 lmia subsection by gender
lsub_temp %>%
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Gender_of_FW),
         Fiscal_Year == '17/18', 
         LMIA_Regulation_Subsection %in% slice) %>%
  tally() -> lsub_a_two

# figure
ggplot(lsub_temp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(Gender_of_FW),
                Fiscal_Year == '17/18', 
                LMIA_Regulation_Subsection %in% slice), 
       aes(x=LMIA_Regulation_Subsection, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust=-0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Subsection by Gender of Foreign Worker",
       subtitle = str_c("2017/2018 (",lsub_a_two$n," observations)"),
       x = "",
       y = "",
       caption = 
         "Subsections not shown due to low representation:\nR204(b), R205(c)(i), R205(c)(ii), R205(d)") +
  scale_fill_tableau() +
  theme_minimal()

lsub_temp %>% 
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Gender_of_FW), 
         Fiscal_Year == '17/18') -> lmias_gen

( summary_table(dplyr::group_by(lmias_gen, Gender_of_FW), lmias_gen_summary1) )


# 2018/2019 lmia subsection by gender
lsub_temp %>%
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Gender_of_FW),
         Fiscal_Year == '18/19', 
         LMIA_Regulation_Subsection %in% slice) %>%
  tally() -> lsub_a_three

# figure
ggplot(lsub_temp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(Gender_of_FW),
                Fiscal_Year == '18/19', 
                LMIA_Regulation_Subsection %in% slice), 
       aes(x=LMIA_Regulation_Subsection, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label=..count..), vjust = -0.5, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Subsection by Gender of Foreign Worker",
       subtitle = str_c("2018/2019 (",lsub_a_three$n," observations)"),
       x = "",
       y = "",
       caption = 
         "Subsections not shown due to low representation:\nR204(b), R205(c)(i), R205(c)(ii), R205(d)") +
  scale_fill_tableau() +
  theme_minimal()

lsub_temp %>% 
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Gender_of_FW), 
         Fiscal_Year == '18/19') -> lmias_gen

summary_table(dplyr::group_by(lmias_gen, Gender_of_FW), lmias_gen_summary1)
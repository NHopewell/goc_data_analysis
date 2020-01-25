# 2016/2017 lima subsection by region
lsub_temp %>%
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Region), 
         Fiscal_Year == '16/17', 
         LMIA_Regulation_Subsection %in% slice) %>%
  tally() -> lreg_a_one

# figure
ggplot(lsub_temp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(Region), 
                Fiscal_Year == '16/17', 
                LMIA_Regulation_Subsection %in% slice), 
       aes(x=fct_infreq(LMIA_Regulation_Subsection), fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label = ..count..), vjust=-0.3, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Subsection by Region",
       subtitle = str_c("2016/2017 (",lreg_a_one$n," observations)\n"),
       x = "",
       y = "",
       caption = 
         "Subsections not shown due to low representation:\nR204(b), R205(c)(i), R205(c)(ii), R205(d)") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position="bottom")

lsub_temp %>% 
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Region), 
         LMIA_Regulation_Subsection %in% slice, Fiscal_Year == '16/17') -> lmias_reg 

( summary_table(dplyr::group_by(lmias_reg, Region), lmias_reg_summary1) )


# 2017/2018 lmia subsection by region
lsub_temp %>%
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Region), 
         Fiscal_Year == '17/18', 
         LMIA_Regulation_Subsection %in% slice) %>%
  tally() -> lreg_a_two

# figure
ggplot(lsub_temp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(Region), 
                Fiscal_Year == '17/18', 
                LMIA_Regulation_Subsection %in% slice), 
       aes(x = fct_infreq(LMIA_Regulation_Subsection), fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label=..count..), vjust=-0.3, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Subsection by Region",
       subtitle = str_c("2017/2018 (",lreg_a_two$n," observations)\n"),
       x = "",
       y = "",
       caption = 
         "Subsections not shown due to low representation:\nR204(b), R205(c)(i), R205(c)(ii), R205(d)") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position="bottom")

lsub_temp %>% 
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Region),
         LMIA_Regulation_Subsection %in% slice, Fiscal_Year == '17/18') -> lmias_reg

( summary_table(dplyr::group_by(lmias_reg, Region), lmias_reg_summary1) )


# 2018/2019 lmia subsection by region
lsub_temp %>%
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Region), 
         Fiscal_Year == '18/19', 
         LMIA_Regulation_Subsection %in% slice) %>%
  tally() -> lreg_a_three

# figure
ggplot(lsub_temp %>%
         filter(!is.na(LMIA_Regulation_Subsection), !is.na(Region), 
                Fiscal_Year == '18/19', 
                LMIA_Regulation_Subsection %in% slice), 
       aes(x=fct_infreq(LMIA_Regulation_Subsection), fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of LMIA_Regulation_Subsection by Region",
       subtitle = str_c("2018/2019 (",lreg_a_three$n," observations)\n"),
       x = "",
       y = "",
       caption = 
         "Subsections not shown due to low representation:\nR204(b), R205(c)(i), R205(c)(ii), R205(d)") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position="bottom")
```
```{r, results='asis'}
lsub_temp %>% 
  filter(!is.na(LMIA_Regulation_Subsection), !is.na(Region),
         LMIA_Regulation_Subsection %in% slice, Fiscal_Year == '18/19') -> lmias_reg 

( summary_table(dplyr::group_by(lmias_reg, Region), lmias_reg_summary1) )
### LMIA Subsection Faceted by fiscal year  

# 2016/2017 lmia code
employer_data_comp %>%
  filter(!is.na(LMIA_Exemption_Code), 
         Fiscal_Year == '16/17') %>%
  group_by(LMIA_Exemption_Code) %>%
  tally() -> LMIA_c_one

LMIA_c_one %>%
  mutate(LMIA_Exemption_Code = fct_recode(
    LMIA_Exemption_Code, 
    "Emergency repairs or repair personnel for\nout-of-warranty equipment" =
      "Emergency repairs or repair personnel for out-of-warranty equipment")
  ) -> LMIA_c_one

# figure
LMIA_c_one %>%
  ggplot(aes(x = reorder(LMIA_Exemption_Code, n), y = n)) +
  geom_point(col = "#d63a3a", size = 4) +
  ylim(0, max(LMIA_c_one$n)+30) +
  geom_segment(aes(x = LMIA_Exemption_Code, 
                   xend = LMIA_Exemption_Code, 
                   y = n + 10, 
                   yend = max(LMIA_c_one$n)+30), 
               linetype = "dashed", 
               size = 0.1,
               color = "grey70") +
  geom_segment(aes(x = LMIA_Exemption_Code,
                   xend = LMIA_Exemption_Code,
                   y = 0, 
                   yend = n), 
               size = 1,
               color = "#d63a3a") + 
  labs(title = "LMIA Exemption Code Breakdown", 
       subtitle = str_c("2016/2017 (",sum(LMIA_c_one$n)," observations)\n"),
       x = "",
       y = "") + 
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.9) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


# 2017/2018 lmia code
employer_data_comp %>%
  filter(!is.na(LMIA_Exemption_Code), 
         Fiscal_Year == '17/18') %>%
  group_by(LMIA_Exemption_Code) %>%
  tally() -> LMIA_c_two

LMIA_c_two %>%
  mutate(LMIA_Exemption_Code = fct_recode(
    LMIA_Exemption_Code, 
    "Emergency repairs or repair personnel for\nout-of-warranty equipment" =
      "Emergency repairs or repair personnel for out-of-warranty equipment")
  ) -> LMIA_c_two

# figure
LMIA_c_two %>%
  ggplot(aes(x = reorder(LMIA_Exemption_Code, n), y = n)) +
  geom_point(col = "#d63a3a", size=4) +
  ylim(0, max(LMIA_c_two$n)+150) +
  geom_segment(aes(x = LMIA_Exemption_Code, 
                   xend = LMIA_Exemption_Code, 
                   y = n + 10, 
                   yend = max(LMIA_c_two$n)+150), 
               linetype = "dashed", 
               size = 0.1,
               color = "grey70") +
  geom_segment(aes(x = LMIA_Exemption_Code,
                   xend = LMIA_Exemption_Code,
                   y = 0, 
                   yend = n), 
               size = 1,
               color = "#d63a3a") + 
  labs(title = "LMIA Exemption Code Breakdown", 
       subtitle = str_c("2017/2018 (",sum(LMIA_c_two$n)," observations)\n"),
       x = "",
       y = "") + 
  coord_flip() +
  geom_text(aes(label= n), hjust = -0.9) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# 2018/2019 lmia code
employer_data_comp %>%
  filter(!is.na(LMIA_Exemption_Code), 
         Fiscal_Year == '18/19') %>%
  group_by(LMIA_Exemption_Code) %>%
  tally() -> LMIA_c_three

LMIA_c_three %>%
  mutate(LMIA_Exemption_Code = fct_recode(
    LMIA_Exemption_Code, 
    "Emergency repairs or repair personnel for\nout-of-warranty equipment" =
      "Emergency repairs or repair personnel for out-of-warranty equipment")
  ) -> LMIA_c_three

# figure
LMIA_c_three %>%
  ggplot(aes(x = reorder(LMIA_Exemption_Code, n), y = n)) +
  geom_point(col = "#d63a3a", size=4) +
  ylim(0, max(LMIA_c_three$n)+200) +
  geom_segment(aes(x = LMIA_Exemption_Code, 
                   xend = LMIA_Exemption_Code, 
                   y = n + 10, 
                   yend = max(LMIA_c_three$n)+200), 
               linetype = "dashed", 
               size = 0.1,
               color = "grey70") +
  geom_segment(aes(x = LMIA_Exemption_Code,
                   xend = LMIA_Exemption_Code,
                   y = 0, 
                   yend = n), 
               size = 1,
               color = "#d63a3a") + 
  labs(title = "LMIA Exemption Code Breakdown", 
       subtitle = str_c("2018/2019 (",sum(LMIA_c_three$n)," observations)\n"),
       x = "",
       y = "") + 
  coord_flip() +
  geom_text(aes(label = n), hjust=-0.9) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())



# lmia code
employer_data_comp %>%
  filter(!is.na(LMIA_Exemption_Code)) %>%
  group_by(LMIA_Exemption_Code) %>%
  tally() -> LMIA_c

LMIA_c %>%
  mutate(LMIA_Exemption_Code = fct_recode(
    LMIA_Exemption_Code, 
    "Emergency repairs or repair personnel for\nout-of-warranty equipment" =
      "Emergency repairs or repair personnel for out-of-warranty equipment")
  ) -> LMIA_c

# figure
LMIA_c %>%
  ggplot(aes(x = reorder(LMIA_Exemption_Code, n), y = n)) +
  geom_point(col = "#d63a3a", size = 4) +
  ylim(0, max(LMIA_c$n)+200) +
  geom_segment(aes(x = LMIA_Exemption_Code, 
                   xend = LMIA_Exemption_Code, 
                   y = n + 10, 
                   yend =  max(LMIA_c$n)+200), 
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
       subtitle = str_c("All years (",sum(LMIA_c$n)," observations)\n"),
       x = "",
       y = "") + 
  coord_flip() +
  geom_text(aes(label =  n), hjust = -0.5) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
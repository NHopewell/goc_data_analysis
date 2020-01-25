#### LMIA Section  

##### All years
# lmia regulation section
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section)) %>%
  tally() -> n_lmia

# figure
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Section)) %>%
  mutate(LMIA_Regulation_Section = fct_recode(
    LMIA_Regulation_Section, 
    "Permanent residence\napplicants in Canada" = 
      "Permanent residence applicants in Canada")
  ) %>%
  ggplot(aes(x = LMIA_Regulation_Section)) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.5) +
  labs(title="LMIA Regulation Section Breakdown", 
       subtitle=str_c("All years (",n_lmia$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")

employer_data_comp %>% 
  filter(!is.na(LMIA_Regulation_Section)) %>%
  group_by(LMIA_Regulation_Section) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('LMIA_Regulation_Section (all years)' = 'LMIA_Regulation_Section',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 3))
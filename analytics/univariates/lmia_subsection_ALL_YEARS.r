#### LMIA Sub-Section All years 

# labels for plotting
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection)) %>%
  mutate(LMIA_Regulation_Subsection = fct_recode(
    LMIA_Regulation_Subsection, 
    "R205(a)" = "R205(a) Significant benefit exemption codes",
    "R205(b)" = "R205(b) Reciprocal employment exemption codes",
    "R204(a)" = "R204(a) Canada-international exemption codes",
    "R204(c)" = 
      "R204(c) Canada-provincial/territorial exemption codes",
    "R205(d)" = 
      "R205(d) Charitable or religious work exemption code",
    "R207" = 
      "R207: Permanent residence applicants in Canada",
    "R205(c)(ii)" = 
      "R205(c)(ii) Competitiveness and public policy exemption codes",
    "R205(c)(i)" = "R205(c)(i) Research exemption codes",
    "R204(b)" = 
      "R204(b) Provincial/territorial-international exemption codes"
  )) -> employer_data_temp

# lmia regulation subsection 
employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection)) %>%
  tally() -> n_sub

# figure
employer_data_temp %>%    
  ggplot(aes(x = fct_rev(fct_infreq(LMIA_Regulation_Subsection)))) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  coord_flip() +
  geom_text(stat = 'count', aes(label = ..count..), 
            hjust = -0.06) +
  labs(title = "LMIA Regulation Sub-Section Breakdown", 
       subtitle = str_c("All years (",n_sub$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")


employer_data_comp %>%
  filter(!is.na(LMIA_Regulation_Subsection)) %>%
  group_by(fct_infreq(LMIA_Regulation_Subsection)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('LMIA_Regulation_Subsection (all years)' = 'fct_infreq(LMIA_Regulation_Subsection)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 9))
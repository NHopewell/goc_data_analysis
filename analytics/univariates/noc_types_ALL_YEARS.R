#### NOC_Type All years 

# noc type
employer_data_comp %>%
  filter(!is.na(NOC_Type)) %>%
  tally() -> n_noc

# figure
employer_data_comp %>%
  filter(!is.na(NOC_Type)) %>%
  ggplot(aes(x = fct_infreq(NOC_Type))) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat = 'count', aes(label=..count..), 
            vjust = -0.4) +
  labs(title = "NOC Type Breakdown", 
       subtitle = str_c("All years (",n_noc$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none")

employer_data_comp %>% 
  filter(!is.na(NOC_Type)) %>%
  group_by(fct_infreq(NOC_Type)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('NOC_Type (all years)' = 'fct_infreq(NOC_Type)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 5))
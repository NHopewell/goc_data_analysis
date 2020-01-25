employer_data_comp %>%
  filter(!is.na(NOC_Major_Group)) %>%
  group_by(fct_infreq(NOC_Major_Group)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('NOC_Major_Group (all years)' = 'fct_infreq(NOC_Major_Group)',
         count = n) %>%
  filter(count >= 100) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 19))
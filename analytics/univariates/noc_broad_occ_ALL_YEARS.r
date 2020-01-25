#### NOC_Broad Occupation All years 

# noc broad occ
employer_data_comp %>%
  filter(!is.na(NOC_Broad_Occupation)) %>%
  mutate(NOC_Broad_Occupation = fct_recode(
    NOC_Broad_Occupation,
    "Natural and applied sciences\nand related occupations" =
      "Natural and applied sciences and related occupations",
    "Natural resources, agriculture\nand related production occupations" =
      "Natural resources, agriculture and related production occupations",
    "Occupations in art, culture,\nrecreation and sport" =
      "Occupations in art, culture, recreation and sport",
    "Occupations in education, law and social,\ncommunity and government services" =
      "Occupations in education, law and social, community and government services",
    "Occupations in manufacturing\nand utilities" = 
      "Occupations in manufacturing and utilities",
    "Trades, transport and equipment\noperators and related occupations" =
      "Trades, transport and equipment operators and related occupations")
  ) -> employer_data_temp

employer_data_temp %>%
  tally() -> n_nb

# figure    
employer_data_temp %>%   
  ggplot(aes(x = fct_rev(fct_infreq(NOC_Broad_Occupation)))) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  coord_flip() +
  geom_text(stat = 'count', aes(label = ..count..), 
            hjust = -0.05) +
  labs(title = "NOC Broad Occupation Breakdown", 
       subtitle = str_c("All years (",n_nb$n," observations)\n"),
       x = "",
       y = "") + 
  theme_minimal() +
  theme(legend.position="none")
}
employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation)) %>%
  group_by(fct_infreq(NOC_Broad_Occupation)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('NOC_Broad_Occupation (all years)' = 'fct_infreq(NOC_Broad_Occupation)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 9))
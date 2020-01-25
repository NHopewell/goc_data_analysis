#### NOC Occupation, Descision config A, All Year

# to slice
broad_slice = c("Natural and applied sciences and related occupations",
                "Sales and service occupations",
                "Occupations in art, culture, recreation and sport",
                "Occupations in education, law and social, community and government services",
                "Trades, transport and equipment operators and related occupations",
                "Management occupations")

# slice and fix labels
broad_data <- employer_data_comp %>%
  filter(!is.na(NOC_Broad_Occupation), !is.na(target),
         NOC_Broad_Occupation %in% broad_slice) %>%
  mutate(NOC_Broad_Occupation = 
           fct_recode(NOC_Broad_Occupation,
                      "Natural and applied\nsciences and\nrelated occupations" =
                        "Natural and applied sciences and related occupations",
                      "Sales and service\noccupations" = "Sales and service occupations",
                      "Occupations in art,\nculture, recreation\nand sport" = 
                        "Occupations in art, culture, recreation and sport",
                      "Occupations in\neducation, law\nand social,\ncommunity and\ngovernment services" =
                        "Occupations in education, law and social, community and government services",
                      "Trades, transport\nand equipment\noperators and\nrelated occupations" =
                        "Trades, transport and equipment operators and related occupations",
                      "Management\noccupations" = "Management occupations"))

broad_data %>%
  tally() -> broad_n_a

# noc broad occ config a, all years
ggplot(broad_data, 
       aes(x = NOC_Broad_Occupation, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.4, 
            position = position_dodge(width = .9)) +
  labs(title = "Decision Breakdown by NOC_Broad_Occupation", 
       subtitle = str_c("All Years (",broad_n_a$n," observations)\n"),
       y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(legend.position = "bottom")


employer_data_comp %>% 
  filter(!is.na(target), !is.na(NOC_Broad_Occupation), 
         NOC_Broad_Occupation %in% broad_slice) -> nocs_b

nocb_summary1 <-
  list("Decision config a" =
         list("compliant" = ~ n_perc(.data$target == 'compliant'),
              "non-compliant" = ~ n_perc(.data$target == 'non-compliant')
         ))

( summary_table(dplyr::group_by(nocs_b, NOC_Broad_Occupation), nocb_summary1) )
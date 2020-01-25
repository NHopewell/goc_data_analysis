#### NOC_Broad Occupation By Region, All years 

employer_data_comp %>%
  mutate(NOC_Broad_Occupation = fct_recode(NOC_Broad_Occupation,
                                           "Trades, transport\nand equipment\noperators and\nrelated occupations" =
                                             "Trades, transport and equipment operators and related occupations",
                                           "Sales and service\noccupations" = "Sales and service occupations",
                                           "Occupations in\nmanufacturing\nand utilities" = 
                                             "Occupations in manufacturing and utilities",
                                           "Occupations in\neducation, law and\nsocial,community\nand government \nservices" =
                                             "Occupations in education, law and social, community and government services",
                                           "Occupations in\nart, culture,\nrecreation and\nsport" =
                                             "Occupations in art, culture, recreation and sport",
                                           "Natural resources,\nagriculture and\nrelated production\noccupations" =           
                                             "Natural resources, agriculture and related production occupations",
                                           "Natural and\napplied sciences\nand related\noccupations" =           
                                             "Natural and applied sciences and related occupations",
                                           "Management\noccupations" = "Management occupations",
                                           "Health\noccupations" = "Health occupations")) -> bd_temp 

# noc broad occ by region
employer_data_comp %>%
  filter(!is.na(NOC_Broad_Occupation), 
         !is.na(Region)) %>%
  tally() -> nocb_reg_tot

# figure
ggplot(bd_temp %>%
         filter(!is.na(NOC_Broad_Occupation), !is.na(Region)), 
       aes(x=NOC_Broad_Occupation, fill = Region)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), hjust=-0.1, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Broad_Occupation by Region",
       subtitle = "All years (8146 observations)\n",
       x = "",
       y = "") +
  coord_flip() +
  scale_fill_tableau()+
  theme_minimal() +
  theme(legend.position = "top") + 
  guides(fill = guide_legend(reverse=T))


employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation), !is.na(Region)) -> nocb_reg

nocb_reg_summary1 <-
  list("NOC_Broad_Occupation" =
         list("Trades, transport and equipment operators and related occupations"  = 
                ~ n_perc(.data$NOC_Broad_Occupation == "Trades, transport and equipment operators and related occupations"),
              "Sales and service occupations" = ~ n_perc(.data$NOC_Broad_Occupation == "Sales and service occupations"),
              "Occupations in manufacturing and utilities" = 
                ~ n_perc(.data$NOC_Broad_Occupation == "Occupations in manufacturing and utilities"),
              "Occupations in education, law and social, community and government services"  = 
                ~ n_perc(.data$NOC_Broad_Occupation == 
                           "Occupations in education, law and social, community and government services"),
              "Occupations in art, culture, recreation and sport" = 
                ~ n_perc(.data$NOC_Broad_Occupation == "Occupations in art, culture, recreation and sport"),
              "Natural resources, agriculture and related production occupations"  = 
                ~ n_perc(.data$NOC_Broad_Occupation == "Natural resources, agriculture and related production occupations"),
              "Natural and applied sciences and related occupations"    = 
                ~ n_perc(.data$NOC_Broad_Occupation == "Natural and applied sciences and related occupations" ),
              "Management occupations" = ~ n_perc(.data$NOC_Broad_Occupation == "Management occupations" ),
              "Health occupations" = ~ n_perc(.data$NOC_Broad_Occupation == "Health occupations" )
         ))

( summary_table(dplyr::group_by(nocb_reg, fct_rev(Region)), nocb_reg_summary1) )
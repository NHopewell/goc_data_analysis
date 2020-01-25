##### NOC broad occ By Gender of Foriegn Worker, All years 


employer_data_comp %>%
  filter(!is.na(NOC_Broad_Occupation), 
         !is.na(Gender_of_FW)) %>%
  tally() -> nocb_gen_tot

# figure
ggplot(bd_temp %>%
         filter(!is.na(NOC_Broad_Occupation), !is.na(Gender_of_FW)), 
       aes(x=NOC_Broad_Occupation, fill = Gender_of_FW)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label=..count..), hjust=-0.05, position = position_dodge(width = .9)) +
  labs(title = "Breakdown of NOC_Broad_Occupation by Gender of Foreign Worker",
       subtitle = str_c("All years (",nocb_gen_tot$n," observations)\n"),
       x = "",
       y = "") +
  coord_flip() +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "top") + 
  guides(fill = guide_legend(reverse=T))

employer_data_comp %>% 
  filter(!is.na(NOC_Broad_Occupation), !is.na(Gender_of_FW)) -> nocb_gen

nocb_gen_summary1 <-
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

( summary_table(dplyr::group_by(nocb_gen, fct_rev(Gender_of_FW)), nocb_gen_summary1) )
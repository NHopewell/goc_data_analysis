### NAICS, config A, all years


names <- employer_data_comp %>%
  filter(!is.na(NAICS)) %>%
  dplyr::group_by(NAICS) %>%
  tally() %>%
  filter(n >= 206) %>%
  .$NAICS

naics_names <- c("Accommodation and food services", 
                 "Agriculture, Forestry, Fishing and Hunting",
                 "Arts, entertainment and recreation",
                 "Manufacturing", 
                 "Other services (except public administration)", 
                 "Professional, scientific and technical services")


naics_sliced <- employer_data_comp %>%
  filter(!is.na(NAICS), !is.na(target)) %>%
  mutate(NAICS = fct_recode(
    NAICS, "Accommodation and\nfood services" = 
      "Accommodation and food services", 
    "Agriculture, Forestry,\nFishing and Hunting" = 
      "Agriculture, Forestry, Fishing and Hunting",
    "Arts, entertainment\nand recreation" = 
      "Arts, entertainment and recreation", 
    "Manufacturing" = "Manufacturing", 
    "Other services\n(except public\nadministration)" = 
      "Other services (except public administration)",
    "Professional, scientific\nand technical services" =  
      "Professional, scientific and technical services",
    NULL = "Construction",
    NULL = "Retail trade", 
    NULL = "Health care and social assistance",
    NULL = "Transportation and warehousing",
    NULL = "Educational services",
    NULL = "Management of companies and enterprises",
    NULL = "Mining",
    NULL = 
      "Administrative and support, waste management and remediation services",
    NULL = "Information and cultural industries",
    NULL = "Utilities",
    NULL = "Wholesale trade",
    NULL = "Public administration",
    NULL = "Real estate and rental and leasing"))

# naics config a, all years
naics_sliced %>%
  filter(!is.na(NAICS), !is.na(target)) %>%
  tally() -> naics_n_a

# figure
naics_sliced %>%
  filter(!is.na(NAICS), !is.na(target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(NAICS, target), 
                  fill = target), na.rm=TRUE, color = "grey40") +
  labs(title="Decision Breakdown by NAICS", 
       subtitle= str_c("All years (",naics_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "right") -> p1

# figure
ggplot(naics_sliced %>%
         filter(!is.na(NAICS), !is.na(target)), 
       aes(x=NAICS, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.2, position = position_dodge(width = .9)) +
  labs(y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "bottom") -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)


employer_data_comp %>% 
  filter(!is.na(target), !is.na(NAICS),
         NAICS %in% naics_names) -> naics_sub

naics_summary1 <-
  list("Decision config a" =
         list("compliant" = ~ n_perc(.data$target == 'compliant'),
              "non-compliant" = ~ n_perc(.data$target == 'non-compliant')
         ))

( summary_table(dplyr::group_by(naics_sub, NAICS), naics_summary1) )

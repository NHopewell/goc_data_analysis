#### naics all years

employer_data_comp %>%
  filter(!is.na(NAICS)) %>%
  mutate(NAICS =  fct_recode(
    NAICS,"Administrative and support, waste management\nand remediation services" =
      "Administrative and support, waste management and remediation services")
  ) -> NAICS

NAICS %>%
  group_by(NAICS) %>%
  tally() %>%
  dplyr::arrange(desc(n)) -> NAICS_a

# figure
NAICS_a %>%
  ggplot(aes(x = reorder(NAICS, n), y = n)) +
  geom_point(col = "#d63a3a", size = 4) +
  ylim(0, max(NAICS_a$n+100)) +
  geom_segment(aes(x = NAICS, 
                   xend = NAICS, 
                   y = n + 10, 
                   yend = NAICS_a$n+100), 
               linetype = "dashed", 
               size = 0.1,
               color = "grey70") +
  geom_segment(aes(x = NAICS, 
                   xend = NAICS, 
                   y = 0, 
                   yend = n), 
               size = 1,
               color = "#d63a3a") + 
  labs(title = "NAICS Breakdown", 
       subtitle = str_c("All years (",sum(NAICS_a$n)," observations)\n"),
       x = "",
       y = "") + 
  coord_flip() +
  geom_text(aes(label= n), hjust=-0.6) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
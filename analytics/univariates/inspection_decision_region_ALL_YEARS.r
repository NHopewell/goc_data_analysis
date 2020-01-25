### IMP Univariate findings ###

### Inspection Decision & Region  

#### All years

# Decision
employer_data_comp %>%
  filter(!is.na(multi_target)) %>%
  tally() -> n_id

# figure
employer_data_comp %>%
  filter(!is.na(multi_target)) %>%
  ggplot(aes(x = multi_target)) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.8) +
  labs(title = "Inspection Decision Breakdown", 
       subtitle = str_c("All years (",n_id$n," observations)\n"),
       x = "\n Decision",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none") -> p1

# Region
employer_data_comp %>%
  filter(!is.na(Region)) %>%
  tally() -> n_r

# figure
employer_data_comp %>%
  filter(!is.na(Region)) %>%
  ggplot(aes(x = Region)) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  geom_text(stat='count', aes(label=..count..), 
            vjust = -0.8) +
  labs(title = "Region Breakdown", 
       subtitle = str_c("All years (",n_r$n," observations)\n"),
       x = "\nRegion",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none") -> p2


grid.arrange(p1, p2, 
             ncol=2)


# style options
custom_bootstrap <- c("striped", 
                      "hover")

employer_data_comp %>% 
  filter(!is.na(multi_target)) %>%
  group_by(multi_target) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Decision (all years)' = multi_target,
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left") %>%
  add_indent(seq(1, 4))



employer_data_comp %>%
  filter(!is.na(Region)) %>%
  group_by(Region) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Region (all years)' = 'Region',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left")%>%
  add_indent(seq(1, 5))
### Trigger & Gender of Foriegn Worker 

#### All years

# Trigger
employer_data_comp %>%
  filter(!is.na(Trigger)) %>%
  group_by(Trigger) %>%
  tally() -> n_trig

# figure
n_trig %>%
  ggplot(aes(x = 2, y = n, fill = Trigger)) +
  geom_col(position = "stack", width = 1, color = "grey40") +
  geom_text_repel(aes(label = n), 
                  position = position_stack(vjust = 0.75), size = 4) +
  labs(title = "Trigger",
       subtitle = str_c("All years (",sum(n_trig$n)," observations)\n")) + 
  xlim(0.5, 2.5) +
  coord_polar("y") +
  scale_fill_tableau() +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.49),
        plot.subtitle = element_text(hjust = 0.49)) -> p1

# Gender of fw
employer_data_comp %>%
  filter(!is.na(Gender_of_FW)) %>%
  group_by(Gender_of_FW) %>%
  tally() -> n_gen

# figure
n_gen %>%
  ggplot(aes(x = 2, y = n, fill = Gender_of_FW)) +
  geom_col(position = "stack", width = 1, color = "grey40") +
  geom_text(aes(label = n), 
            position = position_stack(vjust = 0.75), size = 4) +
  labs(title = "Gender of Foreign Worker",
       subtitle = str_c("All years (",sum(n_gen$n)," observations)\n"),
       fill = "") + 
  xlim(0.5, 2.5) +
  coord_polar("y") +
  scale_fill_tableau() +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.49),
        plot.subtitle = element_text(hjust = 0.49)) -> p2

grid.arrange(p1, p2, 
             ncol=2)


# Trigger
employer_data_comp %>%
  filter(!is.na(Trigger)) %>%
  group_by(fct_infreq(Trigger)) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Trigger (all years)' = 'fct_infreq(Trigger)',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left") %>%
  add_indent(seq(1, 3))

# Gender of FW
employer_data_comp %>%
  filter(!is.na(Gender_of_FW)) %>%
  group_by(Gender_of_FW) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Gender_of_FW (all years)' = 'Gender_of_FW',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left") %>%
  add_indent(seq(1, 2))
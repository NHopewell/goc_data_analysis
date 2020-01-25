# Inspection decision vs region by fiscal year

#### Faceted by fiscal year 

# 2016/2017 Decision
employer_data_comp %>%
  filter(!is.na(multi_target), 
         Fiscal_Year == '16/17') %>%
  tally() -> nid_one

# figure
employer_data_comp %>%
  filter(!is.na(multi_target), 
         Fiscal_Year == '16/17') %>%
  ggplot(aes(x = multi_target)) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.8) +
  labs(title="Inspection Decision Breakdown", 
       subtitle = str_c("2016/2017 (",nid_one$n," observations)\n"),
       x = "\n Decision",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none") -> p1

# 2016/2017 Region
employer_data_comp %>%
  filter(!is.na(Region), 
         Fiscal_Year == '16/17') %>%
  tally() -> nr_one

# figure
employer_data_comp %>%
  filter(!is.na(Region), 
         Fiscal_Year == '16/17') %>%
  ggplot(aes(x = Region)) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  geom_text(stat='count', aes(label = ..count..), 
            vjust = -0.8) +
  labs(title = "Region Breakdown", 
       subtitle=str_c("2016/2017 (",nr_one$n," observations)\n"),
       x = "\nRegion",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none") -> p2


grid.arrange(p1, p2, 
             ncol=2)



employer_data_comp %>% 
  filter(!is.na(multi_target), 
         Fiscal_Year == '16/17') %>%
  group_by(multi_target) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Decision (2016/2017)' = multi_target,
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left") %>%
  add_indent(seq(1, 4))



employer_data_comp %>%
  filter(!is.na(Region), 
         Fiscal_Year == '16/17') %>%
  group_by(Region) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Region (2016/2017)' = 'Region',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left")%>%
  add_indent(seq(1, 5))


# 2017/2018 Decision
employer_data_comp %>%
  filter(!is.na(multi_target), 
         Fiscal_Year ==
           '17/18') %>%
  tally() -> nid_two

# figure
employer_data_comp %>%
  filter(!is.na(multi_target), 
         Fiscal_Year == '17/18') %>%
  ggplot(aes(x = multi_target)) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.8) +
  labs(title = "Inspection Decision Breakdown", 
       subtitle = str_c("2017/2018 (",nid_two$n," observations)\n"),
       x = "\n Decision",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none") -> p1

# 2017/2018 Region
employer_data_comp %>%
  filter(!is.na(Region), Fiscal_Year == '17/18') %>%
  tally() -> nr_two

# figure
employer_data_comp %>%
  filter(!is.na(Region), 
         Fiscal_Year == '17/18') %>%
  ggplot(aes(x = Region)) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.8) +
  labs(title="Region Breakdown", 
       subtitle = str_c("2017/2018 (", nr_two$n," observations)\n"),
       x = "\nRegion",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none") -> p2


grid.arrange(p1, p2, 
             ncol=2)



employer_data_comp %>% 
  filter(!is.na(multi_target), 
         Fiscal_Year == '17/18') %>%
  group_by(multi_target) %>%
  tally()  %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Decision (2017/2018)' = multi_target,
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left") %>%
  add_indent(seq(1, 4))


employer_data_comp %>%
  filter(!is.na(Region), 
         Fiscal_Year == '17/18') %>%
  group_by(Region) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Region (2017/2018)' = 'Region',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left")%>%
  add_indent(seq(1, 5))



# 2018/2019 Decision
employer_data_comp %>%
  filter(!is.na(multi_target), 
         Fiscal_Year == '18/19') %>%
  tally() -> nid_three

# figure
employer_data_comp %>%
  filter(!is.na(multi_target), 
         Fiscal_Year == '18/19') %>%
  ggplot(aes(x = multi_target)) +
  geom_bar(color = "grey40", fill = "#ff7f0e") +
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.8) +
  labs(title = "Inspection Decision Breakdown", 
       subtitle = str_c("2018/2019 (",nid_three$n," observations)\n"),
       x = "\n Decision",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none") -> p1

# 2018/2019 Region
employer_data_comp %>%
  filter(!is.na(Region), 
         Fiscal_Year == '18/19') %>%
  tally() -> nr_three

# figure
p2 <- employer_data_comp %>%
  filter(!is.na(Region), 
         Fiscal_Year == '18/19') %>%
  ggplot(aes(x = Region)) +
  geom_bar(color = "grey40", fill = "#1f83b4") +
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust = -0.8) +
  labs(title = "Region Breakdown", 
       subtitle = str_c("2018/2019 (",nr_three$n," observations)\n"),
       x = "\nRegion",
       y = "") + 
  theme_minimal() +
  theme(legend.position = "none") -> p2


grid.arrange(p1, p2, 
             ncol=2)


employer_data_comp %>% 
  filter(!is.na(multi_target), 
         Fiscal_Year == '18/19') %>%
  group_by(multi_target) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Decision (2018/2019)' = multi_target,
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left") %>%
  add_indent(seq(1, 4))

employer_data_comp %>%
  filter(!is.na(Region), 
         Fiscal_Year == '18/19') %>%
  group_by(Region) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Region (2018/2019)' = 'Region',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left")%>%
  add_indent(seq(1, 5))
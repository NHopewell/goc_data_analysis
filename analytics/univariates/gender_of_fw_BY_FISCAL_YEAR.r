#### Gender of foreign worker Faceted by fiscal year


# 2016/2017 gender of fw
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), 
         Fiscal_Year =='16/17') %>%
  group_by(Gender_of_FW) %>%
  tally() -> n_gen_one

# figure
n_gen_one %>%
  ggplot(aes(x = 2, y = n, fill = Gender_of_FW)) +
  geom_col(position = "stack", width = 1, color = "grey40") +
  geom_text(aes(label = n), 
            position = position_stack(vjust = 0.75), size = 4) +
  labs(title = "Gender of Foreign Worker",
       subtitle = str_c("2016/2017 (", sum(n_gen_one$n)," observations)"),
       fill = "") + 
  xlim(0.5, 2.5) +
  coord_polar("y") +
  scale_fill_tableau() +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.49),
        plot.subtitle = element_text(hjust = 0.49)) -> p1

#2017/2018 gender of fw
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), 
         Fiscal_Year =='17/18') %>%
  group_by(Gender_of_FW) %>%
  tally() -> n_gen_two

# figure  
n_gen_two %>%
  ggplot(aes(x = 2, y = n, fill = Gender_of_FW)) +
  geom_col(position = "stack", width = 1, color = "grey40") +
  geom_text(aes(label = n), 
            position = position_stack(vjust = 0.75), size = 4) +
  labs(title = "Gender of Foreign Worker",
       subtitle = str_c("2017/2018 (",sum(n_gen_two$n)," observations)"),
       fill = "") + 
  xlim(0.5, 2.5) +
  coord_polar("y") +
  scale_fill_tableau() +
  theme_void() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.49),
        plot.subtitle = element_text(hjust = 0.49)) -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 2)


# Gender of FW
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), 
         Fiscal_Year == '16/17') %>%
  group_by(Gender_of_FW) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Gender_of_FW (2016/2017)' = 'Gender_of_FW',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left") %>%
  add_indent(seq(1, 2))


# Gender of FW
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), 
         Fiscal_Year == '17/18') %>%
  group_by(Gender_of_FW) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Gender_of_FW (2017/2018)' = 'Gender_of_FW',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = T, position = "left") %>%
  add_indent(seq(1, 2))

#2018/2019 gender of fw
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), 
         Fiscal_Year =='18/19') %>%
  group_by(Gender_of_FW) %>%
  tally() -> n_gen_three

# figure
n_gen_three %>%
  ggplot(aes(x = 2, y = n, fill = Gender_of_FW)) +
  geom_col(position = "stack", width = 1, color = "grey40") +
  geom_text(aes(label = n), 
            position = position_stack(vjust = 0.75), size = 4) +
  labs(title = "Gender of Foreign Worker",
       subtitle = str_c("2018/2019 (",sum(n_gen_three$n)," observations)"),
       fill = "") + 
  xlim(0.5, 2.5) +
  coord_polar("y") +
  scale_fill_tableau() +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.49),
        plot.subtitle = element_text(hjust = 0.49))

# Gender of FW
employer_data_comp %>%
  filter(!is.na(Gender_of_FW), 
         Fiscal_Year == '18/19') %>%
  group_by(Gender_of_FW) %>%
  tally() %>%
  mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
  rename('Gender_of_FW (2018/2019)' = 'Gender_of_FW',
         count = n) %>%
  kable() %>%
  kable_styling(
    bootstrap_options = 
      custom_bootstrap, 
    full_width = F, position = "center") %>%
  add_indent(seq(1, 2))
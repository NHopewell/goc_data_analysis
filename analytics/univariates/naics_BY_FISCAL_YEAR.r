#### naics by fiscal year


# 2016/2017 naics
NAICS %>%
  filter(Fiscal_Year == '16/17') %>%
  group_by(NAICS) %>%
  tally() %>%
  dplyr::arrange(desc(n)) -> NAICS_one

# figure
NAICS_one %>%
  ggplot(aes(x = reorder(NAICS, n), y = n)) +
  geom_point(col = "#d63a3a", size=4) +
  ylim(0, max(NAICS_one$n+10)) +
  geom_segment(aes(x = NAICS, 
                   xend = NAICS, 
                   y = n + 10, 
                   yend = NAICS_one$n+10), 
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
       subtitle = str_c("2016/2017 (",sum(NAICS_one$n)," observations)\n"),
       x = "",
       y = "") + 
  coord_flip() +
  geom_text(aes(label = n), hjust = -1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


# 2017/2018 naics
NAICS %>%
  filter(Fiscal_Year == '17/18') %>%
  group_by(NAICS) %>%
  tally() %>%
  dplyr::arrange(desc(n)) -> NAICS_two

# figure
NAICS_two %>%
  ggplot(aes(x = reorder(NAICS, n), y = n)) +
  geom_point(col = "#d63a3a", size = 4) + 
  ylim(0, max(NAICS_two$n+50)) +
  geom_segment(aes(x = NAICS, 
                   xend = NAICS, 
                   y = n + 10, 
                   yend = NAICS_two$n+50), 
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
       subtitle = str_c("2017/2018 (",sum(NAICS_two$n)," observations)\n"),
       x = "",
       y = "") + 
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.6) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


# 2018/2019 naics
NAICS %>%
  filter(Fiscal_Year == '18/19') %>%
  group_by(NAICS) %>%
  tally() %>%
  dplyr::arrange(desc(n)) -> NAICS_three

# figure
NAICS_three %>%
  ggplot(aes(x = reorder(NAICS, n), y = n)) +
  geom_point(col = "#d63a3a", size = 4) +
  ylim(0, max(NAICS_three$n+60)) +
  geom_segment(aes(x = NAICS, 
                   xend = NAICS, 
                   y = n + 10, 
                   yend = NAICS_three$n+60), 
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
       subtitle = str_c("2018/2019 (",sum(NAICS_three$n)," observations)\n"),
       x = "",
       y = "") + 
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.6) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
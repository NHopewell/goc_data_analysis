## Different Ways to label

p1 <- employer_data_comp %>%
  filter(!is.na(target)) %>%
  group_by(target) %>%
  tally() %>% 
  ggplot(aes(x = 2, y = n, fill = target)) +
  geom_col(position = "stack", width = 1, color = "grey40") +
  geom_text_repel(aes(label = n), 
                  position = position_stack(vjust = 0.75), size = 4) +
  xlim(0.5, 2.5) +
  coord_polar("y") +
  scale_fill_tableau() +
  labs(title = "Decision Configuration A\n",
       fill = "") +
  theme_void() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.49))

p2 <- employer_data_comp %>%
  filter(!is.na(multi_target)) %>%
  group_by(multi_target) %>%
  tally() %>% 
  ggplot(aes(x = 2, y = n, fill = multi_target)) +
  geom_col(position = "stack", width = 1, color = "grey40") +
  geom_text_repel(aes(label = n), 
                  position = position_stack(vjust = 0.75), size = 4) +
  xlim(0.5, 2.5) +
  coord_polar("y") +
  scale_fill_tableau() +
  labs(title = "Decision Configuration B\n",
       fill = "") +
  theme_void() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.49))


grid.arrange(p1, p2, 
             ncol=2)


p3 <- employer_data_comp %>%
  filter(!is.na(target_three)) %>%
  group_by(target_three) %>%
  tally() %>% 
  ggplot(aes(x = 2, y = n, fill = target_three)) +
  geom_col(position = "stack", width = 1, color = "grey40") +
  geom_text_repel(aes(label = n), 
                  position = position_stack(vjust = 0.75), size = 4) +
  xlim(0.5, 2.5) +
  coord_polar("y") +
  scale_fill_tableau() +
  labs(title = "Decision Configuration C\n",
       fill = "") +
  theme_void() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.49))

p4 <- employer_data_comp %>%
  filter(!is.na(target_down)) %>%
  group_by(target_down) %>%
  tally() %>% 
  ggplot(aes(x = 2, y = n, fill = target_down)) +
  geom_col(position = "stack", width = 1, color = "grey40") +
  geom_text(aes(label = n), 
            position = position_stack(vjust = 0.75), size = 4) +
  xlim(0.5, 2.5) +
  coord_polar("y") +
  scale_fill_tableau() +
  labs(title = "Decision Configuration D\n",
       fill = "") +
  theme_void() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.49))

grid.arrange(p3, p4, 
             ncol=2)
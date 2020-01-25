#### NOC Type,  Descision config A, All years


employer_data_comp %>%
  mutate(NOC_Type = fct_recode(NOC_Type,  
                               "Technical jobs\n& skilled trade" =
                                 "Technical jobs & skilled trade"
  )) -> noc_data_temp 

# noc config a, all years
noc_data_temp %>%
  filter(!is.na(NOC_Type), !is.na(target)) %>%
  tally() -> noc_n_a

# figure
noc_data_temp %>%
  filter(!is.na(NOC_Type), !is.na(target)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(NOC_Type, target), 
                  fill = target), na.rm = TRUE, color = "grey40") +
  labs(title = "Decision Breakdown by NOC_Type", 
       subtitle=str_c("All years (",noc_n_a$n," observations)\n"),
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
ggplot(noc_data_temp %>%
         filter(!is.na(NOC_Type), !is.na(target)), 
       aes(x = NOC_Type, fill = target)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label=..count..), vjust=-0.4, 
            position = position_dodge(width = .9)) +
  labs(y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(legend.position="bottom") -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)


employer_data_comp %>% 
  filter(!is.na(target), !is.na(NOC_Type)) -> nocs

noc_summary1 <-
  list("Decision config a" =
         list("compliant" = ~ n_perc(.data$target == 'compliant'),
              "non-compliant" = ~ n_perc(.data$target == 'non-compliant')
         ))

(  summary_table(dplyr::group_by(nocs, NOC_Type), noc_summary1) )
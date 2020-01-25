###### noc config d, all years

noc_data_temp %>%
  filter(!is.na(NOC_Type), !is.na(target_down)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(NOC_Type, target_down), 
                  fill = target_down), na.rm = TRUE, color = "grey40") +
  labs(title = "Decision Breakdown by NOC_Type", 
       subtitle = str_c("All years (",noc_n_a$n," observations)\n"),
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
         filter(!is.na(NOC_Type), !is.na(target_down)), 
       aes(x = NOC_Type, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat = 'count', aes(label = ..count..), 
            vjust=-0.4, position = position_dodge(width = .9)) +
  labs(y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(legend.position = "bottom") -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)

employer_data_comp %>% 
  filter(!is.na(target_down), !is.na(NOC_Type)) -> tdnoc

noc_summary4 <-
  list("Decision config d" =
         list("compliant" = ~ n_perc(.data$target_down == "compliant"),
              "non-compliant" = ~ n_perc(.data$target_down == "non-compliant")
         ))

( summary_table(dplyr::group_by(tdnoc, NOC_Type), noc_summary4) )



#### Region Descision conf C, All years  

# region config c
employer_data_comp %>%
  filter(!is.na(Region), !is.na(target_three)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(Region, target_three),
                  fill=target_three), na.rm=TRUE, color = "grey40") +
  labs(title="Decision Breakdown by Province", 
       subtitle= str_c("All years (",reg_n_a$n," observations)\n"),
       x = "",
       y = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal()  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "right") -> p1

# figure
ggplot(employer_data_comp %>%
         filter(!is.na(Region), !is.na(target_three)), 
       aes(x=Region, fill = target_three)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label = ..count..), 
            vjust=-0.5, position = position_dodge(width = .9)) +
  labs(y = "",
       x = "") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") -> p2

gridExtra::grid.arrange(p1, p2, 

employer_data_comp %>% 
  filter(!is.na(target_three), !is.na(Region)) -> t3regs 

region_summary3 <-
  list("Decision config c" =
         list("compliant" = ~ n_perc(.data$target_three == "compliant"),
              "compliant w/ justification" = 
                ~ n_perc(.data$target_three == "compliant w/\njustification"),
              "non-compliant" = ~ n_perc(.data$target_three == "non-compliant")
         ))

( summary_table(dplyr::group_by(t3regs, Region), region_summary3) )
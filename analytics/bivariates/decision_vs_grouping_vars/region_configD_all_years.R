#### Region Descision conf D, All years  

# region config d
employer_data_comp %>%
  filter(!is.na(Region), !is.na(target_down)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(Region, target_down), 
                  fill = target_down), na.rm=TRUE, color = "grey40") +
  labs(title = "Decision Breakdown by Province", 
       subtitle= str_c("All years (",reg_n_a$n," observations)\n"),
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
ggplot(employer_data_comp %>%
         filter(!is.na(Region), !is.na(target_down)), 
       aes(x=Region, fill = target_down)) +
  geom_bar(position = position_dodge(), color = "grey40")+
  geom_text(stat='count', aes(label=..count..), 
            vjust=-0.5, position = position_dodge(width = .9)) +
  labs(y = "",
       x = "",
       fill = "Decision") +
  scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "right") -> p2

gridExtra::grid.arrange(p1, p2, 
                        ncol = 1)

employer_data_comp %>% 
  filter(!is.na(target_down), !is.na(Region)) -> tdregs

region_summary4 <-
  list("Decision config d" =
         list("compliant" = ~ n_perc(.data$target_down == "compliant"),
              "non-compliant" = ~ n_perc(.data$target_down == "non-compliant")
         ))


( summary_table(dplyr::group_by(tdregs, Region), region_summary4) )

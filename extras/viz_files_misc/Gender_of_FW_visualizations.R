x <- employer_data_comp %>%
             filter(!is.na(Gender_of_FW), !is.na(target)) %>%
            dplyr::group_by(Gender_of_FW) %>% 
            tally() 

sum(x$n)
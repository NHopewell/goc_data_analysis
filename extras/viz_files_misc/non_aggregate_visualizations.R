View(employer_data_comp)


### UNIVARIATE BAR PLOTS


    

# multi-decision
employer_data_comp %>%
    filter(!is.na(multi_target)) %>%
    group_by(multi_target) %>%
    tally() -> x

sum(x$n)

employer_data_comp %>%
    filter(!is.na(multi_target)) %>%
        ggplot(aes(x=multi_target)) +
            geom_bar(color = "grey40", fill = "#a1d99b") +
            coord_flip() +
            geom_text(stat='count', aes(label=..count..), vjust=-1) +
            labs(title="Inspection Decision Breakdown", 
                 subtitle="All years (7215 observations)\n",
                 caption= str_c("\nSource: Master Tracking Sheet\n",
                                sep = " "),
                 x = "\n Decision",
                 y = "") + 
            theme_minimal() +
            theme(legend.position="none")
        


# Region
employer_data_comp %>%
    filter(!is.na(Region)) %>%
    group_by(Region) %>%
    tally() -> x

sum(x$n)

employer_data_comp %>%
    filter(!is.na(Region)) %>%
        ggplot(aes(x = Region)) +
            geom_bar(color = "grey40", fill = "#a1d99b") +
            geom_text(stat='count', aes(label=..count..), vjust=-1) +
            labs(title="Region Breakdown", 
                 subtitle="All years (10566 observations)\n",
                 caption= str_c("\nSource: Master Tracking Sheet\n",
                                sep = " "),
                 x = "\nRegion",
                 y = "") + 
            theme_minimal() +
            theme(legend.position="none")
    


# Trigger
employer_data_comp %>%
    filter(!is.na(Trigger)) %>%
    group_by(fct_infreq(Trigger)) %>%
    tally() %>%
    kable() %>%
    kable_styling(
        bootstrap_options =c("striped",
                             "hover", 
                             "condensed"), 
        full_width = F)
employer_data_comp %>%
    filter(!is.na(Trigger)) %>%
    group_by(Trigger) %>%
    tally() %>% 
        ggplot(aes(x = 2, y = n, fill = Trigger)) +
            geom_col(position = "stack", width = 1, color = "grey40") +
            geom_text(aes(label = n), 
                      position = position_stack(vjust = 0.75), size = 3) +
            xlim(0.5, 2.5) +
            coord_polar("y") +
            theme_void() +
            theme(legend.position="top")



# Gender of FW
employer_data_comp %>%
    filter(!is.na(Gender_of_FW)) %>%
    group_by(Gender_of_FW) %>%
    tally() %>%
        kable() %>%
        kable_styling(
            bootstrap_options =c("striped",
                                 "hover", 
                                 "condensed"), 
            full_width = F)

employer_data_comp %>%
    filter(!is.na(Gender_of_FW)) %>%
    group_by(Gender_of_FW) %>%
    tally() %>% 
        ggplot(aes(x = 2, y = n, fill = Gender_of_FW)) +
            geom_col(position = "stack", width = 1, color = "grey40") +
            geom_text(aes(label = n), 
                      position = position_stack(vjust = 0.75), size = 3) +
            xlim(0.5, 2.5) +
            coord_polar("y") +
            theme_void() +
            theme(legend.position="top")



# Noc Type
employer_data_comp %>%
    filter(!is.na(NOC_Type)) %>%
    group_by(NOC_Type) %>%
    tally() -> x

sum(x$n)

employer_data_comp %>%
    filter(!is.na(NOC_Type)) %>%
        ggplot(aes(x = fct_infreq(NOC_Type))) +
            geom_bar(color = "grey40", fill = "#a1d99b") +
            geom_text(stat='count', aes(label=..count..), vjust=-1) +
            labs(title="NOC Type Breakdown", 
                 subtitle="All years (9325 observations)\n",
                 caption= str_c("\nSource: Master Tracking Sheet\n",
                                sep = " "),
                 x = "",
                 y = "") + 
            theme_minimal() +
            theme(legend.position="none")



# Noc Broad Occupation
employer_data_comp %>%
    filter(!is.na(NOC_Broad_Occupation)) %>%
    mutate(NOC_Broad_Occupation = fct_recode(
        NOC_Broad_Occupation,
        "Natural and applied sciences\nand related occupations" =
            "Natural and applied sciences and related occupations",
        "Natural resources, agriculture\nand related production occupations" =
            "Natural resources, agriculture and related production occupations",
        "Occupations in art, culture,\nrecreation and sport" =
            "Occupations in art, culture, recreation and sport",
        "Occupations in education, law and social,\ncommunity and government services" =
            "Occupations in education, law and social, community and government services",
        "Occupations in manufacturing\nand utilities" = 
            "Occupations in manufacturing and utilities",
        "Trades, transport and equipment\noperators and related occupations" =
            "Trades, transport and equipment operators and related occupations")
    ) %>%
    group_by(NOC_Broad_Occupation) %>%
    tally() -> x

sum(x$n)

employer_data_comp %>%
    filter(!is.na(NOC_Broad_Occupation)) %>%
    mutate(NOC_Broad_Occupation = fct_recode(
        NOC_Broad_Occupation,
            "Natural and applied sciences\nand related occupations" =
                "Natural and applied sciences and related occupations",
            "Natural resources, agriculture\nand related production occupations" =
                "Natural resources, agriculture and related production occupations",
            "Occupations in art, culture,\nrecreation and sport" =
                "Occupations in art, culture, recreation and sport",
            "Occupations in education, law and social,\ncommunity and government services" =
                "Occupations in education, law and social, community and government services",
            "Occupations in manufacturing\nand utilities" = 
                "Occupations in manufacturing and utilities",
            "Trades, transport and equipment\noperators and related occupations" =
                "Trades, transport and equipment operators and related occupations")
        ) %>%
        ggplot(aes(x = fct_rev(fct_infreq(NOC_Broad_Occupation)))) +
            geom_bar(color = "grey40", fill = "didgerblue") +
            coord_flip() +
            geom_text(stat='count', aes(label=..count..), hjust=-0.1) +
            labs(title="NOC Broad Occupation Breakdown", 
                 subtitle="All years (9325 observations)\n",
                 caption= str_c("\nSource: Master Tracking Sheet\n",
                                sep = " "),
                 x = "",
                 y = "") + 
            theme_minimal() +
            theme(legend.position="none")


# NOC Major Group

employer_data_comp %>%
    filter(!is.na(NOC_Major_Group)) %>%
    group_by(fct_infreq(NOC_Major_Group)) %>%
    tally() %>%
    kable() %>%
    kable_styling(
        bootstrap_options =c("striped",
                             "hover", 
                             "condensed"), 
        full_width = F)

sum(x$n)







# NAICS
NAICS <- employer_data_comp %>%
    filter(!is.na(NAICS)) %>%
    mutate(NAICS =  fct_recode(
        NAICS,"Administrative and support, waste management\nand remediation services" =
            "Administrative and support, waste management and remediation services")
    ) %>%
    group_by(NAICS) %>%
    tally() %>%
    dplyr::arrange(desc(n)) 


sum(NAICS$n)

employer_data_comp %>%
    filter(!is.na(NAICS)) %>%
    group_by(NAICS) %>%
    tally() %>%
    dplyr::arrange(desc(n)) %>%
        kable() %>%
            kable_styling(
                bootstrap_options =c("striped",
                                     "hover", 
                                     "condensed"), 
                full_width = F)



employer_data_comp %>%
    filter(!is.na(NAICS)) %>%
        ggplot(aes(x = fct_rev(fct_infreq(NAICS)))) +
            geom_bar(color = "grey40", fill = "#a1d99b") +
            coord_flip() +
            geom_text(stat='count', aes(label=..count..), hjust=-1) +
            labs(title="NAICS Breakdown", 
                 subtitle="All years (4008 observations)\n",
                 #caption= str_c("\nSource: Master Tracking Sheet\n",
                 #               sep = " "),
                 x = "",
                 y = "") + 
            theme_minimal() +
            theme(legend.position="none")

NAICS %>%
    ggplot(aes(x = reorder(NAICS, n), y = n)) +
        geom_point(col="dodgerblue2", size=4) +
        geom_segment(aes(x=NAICS, 
                         xend=NAICS, 
                         y=n + 10, 
                         yend=900), 
                     linetype="dashed", 
                     size=0.1,
                     color = "grey70") +
        geom_segment(aes(x=NAICS, 
                         xend=NAICS, 
                         y=0, 
                         yend=n), 
                     size=1,
                     color = "dodgerblue1") + 
        labs(title="NAICS Breakdown", 
             subtitle="All years (4008 observations)\n",
             #caption= str_c("\nSource: Master Tracking Sheet\n",
             #               sep = " "),
             x = "",
             y = "") + 
        coord_flip() +
        geom_text(aes(label= n), hjust=-0.8) +
        theme_minimal() +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())







# LMIA exception code
employer_data_comp %>%
    filter(!is.na(LMIA_Exemption_Code)) %>%
    group_by(LMIA_Exemption_Code) %>%
    tally() -> x

sum(x$n)


# employer_data_comp %>%
#     filter(!is.na(LMIA_Exemption_Code)) %>%
#         ggplot(aes(x = fct_rev(fct_infreq(LMIA_Exemption_Code)))) +
#             geom_bar(color = "grey40", fill = "#a1d99b") +
#             coord_flip() +
#             geom_text(stat='count', aes(label=..count..), hjust=-1) +
#             labs(title="LMIA Exemption Code Breakdown", 
#                  subtitle="All years (10571 observations)\n",
#                  caption= str_c("\nSource: Master Tracking Sheet\n",
#                                 sep = " "),
#                  x = "\nLMIA Exemption Code Breakdown",
#                  y = "") + 
#             theme_minimal() +
#             theme(legend.position="none")


LMIA_c <- employer_data_comp %>%
            filter(!is.na(LMIA_Exemption_Code)) %>%
            group_by(LMIA_Exemption_Code) %>%
            tally()



LMIA_c %>%
    filter(!is.na(LMIA_Exemption_Code)) %>%
    mutate(LMIA_Exemption_Code = fct_recode(
        LMIA_Exemption_Code, 
        "Emergency repairs or repair personnel for\nout-of-warranty equipment" =
            "Emergency repairs or repair personnel for out-of-warranty equipment")
    ) %>%
        ggplot(aes(x = reorder(LMIA_Exemption_Code, n), y = n)) +
            geom_point(col="tomato2", size=4) +
            geom_segment(aes(x=LMIA_Exemption_Code, 
                             xend=LMIA_Exemption_Code, 
                             y=n + 10, 
                             yend=2200), 
                         linetype="dashed", 
                         size=0.1,
                         color = "grey70") +
            geom_segment(aes(x=LMIA_Exemption_Code,
                             xend=LMIA_Exemption_Code,
                             y=0, 
                             yend=n), 
                         size=1,
                         color = "tomato") + 
            labs(title="LMIA Exemption Code Breakdown", 
                 subtitle="All years (4008 observations)\n",
                 #caption= str_c("\nSource: Master Tracking Sheet\n",
                 #               sep = " "),
                 x = "",
                 y = "") + 
            coord_flip() +
            geom_text(aes(label= n), hjust=-0.9) +
            theme_minimal() +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank())









# LMIA Regulation Section
employer_data_comp %>%
    filter(!is.na(LMIA_Regulation_Section)) %>%
    group_by(LMIA_Regulation_Section) %>%
    tally() -> x

sum(x$n)

p1 <- employer_data_comp %>%
    filter(!is.na(LMIA_Regulation_Section)) %>%
    mutate(LMIA_Regulation_Section = fct_recode(
        LMIA_Regulation_Section, 
        "Permanent residence applicants\nin Canada" = 
            "Permanent residence applicants in Canada")
    ) %>%
        ggplot(aes(x = LMIA_Regulation_Section)) +
            geom_bar(color = "grey40", fill = "darkorchid1") +
            geom_text(stat='count', aes(label=..count..), vjust=-1) +
            labs(title="LMIA Regulation Section Breakdown", 
                 subtitle="All years (10571 observations)\n",
                 caption= str_c("\nSource: Master Tracking Sheet\n",
                                sep = " "),
                 x = "\nRegulation Section",
                 y = "") + 
            theme_minimal() +
            theme(legend.position="none")
            

# LMIA Regulation SubSection
employer_data_comp %>%
    filter(!is.na(LMIA_Regulation_Subsection)) %>%
    group_by(LMIA_Regulation_Subsection) %>%
    tally() -> x

sum(x$n)

employer_data_comp %>%
    filter(!is.na(LMIA_Regulation_Subsection)) %>%
    mutate(LMIA_Regulation_Subsection = fct_recode(
        LMIA_Regulation_Subsection, 
        "R205(a) Significant benefit" = "R205(a) Significant benefit exemption codes",
        "R205(b) Reciprocal employment" = "R205(b) Reciprocal employment exemption codes",
        "R204(a) Canada-international" = "R204(a) Canada-international exemption codes",
        "R204(c) Canada-provincial/territorial" = 
            "R204(c) Canada-provincial/territorial exemption codes",
        "R205(d) Charitable or religious work" = 
            "R205(d) Charitable or religious work exemption code",
        "R207: Permanent residence applicants\nin Canada" = 
            "R207: Permanent residence applicants in Canada",
        "R205(c)(ii) Competitiveness\nand public policy" = 
            "R205(c)(ii) Competitiveness and public policy exemption codes",
        "R205(c)(i) Research exemption" = "R205(c)(i) Research exemption codes",
        "R204(b) Provincial/territorial-international" = 
            "R204(b) Provincial/territorial-international exemption codes"
    )) %>%
        ggplot(aes(x = fct_rev(fct_infreq(LMIA_Regulation_Subsection)))) +
            geom_bar(color = "grey40", fill = "#a1d99b") +
            coord_flip() +
            geom_text(stat='count', aes(label=..count..), hjust=-0.1) +
            labs(title="LMIA Regulation Sub-Section Breakdown", 
                 subtitle="All years (10571 observations)\n",
                 x = "",
                 y = "") + 
            theme_minimal() +
            theme(legend.position="none")
        



















### MOSIACS


# target  by region
employer_data_comp %>%
    filter(!is.na(Region), !is.na(target)) %>%
        ggplot() +
            geom_mosaic(aes(x = product(Region, target), fill=target), na.rm=TRUE) +
            labs(title="Decision Breakdown by Province", 
                 subtitle="All years (4008 observations)\n",
                 #caption= str_c("\nSource: Master Tracking Sheet\n",
                 #               sep = " "),
                 x = "",
                 y = "") +
            scale_fill_tableau() +
            theme_minimal() +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.text.x=element_blank())

        # target  by region
        employer_data_comp %>%
            filter(!is.na(Region), !is.na(target)) %>%
                ggplot() +
                    geom_mosaic(aes(x = product(Region, target), fill=target), na.rm=TRUE) +
                    facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                                       "17/18" = " 2017/2018",
                                                                       "18/19" = "2018/2019"))) +
                    labs(title="Decision Breakdown by Province", 
                         subtitle="All years (4008 observations)\n",
                         caption= str_c("\nSource: Master Tracking Sheet\n",
                                        sep = " "),
                         x = "",
                         y = "") +
                    scale_fill_tableau() +
                    theme_minimal() +
                    theme(panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          axis.text.x=element_blank())



# multi_target  by region
employer_data_comp %>%
    filter(!is.na(Region), !is.na(multi_target)) %>%
        ggplot() +
            geom_mosaic(aes(x = product(Region, multi_target), fill=multi_target), na.rm=TRUE) +
            labs(title="Decision Breakdown by Province", 
                 subtitle="All years (4008 observations)\n",
                # caption= str_c("\nSource: Master Tracking Sheet\n",
                 #               sep = " "),
                 x = "",
                 y = "") +
            scale_fill_tableau() +
            theme_minimal() +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.text.x=element_blank())


        # facetted by year
        employer_data_comp %>%
            filter(!is.na(Region), !is.na(multi_target)) %>%
                ggplot() +
                    geom_mosaic(aes(x = product(Region, multi_target), fill=multi_target), na.rm=TRUE) +
                    facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                                       "17/18" = " 2017/2018",
                                                                       "18/19" = "2018/2019"))) +
                    labs(title="Decision Breakdown by Province", 
                         subtitle="All years (4008 observations)\n",
                         #caption= str_c("\nSource: Master Tracking Sheet\n",
                         #               sep = " "),
                         x = "",
                         y = "") +
                    scale_fill_tableau() +
                    theme_minimal() +
                    theme(panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          axis.text.x=element_blank())


        
# target_three  by region
employer_data_comp %>%
    filter(!is.na(Region), !is.na(target_three)) %>%
    ggplot() +
    geom_mosaic(aes(x = product(Region, target_three), fill=target_three), na.rm=TRUE) +
    labs(title="Decision Breakdown by Province", 
         subtitle="All years (4008 observations)\n",
         caption= str_c("\nSource: Master Tracking Sheet\n",
                        sep = " "),
         x = "",
         y = "") +
    scale_fill_tableau() +
    theme_minimal()  +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x=element_blank())

        # facetted by year
        employer_data_comp %>%
            filter(!is.na(Region), !is.na(target_three)) %>%
            ggplot() +
            geom_mosaic(aes(x = product(Region, target_three), fill=target_three), na.rm=TRUE) +
            facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                               "17/18" = " 2017/2018",
                                                               "18/19" = "2018/2019"))) +
            labs(title="Decision Breakdown by Province", 
                 subtitle="All years (4008 observations)\n",
                 caption= str_c("\nSource: Master Tracking Sheet\n",
                                sep = " "),
                 x = "",
                 y = "") +
            scale_fill_tableau() +
            theme_minimal()  +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.text.x=element_blank())
        

# target_down  by region
employer_data_comp %>%
    filter(!is.na(Region), !is.na(target_down)) %>%
        ggplot() +
            geom_mosaic(aes(x = product(Region, target_down), fill=target_down), na.rm=TRUE) +
            labs(title="Decision Breakdown by Province", 
                 subtitle="All years (4008 observations)\n",
                 caption= str_c("\nSource: Master Tracking Sheet\n",
                                sep = " "),
                 x = "",
                 y = "") +
            scale_fill_tableau() +
            theme_minimal() +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.text.x=element_blank())


        # facetted by year
        employer_data_comp %>%
            filter(!is.na(Region), !is.na(target_down)) %>%
                ggplot() +
                    geom_mosaic(aes(x = product(Region, target_down), fill=target_down), na.rm=TRUE) +
                    facet_grid(Fiscal_Year~., labeller = as_labeller(c("16/17" = "2016/2017",
                                                                     "17/18" = " 2017/2018",
                                                                     "18/19" = "2018/2019"))) +
                    labs(title="Decision Breakdown by Province", 
                         subtitle="All years (4008 observations)\n",
                         caption= str_c("\nSource: Master Tracking Sheet\n",
                                        sep = " "),
                         x = "",
                         y = "") +
                    scale_fill_tableau() +
                    theme_minimal() +
                    theme(panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          axis.text.x=element_blank())
    














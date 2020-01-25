####### Barplot grouped ########
## fill by target
employer_data = read_excel("F:/IMP-R proj/data/tidy_data/data_clean_good_5.xlsx")

employer_data <- employer_data %>% 
    dplyr::select(-'..1')

# remove nan strings artifact from pandas cleaning 
employer_data[] <- lapply(employer_data, gsub, pattern = 'nan', replacement = NA)
# fix spaces in cols
colnames(employer_data) <- str_replace_all(colnames(employer_data), " ", "_") 

# factors to apply
fac <- c("Fiscal_Year", "Inspection_Status", "Decision", "SDM_Decision",
         "Region", "Province", "Trigger", "On-Site_Inspection", "High_Profile",
         "Referral_made_to_CBSA/RCMP", "Compensation_Type", "Non_Compliance_Type",
         "Gender_of_FW", "LMIA_Exemption_Code", "LMIA_Regulation_Section",
         "LMIA_Regulation_Subsection", "NOC_Type", "NOC_Broad_Occupation",
         "NOC_Major_Group", "NAICS", "WP_Verification_Activity", "target", "multi_target")

employer_data[fac] <- lapply(employer_data[fac], factor)

# get table of % missing data
pct_missing <- employer_data %>%
    select(everything()) %>% 
    summarise_all(funs(mean(is.na(.)))) %>%
    gather()

# filter only data at least 30% complete
comp_cases <- pct_missing %>%
    filter(value < 0.7) %>%
    select(key) %>%
    flatten() %>%
    unlist()

# subset complete fields
employer_data_comp <- employer_data %>%
    select(comp_cases)

employer_data_comp %>%
    filter(!is.na(Region), !is.na(target)) %>%
    group_by(Region, target) %>%
    summarize(n = n()) %>%
    table()

regs <- employer_data_comp %>% filter(!is.na(target), !is.na(Region))
region_summary1 <-
    list("Decision (config a).)" =
             list("compliant" = ~ n_perc(dplyr::filter(regs, 
                                                             target == 'compliant'))  %>% flatten() %>% unlist(),
                  "non-compliant" = ~ n_perc(dplyr::filter(regs, 
                                                                 target == 'non-compliant'))  %>% flatten() %>% unlist()
             ))
# default is latex tables
orig_opt <- options()$qwraps2_markup
options(qwraps2_markup = "markdown")
tab <- summary_table(dplyr::group_by(regs, Region), region_summary1)
tab





ggplot(employer_data_comp %>%
           filter(!is.na(Region), !is.na(target)), 
       aes(x=Region, fill = target)) +
    geom_bar(position = position_dodge(), color = "grey40")+
    geom_text(stat='count', aes(label=..count..), 
              vjust=-1, position = position_dodge(width = .9)) +
    labs(y = "",
         x = "") +
    scale_fill_tableau() +
    theme_minimal() +
    theme(legend.position = "right")


# facetted by fiscal year
ggplot(employer_data_comp %>%
           filter(!is.na(Region), !is.na(target)), 
       aes(x=Region, fill = target)) +
    geom_bar(position = position_dodge(), color = "grey40")+
    geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = .9)) +
    facet_wrap(~Fiscal_Year, ncol=3, labeller = as_labeller(c("16/17" = "2016/2017",
                                                              "17/18" = " 2017/2018",
                                                              "18/19" = "2018/2019"))) + 
    scale_fill_tableau() +
    theme_few()



## fill by multi target
employer_data_comp %>%
    filter(!is.na(Region), !is.na(multi_target)) %>%
    group_by(Region, multi_target) %>%
    summarize(n = n())


ggplot(employer_data_comp %>%
           filter(!is.na(Region), !is.na(multi_target)), 
       aes(x=Region, fill = multi_target)) +
    geom_bar(position = position_dodge(), color = "grey40")+
    geom_text(stat='count', aes(label=..count..), 
              vjust=-1, position = position_dodge(width = .9)) +
    labs(y = "",
         x = "") +
    scale_fill_tableau() +
    theme_minimal() +
    theme(legend.position = "right")



# facetted by fiscal year
ggplot(employer_data_comp %>%
           filter(!is.na(Region), !is.na(multi_target)), 
       aes(x=Region, fill = multi_target)) +
    geom_bar(position = position_dodge(), color = "grey40")+
    geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = .9)) +
    facet_wrap(~Fiscal_Year, ncol=3, labeller = as_labeller(c("16/17" = "2016/2017",
                                                              "17/18" = "2017/2018",
                                                              "18/19" = "2018/2019"))) +
    scale_fill_tableau() +
    theme_few()




# fill by target_three
ggplot(employer_data_comp %>%
           filter(!is.na(Region), !is.na(target_three)), 
       aes(x=Region, fill = target_three)) +
    geom_bar(position = position_dodge(), color = "grey40")+
    geom_text(stat='count', aes(label=..count..), 
              vjust=-1, position = position_dodge(width = .9)) +
    labs(y = "",
         x = "") +
    scale_fill_tableau() +
    theme_minimal() +
    theme(legend.position = "right")




















### facetted, grouped, stacked bar plot (3 variables) -> multitarget
employer_data_comp %>%
    filter(!is.na(multi_target), !is.na(NOC_Type), !is.na(Region)) %>%
    group_by(multi_target, NOC_Type, Region) %>%
    summarize(n = n())

ggplot(employer_data_comp %>%
                   filter(!is.na(multi_target), !is.na(NOC_Type), !is.na(Region)))+ 
            geom_bar(mapping = aes(multi_target,fill = NOC_Type), position='fill',
                     color = "grey40") +
            labs(title="Decision Breakdown by NOC Type", 
                 subtitle="Facetted by region",
                 caption= str_c("\nSource: Master Tracking Sheet\n",
                                sep = " "),
                 x = "",
                 y = "") + 
            facet_wrap(~Region, ncol=1) + 
            coord_flip() + 
            scale_y_continuous(labels=percent) + 
            ylab('Percent') +
            scale_fill_economist()+
            #scale_fill_manual(values = (brewer.pal(5, "Greens"))) + 
            #scale_fill_tableau() +
            #theme_few() 
            theme_minimal()


## same but with target
employer_data_comp %>%
    filter(!is.na(target), !is.na(NOC_Type), !is.na(Region)) %>%
    group_by(target, NOC_Type, Region) %>%
    summarize(n = n())

ggplot(employer_data_comp %>%
                   filter(!is.na(target), !is.na(NOC_Type), !is.na(Region)), 
       mapping = aes(target,fill = NOC_Type)) + 
            geom_bar(position='fill', color = "grey40") +
            labs(title="Decision Breakdown by NOC Type", 
                 subtitle="Facetted by region",
                 caption= str_c("\nSource: Master Tracking Sheet\n",
                                sep = " "),
                 x = "",
                 y = "") + 
            scale_fill_economist()+
            #scale_fill_manual(values = (brewer.pal(5, "Greens"))) + 
            facet_wrap(~Region, ncol=1) + 
            coord_flip() + 
            scale_y_continuous(labels=percent) + 
            ylab('Percent') +
            theme_minimal()



# get table of % missing data
employer_data %>%
    select(multi_target, everything(), -target) %>% 
    summarise_all(funs(mean(is.na(.)))) %>%
    gather() %>%
    arrange(value) -> comp_table

# filter only data at least 30% complete
pct_missing %>%
    filter(value < 0.7) %>%
    select(key) %>%
    flatten() %>%
    unlist() -> comp_cases


# style options
custom_bootstrap <- c("striped", 
                      "hover")

employer_data_comp %>% 
    filter(!is.na(multi_target)) %>%
    group_by(multi_target) %>%
    tally() %>%
    mutate(freq = str_c("(",round(100 * n/sum(n), 2), "%)")) %>%
    rename('Decision (all years)' = multi_target,
           count = n) %>%
        kable() %>%
            kable_styling(
                bootstrap_options = 
                    custom_bootstrap, 
                full_width = T, position = "left") %>%
            add_indent(seq(1, 4))


# side-by-side
data_yr_one  %>%
    filter(!is.na(Region), !is.na(NOC_Type),
           target == 'non-compliant') %>%
    group_by(Region, NOC_Type) %>%
    summarize(n = n()) -> totals_by_region

ggplot(data_yr_one %>%
           filter(!is.na(Region), !is.na(NOC_Type),
                  target == 'non-compliant'), aes(x=NOC_Type, fill = Region)) + 
            geom_bar(position = position_dodge(), 
                     color = "grey40") +
            geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = .9)) +
            scale_fill_tableau() +
            labs(title="", 
                 subtitle="",
                 caption="Source: Master Tracking Sheet",
                 x = "\n ",
                 y = " \n") +
            theme_gdocs()  +
            theme(plot.title = element_text(hjust = 0),
                  plot.caption = element_text(face="italic"))




# only non-compliant from year 1!!!!!
totals_by_lmia <- data_yr_one %>%
                    filter(!is.na(Region), !is.na(LMIA_Regulation_Section), 
                           !is.na(target),
                           target == 'non-compliant') %>%
                    group_by(LMIA_Regulation_Section, Region) %>%
                    summarize(n = n())

ggplot(data_yr_one %>%
           filter(!is.na(target), !is.na(Region), !is.na(LMIA_Regulation_Section),
                  target == 'non-compliant'), aes(x = LMIA_Regulation_Section, fill = Region)) + 
    geom_bar( position = position_dodge(), 
             color = "grey40")+
    geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = .9)) +
    scale_fill_tableau() +
    labs(title="", 
         subtitle="",
         caption="Source: Master Tracking Sheet",
         x = "\n ",
         y = " \n") + 
    theme_gdocs()  +
    theme(plot.title = element_text(hjust = 0),
          plot.caption = element_text(face="italic")) 

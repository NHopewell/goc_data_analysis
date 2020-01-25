
############# all year plots #######################

####### Barplot grouped ########


x <- employer_data_comp %>%
        filter(!is.na(NOC_Type), !is.na(target)) %>%
        group_by(NOC_Type, target) %>%
        summarize(n = n())

sum(x$n)


p1 <- employer_data_comp %>%
    filter(!is.na(NOC_Type), !is.na(target)) %>%
    ggplot() +
    geom_mosaic(aes(x = product(NOC_Type, target), 
                    fill=target), na.rm=TRUE, color = "grey40") +
    labs(title="Decision Breakdown by NOC_Type", 
         subtitle="All years (7018 observations)\n",
         #caption= str_c("\nSource: Master Tracking Sheet\n",
         #               sep = " "),
         x = "",
         y = "") +
    scale_fill_tableau() +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x=element_blank(),
          legend.position="right")

p2 <- ggplot(employer_data_comp %>%
                 filter(!is.na(NOC_Type), !is.na(target)), aes(x=NOC_Type, fill = target)) +
    geom_bar(position = position_dodge(), color = "grey40")+
    geom_text(stat='count', aes(label=..count..), vjust=-0.5, 
              position = position_dodge(width = .9)) +
    labs(y = "",
         x = "") +
    scale_fill_tableau() +
    theme_minimal()


x <- employer_data_comp %>%
    filter(!is.na(NOC_Type), !is.na(multi_target)) %>%
    group_by(NOC_Type, multi_target) %>%
    summarize(n = n())

sum(x$n)



ggplot(employer_data_comp %>%
           filter(!is.na(NOC_Type), !is.na(Region)), aes(x=NOC_Type, fill = Region)) +
    geom_bar(position = position_dodge(), color = "grey40")+
    geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = .9)) +
    scale_fill_economist() +
    theme_minimal()


### stacked bar

# for labeling:
# totals <- employer_data_comp %>%
#             filter(!is.na(NOC_Type), !is.na(NOC_Broad_Occupation)) %>%
#             tally() 
# 
# 
# all_yrs_gbp <- ggplot(employer_data_comp %>%
#                                           filter(!is.na(NOC_Broad_Occupation), !is.na(NOC_Type))) + 
#                     geom_bar(mapping = aes(x = NOC_Type, fill = NOC_Broad_Occupation), 
#                              color = "grey40") +
#                     scale_fill_tableau("Red-Blue-Brown") +
#                     labs(title="National Occupational Classification (NOC) Breakdown", 
#                          subtitle="All years (6954 observations)",
#                          caption= str_c("\nNote: only includes records from all three", 
#                                         "fiscal years where data was present (not missing) in related fields.\nSource: Master Tracking Sheet\n",
#                                         sep = " "),
#                          x = "\n NOC_Type",
#                          y = "count\n") + 
#                     theme_gdocs() +
#                     theme(plot.title = element_text(hjust = 0),
#                           plot.caption = element_text(face="italic"))  +
#                     geom_text(aes(NOC_Type, n, label = n, fill = NULL), 
#                               data = totals, vjust = -0.5)


### facetted, grouped, stacked bar plot (3 variables) -> multitarget
ggplot(employer_data_comp %>%
                   filter(!is.na(multi_target), !is.na(NOC_Type)))+ 
            geom_bar(mapping = aes(multi_target,fill = NOC_Type), position='fill',
                     color = "grey40") +
            labs(title="Decision Breakdown by NOC Type", 
                 subtitle="All fiscal years",
                 caption= str_c("\nSource: Master Tracking Sheet\n",
                                sep = " "),
                 x = "",
                 y = "") + 
            scale_fill_economist()+
            #scale_fill_manual(values = (brewer.pal(5, "Greens"))) + 
            coord_flip() + 
            scale_y_continuous(labels=percent) + 
            ylab('Percent') +
            theme_minimal()

f_multi <- ggplot(employer_data_comp %>%
                                  filter(!is.na(multi_target), !is.na(NOC_Type), !is.na(Fiscal_Year)))+ 
                geom_bar(mapping = aes(multi_target,fill = NOC_Type), position='fill',
                         color = "grey40") +
                labs(title="Decision Breakdown by NOC Type", 
                     subtitle="Facetted by fiscal year",
                     caption= str_c("\nSource: Master Tracking Sheet\n",
                                    sep = " "),
                     x = "",
                     y = "") + 
                scale_fill_economist()+
                #scale_fill_manual(values = (brewer.pal(5, "Greens"))) + 
                facet_wrap(~Fiscal_Year, ncol=1, 
                           labeller = as_labeller(c("16/17" = "2016/2017",
                                                    "17/18" = "2017/2018",
                                                    "18/19" = "2018/2019"))) + 
                coord_flip() + 
                scale_y_continuous(labels=percent) + 
                ylab('Percent') +
                theme_minimal()

ggplotly(f_multi)


# same but with target
ggplot(employer_data_comp %>%
           filter(!is.na(target), !is.na(NOC_Type)))+ 
    geom_bar(mapping = aes(target,fill = NOC_Type), position='fill',
             color = "grey40") +
    labs(title="Decision Breakdown by NOC Type", 
         subtitle="All fiscal years",
         caption= str_c("\nSource: Master Tracking Sheet\n",
                        sep = " "),
         x = "",
         y = "") + 
    scale_fill_economist()+
    #scale_fill_manual(values = (brewer.pal(5, "Greens"))) + 
    coord_flip() + 
    scale_y_continuous(labels=percent) + 
    ylab('Percent') +
    theme_minimal()

# target facetted by fiscal year
ggplot(employer_data_comp %>%
           filter(!is.na(target), !is.na(NOC_Type), !is.na(Fiscal_Year)))+ 
    geom_bar(mapping = aes(target,fill = NOC_Type), position='fill', 
             color = "grey40")+
    labs(title="Decision Breakdown by NOC Type", 
         subtitle="Facetted by fiscal year",
         caption= str_c("\nSource: Master Tracking Sheet\n",
                        sep = " "),
         x = "",
         y = "") +
    scale_fill_economist()+
    #scale_fill_manual(values = (brewer.pal(5, "Greens"))) + 
    facet_wrap(~Fiscal_Year, ncol=1, 
               labeller = as_labeller(c("16/17" = "2016/2017",
                                        "17/18" = "2017/2018",
                                        "18/19" = "2018/2019"))) + 
    coord_flip() + 
    scale_y_continuous(labels=percent) + 
    ylab('Percent') +
    theme_minimal()


### side-by-side bar
ggplot(employer_data_comp %>%
           filter(!is.na(target), !is.na(NOC_Type))) + 
    geom_bar(mapping = aes(NOC_Type, fill = target), position = "dodge", 
             color = "grey40")



# noc type by noc broad occ
yr_one_gbp <- ggplot(data_yr_one %>%
                                         filter(!is.na(NOC_Broad_Occupation), !is.na(NOC_Type))) + 
                    geom_bar(mapping = aes(x = NOC_Type, fill = NOC_Broad_Occupation),
                             color = "grey40") +
                    scale_fill_tableau(palette = "Red-Blue-Brown") +
                    labs(title="National Occupational Classification (NOC) Breakdown", 
                         subtitle="2016/2017 Fiscal Year (1005 observations)",
                         caption= str_c("\nNote: only includes records from fiscal year", 
                                        "where data was present (not missing) in related fields.\nSource: Master Tracking Sheet\n",
                                        sep = " "),
                         x = "\n NOC_Type",
                         y = "count\n") + 
                    theme_gdocs() +
                    theme(plot.title = element_text(hjust = 0),
                          plot.caption = element_text(face="italic"))  +
                    geom_text(aes(NOC_Type, n, label = n, fill = NULL), 
                              data = totals_yr_one, vjust = -0.5)


### noc type by target

# year one stacked bar
totals_one_tar <- data_yr_one %>%
    filter(!is.na(NOC_Type), !is.na(target)) %>%
    group_by(NOC_Type) %>%
    summarize(n = n())

ggplot(data_yr_one %>%
           filter(!is.na(target), !is.na(NOC_Type))) + 
    geom_bar(mapping = aes(x = NOC_Type, fill = target), 
             color = "grey40") +
    scale_fill_tableau() +
    labs(title="National Occupational Classification (NOC) Breakdown", 
         subtitle="All years (10699 observations)",
         caption="Source: Master Tracking Sheet",
         x = "\n NOC_Type",
         y = "count\n") + 
    theme_gdocs() +
    theme(plot.title = element_text(hjust = 0),
          plot.caption = element_text(face="italic"))  +
    geom_text(aes(NOC_Type, n, label = n, fill = NULL), 
              data = totals_one_tar, vjust = -0.5)

# side-by-side bar
totals_one_grp <- data_yr_one %>%
                    filter(!is.na(NOC_Type), !is.na(target)) %>%
                    group_by(NOC_Type, target) %>%
                    summarize(n = n())

ggplot(data_yr_one %>%
           filter(!is.na(target), !is.na(NOC_Type)),
       aes(x = NOC_Type, fill = target)) + 
    geom_bar(position = position_dodge(), 
             color = "grey40") +
    scale_fill_tableau() +
    labs(title="National Occupational Classification (NOC) Breakdown", 
         subtitle="All years (10699 observations)",
         caption="Source: Master Tracking Sheet",
         x = "\n NOC_Type",
         y = "count\n") + 
    theme_gdocs()  +
    theme(plot.title = element_text(hjust = 0),
          plot.caption = element_text(face="italic"))  +
    geom_text(stat='count', aes(label=..count..), 
              vjust=-1, position = position_dodge(width = .9))





# only non-compiant 
ggplot(data_yr_one %>%
           filter(!is.na(target), !is.na(NOC_Type), target == 'non-compliant')) + 
    geom_bar(mapping = aes(x = NOC_Type, fill = target), 
             color = "grey40") +
    scale_fill_tableau() +
    labs(title="National Occupational Classification (NOC) Breakdown", 
         subtitle="All years (10699 observations)",
         caption="Source: Master Tracking Sheet",
         x = "\n NOC_Type",
         y = "count\n") + 
    theme_gdocs() +
    theme(plot.title = element_text(hjust = 0))

# only compiant 
ggplot(data_yr_one %>%
           filter(!is.na(target), !is.na(NOC_Type), target == 'compliant')) + 
    geom_bar(mapping = aes(x = NOC_Type, fill = target), 
             color = "grey40") +
    scale_fill_tableau() +
    labs(title="National Occupational Classification (NOC) Breakdown", 
         subtitle="All years (10699 observations)",
         caption="Source: Master Tracking Sheet",
         x = "\n NOC_Type",
         y = "count\n") + 
    theme_gdocs() +
    theme(plot.title = element_text(hjust = 0))

# year two grouped bar
yr_two_gbp <- ggplot(data_yr_two %>%
                                         filter(!is.na(NOC_Broad_Occupation), !is.na(NOC_Type))) + 
                    geom_bar(mapping = aes(x = NOC_Type, fill = NOC_Broad_Occupation), 
                             color = "grey40") +
                    scale_fill_tableau(palette = "Red-Blue-Brown") +
                    labs(title="National Occupational Classification (NOC) Breakdown", 
                         subtitle="2017/2018 Fiscal Year (3650 observations)",
                         caption= str_c("\nNote: only includes records from fiscal year", 
                                        "where data was present (not missing) in related fields.\nSource: Master Tracking Sheet\n",
                                        sep = " "),
                         x = "\n NOC_Type",
                         y = "count\n") + 
                    theme_gdocs() +
                    theme(plot.title = element_text(hjust = 0),
                          plot.caption = element_text(face="italic")) +
                    geom_text(aes(NOC_Type, n, label = n, fill = NULL), 
                          data = totals_yr_two, vjust = -0.5)

# year three grouped bar
yr_two_gbp <- ggplot(data_yr_three %>%
                         filter(!is.na(NOC_Broad_Occupation), !is.na(NOC_Type))) + 
    geom_bar(mapping = aes(x = NOC_Type, fill = NOC_Broad_Occupation), 
             color = "grey40") +
    scale_fill_tableau(palette = "Red-Blue-Brown") +
    labs(title="National Occupational Classification (NOC) Breakdown", 
         subtitle="2018/2019 Fiscal Year (2156 observations)",
         caption= str_c("\nNote: only includes records from fiscal year", 
                        "where data was present (not missing) in related fields.\nSource: Master Tracking Sheet\n",
                        sep = " "),
         x = "\n NOC_Type",
         y = "count\n") + 
    theme_gdocs() +
    theme(plot.title = element_text(hjust = 0),
          plot.caption = element_text(face="italic")) +
    geom_text(aes(NOC_Type, n, label = n, fill = NULL), 
              data = totals_yr_three, vjust = -0.5)



x <- employer_data_comp %>%
    filter(!is.na(NOC_Broad_Occupation), !is.na(target)) %>%
    group_by(NOC_Broad_Occupation, target) %>%
    summarize(n = n())

sum(x$n)



var <- employer_data_comp$LMIA_Regulation_Section
df <- expand.grid(y = 1:10, x = 1:10)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
categ_table[1] = 59
df$category <- factor(rep(names(categ_table), categ_table))  

## waffle Plot
p <- ggplot(df, aes(x = x, y = y, fill = category)) + 
    geom_tile(color = "black", size = 0.5) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
    scale_fill_few() +
    labs(title="Labour Market Impact Assessment (LMIA) Breakdown", subtitle="LMIA Regulation Section",
         caption="Source: Master Tracking Sheet") + 
    theme(plot.title = element_text(size = rel(1.2)),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.position = "right")



### facetted
# totals_lmia_yr <- employer_data_comp %>%
#                     filter(!is.na(target), !is.na(Fiscal_Year), !is.na(LMIA_Regulation_Section)) %>%
#                     group_by(Fiscal_Year, LMIA_Regulation_Section, target) %>%
#                     summarize(n = n())
# 
# 
# ggplot(employer_data_comp %>%
#            filter(!is.na(target), !is.na(LMIA_Regulation_Section), !is.na(Fiscal_Year)),
#         aes(x =Fiscal_Year, fill = LMIA_Regulation_Section)) + 
#     geom_bar(color = "grey40", position = position_dodge()) +
#     facet_wrap(~target, ncol=2) +
#     scale_fill_tableau(palette = "Red-Blue-Brown") +
#     geom_text(stat='count', aes(label=..count..), 
#               vjust=-1, position = position_dodge(width = .9)) +
#     theme_few()

# ggplot(employer_data_comp %>%
#            filter(!is.na(target), !is.na(LMIA_Regulation_Section)), 
#        mapping = aes(LMIA_Regulation_Section,fill = target)) + 
#     geom_bar(color = "grey40") +
#     labs(title="Decision Breakdown by NOC Type", 
#          subtitle="Facetted by region",
#          #caption= str_c("\nSource: Master Tracking Sheet\n",
#          #               sep = " "),
#          x = "",
#          y = "") + 
#     scale_fill_economist()+
#     #scale_fill_manual(values = (brewer.pal(5, "Greens"))) + 
#     #facet_wrap(~Region, ncol=1) + 
#     coord_flip() + 
#     scale_y_continuous(labels=percent) + 
#     ylab('Percent') +
#     theme_minimal()

nocs_b <- employer_data_comp %>% filter(!is.na(target), !is.na(NOC_Broad_Occupation))
nocb_summary1 <-
    list("NOC_Broad_Occupation (config a).)" =
             list("Health occupations"  = ~ n_perc(.data$NOC_Broad_Occupation == "Health occupations" ),
                  "Management occupations" = ~ n_perc(.data$NOC_Broad_Occupation == "Management occupations"),
                  "Natural and applied\nsciences and/n related occupations" = ~ n_perc(
                      .data$NOC_Broad_Occupation == "Natural and applied sciences and related occupations"),
                  "Natural resources,\nagriculture and related\nproduction occupations" = ~ n_perc(
                      .data$NOC_Broad_Occupation == "Natural resources, agriculture and related production occupations"),
                  "Occupations in art,\nculture, recreation & sport" = ~ n_perc(
                      .data$NOC_Broad_Occupation == "Occupations in art, culture, recreation and sport"),
                  "Occupations in education,\nlaw and social, community\nand government services" = ~ n_perc(
                      .data$NOC_Broad_Occupation == "Occupations in education, law and social, community and government services"),
                  "Occupations in manufacturing\nand utilities" = ~ n_perc(
                      .data$NOC_Broad_Occupation == "Occupations in manufacturing and utilities"),
                  "Sales and service\noccupations" = ~ n_perc(.data$NOC_Broad_Occupation == "Sales and service occupations"),
                  "Trades, transport and\nequipment operators\nand related occupations" = ~ n_perc(
                      .data$NOC_Broad_Occupation == "Trades, transport and equipment operators and related occupations")
             ))

# default is latex tables
orig_opt <- options()$qwraps2_markup
options(qwraps2_markup = "markdown")
tab <- summary_table(dplyr::group_by(nocs_b, target), nocb_summary1)
tab



x <- employer_data_comp %>%
    filter(!is.na(LMIA_Regulation_Section), !is.na(target)) %>%
    group_by(LMIA_Regulation_Section, target) %>%
    summarize(n = n())

sum(x$n)

p1 <- employer_data_comp %>%
    filter(!is.na(LMIA_Regulation_Section), !is.na(target)) %>%
    ggplot() +
    geom_mosaic(aes(x = product(LMIA_Regulation_Section, target), 
                    fill=target), na.rm=TRUE, color = "grey40") +
    labs(title="Decision Breakdown by LMIA_Regulation_Section", 
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
                 filter(!is.na(LMIA_Regulation_Section), !is.na(target)), 
             aes(x=LMIA_Regulation_Section, fill = target)) +
    geom_bar(position = position_dodge(), color = "grey40")+
    geom_text(stat='count', aes(label=..count..), vjust=-0.5, 
              position = position_dodge(width = .9)) +
    labs(y = "",
         x = "") +
    scale_fill_tableau() +
    theme_minimal()



# Stacked Percent


employer_data_comp %>%
    filter(!is.na(LMIA_Regulation_Subsection), !is.na(target)) %>%
    ggplot() +
    geom_mosaic(aes(x = product(LMIA_Regulation_Subsection, target), 
                    fill=target), na.rm=TRUE, color = "grey40") +
    labs(title="Decision Breakdown by LMIA_Regulation_Subsection", 
         subtitle="All years (7120 observations)\n",
         x = "",
         y = "") +
    scale_fill_tableau() +
    scale_y_productlist(labels=c("R204(a) Canada-international exemption codes" = "R204(a)", 
                              "R204(b) Provincial/territorial-international exemption codes" = "R204(b)",
                              "R204(c) Canada-provincial/territorial exemption codes" = "R204(c)",
                              "R205(a) Significant benefit exemption codes" = "R205(a)",
                              "R205(b) Reciprocal employment exemption codes" = "R205(b)",
                              "R205(c)(i) Research exemption codes" = "R205(c)(i)",
                              "R205(c)(ii) Competitiveness and public policy exemption codes"= "R205(c)(ii)",
                              "R205(d) Charitable or religious work exemption code" = "R205(d)",
                              "R207: Permanent residence applicants in Canada" = "R207")) + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x=element_blank(),
          legend.position="right")









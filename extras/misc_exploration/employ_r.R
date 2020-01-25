if(!require(pacman)) {
    install.packages("pacman")
    library(pacman)
}

pacman::p_load('tidyverse', 'readxl', 
               'ggthemes', 'plotly', 'ggmosaic',
               'scales', 'RColorBrewer',
               'knitr', 'kableExtra')

setwd('C:\\Users\\Nicholas.Hopewell\\Employer-Inspection-proj')

source("AAL_themes.R")
# source("finalise_plot.R")


employer_data = read_excel("./data/tidy_data/data_clean_good.xlsx")

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
         "NOC_Major_Group", "NAICS", "WP_Verification_Activity", "target", "multi_target",
         "target_three", "target_down")

employer_data[fac] <- lapply(employer_data[fac], factor)

# get table of % missing data
#pct_missing <- employer_data %>%
#                    dplyr::select(everything()) %>% 
#                    summarise_each(funs(mean(is.na(.)))) %>%
#                    gather()
    
pct_missing <- employer_data %>%
                    map(~ mean(is.na(.))) %>%
                    as_tibble() %>%
                    gather() %>%
                    dplyr::arrange(value)
            

# filter only data at least 30% complete
comp_cases <- pct_missing %>%
                    filter(value < 0.7) %>%
                    select(key) %>%
                    flatten() %>%
                    unlist()



# subset complete fields
employer_data_comp <- employer_data %>%
                         select(comp_cases)

write_csv(employer_data_comp, path = "~/comp_data.csv", na="")

glimpse(employer_data_comp)

# filter by fiscal year
data_yr_one <- employer_data %>% #yr 1
    filter(Fiscal_Year == '16/17')
totals_yr_one <- data_yr_one %>%  # filtered counts for fyear
    filter(!is.na(NOC_Type), !is.na(NOC_Broad_Occupation)) %>%
    group_by(NOC_Type) %>%
    summarize(n = n())

data_yr_two <- employer_data %>% # yr 2
    filter(Fiscal_Year == '17/18')
totals_yr_two <- data_yr_two %>%  # filtered counts for fyear
    filter(!is.na(NOC_Type), !is.na(NOC_Broad_Occupation)) %>%
    group_by(NOC_Type) %>%
    summarize(n = n())

data_yr_three <- employer_data %>% # yr 3
    filter(Fiscal_Year == '18/19')
totals_yr_three <- data_yr_three %>%  # filtered counts for fyear
    filter(!is.na(NOC_Type), !is.na(NOC_Broad_Occupation)) %>%
    group_by(NOC_Type) %>%
    summarize(n = n())


employer_data_comp %>%
    filter(!is.na(LMIA_Regulation_Subsection), !is.na(target)) %>%
    group_by(LMIA_Regulation_Subsection) %>%
    summarize(n = n())

# Tableau 10, Tableau 20, Color Blind, Red-Blue-Brown

############# year one plots #######################




### >>>> interesting cols

# 'Fiscal_Year'               -> 3 levels
# 'Gender_of_FW'              -> 2 levels
# 'Region'                    -> 5 levels
# 'Trigger'                   -> 3 levels (very unbalanced, filter by 'Reason to Suspect')
# 'LMIA_Exemption_Code'       -> 29 levels (maybe filter top 6 out?)
# 'LMIA_Regulation_Section'   -> 3 levels
# 'LMIA Regulation Subsection'-> 9 levels
# 'NOC_Type'                  -> 5 levels
# 'NOC_Broad_Occupation'      -> 10 levels
# 'NOC_Major_Group'           -> tons of levels
# 'NAICS'                     -> ~17 levels
# 'WP_Verification_Activity'  -> 4 levels (almost all fall into one level)
# 'target'                    -> 2 levels
# 'multi_target'              -> 4 levels 

####################################################

### fix labels for plotting anf tables
employer_data_comp <- employer_data_comp %>%
    mutate(multi_target = fct_recode(multi_target,
                                     "compliant" = "compliant",
                                     "compliant w/\njustification" = "compliant with justification", 
                                     "compliant w/\njustification and\ncompensation" = "compliant with justification and compensation",
                                     "non-compliant" = "non-compliant"
))








# tims appears most often 
employer_data %>% 
    filter(!is.na(Employer)) %>%
    count(Employer, sort = TRUE)

## only non-compliant
noncompliant_data <- employer_data %>%
    filter(target == 'non-compliant')

noncompliant_data %>% 
    count(Employer, sort = TRUE)

View(noncompliant_data)


# count of target by trigger counts
employer_data %>% 
    filter(!is.na(target), !is.na(Trigger)) %>% 
    group_by(target, Trigger) %>% 
    count() # notice how few are not random


# count of multi_target by trigger counts
employer_data %>% 
    filter(!is.na(multi_target), !is.na(Trigger)) %>% 
    group_by(multi_target, Trigger) %>% 
    count() # distribution chanes

employer_data %>% 
    filter(!is.na(Trigger)) %>% 
    group_by(multi_target, Trigger) %>% 
    count() # NOTICE 48 of the 87 'Reason to suspect' have NA for decision


employer_data %>% 
    filter(!is.na(Trigger), Trigger == 'Reason To Suspect') %>% 
    group_by(multi_target, Trigger) %>% 
    count() 



# look at reason to suspect files alone
suspect <- employer_data %>%
            filter(Trigger == 'Reason To Suspect')
suspect %>% 
    count(Employer, sort = TRUE)

View(suspect)

























































    
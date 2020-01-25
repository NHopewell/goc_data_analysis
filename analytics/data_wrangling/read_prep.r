##### SET UP AND INITIAL PROCESSING ####
if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

pacman::p_load('tidyverse', 'readxl', 
               'ggthemes', 'plotly',
               'scales', 'RColorBrewer',
               'knitr', 'kableExtra',
               'gridExtra', 'ggrepel',
               'ggmosaic', 'qwraps2')


employer_data = read_excel("F:/IMP-R proj/data/tidy_data/data_clean_good_4.xlsx")

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

# fix labels for plotting 
employer_data_comp <- employer_data_comp %>%
  mutate(multi_target = fct_recode(multi_target,
                                   "compliant" = "compliant",
                                   "compliant w/\njustification" = 
                                     "compliant with justification", 
                                   "compliant w/\njustification and\ncompensation" = 
                                     "compliant with justification and compensation",
                                   "non-compliant" = "non-compliant"
  ))


employer_data_comp <- employer_data_comp %>%
  mutate(target_three = fct_recode(target_three,
                                   "compliant" = "compliant",
                                   "compliant w/\njustification" = 
                                     "compliant with justification",
                                   "non-compliant" = "non-compliant"
  ))

# check 

employer_data %>% 
  filter(!is.na(Decision))%>% 
  
  

  tail(10)
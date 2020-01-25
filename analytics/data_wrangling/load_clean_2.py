# -*- coding: utf-8 -*-
"""
Created on Thu Mar 14 15:09:44 2019

@author: Nicholas.Hopewell
"""

# -*- coding: utf-8 -*-
"""
Created on Mon Jan 21 13:26:20 2019

@author: Nicholas.Hopewell
"""
import os
import pandas as pd
import numpy as np
from IPython.display import display
from numpy import nan as NA

cwd = os.getcwd()

PATH = 'C:\\Users\\Nicholas.Hopewell\\Employer-Inspection-proj\\data\\raw_data\\raw_data_3.xlsx'
xlsx = pd.ExcelFile(PATH)

dates = ["Inspection Initiation Date (In-house)",
         "Submissions Due Date(In-House)",
         "Date Referred to ESDC",
         "Date Referral Received by ECIU",
         "Date Assigned to First Level Review",
         "First Level Review Completion Date",
         "Date Assigned to First Level Review",
         "First Level Review Completion Date",
         "Date Assigned to Complex Review/In-house",
         "Complex Review Completion Date",
         "Date Inspection Put on Hold",
         "Date Inspection Taken Off  Hold",
         "Date sent for Onsite",
         "Date received after onsite",
         "Date 1st draft of NOPF sent to TL",
         "Date final NOPF sent to TL",
         "Date NOPF Sent to ER",
         "Date Assigned to SDM",
         "ECIU Decision Date"]
         
date_parser = lambda c: pd.to_datetime(c, format='%Y-%m-%d', errors = 'coerce') # didnt need second read
df_raw = pd.read_excel(xlsx, '2015-2018', parse_dates = dates)
## >> many were not converted to date objects !!

df_raw.head()

def display_all(df):
    with pd.option_context("display.max_rows", 1000): 
        with pd.option_context("display.max_columns", 1000): 
            display(df)
            
display_all(df_raw.tail().transpose())

# lots of missing data - probably for many reasons. Including decision
df_raw.info()

df_raw.isna().sum() # almot 3500 na in 'Decision'
df_raw.isna().sum() / len(df_raw) # 33% missing or 1 out of 3 


### Decision

# lots of data entry errors
pd.value_counts(df_raw.Decision)  # <<<<<<<<
pd.value_counts(df_raw.Decision).sum() # 7215 decisions made 

label_to_cleaned = {
        'compliant' : 'compliant',
        'compliant with justification' : 'compliant with justification',
        'compliant with justification ' : 'compliant with justification',
        'compliant with justification and compensation' : 
            'compliant with justification and compensation',
        'non-compliant' : 'non-compliant',
        'non-compliant ' : 'non-compliant'
}

lower_labs = df_raw['Decision'].str.lower()

df_raw['multi_target'] = lower_labs.map(label_to_cleaned)

# df_raw['Decision'].map(lambda x: label_to_cleaned[x.lower()])


# cleaned multi-target
pd.value_counts(df_raw.multi_target)  # <<<<<<<<
pd.value_counts(df_raw.multi_target).sum() # 7215

multi_to_binary = {
        'compliant' : 'compliant',
        'compliant with justification' : 'compliant',
        'compliant with justification and compensation' : 'compliant',
        'non-compliant' : 'non-compliant'
}

df_raw['target'] = df_raw['multi_target'].map(multi_to_binary)

# cleaned binary-target
pd.value_counts(df_raw.target)  # <<<<<<<<
pd.value_counts(df_raw.target).sum() # 7215

perc_comply = 1 - (pd.value_counts(df_raw.target)[1] / 
                   pd.value_counts(df_raw.target)[0])
perc_comply

# =============================================================================
#    ** Regulations including violations and justifications:
#     https://laws-lois.justice.gc.ca/eng/regulations/sor-2002-227/
#     
#    ** Noc code:
#     https://www.statcan.gc.ca/eng/subjects/standard/noc/2016/indexV1.2
# =============================================================================

# any duplicated data?
df_raw.duplicated().sum()

display_all(df_raw[df_raw.duplicated()].transpose())
# compare to isnull().all()
df_raw[df_raw.isnull().all(axis=1)]
# check
df_raw.iloc[1055, :]
    

### Fiscal Year
pd.value_counts(df_raw['Fiscal Year'])
# =============================================================================
# 18/19    4109
# 17/18    4942
# 16/17    1648
# disproportional representation of latest fiscal
# =============================================================================


### Employer
pd.value_counts(df_raw['Employer']).count()
# 10503 unique employers, probably a few less with entry errors


#### inspection status
pd.value_counts(df_raw['Inspection Status'])

map_status = {
        'NoPF Sent' : 'NoPF sent',
        'complete' : 'Complete',
        'COmplete' : 'Complete',
        'cOMPLETE' : 'Complete',
        'Deselected after Initiation ' : 'Deselected',
        'Inspection on hold' : 'Inspection On Hold'
}


df_raw['Inspection Status'].replace(map_status, regex= False, inplace = True)


### SDM Decision
pd.value_counts(df_raw['SDM Decision'])
# What does this mean?? almost all missing


### CRA Business Number
pd.value_counts(df_raw['CRA Business Number']).count() # 10562


### GCMS Org #
pd.value_counts(df_raw['GCMS Org #']).count() # 10442


### Region
pd.value_counts(df_raw['Region'])

map_region = {
        'ONTARIO' : 'Ontario',
        'Québec' : 'Quebec',
}


df_raw['Region'].replace(map_region, regex = False, inplace = True)
    

### Province
pd.value_counts(df_raw['Province'])
# this col might not be needed
df_raw['Province'].replace('PEI', 'PE', regex = False, inplace = True)


### Trigger
pd.value_counts(df_raw['Trigger'])

map_trigger = {
        'Random' : 'Random Selection',
        'Reason to Suspect' : 'Reason To Suspect',
        'Reason to suspect' : 'Reason To Suspect',
        'Past Non-Compliance' : 'Previous Non-compliance'
}

df_raw['Trigger'].replace(map_trigger, regex = False, inplace = True)


### On-Site Inspection
pd.value_counts(df_raw['On-Site Inspection'])

map_site = {
        'no' : 'No',
        'YES - Visit (ESDC)' : 'Yes'
}

df_raw['On-Site Inspection'].replace(map_site, regex = False, inplace = True)


### Currently Assigned
display_all(pd.value_counts(df_raw['Currently Assigned'])) 

map_assign = {
        'Christiane' :  'Christine',
        'eric l.' : 'Eric L.',
        'MAlik' : 'Malik',
        'andrew' : 'Andrew',
        'caroline' : 'Caroline',
        'Chris ' : 'Chris',
        'julie' : 'Julie',
}

df_raw['Currently Assigned'].replace(map_assign, regex = False, inplace = True)


### Triage Team Contact
display_all(pd.value_counts(df_raw['Triage Team Contact'])) 

map_triage = {
        'caroline' : 'Caroline',
        'Christiane' : 'Christine',
        'Eric l.' : 'Eric L.'
}

df_raw['Triage Team Contact'].replace(map_triage, regex = False, inplace = True)


### Investigative Analyst Contact
display_all(pd.value_counts(df_raw['Investigative Analyst Contact'])) 

df_raw['Investigative Analyst Contact'].replace('maureen', 'Maureen', 
          regex = False, inplace = True)


### Inspection Initiation Date (In-house)
 # not converted to date, some errors to fix
 
idd_dict = {'2018-122-18' : '2018-12-18', 
            '`' : NA,}
df_raw['Inspection Initiation Date (In-house)'].replace(
        idd_dict, inplace = True)
 
df_raw['Inspection Initiation Date (In-house)'] = pd.to_datetime(
        df_raw['Inspection Initiation Date (In-house)'],
        format = '%Y-%m-%d')

# =============================================================================
# NOTE - I have to go throughh each of these date columns BELOW
# which did not parse individually rather than handling them in 
# bulk with a function to make sure proper coersions are made
# Also lots of unique errors. 
# =============================================================================

### Submissions Due Date(In-House) 
df_raw.loc[df_raw['Submissions Due Date(In-House)'] == '2018-09-31']

df_raw['Submissions Due Date(In-House)'] = pd.to_datetime(
        df_raw['Submissions Due Date(In-House)'], 
        format = '%Y-%m-%d', errors= 'coerce')


### Date Referral Received by ECIU 
df_raw.loc[df_raw['Date Referral Received by ECIU'] == ' ']
df_raw['Date Referral Received by ECIU'].replace(' ', 
                        NA, inplace = True)
#df_raw.loc['2941', 'Date Referral Received by ECIU'] = NA

df_raw['Date Referral Received by ECIU'] = pd.to_datetime(
        df_raw['Date Referral Received by ECIU'], format = '%Y-%m-%d')


### Date Assigned to First Level Review
df_raw['Date Assigned to First Level Review'] = pd.to_datetime(
        df_raw['Date Assigned to First Level Review'], format = '%Y-%m-%d')


### First Level Review Completion Date
df_raw['First Level Review Completion Date'] = pd.to_datetime(
        df_raw['First Level Review Completion Date'], 
        format = '%Y-%m-%d', errors = 'coerce')


### Date Assigned to Complex Review/In-house 
rd = {' ' : NA, 
      '2018-11*16' : '2018-11-16', 
      '208-12-10' : '2018-12-10',
      '43468' : NA, 
      '43476' : NA
}

df_raw['Date Assigned to Complex Review/In-house'].replace(rd, inplace = True)


# =============================================================================
#  10 rows have the value '43468' in this column. Is this a code to come
#      back to ???
#  These records are all in order from 10365 to 10374
#  Same thing is present with recent data as well, with codes like '43476'
#  Ask the 2 contacts?
# =============================================================================

df_raw['Date Assigned to Complex Review/In-house'].iloc[10137] = NA

df_raw['Date Assigned to Complex Review/In-house'] = pd.to_datetime(
        df_raw['Date Assigned to Complex Review/In-house'], 
        format = '%Y-%m-%d')


### Complex Review Completion Date 

# Same thing happening in this field with newer dates
df_raw['Complex Review Completion Date'].replace('43475', NA, inplace = True)

df_raw['Complex Review Completion Date'] = pd.to_datetime(
        df_raw['Complex Review Completion Date'], 
        format = '%Y-%m-%d')


### Date NOPF Sent to ER  
df_raw['Date NOPF Sent to ER'] = pd.to_datetime(
        df_raw['Date NOPF Sent to ER'], format = '%Y-%m-%d')


#### Date Assigned to SDM 
df_raw['Date Assigned to SDM'] = pd.to_datetime(
        df_raw['Date Assigned to SDM'], format = '%Y-%m-%d', errors = 'coerce')

pd.value_counts(df_raw['Date Assigned to SDM']) # check


### ECIU Decision Date 
df_raw['ECIU Decision Date'].replace('2019-0-16', NA, inplace = True)

df_raw['ECIU Decision Date'] = pd.to_datetime(
        df_raw['ECIU Decision Date'], format = '%Y-%m-%d')


### High Profile

# fix trailing space
df_raw.rename(index = str, columns = {'High Profile ' : 'High Profile'}, 
                  inplace = True)


### Referral made to CBSA/RCMP
pd.value_counts(df_raw['Referral made to CBSA/RCMP'])

# =============================================================================
#  note: 3 records with date and 9 with 'no' - dates = 'yes'
#  make these two columns? yes no and date?
# =============================================================================


### Justification Type
pd.value_counts(df_raw['Justification Type'])

# =============================================================================
# ### >>> FIGURE OUT WHAT TO DO HERE <<<<
# =============================================================================


### Compensation Type
pd.value_counts(df_raw['Compensation Type'])

# =============================================================================
# Almost no values here. 
# Does simply the presence of compensation mean something?
# =============================================================================


### Compensation paid to FW
comp_dict = {'the worker gained access to employer-sponsored group pension plan that was illegitimately withheld by employer' : NA,
             ' ' : NA,
             'An extra week of vacation in lieu of payment - Value of $840' : 840.00,
             '$1, 000' : 1000.00,
             'Dental Benefits' : 609.53, # median
             '$189,80' : 189.80      
 }

df_raw['Compensation paid to FW'].replace(comp_dict, inplace = True)
df_raw['Compensation paid to FW'] = pd.to_numeric(
        df_raw['Compensation paid to FW'], downcast='float')



### AMP Warning Letter  DERIVED 
df_raw['AMP Warning Letter'] = np.where(df_raw['AMP Amount'] == 'Warning Letter', 
                                       'Warning Letter', NA)

### AMP Amount 
df_raw['AMP Amount'].replace(
        {'Warning Letter' : NA, ' ' : NA}, regex = False, inplace = True)


### Non Compliance Type
df_raw['Non Compliance Type'] = df_raw['Non Compliance Type'].str.strip()

map_nc = { 'A,C' : 'multiple',
           'A and B' : 'multiple',
           'C,A' : 'multiple',
           'A, C ' : 'multiple',
           'R209.4(1)(b)' : 'B'        
}

df_raw['Non Compliance Type'].replace(map_nc, regex = False, inplace = True)


### Gender of FW
df_raw['Gender of FW'] = df_raw['Gender of FW'].str.strip().str.upper()

map_gen = { 'MULTIPLE' : NA       
}

df_raw['Gender of FW'].replace(map_gen, regex = False, inplace = True)

### LMIA Exempt # 

# trim whitespace and uppercase
df_raw['LMIA Exempt #'] = df_raw['LMIA Exempt #'].str.strip().str.upper()
       
df_raw['LMIA Exempt #'].str.startswith(('A','a'), 
       na= False).sum() # 10447

# =============================================================================
#  THESE SHOULD ALL START WITH 'A' - see stevens questions 2 note
#  Some have multiple codes in this field
# =============================================================================

_ = df_raw[df_raw['LMIA Exempt #'].str.startswith(('A','a'), 
   na= False)]

pd.value_counts(_['target'])
# =============================================================================
# Of only LMIA begining with 'A'
# compliant        6914
# non-compliant      80

# LOSE 17 non-compliant, big deal
# =============================================================================


### LMIA Exemption Code 

# trim whitespace and uppercase    
df_raw['LMIA Exemption Code'] = df_raw['LMIA Exemption Code'].str.strip().str.upper()
       
df_raw['LMIA Exemption Code'].str.startswith(('C', 'T', 'A'), 
       na= False).sum()   # 10571 start with these letters
    
df_raw['LMIA Exemption Code'][~df_raw['LMIA Exemption Code'].str.startswith(
        ('C', 'T', 'A'), na= False)].notna().sum() # rest are NA
    
# fix errors and duplicated codes
df_raw['LMIA Exemption Code'].replace(['C10 (A75)', 'A70, T13, T13, A70, T13', 'C18; T13'], 
                        ['C10', 'A70', 'C18'], inplace = True)


sr_keys = ['T47','T44', 'T43', 'T33', 'T24', 'T23', 'T22', 'T21', 'T11',
     'T12', 'T13', 'C18', 'C16', 'C14', 'C13', 'C12', 'C11', 'C10', 'A75',
     'C23', 'C22', 'C21', 'C20', 'C32', 'C31', 'C45', 'C44', 'C50', 'A70']

sr_vals =  [['R204(a) Canada-international exemption codes'] * 9 +
 ['R204(b) Provincial/territorial-international exemption codes'] +
 ['R204(c) Canada-provincial/territorial exemption codes'] * 2 +
 ['R205(a) Significant benefit exemption codes'] * 7 +
 ['R205(b) Reciprocal employment exemption codes'] * 4 +
 ['R205(c)(i) Research exemption codes'] * 2 +
 ['R205(c)(ii) Competitiveness and public policy exemption codes'] *2 +
 ['R205(d) Charitable or religious work exemption code'] +
 ['R207: Permanent residence applicants in Canada']].pop()

lmia_subreg_dict = {key:val for val, key in zip(sr_vals, sr_keys)}
     

###  'LMIA Regulation Subsection' DERVIVED  
df_raw['LMIA Regulation Subsection'] = df_raw['LMIA Exemption Code'].map(
        lmia_subreg_dict)

###  'LMIA Regulation Section' DERVIVED  

r_keys = ['R204(a) Canada-international exemption codes',
          'R204(b) Provincial/territorial-international exemption codes',
          'R204(c) Canada-provincial/territorial exemption codes',
          'R205(a) Significant benefit exemption codes',
          'R205(b) Reciprocal employment exemption codes',
          'R205(c)(i) Research exemption codes',
          'R205(c)(ii) Competitiveness and public policy exemption codes',
          'R205(d) Charitable or religious work exemption code',
          'R207: Permanent residence applicants in Canada']

r_vals = [['International agreements'] * 3 + ['Canadian interests'] * 5 +
          ['Permanent residence applicants in Canada']].pop()


lmia_reg_dict = {key:val for val, key in zip(r_vals, r_keys)}

df_raw['LMIA Regulation Section'] = df_raw['LMIA Regulation Subsection'].map(
        lmia_reg_dict)


## now I can map the original LMIA field >>
arr = df_raw['LMIA Exemption Code'].dropna().unique()
lmia_keys = arr.tolist()
lmia_keys.sort(reverse = True)

lmia_vals = ['Contractual service supplier (CETA)', 
             'Intra-corporate (company) transferee (CETA)',
             'Independent professional (CETA)',
             'GATS professional', 'Intra-company transferee (FTA)',
             'Professional/technician (FTA)', 'Investor (FTA)',
             'Trader (FTA)', 'Canada-provincial/territorial',
             'Provincial/territorial-international',
             'Non-trade' , 'Charitable or religious work',
             'Medical residents and fellows',
             'Post-doctoral Ph.D. fellows and award recipients',
            'Educational co-op – post-secondary',
            'Research', 'Performing arts',
            'Academic exchanges (professors, visiting lecturers)',
            'Youth exchange programs', 'Reciprocal employment',
            'Atlantic Immigration Pilot Program', 'Francophone mobility',
            'Television and film production workers',
            'Emergency repairs or repair personnel for out-of-warranty equipment',
            'Intra-company transferees (including GATS)', 'Entrepreneurs',
            'Significant benefit', 'Bridging open work permits',
            'Permanent residence applicants in Canada']

lmia_dict = {key:val for val, key in zip(lmia_vals, lmia_keys)}

# replace vals
df_raw['LMIA Exemption Code'].replace(lmia_dict, 
      regex = False, inplace = True)



### NOC code
pd.to_numeric(df_raw['NOC Code']).describe()   
# notice some have multiple NOC Code  
pd.value_counts(df_raw['NOC Code'].astype(str).str.len())
df_raw['NOC Code'].astype(str).str.startswith('1').sum()

df_raw.loc['4887', 'NOC Code'] = 6322000
df_raw['NOC Code'] = df_raw['NOC Code'].astype(str)

df_raw['NOC Code'].str.startswith(('112'), 
       na= False).sum() 

# fix errors where leadin 0 not appended: 

map_fix_noc = {
        "16000" : "016000",
        "160000" : "0160000",
        "6010000" : "06010000",
        "1600000" : "01600000",
        "601000" : "0601000",
        "71100" : "071100",
        "711000" : "0711000",
        "714000" : "0714000",
        "712000" : "0712000",
        "811000" : "0811000",
        "911000" : "0911000",
        "912000" : "0912000"
}

df_raw['NOC Code'].replace(map_fix_noc, regex = False, inplace = True)


### NOC Type
pd.value_counts(df_raw['NOC Type']) 

map_noc = { 
        'A' : 'Professional jobs',
        'B' : 'Technical jobs & skilled trade', 
        'C' : 'Intermediate jobs',
        'D' : 'Labour jobs', 
         0  : 'Management jobs',
        '*' : NA,
        'NO RESULTS' : NA
}

df_raw['NOC Type'].replace(map_noc, regex = False, inplace = True)


### NOC Broad Occupation
# derived <
gen_occ_map = {'^0.*' : 'Management occupations',
               '^1.*' : 'Business, finance and administration occupations',
               '^2.*' : 'Natural and applied sciences and related occupations',
               '^3.*' : 'Health occupations',
               '^4.*' : 'Occupations in education, law and social, community and government services',
               '^5.*' : 'Occupations in art, culture, recreation and sport',
               '^6.*' : 'Sales and service occupations',
               '^7.*' : 'Trades, transport and equipment operators and related occupations',
               '^8.*' : 'Natural resources, agriculture and related production occupations',
               '^9.*' : 'Occupations in manufacturing and utilities'
}

df_raw['NOC Broad Occupation'] = df_raw['NOC Code'].replace(
        gen_occ_map, regex = True)

#df_raw['NOC Broad Occupation'].replace(
#        "Health occupations\tHealth occupations", "Health occupations", inplace = True)


### NOC Major Group
# derived <

major_grp_map = {'^00.*' : 'Management occupations',
               '^(01|02|03|04|05).*' : 'Specialized middle management occupations',
               '^06.*' : 'Middle management occupations in retail and wholesale trade and customer services',
               '^(07|08|09).*' : 'Middle management occupations in trades, transportation, production and utilitie',
               
               '^11.*' : 'Professional occupations in business and finance',
               '^12.*' : 'Administrative and financial supervisors and administrative occupations',
               '^13.*' : 'Finance, insurance and related business administrative occupations',
               '^14.*' : 'Office support occupations',
               '^15.*' : 'Distribution, tracking and scheduling co-ordination occupations',
               
               '^21.*' : 'Professional occupations in natural and applied sciences',
               '^22.*' : 'Technical occupations related to natural and applied sciences',
               
               '^30.*' : 'Professional occupations in nursing',
               '^31.*' : 'Professional occupations in health (except nursing)',
               '^32.*' : 'Technical occupations in health',
               '^34.*' : 'Assisting occupations in support of health services',
               
               '^40.*' : 'Professional occupations in education services',
               '^41.*' : 'Professional occupations in law and social, community and government services',
               '^42.*' : 'Paraprofessional occupations in legal, social, community and education services',
               '^43.*' : 'Occupations in front-line public protection services',
               '^44.*' : 'Care providers and educational, legal and public protection support occupations',
               
               '^51.*' : 'Professional occupations in art and culture',
               '^52.*' : 'Technical occupations in art, culture, recreation and sport',
               
               '^62.*' : 'Retail sales supervisors and specialized sales occupations',
               '^63.*' : 'Service supervisors and specialized service occupations',
               '^64.*' : 'Sales representatives and salespersons - wholesale and retail trade',
               '^65.*' : 'Service representatives and other customer and personal services occupations',
               '^66.*' : 'Sales support occupations',
               '^67.*' : 'Service support and other service occupations, n.e.c.',
               
               '^72.*' : 'Industrial, electrical and construction trades',
               '^73.*' : 'Maintenance and equipment operation trades',
               '^74.*' : 'Other installers, repairers and servicers and material handlers',
               '^75.*' : 'Transport and heavy equipment operation and related maintenance occupations',
               '^76.*' : 'Trades helpers, construction labourers and related occupations',
               
               '^82.*' : 'Supervisors and technical occupations in natural resources, agriculture and related production',
               '^84.*' : 'Workers in natural resources, agriculture and related production',
               '^86.*' : 'Harvesting, landscaping and natural resources labourers',
               '^88.*' : 'Entrepreneurs - Temp Residents',
               
               '^92.*' : 'Processing, manufacturing and utilities supervisors and central control operators',
               '^94.*' : 'Processing and manufacturing machine operators and related production workers',
               '^95.*' : 'Assemblers in manufacturing',
               '^96.*' : 'Labourers in processing, manufacturing and utilities'               
}
    

           
df_raw['NOC Major Group'] = df_raw['NOC Code'].replace(
        major_grp_map, regex = True)

# fix code errors
df_raw['NOC Major Group'] = df_raw['NOC Major Group'].replace(
        '9914000', NA, regex = False)


# =============================================================================
# DERIVE MORE COLUMNS FROM EACH SUB GROUPING OF THE NOC CODES
# MIGHT BE INTERESTING FOR LATER            
# =============================================================================



### NAICS
# https://pwp.vpl.ca/siic/job-search-resources/what-is-naics/
# ================================================================

df_raw['NAICS'].describe() # just to check is all numbers 
df_raw['NAICS'].value_counts()

# 82, 87, 77, 70, 2, 74, 35 all entry errors
df_raw['NAICS'].replace([82, 87, 84, 58, 77, 70, 50, 12, 2, 74, 35, 42], NA, inplace = True)

arr = df_raw['NAICS'].unique()
keys = np.sort(arr).tolist()
keys.pop() # remove nan from keys

vals = ['Agriculture, Forestry, Fishing and Hunting',
        'Mining/quarrying', 
        'Utilities', 
        'Construction', 
        'Manufacturing',
        'Manufacturing',
        'Wholesale trade', 
        'Retail trade', 
        'Transportation and warehousing', 
        'Information and cultural industries', 
        'Finance and insurance', 
        'Real estate and rental and leasing',
        'Professional, scientific and technical services',
        'Management of companies and enterprises', 
        'Administrative and support, waste management and remediation services',
        'Educational services', 
        'Health care and social assistance',
        'Arts, entertainment and recreation', 
        'Accommodation and food services',
        'Other services (except public administration)',
        'Public administration']

map_naics = {key:val for val, key in zip(vals, keys)}

df_raw['NAICS'].replace(map_naics, regex = False, inplace = True)


### WP Verification Activity 
map_wp = {
        'Closed' :  'closed',
        'closed' : 'closed',
        'CLosed' : 'closed',
        'Closed ' : 'closed',
        'closed ' : 'closed',
        'CLOSED' : 'closed',
        'Cancelled' : 'closed',
        'Created' : 'created',
        'Created ' : 'created'     
}

df_raw['WP Verification Activity'].replace(map_wp, regex = False, 
      inplace = True)

df_raw['WP Verification Activity'].replace(
        'verification activity unable to complete due to WP status being "In Progress"', NA, 
        inplace = True)

# mostly closed. Figure out what this means
df_raw['WP Verification Activity'].value_counts()


### Assessed Using Indicator Brief
# fix trailing space
df_raw.rename(index = str, columns = {'Assessed Using Indicator Brief ' : 
                                      'Assessed Using Indicator Brief'}, 
                  inplace = True)

df_raw['Assessed Using Indicator Brief'].value_counts()    
# FIGURE OUT WHAT IS GOING ON HERE


###  QA review  
df_raw['QA review'].value_counts()

# what does this mean?


# reindex df
list(df_raw.columns.values)

col_order = ['Fiscal Year',
 'GCMS Case Number', 'Referral Number', 'Employer',
 'Inspection Status', 'Decision', 'SDM Decision',
 'CRA Business Number', 'GCMS Org #', 'Region',
 'Province', 'Trigger', 'On-Site Inspection',
 'Currently Assigned', 'Triage Team Contact',
 'Investigative Analyst Contact',
 'Inspection Initiation Date (In-house)',
 'Submissions Due Date(In-House)', 'Date Referred to ESDC',
 'Date Referral Received by ECIU',
 'Date Assigned to First Level Review',
 'First Level Review Completion Date',
 'Date Assigned to Complex Review/In-house',
 'Complex Review Completion Date',
 'Date Inspection Put on Hold',
 'Date Inspection Taken Off  Hold',
 'Date sent for Onsite', 'Date received after onsite',
 'Date 1st draft of NOPF sent to TL',
 'Date final NOPF sent to TL', 'Date NOPF Sent to ER',
 'Date Assigned to SDM', 'ECIU Decision Date',
 'High Profile', 'Referral made to CBSA/RCMP',
 'Justification Type', 'Justification 2',
 'Justification 3', 'Justification 4',
 'Compensation Type', 'Compensation paid to FW',
 'AMP Amount', 'AMP Warning Letter', 'IRPR Violation Type', 
 'Violation 2','Violation 3', 'Violation 4', 'Non Compliance Type',
 'Ban Length', 'Gender of FW', 'LMIA Exempt #',
 'LMIA Exemption Code', 'LMIA Regulation Section',
 'LMIA Regulation Subsection', 'NOC Code',
 'NOC Type', 'NOC Broad Occupation','NOC Major Group',
 'NAICS', 'WP Verification Activity',
 'Assessed Using Indicator Brief', 'QA review',
 'target', 'multi_target']

df_raw = df_raw.reindex(columns = col_order)



# =============================================================================
#                             >>>>> TODO <<<<<<
# - derive more from noc codes
# - figure out how to handle those regulation and justification fields (read)
# =============================================================================

#out = "C:\\Users\\Nicholas.Hopewell\\Employer-Inspection-proj\\data\\tidy_data"
#df_raw.to_excel(out + "\\data_clean_3.xlsx")


# clean
#CPATH = 'C:\\Users\\Nicholas.Hopewell\\Employer-Inspection-proj\\data\\tidy_data\\data_clean_3.xlsx'
#inspection_df = pd.read_excel(CPATH)



## creating 2 new columns to help aggregate and visualize and for model testing

multi_to_three = {
        'compliant' : 'compliant',
        'compliant with justification' : 'compliant with justification',
        'compliant with justification and compensation' : 
            'compliant with justification',
        'non-compliant' : 'non-compliant'
}

multi_binary_down = {
        'compliant' : 'compliant',
        'compliant with justification' : 'non-compliant',
        'compliant with justification and compensation' : 'non-compliant',
        'non-compliant' : 'non-compliant'
}


df_raw['target_three'] = df_raw['multi_target'].map(multi_to_three)
df_raw['target_down'] = df_raw['multi_target'].map(multi_binary_down)


df_raw['target'].value_counts()
df_raw['multi_target'].value_counts()
df_raw['target_three'].value_counts()
df_raw['target_down'].value_counts()

tars = ['target', 'multi_target', 
        'target_three', 'target_down']



#inspection_df[tars].apply(lambda s: s.value_counts())


out = "C:\\Users\\Nicholas.Hopewell\\Employer-Inspection-proj\\data\\tidy_data"
df_raw.to_excel(out + "\\data_clean_good_3.xlsx")


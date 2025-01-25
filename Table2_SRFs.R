##### SOCIAL RISK FACTORS ##### 

#load or install required packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
if (!require(DBI)) install.packages("DBI")
if (!require(odbc)) install.packages("odbc")
if (!require(arrow)) install.packages("arrow")
if (!require(pacman)) install.packages("pacman")
if (!require(EpiFunc)) install.packages("EpiFunc")
if (!require(janitor)) install.packages("janitor")
if (!require(gtsummary)) install.packages("gtsummary")
if (!require(scales)) install.packages("scales")

##### load master template table to join SRFs data to #####
#NOTE TO USER: may need to update file path with the letter corresponding to your mapped drive
template_table <- read.csv("I:/Evaluations/ED opt out BBV testing evaluation/Analysis/Template_table.csv")

##### SOCIAL RISK FACTORS ##### 

# load connection to Sentinel
Sentinel <- odbc::dbConnect(odbc::odbc(),
                            .connection_string = "driver={SQL Server};server=SQLClusColHFN17\\HFN17;
                             database=NIS_HepatitisSentinel;
                             Encrypt=true;trusted_connection=true",
                            timeout = 60,
                            timezone = Sys.timezone(),
                            timezone_out = Sys.timezone())


##### HCV DATA #####

# Load the HCV diagnoses table from Sentinel
HCV_DIAG_SENTINEL_srf <- DBI::dbGetQuery(conn = Sentinel , statement = "
select * from SentinelEDApril22_HCVnew
where FirstHCVtestdate <= '2023-04-07' AND (antiHCV like 'Positive' or HCVAg like 'Positive' or HCVRNA like 'Positive')
                                          and LIVE = 'Live'")

## data cleaning for HCV_DIAGNOSES_SENTINEL ########
## don't need this now as has been done in original table
# # format dates  
HCV_DIAG_SENTINEL_srf <- HCV_DIAGNOSES_SENTINEL_srf %>% 
  mutate(antiHCVdate =as.Date(antiHCVdate, format = '%Y-%m-%d'),
         HCVRNAdate = as.Date(HCVRNAdate, format = '%Y-%m-%d'),
         FirstHCVtestdate = as.Date(FirstHCVtestdate, format = '%Y-%m-%d'),
         EED = as.Date(EED, format = '%Y-%m-%d'),
         preRxdate = as.Date(preRxdate, format = '%Y-%m-%d'),
         postRxdate = as.Date(postRxdate, format = '%Y-%m-%d'),
         mincreatedate = as.Date(mincreatedate, format = '%Y-%m-%d'),
         minreferraldate = as.Date(minreferraldate, format = '%Y-%m-%d'),
         earliestRxdate = as.Date(earliestRxdate, format = '%Y-%m-%d'),
         minsentineldate = as.Date(minsentineldate, format = '%Y-%m-%d'),
         DOB = as.Date(DOB, format = '%Y-%m-%d')) %>% replace(is.na(.), 0)


# lower case 
HCV_DIAG_SENTINEL_srf <- mutate(HCV_DIAG_SENTINEL_srf, antiHCV = tolower(antiHCV)) 
HCV_DIAG_SENTINEL_srf <- mutate(HCV_DIAG_SENTINEL_srf, HCVRNA = tolower(HCVRNA)) 

# Recode PWID
# 
HCV_DIAG_SENTINEL_srf <- HCV_DIAG_SENTINEL_srf %>% 
  mutate(PWID_2 = case_when(PWID == "Current/RecentPWID" | PWID == "Yes" ~ "Current/Recent PWID", # is yes current or past?
                            PWID == "PastPWID" ~ "Past PWID",
                            PWID == "NeverPWID" | PWID == "No" | PWID == "Never PWID" ~ "Never PWID"))  
# 
# 
# # Recode some new that were not correctly defined and create rna_positive_new field
# 
HCV_DIAG_SENTINEL_srf <- HCV_DIAG_SENTINEL_srf %>% 
  mutate(Newdiag = case_when(HCVRNA == 'positive' & Newdiag == 'Yes' & (preRxdate < HCVRNAdate | EED < HCVRNAdate) ~ "",
                             HCVRNA == 'positive' & is.na (preRxdate) & is.na(EED)& is.na(minsentineldate)~ "Yes",
                             TRUE ~ Newdiag))
# 
HCV_DIAG_SENTINEL_srf <- HCV_DIAG_SENTINEL_srf %>% 
  mutate(rna_positive_new = case_when(HCVRNA == 'positive' & Newdiag == 'Yes' ~ "New",
                                      TRUE ~ ""))
# 
# ### add go live dates
# 
HCV_DIAG_SENTINEL_srf<- HCV_DIAG_SENTINEL_srf%>%
  mutate(live_HCV = case_when ((Hospital ==  'Charing Cross Hospital' & FirstHCVtestdate >= '2022-07-01') 
                               |(Hospital == 'Chelsea & Westminster Hospital'& FirstHCVtestdate >= '2022-04-01')
                               |(Hospital == 'Croydon University Hospital'& FirstHCVtestdate >= '2023-03-20')
                               |(Hospital == 'Hillingdon Hospital'& FirstHCVtestdate >= '2022-07-22')
                               |(Hospital == 'Homerton University Hospital'& FirstHCVtestdate >= '2022-09-12')
                               |(Hospital == 'Kings College Hospital'& FirstHCVtestdate >= '2022-11-16')
                               |(Hospital == 'Kingston Hospital'& FirstHCVtestdate >= '2023-04-24')
                               |(Hospital == 'Newham General Hospital'& FirstHCVtestdate >= '2022-04-01')
                               |(Hospital == 'Royal Sussex County Hospital'& FirstHCVtestdate >= '2023-03-06')
                               |(Hospital == 'St Georges Hospital'& FirstHCVtestdate >= '2022-11-17')
                               |(Hospital == 'St Marys Hospital'& FirstHCVtestdate >= '2022-08-15')
                               |(Hospital == 'St Thomas Hospital'& FirstHCVtestdate >= '2022-11-01')
                               |(Hospital == 'The Royal London Hospital'& FirstHCVtestdate >= '2022-04-01')
                               |(Hospital == 'West Middlesex University Hospital'& FirstHCVtestdate >= '2022-04-01')
                               |(Hospital == 'Whipps Cross University Hospital'& FirstHCVtestdate >= '2022-04-04')
                               ~ "LIVE",
                               TRUE~ "NOT LIVE"))%>%
  filter(live_HCV == 'LIVE' & age >15)

# Replace "15-24" with "16-24" in the age group column
HCV_DIAG_SENTINEL_srf$agegroup[HCV_DIAG_SENTINEL_srf$agegroup == "15-24"] <- "16-24"

# Rename gender variable in the sex column
HCV_DIAG_SENTINEL_srf <- HCV_DIAG_SENTINEL_srf %>% 
  mutate(sex = replace(sex, sex=="1", "Men")) %>% 
  mutate(sex = replace(sex, sex=="2", "Women")) %>% 
  mutate(CombinedEthnicity = replace(CombinedEthnicity, CombinedEthnicity=="Mixed", "Mixed/Multiple")) 


# Recode IMD and unknowns
HCV_DIAG_SENTINEL_srf <- HCV_DIAG_SENTINEL_srf %>% 
  mutate(IMD = replace(IMD,IMD== 1, "1")) %>% 
  mutate(IMD = replace(IMD,IMD== 2, "1")) %>% 
  mutate(IMD = replace(IMD,IMD== 3, "2")) %>%
  mutate(IMD = replace(IMD,IMD== 4, "2")) %>%
  mutate(IMD = replace(IMD,IMD== 5, "3")) %>%
  mutate(IMD = replace(IMD,IMD== 6, "3")) %>%
  mutate(IMD = replace(IMD,IMD== 7, "4")) %>%
  mutate(IMD = replace(IMD,IMD== 8, "4")) %>%
  mutate(IMD = replace(IMD,IMD== 9, "5")) %>%
  mutate(IMD = replace(IMD,IMD== 10, "5")) %>% 
  mutate(IMD = if_else(
    is.na(IMD), 
    "Unknown IMD", 
    IMD, 
    IMD),
    Gender = if_else(
      is.na(sex), 
      "Unknown Gender", 
      sex, 
      sex),
    CombinedEthnicity = if_else(
      (is.na(CombinedEthnicity) |
         CombinedEthnicity == "Unknown"), 
      "Unknown Ethnicity", 
      CombinedEthnicity,
      CombinedEthnicity)) %>% 
  mutate(All = case_when(age >= 0 ~ "All")) %>%
  
  mutate(agegroup = str_replace_all(agegroup, "-", " to "),
         agegroup = str_replace_all(agegroup, "80\\+", "80 and over"),
         agegroup = str_replace_all(agegroup, "(\\d+)(to)(\\d+)", "\\1 to \\3"))



##Loop using age group, sex, ethnic group, IMD - Patients flagged as homeless who are tested for HCV
### comment these out for now as we are not reporting on people tested for the 16 sites

# HCVtested_homeless <- HCV_DIAG_SENTINEL_srf %>% filter(!is.na(antiHCV) | !is.na(HCVAg)) %>% filter(homelessflag == "Yes")
# 
# ##Generate template table using one of the aggregation variables of the master table
# aggregation_tableSRF1 <- HCVtested_homeless %>% 
#   group_by(agegroup) %>% 
#   tally() %>% 
#   collect() %>% #collect from the server after doing anything computationally heavy
#   rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
#   rename("Number of patients who are flagged as homeless and tested for HCV" = n)
# 
# 
# ## define the variables you want to loop through for master table
# vec <- c("Gender", "CombinedEthnicity", "IMD", "All")
# 
# #set up for loop
# for(i in 1:length(vec)){
#   
#   ## test 1 print the var you are agg by
#   print(vec[i])
#   
#   #create temp table to store aggregation
#   temp <- HCVtested_homeless %>% 
#     group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
#     tally() %>%   
#     collect() %>% 
#     rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
#     rename("Number of patients who are flagged as homeless and tested for HCV" = n)
#   
#   
#   # test 2 print temp table check its right
#   print(temp)
#   
#   # bind temp table rows to agg table
#   aggregation_tableSRF1 <- bind_rows(aggregation_tableSRF1, temp) 
#   
# }
# 
# #test 3 - look at table
# View(aggregation_tableSRF1)
# 
# 
# ##Loop using age group, sex, ethnic group, IMD - Patients flagged as PWID who are tested for HCV
# 
# HCVtested_PWID <- HCV_DIAG_SENTINEL_srf %>% filter(!is.na(antiHCV) | !is.na(HCVAg)) %>% filter(PWID == "Yes")
# 
# ##Generate template table using one of the aggregation variables of the master table
# aggregation_tableSRF2 <- HCVtested_PWID %>% 
#   group_by(agegroup) %>% 
#   tally() %>% 
#   collect() %>% #collect from the server after doing anything computationally heavy
#   rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
#   rename("Number of patients who are flagged as PWID and tested for HCV" = n)
# 
# 
# ## define the variables you want to loop through for master table
# vec <- c("Gender", "CombinedEthnicity", "IMD", "All")
# 
# #set up for loop
# for(i in 1:length(vec)){
#   
#   ## test 1 print the var you are agg by
#   print(vec[i])
#   
#   #create temp table to store aggregation
#   temp <- HCVtested_PWID %>% 
#     group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
#     tally() %>%   
#     collect() %>% 
#     rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
#     rename("Number of patients who are flagged as PWID and tested for HCV" = n)
#   
#   
#   # test 2 print temp table check its right
#   print(temp)
#   
#   # bind temp table rows to agg table
#   aggregation_tableSRF2 <- bind_rows(aggregation_tableSRF2, temp) 
#   
# }
# 
# #test 3 - look at table
# View(aggregation_tableSRF2)
# 
# 
# ##Loop using age group, sex, ethnic group, IMD - Patients tested for HCV in a prison
# 
# HCVtested_prison <- HCV_DIAG_SENTINEL_srf %>% filter(!is.na(antiHCV) | !is.na(HCVAg)) %>% filter(testedinaprison == "Yes")
# 
# ##Generate template table using one of the aggregation variables of the master table
# aggregation_tableSRF3 <- HCVtested_prison %>% 
#   group_by(agegroup) %>% 
#   tally() %>% 
#   collect() %>% #collect from the server after doing anything computationally heavy
#   rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
#   rename("Number of patients who are tested in a prison for HCV" = n)
# 
# 
# ## define the variables you want to loop through for master table
# vec <- c("Gender", "CombinedEthnicity", "IMD", "All")
# 
# #set up for loop
# for(i in 1:length(vec)){
#   
#   ## test 1 print the var you are agg by
#   print(vec[i])
#   
#   #create temp table to store aggregation
#   temp <- HCVtested_prison %>% 
#     group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
#     tally() %>%   
#     collect() %>% 
#     rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
#     rename("Number of patients who are tested in a prison for HCV" = n)
#   
#   
#   # test 2 print temp table check its right
#   print(temp)
#   
#   # bind temp table rows to agg table
#   aggregation_tableSRF3 <- bind_rows(aggregation_tableSRF3, temp) 
#   
# }
# 
# #test 3 - look at table
# View(aggregation_tableSRF3)
# 
# 
# ##Loop using age group, sex, ethnic group, IMD - Patients tested for HCV in a drug service
# 
# HCVtested_drugservice <- HCV_DIAG_SENTINEL_srf %>% filter(!is.na(antiHCV) | !is.na(HCVAg)) %>% filter(testedindrugservice == "Yes")
# 
# ##Generate template table using one of the aggregation variables of the master table
# aggregation_tableSRF4 <- HCVtested_drugservice %>% 
#   group_by(agegroup) %>% 
#   tally() %>% 
#   collect() %>% #collect from the server after doing anything computationally heavy
#   rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
#   rename("Number of patients who are tested in a drug service for HCV" = n)
# 
# 
# ## define the variables you want to loop through for master table
# vec <- c("Gender", "CombinedEthnicity", "IMD", "All")
# 
# #set up for loop
# for(i in 1:length(vec)){
#   
#   ## test 1 print the var you are agg by
#   print(vec[i])
#   
#   #create temp table to store aggregation
#   temp <- HCVtested_drugservice %>% 
#     group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
#     tally() %>%   
#     collect() %>% 
#     rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
#     rename("Number of patients who are tested in a drug service for HCV" = n)
#   
#   
#   # test 2 print temp table check its right
#   print(temp)
#   
#   # bind temp table rows to agg table
#   aggregation_tableSRF4 <- bind_rows(aggregation_tableSRF4, temp) 
#   
# }
# 
# #test 3 - look at table
# View(aggregation_tableSRF4)


##Loop using age group, sex, ethnic group, IMD - Patients flagged as homeless who test positive for HCV

HCVpos_homeless <- HCV_DIAG_SENTINEL_srf %>% filter(RNA_all == "Positive") %>% filter(homelessflag == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF5 <- HCVpos_homeless %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as homeless and test positive for HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVpos_homeless %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as homeless and test positive for HCV" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF5 <- bind_rows(aggregation_tableSRF5, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF5)


##Loop using age group, sex, ethnic group, IMD - Patients flagged as PWID who test positive for HCV

HCVpos_PWID <- HCV_DIAG_SENTINEL_srf %>% filter(RNA_all == "Positive") %>% filter(PWID == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF6 <- HCVpos_PWID %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as PWID who test positive for HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVpos_PWID %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as PWID who test positive for HCV" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF6 <- bind_rows(aggregation_tableSRF6, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF6)


##Loop using age group, sex, ethnic group, IMD - Patients who test positive for HCV and have also tested in prison

HCVpos_prison <- HCV_DIAG_SENTINEL_srf %>% filter(RNA_all == "Positive") %>% filter(testedinaprison == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF7 <- HCVpos_prison %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HCV who have also tested in prison" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVpos_prison %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HCV who have also tested in prison" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF7 <- bind_rows(aggregation_tableSRF7, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF7)


##Loop using age group, sex, ethnic group, IMD - Patients who test positive for HCV and have also tested in a drug service

HCVpos_drugservice <- HCV_DIAG_SENTINEL_srf %>% filter(RNA_all == "Positive") %>% filter(testedindrugservice == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF8 <- HCVpos_drugservice %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HCV who have also tested in a drug service" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVpos_drugservice %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HCV who have also tested in a drug service" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF8 <- bind_rows(aggregation_tableSRF8, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF8)


######new diagnoses######

##Loop using age group, sex, ethnic group, IMD - Patients flagged as homeless who are newly diagnosed with HCV

HCVnewdiag_homeless <- HCV_DIAG_SENTINEL_srf %>% filter(rna_positive_new == "New") %>% filter(homelessflag == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF9 <- HCVnewdiag_homeless %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as homeless and newly diagnosed with HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVnewdiag_homeless %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as homeless and newly diagnosed with HCV" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF9 <- bind_rows(aggregation_tableSRF9, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF9)


##Loop using age group, sex, ethnic group, IMD - Patients flagged as PWID who are newly diagnosed with HCV

HCVnewdiag_PWID <- HCV_DIAG_SENTINEL_srf %>% filter(rna_positive_new == "New") %>% filter(PWID == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF10 <- HCVnewdiag_PWID %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as PWID and newly diagnosed with HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVnewdiag_PWID %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as PWID and newly diagnosed with HCV" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF10 <- bind_rows(aggregation_tableSRF10, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF10)


##Loop using age group, sex, ethnic group, IMD - Patients who are newly diagnosed with HCV and have also tested in a prison

HCVnewdiag_prison <- HCV_DIAG_SENTINEL_srf %>% filter(rna_positive_new == "New") %>% filter(testedinaprison == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF11 <- HCVnewdiag_prison %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients newly diagnosed with HCV who have also tested in a prison" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVnewdiag_prison %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients newly diagnosed with HCV who have also tested in a prison" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF11 <- bind_rows(aggregation_tableSRF11, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF11)


##Loop using age group, sex, ethnic group, IMD - Patients who are newly diagnosed with HCV who have also tested in a drug service

HCVnewdiag_drugservice <- HCV_DIAG_SENTINEL_srf %>% filter(rna_positive_new == "New") %>% filter(testedindrugservice == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF12 <- HCVnewdiag_drugservice %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients newly diagnosed with HCV who have also tested in a drug service" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVnewdiag_drugservice %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients newly diagnosed with HCV who have also tested in a drug service" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF12 <- bind_rows(aggregation_tableSRF12, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF12)


##### HBV DATA #####

# Load the HBV diagnoses table from Sentinel
HBV_DIAGNOSES_SENTINEL_srf <- DBI::dbGetQuery(conn = Sentinel , statement = "
select * from SentinelEDApril22_HBV
where FirstHBVtestdate <= '2023-04-07'")

# format dates  
HBV_DIAGNOSES_SENTINEL_srf <- HBV_DIAGNOSES_SENTINEL_srf %>% 
  mutate(HBsAGdate =as.Date(HBsAGdate, format = '%Y-%m-%d'),
         HBV_DNAdate = as.Date(HBV_DNAdate, format = '%Y-%m-%d'),
         HBcIgMdate  = as.Date(HBcIgMdate , format = '%Y-%m-%d'),
         HBctotaldate  = as.Date(HBctotaldate, format = '%Y-%m-%d'),
         DOB = as.Date(DOB, format = '%Y-%m-%d'),
         FirstHBVtestdate = as.Date(FirstHBVtestdate, format = '%Y-%m-%d')) #%>% 
#replace(is.na(.), 0)


### add go live dates

HBV_DIAGNOSES_SENTINEL_srf<- HBV_DIAGNOSES_SENTINEL_srf%>%
  mutate(live_HBV = case_when ((Hospital ==  'Charing Cross Hospital' & FirstHBVtestdate >= '2022-07-01') 
                               |(Hospital == 'Chelsea & Westminster Hospital'& FirstHBVtestdate >= '2022-04-01')
                               |(Hospital == 'Croydon University Hospital'& FirstHBVtestdate >= '2023-03-20')
                               |(Hospital == 'Hillingdon Hospital'& FirstHBVtestdate >= '2022-07-22')
                               |(Hospital == 'Homerton University Hospital'& FirstHBVtestdate >= '2022-09-12')
                               |(Hospital == 'Kings College Hospital'& FirstHBVtestdate >= '2022-11-16')
                               |(Hospital == 'Kingston Hospital'& FirstHBVtestdate >= '2023-04-24')
                               |(Hospital == 'Newham General Hospital'& FirstHBVtestdate >= '2022-04-25')
                               |(Hospital == 'St Georges Hospital'& FirstHBVtestdate >= '2022-11-17')
                               |(Hospital == 'St Marys Hospital'& FirstHBVtestdate >= '2022-08-15')
                               |(Hospital == 'St Thomas Hospital'& FirstHBVtestdate >= '2022-11-01')
                               |(Hospital == 'The Royal London Hospital'& FirstHBVtestdate >= '2022-04-25')
                               |(Hospital == 'West Middlesex University Hospital'& FirstHBVtestdate >= '2022-04-01')
                               |(Hospital == 'Whipps Cross University Hospital'& FirstHBVtestdate >= '2022-04-25')
                               ~ "LIVE",
                               TRUE~ "NOT LIVE"))%>%
  filter(live_HBV == 'LIVE')

# Replace "15-24" with "16-24" in the age group column
HBV_DIAGNOSES_SENTINEL_srf$agegroup[HBV_DIAGNOSES_SENTINEL_srf$agegroup == "15-24"] <- "16-24"

# Modify the data frame to rename gender values
HBV_DIAGNOSES_SENTINEL_srf <- HBV_DIAGNOSES_SENTINEL_srf %>% 
  mutate(sex = recode(sex, "M" = "Men", "F" = "Women")) %>% 
  mutate(CombinedEthnicity = replace(CombinedEthnicity, CombinedEthnicity=="Mixed", "Mixed/Multiple"))

# Recode IMD and unknowns
HBV_DIAGNOSES_SENTINEL_srf <- HBV_DIAGNOSES_SENTINEL_srf %>% 
  mutate(IMD = replace(IMD,IMD== 1, "1")) %>% 
  mutate(IMD = replace(IMD,IMD== 2, "1")) %>% 
  mutate(IMD = replace(IMD,IMD== 3, "2")) %>%
  mutate(IMD = replace(IMD,IMD== 4, "2")) %>%
  mutate(IMD = replace(IMD,IMD== 5, "3")) %>%
  mutate(IMD = replace(IMD,IMD== 6, "3")) %>%
  mutate(IMD = replace(IMD,IMD== 7, "4")) %>%
  mutate(IMD = replace(IMD,IMD== 8, "4")) %>%
  mutate(IMD = replace(IMD,IMD== 9, "5")) %>%
  mutate(IMD = replace(IMD,IMD== 10, "5")) %>% 
  mutate(IMD = if_else(
    is.na(IMD), 
    "Unknown IMD", 
    IMD, 
    IMD),
    Gender = if_else(
      is.na(sex), 
      "Unknown Gender", 
      sex, 
      sex),
    CombinedEthnicity = if_else(
      (is.na(CombinedEthnicity) |
         CombinedEthnicity == "Unknown"), 
      "Unknown Ethnicity", 
      CombinedEthnicity,
      CombinedEthnicity)) %>% 
  mutate(All = case_when(age >= 0 ~ "All")) %>%
  mutate(agegroup = str_replace_all(agegroup, "-", " to "),
         agegroup = str_replace_all(agegroup, "80\\+", "80 and over"),
         agegroup = str_replace_all(agegroup, "(\\d+)(to)(\\d+)", "\\1 to \\3"))



##Loop using age group, sex, ethnic group, IMD - Patients flagged as homeless who test positive for HBV

HBVpos_homeless <- HBV_DIAGNOSES_SENTINEL_srf %>% filter(HBsAg == "Positive" & homelessflag == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF13 <- HBVpos_homeless %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as homeless and test positive for HBV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVpos_homeless %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as homeless and test positive for HBV" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF13 <- bind_rows(aggregation_tableSRF13, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF13)


##Loop using age group, sex, ethnic group, IMD - Patients flagged as PWID who test positive for HBV

HBVpos_PWID <- HBV_DIAGNOSES_SENTINEL_srf %>% filter(HBsAg == "Positive" & PWID == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF14 <- HBVpos_PWID %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as PWID who test positive for HBV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVpos_PWID %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as PWID who test positive for HBV" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF14 <- bind_rows(aggregation_tableSRF14, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF14)


##Loop using age group, sex, ethnic group, IMD - Patients who test positive for HBV and have also tested in prison

HBVpos_prison <- HBV_DIAGNOSES_SENTINEL_srf %>% filter(HBsAg == "Positive" & testedinaprison == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF15 <- HBVpos_prison %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HBV who have also tested in prison" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVpos_prison %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HBV who have also tested in prison" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF15 <- bind_rows(aggregation_tableSRF15, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF15)


##Loop using age group, sex, ethnic group, IMD - Patients who test positive for HBV and have also tested in a drug service

HBVpos_drugservice <- HBV_DIAGNOSES_SENTINEL_srf %>% filter(HBsAg == "Positive" & testedindrugservice == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF16 <- HBVpos_drugservice %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HBV who have also tested in a drug service" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVpos_drugservice %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HBV who have also tested in a drug service" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF16 <- bind_rows(aggregation_tableSRF16, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF16)


######new diagnoses######

##Loop using age group, sex, ethnic group, IMD - Patients flagged as homeless who are newly diagnosed with HBV

HBVnewdiag_homeless <- HBV_DIAGNOSES_SENTINEL_srf %>% filter(Newdiag == "New" & homelessflag == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF17 <- HBVnewdiag_homeless %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as homeless and newly diagnosed with HBV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVnewdiag_homeless %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as homeless and newly diagnosed with HBV" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF17 <- bind_rows(aggregation_tableSRF17, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF17)


##Loop using age group, sex, ethnic group, IMD - Patients flagged as PWID who are newly diagnosed with HBV

HBVnewdiag_PWID <- HBV_DIAGNOSES_SENTINEL_srf %>% filter(Newdiag == "New" & PWID == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF18 <- HBVnewdiag_PWID %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as PWID and newly diagnosed with HBV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVnewdiag_PWID %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as PWID and newly diagnosed with HBV" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF18 <- bind_rows(aggregation_tableSRF18, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF18)


##Loop using age group, sex, ethnic group, IMD - Patients who are newly diagnosed with HBV and have also tested in a prison

HBVnewdiag_prison <- HBV_DIAGNOSES_SENTINEL_srf %>% filter(Newdiag == "New" & testedinaprison == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF19 <- HBVnewdiag_prison %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients newly diagnosed with HBV who have also tested in a prison" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVnewdiag_prison %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients newly diagnosed with HBV who have also tested in a prison" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF19 <- bind_rows(aggregation_tableSRF19, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF19)


##Loop using age group, sex, ethnic group, IMD - Patients who are newly diagnosed with HBV who have also tested in a drug service

HBVnewdiag_drugservice <- HBV_DIAGNOSES_SENTINEL_srf %>% filter(Newdiag == "New" & testedindrugservice == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableSRF20 <- HBVnewdiag_drugservice %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients newly diagnosed with HBV who have also tested in a drug service" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVnewdiag_drugservice %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients newly diagnosed with HBV who have also tested in a drug service" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableSRF20 <- bind_rows(aggregation_tableSRF20, temp) 
  
}

#test 3 - look at table
View(aggregation_tableSRF20)

joined_tableSRF <- template_table %>% 
  left_join(aggregation_tableSRF5, by = "var") %>% 
  left_join(aggregation_tableSRF6, by = "var") %>% 
  left_join(aggregation_tableSRF7, by = "var") %>% 
  left_join(aggregation_tableSRF8, by = "var") %>% 
  left_join(aggregation_tableSRF9, by = "var") %>% 
  left_join(aggregation_tableSRF10, by = "var") %>% 
  left_join(aggregation_tableSRF11, by = "var") %>% 
  left_join(aggregation_tableSRF12, by = "var") %>% 
  left_join(aggregation_tableSRF13, by = "var") %>% 
  left_join(aggregation_tableSRF14, by = "var") %>% 
  left_join(aggregation_tableSRF15, by = "var") %>% 
  left_join(aggregation_tableSRF16, by = "var") %>% 
  left_join(aggregation_tableSRF17, by = "var") %>% 
  left_join(aggregation_tableSRF18, by = "var") %>% 
  left_join(aggregation_tableSRF19, by = "var") %>% 
  left_join(aggregation_tableSRF20, by = "var") %>%
  replace(is.na(.), 0) %>% 
  mutate(var = if_else(is.na(var), "Unknown Gender", var, "Unknown Gender"),
         breakdown = case_when(
           var == "All" ~ "Total",
           var %in% c(
             "Women",
             "Men",
             "Unknown Gender"
           ) ~ "Gender",
           var %in% c(
             "16 to 24",
             "25 to 34",
             "35 to 49",
             "50 to 64",
             "65 to 79",
             "80 and over",
             "Unknown Age"
           ) ~ "Age group",
           var %in% c(
             "Asian Other",
             "Asian: Indian, Pakistani or Bangladeshi",
             "Black African",
             "Black Caribbean",
             "Black Other",
             "Mixed/Multiple",
             "Other",
             "White British",
             "White Other",
             "Unknown Ethnicity"
           ) ~ "Ethnic group",
           var %in% c(
             "1",
             "2",
             "3",
             "4",
             "5",
             "Unknown IMD"
           ) ~ "IMD"
         )) %>%
  relocate(breakdown, .before = var) %>% 
  view()


write.csv(joined_tableSRF, "MasterTables_Table2SRF.csv")

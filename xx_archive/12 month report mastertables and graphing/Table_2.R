#Table 2 of MasterTables_export.csv

##Load packages
if (!require(tidyverse)) install.packAges("tidyverse")
if (!require(lubridate)) install.packAges("lubridate")
if (!require(readxl)) install.packAges("readxl")
if (!require(DBI)) install.packAges("DBI")
if (!require(odbc)) install.packAges("odbc")
if (!require(arrow)) install.packAges("arrow")

# load connection to Sentinel
Sentinel <- odbc::dbConnect(odbc::odbc(),
                            .connection_string = "driver={SQL Server};server=SQLClusColHFN17\\HFN17;
                             database=NIS_HepatitisSentinel;
                             Encrypt=true;trusted_connection=true",
                            timeout = 60,
                            timezone = Sys.timezone(),
                            timezone_out = Sys.timezone())


# Load the HCV diagnoses table from Sentinel
HCV_DIAG_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
select * from SentinelEDApril22_HCVnew
where FirstHCVtestdate <= '2023-04-07'AND (antiHCV like 'Positive' or HCVAg like 'Positive' or HCVRNA like 'Positive')
                                          and LIVE = 'Live'")

# lower case 
HCV_DIAG_SENTINEL <- mutate(HCV_DIAG_SENTINEL, antiHCV = tolower(antiHCV)) 
HCV_DIAG_SENTINEL <- mutate(HCV_DIAG_SENTINEL, HCVRNA = tolower(HCVRNA)) 
HCV_DIAG_SENTINEL <- mutate(HCV_DIAG_SENTINEL, HCVAg = tolower(HCVAg))

# Replace "15-24" with "16-24" in the age group column
HCV_DIAG_SENTINEL$agegroup[HCV_DIAG_SENTINEL$agegroup == "15-24"] <- "16-24"

# Rename gender variable in the sex column
HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(sex = replace(sex, sex=="1", "Men")) %>% 
  mutate(sex = replace(sex, sex=="2", "Women")) %>% 
  mutate(CombinedEthnicity = replace(CombinedEthnicity, CombinedEthnicity=="Mixed", "Mixed/Multiple")) %>% 
  mutate(CombinedEthnicity = replace(CombinedEthnicity, CombinedEthnicity=="Asian: Indian, Pakistani or Bangladeshi", "Indian, Pakistani or Bangladeshi"))

# Recode IMD and unknowns
HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
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


##Loop using age group, sex, ethnic group, IMD - Number of patients who test positive for antiHCV in all sites

attendees_antiHCV_all <- HCV_DIAG_SENTINEL %>% filter(antiHCV == "positive")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableH2 <- attendees_antiHCV_all %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for anti-HCV" = n)

## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_antiHCV_all %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for anti-HCV" = n)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableH2 <- bind_rows(aggregation_tableH2, temp) 
  
}

#test 3 - look at table
View(aggregation_tableH2)


##Loop using age group, sex, ethnic group, IMD - Patients receiving a confirmatory RNA test for HCV
attendees_HCVRNA_all <- HCV_DIAG_SENTINEL %>% filter(!is.na(RNA_all) & antiHCV == 'positive')


##Generate template table using one of the aggregation variables of the master table
aggregation_tableI2 <- attendees_HCVRNA_all %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who receive a confirmatory RNA test for HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HCVRNA_all %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who receive a confirmatory RNA test for HCV" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableI2 <- bind_rows(aggregation_tableI2, temp) 
  
}

#test 3 - look at table
View(aggregation_tableI2)




######INDICATOR 4B######

proportion4B <- aggregation_tableI2$"Number of patients who receive a confirmatory RNA test for HCV" / aggregation_tableH2$"Number of patients who test positive for anti-HCV"

indicator4B <- data.frame(var = aggregation_tableH2$var, proportion4B = proportion4B)%>% 
  rename("indicatorHCV_4B: HCV RNA testing for current infection" = proportion4B)

print(indicator4B)


#Loop using age group, sex, ethnic group, IMD - Number of patients with a confirmatory test for HCV done as reflex testing in all sites

attendees_HCVreflex_all <- HCV_DIAG_SENTINEL %>% filter(RNAtiming == "Reflex" & antiHCV == 'positive')

##Generate template table using one of the aggregation variables of the master table
aggregation_tableK2 <- attendees_HCVreflex_all %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients with a confirmatory test for HCV done as reflex testing in all sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HCVreflex_all %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients with a confirmatory test for HCV done as reflex testing in all sites" = n)

  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableK2 <- bind_rows(aggregation_tableK2, temp) 
  
}

#test 3 - look at table
View(aggregation_tableK2)



######INDICATOR 5######

proportion5 <- aggregation_tableK2$"Number of patients with a confirmatory test for HCV done as reflex testing in all sites" / aggregation_tableI2$"Number of patients who receive a confirmatory RNA test for HCV"

indicator5 <- data.frame(var = aggregation_tableH2$var, proportion5 = proportion5)%>% 
  rename("indicatorHCV_5: Proportion of HCV RNA tests done as reflex testing" = proportion5)

print(indicator5)


##Loop using age group, sex, ethnic group, IMD - Number of patients who test positive for HCV RNA

attendees_HCVRNApos_all <- HCV_DIAG_SENTINEL %>% filter(HCVAg == "positive"| HCVRNA == "positive")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableM2 <- attendees_HCVRNApos_all %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HCV RNA" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HCVRNApos_all %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HCV RNA" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableM2 <- bind_rows(aggregation_tableM2, temp) 
  
}

#test 3 - look at table
View(aggregation_tableM2)

#Loop using age group, sex, ethnic group, IMD - Number of patients who are newly diagnosed with HCV in all sites

attendees_HCVnew_all <- HCV_DIAG_SENTINEL %>% filter(rna_positive_new == "New")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableN2 <- attendees_HCVnew_all %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are newly diagnosed with HCV in all sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HCVnew_all %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are newly diagnosed with HCV in all sites" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableN2 <- bind_rows(aggregation_tableN2, temp) 
  
}

#test 3 - look at table
View(aggregation_tableN2)

######################COLUMN O2-P2############

# load connection to Sentinel
Sentinel <- odbc::dbConnect(odbc::odbc(),
                            .connection_string = "driver={SQL Server};server=SQLClusColHFN17\\HFN17;
                             database=NIS_HepatitisSentinel;
                             Encrypt=true;trusted_connection=true",
                            timeout = 60,
                            timezone = Sys.timezone(),
                            timezone_out = Sys.timezone())

# Load the HBV diagnoses table from Sentinel
HBV_DIAGNOSES_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
select * from SentinelEDApril22_HBV
where FirstHBVtestdate <= '2023-04-07'AND (HBsAg like 'Positive' or HBsAg like 'Reactive')")

# format dates  
HBV_DIAGNOSES_SENTINEL <- HBV_DIAGNOSES_SENTINEL %>% 
  mutate(HBsAGdate =as.Date(HBsAGdate, format = '%Y-%m-%d'),
         HBV_DNAdate = as.Date(HBV_DNAdate, format = '%Y-%m-%d'),
         HBcIgMdate  = as.Date(HBcIgMdate , format = '%Y-%m-%d'),
         HBctotaldate  = as.Date(HBctotaldate, format = '%Y-%m-%d'),
         DOB = as.Date(DOB, format = '%Y-%m-%d'),
         FirstHBVtestdate = as.Date(FirstHBVtestdate, format = '%Y-%m-%d')) #%>% 
         #replace(is.na(.), 0)


### add go live dates

HBV_DIAGNOSES_SENTINEL<- HBV_DIAGNOSES_SENTINEL%>%
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
HBV_DIAGNOSES_SENTINEL$agegroup[HBV_DIAGNOSES_SENTINEL$agegroup == "15-24"] <- "16-24"

# Modify the data frame to rename gender values
HBV_DIAGNOSES_SENTINEL <- HBV_DIAGNOSES_SENTINEL %>% 
  mutate(sex = recode(sex, "M" = "Men", "F" = "Women")) %>% 
  mutate(CombinedEthnicity = replace(CombinedEthnicity, CombinedEthnicity=="Mixed", "Mixed/Multiple"))

# Recode IMD and unknowns
HBV_DIAGNOSES_SENTINEL <- HBV_DIAGNOSES_SENTINEL %>% 
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

##Loop using age group, sex, ethnic group, IMD - Number of patients who test positive for HBV in all sites

attendees_HBVpos_all <- HBV_DIAGNOSES_SENTINEL %>% filter(HBsAg == "Positive")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableO2 <- attendees_HBVpos_all %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HBV in all sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HBVpos_all %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HBV in all sites" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableO2 <- bind_rows(aggregation_tableO2, temp) 
  
}

#test 3 - look at table
View(aggregation_tableO2)

##Loop using age group, sex, ethnic group, IMD - Number of patients who are newly diagnosed with HBV in all sites

attendees_HBVnew_all <- HBV_DIAGNOSES_SENTINEL %>% filter(Newdiag == "New")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableP2 <- attendees_HBVnew_all %>% 
  group_by(agegroup) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = agegroup) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are newly diagnosed with HBV in all sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "CombinedEthnicity", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HBVnew_all %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are newly diagnosed with HBV in all sites" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableP2 <- bind_rows(aggregation_tableP2, temp) 
  
}

#test 3 - look at table
View(aggregation_tableP2)



## to bind cols together at the end, we can use a left join
joined_table2 <- aggregation_tableH2 %>% 
  left_join(aggregation_tableI2, by = "var") %>% 
  left_join(indicator4B, by = "var") %>% 
  left_join(aggregation_tableK2, by = "var") %>% 
  left_join(indicator5, by = "var") %>% 
  left_join(aggregation_tableM2, by = "var") %>% 
  left_join(aggregation_tableN2, by = "var") %>% 
  left_join(aggregation_tableO2, by = "var") %>% 
  left_join(aggregation_tableP2, by = "var") %>% 
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
             "Black African",
             "Black Caribbean",
             "Black Other",
             "Indian, Pakistani or Bangladeshi",
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

write.csv(joined_table2, "hcv_update.csv")

## Adding in HIV data
hiv_data <- read_csv(
  "//COLHPAFIL003.HPA.org.uk/ProjectData/IMDATA/Hepatitis/Evaluations/ED opt out BBV testing evaluation/Analysis/hiv_indicator_tab.csv"
) %>%
  rename(var = Group,
         breakdown = "Breakdown type") %>%
  filter(breakdown %in% c("Age group",
                          "Ethnic group",
                          "IMD quintile",
                          "Gender",
                          "Total")) %>%
  mutate(breakdown = recode(breakdown, "IMD quintile" = "IMD"),
         var = (case_when(breakdown == "Gender" & var == "Not known" ~ "Unknown Gender",
                          breakdown == "Age group" & var == "Not known" ~ "Unknown Age",
                          breakdown == "Ethnic group" & var == "Not known" ~ "Unknown Ethnicity",
                          breakdown == "IMD" & var == "Not known" ~ "Unknown IMD",
                          var != "Not known" ~ var))) %>%
  select(breakdown, var, "Number of patients who test positive for HIV (all)", "Number of patients who test positive for HIV (2022 only)", "New diagnosis (2022 only)") %>%
  mutate(var = recode(var, "1 (most deprived)" = "1",
                      "5 (least deprived)" = "5",
                      "Asian: Indian, Pakistani or Bangladeshi" = "Indian, Pakistani or Bangladeshi",
                      "Mixed" = "Mixed/Multiple",
                      "16-24" = "16 to 24",
                      "25-34" = "25 to 34",
                      "35-49" = "35 to 49",
                      "50-64" = "50 to 64",
                      "65-79" = "65 to 79",
                      "80+" = "80 and over"))

# join with joined_table
joined_table2_hiv <- joined_table2 %>%
  left_join(hiv_data, by = c("breakdown", "var")) %>%
  rename(hiv_diags_full = 'Number of patients who test positive for HIV (all)',
         hiv_diags_n = 'Number of patients who test positive for HIV (2022 only)',
         new_hiv_diags_n = 'New diagnosis (2022 only)') %>%
  relocate(hiv_diags_full, .after = var) %>% 
  relocate(hiv_diags_n, .after = hiv_diags_full) %>% 
  relocate(new_hiv_diags_n, .after = hiv_diags_n) %>% 
  rename('Number of patients who test positive for HIV (all)' = hiv_diags_full,
  'Number of patients who test positive for HIV (2022 only)' = hiv_diags_n,
         'New HIV diagnosis (2022 only)' = new_hiv_diags_n)


write.csv(joined_table2_hiv, "MasterTables_table2.csv")





#load or install required packages
# Ensure the "pacman" package is installed
if (!require("pacman")) {
  install.packages("pacman") }

# Load necessary packages using pacman
pacman::p_load(
  rio,        # For data import/export
  here,       # For relative file paths
  skimr,      # For summarizing data
  janitor,    # For data cleaning
  lubridate,  # For date handling
  epikit,     # For epidemiological tools
  tidyverse,  # For data manipulation and visualization
  flextable,  # For creating pretty tables
  scales,     # For scaling functions
  gtsummary,  # For summary statistics and tables
  arrow,      # For data interoperability
  ggplot2,    # For data visualization
  rlang,      # For programming tools
  readxl,     # For reading Excel files
  writexl,    # For writing Excel files
  DBI,        # For database connections
  odbc,       # For ODBC connections
  purrr,      # For functional programming
  rmarkdown,  # For dynamic document generation
  ukhsacharts # For UKHSA chart formats
)

setwd(here("Risk factor analysis"))

# Establish ODBC connections -----------------------------------------------

#ESTABLISH ODBC CONNECTION WITH DATA
Y006 <- odbc::dbConnect(odbc::odbc(),
                        .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;
                             database=Y006_BBV_PID;
                             Encrypt=true;trusted_connection=true",
                        timeout = 60,
                        timezone = Sys.timezone(),
                        timezone_out = Sys.timezone())

##### load master template table to join SRFs data to #####
#NOTE TO USER: may need to update file path with the letter corresponding to your mapped drive
template_table <- read.csv("Y:/Evaluations/ED opt out BBV testing evaluation/Analysis/Template_table.csv")

##### SOCIAL RISK FACTORS ##### 
# load HIV attendees data from Sentinel
attendees_HIV_srf <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHIV]
WHERE (arrdate <= '2024-01-07')"
)

attendees_characteristics_HCV_srf <- attendees_characteristics(attendees_HCV_srf)
view(attendees_characteristics_HCV_srf)

# load HCV attendees data from Sentinel
attendees_HCV_srf <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHCV]
WHERE (arrdate <= '2024-01-07')"
)

# load HBV attendees data from Sentinel
attendees_HBV_srf <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHBV]
WHERE (arrdate <= '2024-01-07')"
)


attendees_HIV_srf <- attendees_HIV_srf %>% 
  mutate(IMD = if_else(
    is.na(IMD), 
    "Unknown IMD", 
    IMD, 
    IMD),
    Gender = if_else( # Replace Sex with Gender
      is.na(Sex), 
      "Unknown Gender", 
      Sex, 
      Sex),
    ethnic_group = if_else(
      (is.na(ethnic_group) |
         ethnic_group == "Unknown"), 
      "Unknown Ethnicity", 
      ethnic_group,
      ethnic_group)) %>% 
  mutate(All = case_when(age >= 0 ~ "All")) %>%
  mutate(Gender = replace(Gender, Gender=="FEMALE", "Women")) %>% # Replace Female
  mutate(Gender = replace(Gender, Gender=="MALE", "Men")) %>%    # Replace Male
  #replace(is.na(.), 0) %>%
  mutate(age_group = str_replace_all(age_group, "-", " to "),
         age_group = str_replace_all(age_group, "80\\+", "80 and over"),
         age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>%
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group))

attendees_HCV_srf <- attendees_HCV_srf %>% 
  mutate(IMD = if_else(
    is.na(IMD), 
    "Unknown IMD", 
    IMD, 
    IMD),
    Gender = if_else( # Replace Sex with Gender
      is.na(Sex), 
      "Unknown Gender", 
      Sex, 
      Sex),
    ethnic_group = if_else(
      (is.na(ethnic_group) |
         ethnic_group == "Unknown"), 
      "Unknown Ethnicity", 
      ethnic_group,
      ethnic_group)) %>% 
  mutate(All = case_when(age >= 0 ~ "All")) %>%
  mutate(Gender = replace(Gender, Gender=="FEMALE", "Women")) %>% # Replace Female
  mutate(Gender = replace(Gender, Gender=="MALE", "Men")) %>%    # Replace Male
  # replace(is.na(.), 0) %>%
  mutate(age_group = str_replace_all(age_group, "-", " to "),
         age_group = str_replace_all(age_group, "80\\+", "80 and over"),
         age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>%
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group))

attendees_HBV_srf <- attendees_HBV_srf %>% 
  mutate(IMD = if_else(
    is.na(IMD), 
    "Unknown IMD", 
    IMD, 
    IMD),
    Gender = if_else( # Replace Sex with Gender
      is.na(Sex), 
      "Unknown Gender", 
      Sex, 
      Sex),
    ethnic_group = if_else(
      (is.na(ethnic_group) |
         ethnic_group == "Unknown"), 
      "Unknown Ethnicity", 
      ethnic_group,
      ethnic_group)) %>% 
  mutate(All = case_when(age >= 0 ~ "All")) %>%
  mutate(Gender = replace(Gender, Gender=="FEMALE", "Women")) %>% # Replace Female
  mutate(Gender = replace(Gender, Gender=="MALE", "Men")) %>%    # Replace Male
  # replace(is.na(.), 0) %>%
  mutate(age_group = str_replace_all(age_group, "-", " to "),
         age_group = str_replace_all(age_group, "80\\+", "80 and over"),
         age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>%
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group))

##Loop using age group, sex, ethnic group, IMD - Patients flagged as homeless who test positive for HCV
HCVpos_homeless <- attendees_HCV_srf %>% filter(HCVAb == "Positive" | HCVPCR == "Positive") %>% filter(homelessflag == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf1 <- HCVpos_homeless %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as homeless and test positive for HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVpos_homeless %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as homeless and test positive for HCV" = n)
  
  # Replace NA values with 0 in the temporary data frame
 # temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf1 <- bind_rows(aggregation_tablesrf1, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf1)


##Loop using age group, sex, ethnic group, IMD - Patients flagged as PWID who test positive for HCV
HCVpos_PWID <- attendees_HCV_srf %>% filter(HCVAb == "Positive" | HCVPCR == "Positive") %>% filter(PWID_2 == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf2 <- HCVpos_PWID %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as PWID who test positive for HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVpos_PWID %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as PWID who test positive for HCV" = n)
  
  # Replace NA values with 0 in the temporary data frame
#  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf2 <- bind_rows(aggregation_tablesrf2, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf2)


##Loop using age group, sex, ethnic group, IMD - Patients who test positive for HCV and have also tested in prison
HCVpos_prison <- attendees_HCV_srf %>% filter(HCVAb == "Positive" | HCVPCR == "Positive") %>% filter(testedinaprison == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf3 <- HCVpos_prison %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HCV who have also tested in prison" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVpos_prison %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HCV who have also tested in prison" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf3 <- bind_rows(aggregation_tablesrf3, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf3)

##Loop using age group, sex, ethnic group, IMD - Patients who test positive for HCV and have also tested in a drug service
HCVpos_drugservice <- attendees_HCV_srf %>% filter(HCVAb == "Positive" | HCVPCR == "Positive") %>% filter(testedindrugservice == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf4 <- HCVpos_drugservice %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HCV who have also tested in a drug service" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVpos_drugservice %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HCV who have also tested in a drug service" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf4 <- bind_rows(aggregation_tablesrf4, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf4)

##Loop using age group, sex, ethnic group, IMD - Patients who test positive for HCV and also have overseas charging status recorded
HCVpos_overseas <- attendees_HCV_srf %>% filter(HCVAb == "Positive" | HCVPCR == "Positive") %>% filter(Overseascharging == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf17 <- HCVpos_overseas %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HCV who also have overseas charging status recorded" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVpos_overseas %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HCV who also have overseas charging status recorded" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf17 <- bind_rows(aggregation_tablesrf17, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf17)

######new diagnoses######

##Loop using age group, sex, ethnic group, IMD - Patients flagged as homeless who are newly diagnosed with HCV
HCVnewdiag_homeless <- attendees_HCV_srf %>% filter(rna_positive_new == "New") %>% filter(homelessflag == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf5 <- HCVnewdiag_homeless %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as homeless and newly diagnosed with HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVnewdiag_homeless %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as homeless and newly diagnosed with HCV" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf5 <- bind_rows(aggregation_tablesrf5, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf5)

##Loop using age group, sex, ethnic group, IMD - Patients flagged as PWID who are newly diagnosed with HCV
HCVnewdiag_PWID <- attendees_HCV_srf %>% filter(rna_positive_new == "New") %>% filter(PWID_2 == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf6 <- HCVnewdiag_PWID %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as PWID and newly diagnosed with HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVnewdiag_PWID %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as PWID and newly diagnosed with HCV" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf6 <- bind_rows(aggregation_tablesrf6, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf6)


##Loop using age group, sex, ethnic group, IMD - Patients who are newly diagnosed with HCV and have also tested in a prison
HCVnewdiag_prison <- attendees_HCV_srf %>% filter(rna_positive_new == "New") %>% filter(testedinaprison == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf7 <- HCVnewdiag_prison %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients newly diagnosed with HCV who have also tested in a prison" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVnewdiag_prison %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients newly diagnosed with HCV who have also tested in a prison" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf7 <- bind_rows(aggregation_tablesrf7, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf7)


##Loop using age group, sex, ethnic group, IMD - Patients who are newly diagnosed with HCV who have also tested in a drug service
HCVnewdiag_drugservice <- attendees_HCV_srf %>% filter(rna_positive_new == "New") %>% filter(testedindrugservice == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf8 <- HCVnewdiag_drugservice %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients newly diagnosed with HCV who have also tested in a drug service" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVnewdiag_drugservice %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients newly diagnosed with HCV who have also tested in a drug service" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf8 <- bind_rows(aggregation_tablesrf8, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf8)

##Loop using age group, sex, ethnic group, IMD - Patients who are newly diagnosed with HCV and also have overseas charging status recorded
HCVnewdiag_overseas <- attendees_HCV_srf %>% filter(rna_positive_new == "New") %>% filter(Overseascharging == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf18 <- HCVnewdiag_overseas %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients newly diagnosed with HCV who also have overseas charging status recorded" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVnewdiag_overseas %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients newly diagnosed with HCV who also have overseas charging status recorded" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf18 <- bind_rows(aggregation_tablesrf18, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf18)


#HBV
##Loop using age group, sex, ethnic group, IMD - Patients flagged as homeless who test positive for HBV
HBVpos_homeless <- attendees_HBV_srf %>% filter(HBVAg == "Positive") %>% filter(homelessflag == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf9 <- HBVpos_homeless %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as homeless and test positive for HBV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVpos_homeless %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as homeless and test positive for HBV" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf9 <- bind_rows(aggregation_tablesrf9, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf9)

##Loop using age group, sex, ethnic group, IMD - Patients flagged as PWID who test positive for HBV
HBVpos_PWID <- attendees_HBV_srf %>% filter(HBVAg == "Positive") %>% filter(PWID_2 == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf10 <- HBVpos_PWID %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as PWID who test positive for HBV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVpos_PWID %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as PWID who test positive for HBV" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf10 <- bind_rows(aggregation_tablesrf10, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf10)

##Loop using age group, sex, ethnic group, IMD - Patients who test positive for HBV and have also tested in prison
HBVpos_prison <- attendees_HBV_srf %>% filter(HBVAg == "Positive") %>% filter(testedinaprison == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf11 <- HBVpos_prison %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HBV who have also tested in prison" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HCVpos_prison %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HBV who have also tested in prison" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf11 <- bind_rows(aggregation_tablesrf11, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf11)


##Loop using age group, sex, ethnic group, IMD - Patients who test positive for HBV and have also tested in a drug service
HBVpos_drugservice <- attendees_HBV_srf %>% filter(HBVAg == "Positive") %>% filter(testedindrugservice == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf12 <- HBVpos_drugservice %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HBV who have also tested in a drug service" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVpos_drugservice %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HBV who have also tested in a drug service" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf12 <- bind_rows(aggregation_tablesrf12, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf12)

##Loop using age group, sex, ethnic group, IMD - Patients who test positive for HBV and also have overseas charging status recorded
HBVpos_overseas <- attendees_HBV_srf %>% filter(HBVAg == "Positive") %>% filter(Overseascharging == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf19 <- HBVpos_overseas %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HBV who also have overseas charging status recorded" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVpos_overseas %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HBV who also have overseas charging status recorded" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf19 <- bind_rows(aggregation_tablesrf19, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf19)


######new diagnoses######

##Loop using age group, sex, ethnic group, IMD - Patients flagged as homeless who are newly diagnosed with HBV
HBVnewdiag_homeless <- attendees_HBV_srf %>% filter(HBsAg_positive_new == "New") %>% filter(homelessflag == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf13 <- HBVnewdiag_homeless %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as homeless and newly diagnosed with HBV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVnewdiag_homeless %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as homeless and newly diagnosed with HBV" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf13 <- bind_rows(aggregation_tablesrf13, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf13)

##Loop using age group, sex, ethnic group, IMD - Patients flagged as PWID who are newly diagnosed with HBV
HBVnewdiag_PWID <- attendees_HBV_srf %>% filter(HBsAg_positive_new == "New") %>% filter(PWID_2 == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf14 <- HBVnewdiag_PWID %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are flagged as PWID and newly diagnosed with HBV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVnewdiag_PWID %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are flagged as PWID and newly diagnosed with HBV" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf14 <- bind_rows(aggregation_tablesrf14, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf14)

##Loop using age group, sex, ethnic group, IMD - Patients who are newly diagnosed with HBV and have also tested in a prison
HBVnewdiag_prison <- attendees_HBV_srf %>% filter(HBsAg_positive_new == "New") %>% filter(testedinaprison == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf15 <- HBVnewdiag_prison %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients newly diagnosed with HBV who have also tested in a prison" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVnewdiag_prison %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients newly diagnosed with HBV who have also tested in a prison" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf15 <- bind_rows(aggregation_tablesrf15, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf15)

##Loop using age group, sex, ethnic group, IMD - Patients who are newly diagnosed with HBV who have also tested in a drug service
HBVnewdiag_drugservice <- attendees_HBV_srf %>% filter(HBsAg_positive_new == "New") %>% filter(testedindrugservice == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf16 <- HBVnewdiag_drugservice %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients newly diagnosed with HBV who have also tested in a drug service" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVnewdiag_drugservice %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients newly diagnosed with HBV who have also tested in a drug service" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf16 <- bind_rows(aggregation_tablesrf16, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf16)

##Loop using age group, sex, ethnic group, IMD - Patients who are newly diagnosed with HBV who have also tested in a drug service
HBVnewdiag_overseas <- attendees_HBV_srf %>% filter(HBsAg_positive_new == "New") %>% filter(Overseascharging == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tablesrf20 <- HBVnewdiag_overseas %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients newly diagnosed with HBV who also have overseas charging status recorded" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# # Define a function to replace NA values with 0 in a data frame
# replace_na_with_zero <- function(df) {
#   df %>% mutate_all(~ ifelse(is.na(.), 0, .))
# }

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- HBVnewdiag_overseas %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients newly diagnosed with HBV who also have overseas charging status recorded" = n)
  
  # Replace NA values with 0 in the temporary data frame
  #temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tablesrf20 <- bind_rows(aggregation_tablesrf20, temp) 
  
}

#test 3 - look at table
View(aggregation_tablesrf20)


joined_tablesrf <- template_table %>% 
  left_join(aggregation_tablesrf1, by = "var") %>% 
  left_join(aggregation_tablesrf2, by = "var") %>% 
  left_join(aggregation_tablesrf3, by = "var") %>% 
  left_join(aggregation_tablesrf4, by = "var") %>% 
  left_join(aggregation_tablesrf5, by = "var") %>% 
  left_join(aggregation_tablesrf6, by = "var") %>% 
  left_join(aggregation_tablesrf7, by = "var") %>% 
  left_join(aggregation_tablesrf8, by = "var") %>% 
  left_join(aggregation_tablesrf9, by = "var") %>% 
  left_join(aggregation_tablesrf10, by = "var") %>% 
  left_join(aggregation_tablesrf11, by = "var") %>% 
  left_join(aggregation_tablesrf12, by = "var") %>% 
  left_join(aggregation_tablesrf13, by = "var") %>% 
  left_join(aggregation_tablesrf14, by = "var") %>% 
  left_join(aggregation_tablesrf15, by = "var") %>% 
  left_join(aggregation_tablesrf16, by = "var") %>%
  left_join(aggregation_tablesrf17, by = "var") %>% 
  left_join(aggregation_tablesrf18, by = "var") %>% 
  left_join(aggregation_tablesrf19, by = "var") %>% 
  left_join(aggregation_tablesrf20, by = "var") %>% 
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


write.csv(joined_tablesrf, "MasterTables_Table1srf_OAC.csv")





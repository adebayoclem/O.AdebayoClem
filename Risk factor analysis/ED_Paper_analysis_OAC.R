
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

# Establish ODBC connections -----------------------------------------------

#ESTABLISH ODBC CONNECTION WITH DATA
Y006 <- odbc::dbConnect(odbc::odbc(),
                        .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;
                             database=Y006_BBV_PID;
                             Encrypt=true;trusted_connection=true",
                        timeout = 60,
                        timezone = Sys.timezone(),
                        timezone_out = Sys.timezone())


# load HIV attendees data from Sentinel
attendees_HIV <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHIV]
WHERE (arrdate <= '2024-01-07')"
)

# load HCV attendees data from Sentinel
attendees_HCV <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHCV]
WHERE (arrdate <= '2024-01-07')"
)

# load HBV attendees data from Sentinel
attendees_HBV <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHBV]
WHERE (arrdate <= '2024-01-07')"
)


attendees_HIV <- attendees_HIV %>% 
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
  mutate(age_group = str_replace_all(age_group, "-", " to "),
         age_group = str_replace_all(age_group, "80\\+", "80 and over"),
         age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>%
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group))

attendees_HCV <- attendees_HCV %>% 
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
  mutate(age_group = str_replace_all(age_group, "-", " to "),
         age_group = str_replace_all(age_group, "80\\+", "80 and over"),
         age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>%
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group))


attendees_HBV <- attendees_HBV %>% 
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
  mutate(age_group = str_replace_all(age_group, "-", " to "),
         age_group = str_replace_all(age_group, "80\\+", "80 and over"),
         age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>%
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group))


######COLUMNS C-E#######

##Loop using age group, sex, ethnic group, IMD - HIV ATTENDEES

##Generate template table using one of the aggregation variables of the master table
aggregation_tableC <- attendees_HIV %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients attending ED (HIV go live dates)" = n)

## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}


#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HIV %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients attending ED (HIV go live dates)" = n)
  
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableC <- bind_rows(aggregation_tableC, temp)
  
  
}

#test 3 - look at table
View(aggregation_tableC)

######COLUMNS F-H#######

##Loop using age group, sex, ethnic group, IMD - HCV ATTENDEES

##Generate template table using one of the aggregation variables of the master table
aggregation_tableD <- attendees_HCV %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients attending ED (HCV go live dates)" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HCV %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients attending ED (HCV go live dates)" = n)
  
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableD <- bind_rows(aggregation_tableD, temp) 
  
}

#test 3 - look at table
View(aggregation_tableD)


##Loop using age group, sex, ethnic group, IMD - HBV ATTENDEES

##Generate template table using one of the aggregation variables of the master table
aggregation_tableE <- attendees_HBV %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients attending ED (HBV go live dates)" = n)

## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HBV %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients attending ED (HBV go live dates)" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableE <- bind_rows(aggregation_tableE, temp) 
  
}

#test 3 - look at table
View(aggregation_tableE)


#number of patients attending ED who have a blood test (HIV go live dates)
attendees_bloodsHIV <- attendees_HIV %>% filter(ECDS_bloods_any == "Yes")

##Loop using age group, sex, ethnic group, IMD - ATTENDEES WITH A BLOOD TEST (HIV GO LIVE DATES)

##Generate template table using one of the aggregation variables of the master table
aggregation_tableF <- attendees_bloodsHIV %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients attending ED who have a blood test (HIV go live dates)" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}


#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_bloodsHIV %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients attending ED who have a blood test (HIV go live dates)" = n)
  
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableF <- bind_rows(aggregation_tableF, temp) 
  
}

#test 3 - look at table
View(aggregation_tableF)


#number of patients attending ED who have a blood test (HCV go live dates)
attendees_bloods_HCVdates <- attendees_HCV %>% filter(ECDS_bloods_any == "Yes")

##Loop using age group, sex, ethnic group, IMD - ATTENDEES WITH A BLOOD TEST (HCV GO LIVE DATES)

##Generate template table using one of the aggregation variables of the master table
aggregation_tableG <- attendees_bloods_HCVdates %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients attending ED who have a blood test (HCV go live dates)" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_bloods_HCVdates %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients attending ED who have a blood test (HCV go live dates)" = n)
  
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableG <- bind_rows(aggregation_tableG, temp) 
  
}

#test 3 - look at table
View(aggregation_tableG)


#number of patients attending ED who have a blood test (HBV go live dates)
attendees_bloods_HBVdates <- attendees_HBV %>% filter(ECDS_bloods_any == "Yes")

##Loop using age group, sex, ethnic group, IMD - ATTENDEES WITH A BLOOD TEST (HBV GO LIVE DATES)

##Generate template table using one of the aggregation variables of the master table
aggregation_tableH <- attendees_bloods_HBVdates %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients attending ED who have a blood test (HBV go live dates)" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_bloods_HBVdates %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients attending ED who have a blood test (HBV go live dates)" = n)
  
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableH <- bind_rows(aggregation_tableH, temp) 
  
}

#test 3 - look at table
View(aggregation_tableH)

######INDICATOR 1A######

# Calculate proportion1A, treating NA as 0 in both numerator and denominator
numerator1A <- ifelse(
  is.na(aggregation_tableF$"Number of patients attending ED who have a blood test (HIV go live dates)"),
  0,
  aggregation_tableF$"Number of patients attending ED who have a blood test (HIV go live dates)"
)

denominator1A <- ifelse(
  is.na(aggregation_tableC$"Number of patients attending ED (HIV go live dates)"),
  0,
  aggregation_tableC$"Number of patients attending ED (HIV go live dates)"
)

proportion1A <- numerator1A / denominator1A

# Create the indicator1A data frame
indicatorHIV_1A <- data.frame(var = aggregation_tableC$var, proportion1A = proportion1A) %>%
  rename("indicatorHIV_1A: Proportion of patients attending ED who have a blood test" = proportion1A)

print(indicatorHIV_1A)


######COLUMNS J-K#######

# load attendances data from Sentinel
attendances <- DBI::dbGetQuery(conn = Y006, statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[24monthECDSattendances_includedsites]
WHERE (ARRIVAL_DATE <= '2024-01-07')"
)

attendances <- attendances %>% 
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
  mutate(age_group = str_replace_all(age_group, "-", " to "),
         age_group = str_replace_all(age_group, "80\\+", "80 and over"),
         age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>% 
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group))

print(attendances)

# Number of attendances to ED (HIV go live dates)
attendances_HIVdates <- attendances %>% 
  filter(SITE =="RQXM1" | SITE == "R1HNH" | SITE == "RJ122"| SITE == "R1H12" | SITE=="R1HKH") %>% 
  filter(live_HIV == "Live")

##Loop using age group, sex, ethnic group, IMD - ATTENDANCES (HIV GO LIVE DATES)

##Generate template table using one of the aggregation variables of the master table
aggregation_tableJ <- attendances_HIVdates %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of attendances to ED (HIV go live dates)" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")


# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendances_HIVdates %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of attendances to ED (HIV go live dates)" = n)
  
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableJ <- bind_rows(aggregation_tableJ, temp) 
  
}

#test 3 - look at table
View(aggregation_tableJ)


# Number of attendances to ED w/ blood test
attendances_bloods <- attendances %>% 
  filter(SITE =="RQXM1" | SITE == "R1HNH" | SITE == "RJ122"| SITE == "R1H12" | SITE=="R1HKH") %>% 
  filter(ECDS_bloods_any == "bloods_any")

##Loop using age group, sex, ethnic group, IMD - ATTENDANCES WITH BLOOD TESTS

##Generate template table using one of the aggregation variables of the master table
aggregation_tableK <- attendances_bloods %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of attendances where a blood test was carried out" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendances_bloods %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of attendances where a blood test was carried out" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableK <- bind_rows(aggregation_tableK, temp) 
  
}

#test 3 - look at table
View(aggregation_tableK)

######INDICATOR 1B######

# Calculate proportion1B, treating NA as 0 in both numerator and denominator
numerator1B <- ifelse(
  is.na(aggregation_tableK$"Number of attendances where a blood test was carried out"),
  0,
  aggregation_tableK$"Number of attendances where a blood test was carried out"
)

denominator1B <- ifelse(
  is.na(aggregation_tableJ$"Number of attendances to ED (HIV go live dates)"),
  0,
  aggregation_tableJ$"Number of attendances to ED (HIV go live dates)"
)

proportion1B <- numerator1B / denominator1B

# Create the indicator1B data frame
indicatorHIV_1B <- data.frame(var = aggregation_tableK$var, proportion1B = proportion1B) %>%
  rename("indicatorHIV_1B: Proportion of attendances where a blood test was carried out" = proportion1B)

print(indicatorHIV_1B)




######COLUMNS M, N, P, R#######

##Loop using age group, sex, ethnic group, IMD - Number of patients who have a blood test and HIV test

attendees_bloodsHIV <- attendees_HIV %>% filter(ECDS_bloods_any == "Yes" & HIV == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableM <- attendees_bloodsHIV %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who have a blood test and HIV test" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_bloodsHIV %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who have a blood test and HIV test" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableM <- bind_rows(aggregation_tableM, temp) 
  
}

#test 3 - look at table
View(aggregation_tableM)


##Loop using age group, sex, ethnic group, IMD - Number of patients who have a blood test and HIV test (UP TO DEC 22)

attendees_bloodsHIV_dec22 <- attendees_HIV %>% filter(ECDS_bloods_any == "Yes" & HIV == "Yes" & arrdate <= "2022-12-31")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableN <- attendees_bloodsHIV_dec22 %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who have a blood test and HIV test (up to Dec 22)" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_bloodsHIV_dec22 %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who have a blood test and HIV test (up to Dec 22)" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableN <- bind_rows(aggregation_tableN, temp) 
  
}

#test 3 - look at table
View(aggregation_tableN)




##Loop using age group, sex, ethnic group, IMD - Number of patients who have a blood test and HCV test

attendees_bloodsHCV <- attendees_HCV %>% filter(ECDS_bloods_any == "Yes" & HCV == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableP <- attendees_bloodsHCV %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who have a blood test and HCV test" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_bloodsHCV %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who have a blood test and HCV test" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableP <- bind_rows(aggregation_tableP, temp) 
  
}

#test 3 - look at table
View(aggregation_tableP)



##Loop using age group, sex, ethnic group, IMD - Number of patients who have a blood test and HBV test

attendees_bloodsHBV <- attendees_HBV %>% filter(ECDS_bloods_any == "Yes" & HBV == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableR <- attendees_bloodsHBV %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who have a blood test and HBV test" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_bloodsHBV %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who have a blood test and HBV test" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableR <- bind_rows(aggregation_tableR, temp) 
  
}

#test 3 - look at table
View(aggregation_tableR)


######INDICATOR 2A######

# Calculate proportion2A, treating NA as 0 in both numerator and denominator
numerator2A <- ifelse(
  is.na(aggregation_tableM$"Number of patients who have a blood test and HIV test"),
  0,
  aggregation_tableM$"Number of patients who have a blood test and HIV test"
)

denominator2A <- ifelse(
  is.na(aggregation_tableF$"Number of patients attending ED who have a blood test (HIV go live dates)"),
  0,
  aggregation_tableF$"Number of patients attending ED who have a blood test (HIV go live dates)"
)

proportion2A <- numerator2A / denominator2A

# Create the indicator2A data frame
indicatorHIV_2A <- data.frame(var = aggregation_tableC$var, proportion2A = proportion2A) %>%
  rename("indicatorHIV_2A: Proportion of patients with a blood test who have an HIV test" = proportion2A)

print(indicatorHIV_2A)



######INDICATOR 2B######

# Calculate proportion2B, treating NA as 0 in both numerator and denominator
numerator2B <- ifelse(
  is.na(aggregation_tableP$"Number of patients who have a blood test and HCV test"),
  0,
  aggregation_tableP$"Number of patients who have a blood test and HCV test"
)

denominator2B <- ifelse(
  is.na(aggregation_tableG$"Number of patients attending ED who have a blood test (HCV go live dates)"),
  0,
  aggregation_tableG$"Number of patients attending ED who have a blood test (HCV go live dates)"
)

proportion2B <- numerator2B / denominator2B

# Create the indicator2B data frame
indicatorHCV_2B <- data.frame(var = aggregation_tableC$var, proportion2B = proportion2B) %>%
  rename("indicatorHCV_2B: Proportion of patients with a blood test who have an HCV test" = proportion2B)

print(indicatorHCV_2B)



######INDICATOR 2C######

# Calculate proportion2C, treating NA as 0 in both numerator and denominator
numerator2C <- ifelse(
  is.na(aggregation_tableR$"Number of patients who have a blood test and HBV test"),
  0,
  aggregation_tableR$"Number of patients who have a blood test and HBV test"
)

denominator2C <- ifelse(
  is.na(aggregation_tableH$"Number of patients attending ED who have a blood test (HBV go live dates)"),
  0,
  aggregation_tableH$"Number of patients attending ED who have a blood test (HBV go live dates)"
)

proportion2C <- numerator2C / denominator2C

# Create the indicator2C data frame
indicatorHBV_2C <- data.frame(var = aggregation_tableC$var, proportion2C = proportion2C) %>%
  rename("indicatorHBV_2C: Proportion of patients with a blood test who have an HBV test" = proportion2C)

print(indicatorHBV_2C)




######COLUMNS T, V, X#######

##Loop using age group, sex, ethnic group, IMD - Number of all attendees who have an HIV test

attendees_HIVtest <- attendees_HIV %>% filter(HIV == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableT <- attendees_HIVtest %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of all attendees who have an HIV test" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HIVtest %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of all attendees who have an HIV test" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableT <- bind_rows(aggregation_tableT, temp) 
  
}

#test 3 - look at table
View(aggregation_tableT)

##Loop using age group, sex, ethnic group, IMD - Number of all attendees who have an HCV test

attendees_HCVtest <- attendees_HCV %>% filter(HCV == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableV <- attendees_HCVtest %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of all attendees who have an HCV test" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HCVtest %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of all attendees who have an HCV test" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableV <- bind_rows(aggregation_tableV, temp) 
  
}

#test 3 - look at table
View(aggregation_tableV)


##Loop using age group, sex, ethnic group, IMD - Number of all attendees who have an HBV test

attendees_HBVtest <- attendees_HBV %>% filter(HBV == "Yes")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableX <- attendees_HBVtest %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of all attendees who have an HBV test" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HBVtest %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of all attendees who have an HBV test" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableX <- bind_rows(aggregation_tableX, temp) 
  
}

#test 3 - look at table
View(aggregation_tableX)


######INDICATOR 3A######

# Calculate proportion3A, treating NA as 0 in both numerator and denominator
numerator3A <- ifelse(
  is.na(aggregation_tableT$"Number of all attendees who have an HIV test"),
  0,
  aggregation_tableT$"Number of all attendees who have an HIV test"
)

denominator3A <- ifelse(
  is.na(aggregation_tableC$"Number of patients attending ED (HIV go live dates)"),
  0,
  aggregation_tableC$"Number of patients attending ED (HIV go live dates)"
)

proportion3A <- numerator3A / denominator3A

# Create the indicator3A data frame
indicatorHIV_3A <- data.frame(var = aggregation_tableC$var, proportion3A = proportion3A) %>%
  rename("indicatorHIV_3A: Proportion of all attendees who have an HIV test" = proportion3A)

print(indicatorHIV_3A)

######INDICATOR 3B######

# Calculate proportion3B, treating NA as 0 in both numerator and denominator
numerator3B <- ifelse(
  is.na(aggregation_tableV$"Number of all attendees who have an HCV test"),
  0,
  aggregation_tableV$"Number of all attendees who have an HCV test"
)

denominator3B <- ifelse(
  is.na(aggregation_tableD$"Number of patients attending ED (HCV go live dates)"),
  0,
  aggregation_tableD$"Number of patients attending ED (HCV go live dates)"
)

proportion3B <- numerator3B / denominator3B

# Create the indicator3B data frame
indicatorHCV_3B <- data.frame(var = aggregation_tableC$var, proportion3B = proportion3B) %>%
  rename("indicatorHCV_3B: Proportion of all attendees who have an HCV test" = proportion3B)

print(indicatorHCV_3B)


######INDICATOR 3C######

# Calculate proportion3C, treating NA as 0 in both numerator and denominator
numerator3C <- ifelse(
  is.na(aggregation_tableX$"Number of all attendees who have an HBV test"),
  0,
  aggregation_tableX$"Number of all attendees who have an HBV test"
)

denominator3C <- ifelse(
  is.na(aggregation_tableE$"Number of patients attending ED (HBV go live dates)"),
  0,
  aggregation_tableE$"Number of patients attending ED (HBV go live dates)"
)

proportion3C <- numerator3C / denominator3C

# Create the indicator3C data frame
indicatorHBV_3C <- data.frame(var = aggregation_tableC$var, proportion3C = proportion3C) %>%
  rename("indicatorHBV_3C: Proportion of all attendees who have an HBV test" = proportion3C)

print(indicatorHBV_3C)



##Loop using age group, sex, ethnic group, IMD - Number of patients who test positive for antiHCV

attendees_antiHCV <- attendees_HCV %>% filter(HCVAb == "Positive" | HCVPCR == "Positive")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableZ <- attendees_antiHCV %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_antiHCV %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HCV" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableZ <- bind_rows(aggregation_tableZ, temp) 
  
}

#test 3 - look at table
View(aggregation_tableZ)

##Loop using age group, sex, ethnic group, IMD - Patients receiving a confirmatory RNA test for HCV

attendees_HCVRNA <- attendees_HCV %>% filter(HCVAb == "Positive" & !is.na(HCVPCR))

##Generate template table using one of the aggregation variables of the master table
aggregation_tableAA <- attendees_HCVRNA %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who receive a confirmatory RNA test for HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HCVRNA %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who receive a confirmatory RNA test for HCV" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableAA <- bind_rows(aggregation_tableAA, temp) 
  
}

#test 3 - look at table
View(aggregation_tableAA)


######INDICATOR 4B######

# Calculate proportion4B, treating NA as 0 in both numerator and denominator
numerator4B <- ifelse(
  is.na(aggregation_tableAA$"Number of patients who receive a confirmatory RNA test for HCV"),
  0,
  aggregation_tableAA$"Number of patients who receive a confirmatory RNA test for HCV"
)

denominator4B <- ifelse(
  is.na(aggregation_tableZ$"Number of patients who test positive for anti-HCV"),
  0,
  aggregation_tableZ$"Number of patients who test positive for anti-HCV"
)

proportion4B <- numerator4B / denominator4B

# Create the indicator4B data frame
indicatorHCV_4B <- data.frame(var = aggregation_tableZ$var, proportion4B = proportion4B) %>%
  rename("indicatorHCV_4B: HCV confirmatory testing rate" = proportion4B)

print(indicatorHCV_4B)


##Loop using age group, sex, ethnic group, IMD - Number of patients with a confirmatory test for HCV done as reflex testing

attendees_HCVreflex <- attendees_HCV %>%  filter(HCVAb == "Positive" & !is.na(HCVPCR) & RNAtiming == "Reflex")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableAC <- attendees_HCVreflex %>%
  group_by(age_group) %>%
  tally() %>%
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients with a confirmatory test for HCV done as reflex testing" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HCVreflex %>%
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%
    collect() %>%
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients with a confirmatory test for HCV done as reflex testing" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableAC <- bind_rows(aggregation_tableAC, temp)
  
}

#test 3 - look at table
View(aggregation_tableAC)


######INDICATOR 5######

# Calculate proportion5, treating NA as 0 in both numerator and denominator
numerator5 <- ifelse(
  is.na(aggregation_tableAC$"Number of patients with a confirmatory test for HCV done as reflex testing"),
  0,
  aggregation_tableAC$"Number of patients with a confirmatory test for HCV done as reflex testing"
)

denominator5 <- ifelse(
  is.na(aggregation_tableAA$"Number of patients who receive a confirmatory RNA test for HCV"),
  0,
  aggregation_tableAA$"Number of patients who receive a confirmatory RNA test for HCV"
)

proportion5 <- numerator5 / denominator5

# Create the indicator5 data frame
indicatorHCV_5 <- data.frame(var = aggregation_tableAA$var, proportion5 = proportion5) %>%
  rename("indicatorHCV_5: HCV reflex testing rate" = proportion5)

print(indicatorHCV_5)



##Loop using age group, sex, ethnic group, IMD - Number of patients who test positive for HCV RNA

attendees_HCVRNApos <- attendees_HCV %>% filter(HCVPCR == "Positive")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableAE <- attendees_HCVRNApos %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>% #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HCV RNA" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HCVRNApos %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HCV RNA" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableAE <- bind_rows(aggregation_tableAE, temp) 
  
}

#test 3 - look at table
View(aggregation_tableAE)

######INDICATOR 7B######

# Calculate proportion7B, treating NA as 0 in both numerator and denominator
numerator7B <- ifelse(
  is.na(aggregation_tableAE$"Number of patients who test positive for HCV RNA"),
  0,
  aggregation_tableAE$"Number of patients who test positive for HCV RNA"
)

denominator7B <- ifelse(
  is.na(aggregation_tableP$"Number of patients who have a blood test and HCV test"),
  0,
  aggregation_tableP$"Number of patients who have a blood test and HCV test"
)

proportion7B <- numerator7B / denominator7B

# Create the indicator7B data frame
indicatorHCV_7B <- data.frame(var = aggregation_tableP$var, proportion7B = proportion7B) %>%
  rename("indicatorHCV_7B: Proportion of patients who test positive for HCV" = proportion7B)

print(indicatorHCV_7B)



##Loop using age group, sex, ethnic group, IMD - Number of patients who are newly diagnosed with HCV

attendees_HCVRNApos <- attendees_HCV %>% filter(rna_positive_new == "New")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableAG <- attendees_HCVRNApos %>%
  group_by(age_group) %>%
  tally() %>%
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are newly diagnosed with HCV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HCVRNApos %>%
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%
    collect() %>%
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are newly diagnosed with HCV" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableAG <- bind_rows(aggregation_tableAG, temp)
  
}

#test 3 - look at table
View(aggregation_tableAG)

######INDICATOR 6B######

# Calculate proportion6B, treating NA as 0 in both numerator and denominator
numerator6B <- ifelse(
  is.na(aggregation_tableAG$"Number of patients who are newly diagnosed with HCV"),
  0,
  aggregation_tableAG$"Number of patients who are newly diagnosed with HCV"
)

denominator6B <- ifelse(
  is.na(aggregation_tableP$"Number of patients who have a blood test and HCV test"),
  0,
  aggregation_tableP$"Number of patients who have a blood test and HCV test"
)

proportion6B <- numerator6B / denominator6B

# Create the indicator6B data frame
indicatorHCV_6B <- data.frame(var = aggregation_tableC$var, proportion6B = proportion6B) %>%
  rename("indicatorHCV_6B: HCV new diagnosis rate" = proportion6B)

print(indicatorHCV_6B)



##Loop using age group, sex, ethnic group, IMD - Number of patients who test positive for HBV

attendees_HBVpos <- attendees_HBV %>% filter(HBVAg == "Positive")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableAI <- attendees_HBVpos %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who test positive for HBV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HBVpos %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who test positive for HBV" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableAI <- bind_rows(aggregation_tableAI, temp) 
  
}

#test 3 - look at table
View(aggregation_tableAI)

######INDICATOR 7C######

# Calculate proportion7C, treating NA as 0 in both numerator and denominator
numerator7C <- ifelse(
  is.na(aggregation_tableAI$"Number of patients who test positive for HBV"),
  0,
  aggregation_tableAI$"Number of patients who test positive for HBV"
)

denominator7C <- ifelse(
  is.na(aggregation_tableR$"Number of patients who have a blood test and HBV test"),
  0,
  aggregation_tableR$"Number of patients who have a blood test and HBV test"
)

proportion7C <- numerator7C / denominator7C

# Create the indicator7C data frame
indicatorHBV_7C <- data.frame(var = aggregation_tableC$var, proportion7C = proportion7C) %>%
  rename("indicatorHBV_7C: Proportion of patients who test positive for HBV" = proportion7C)

print(indicatorHBV_7C)



##Loop using age group, sex, ethnic group, IMD - Number of patients who are newly diagnosed with HBV

attendees_HBVnew <- attendees_HBV %>% filter(HBsAg_positive_new == "New")

##Generate template table using one of the aggregation variables of the master table
aggregation_tableAK <- attendees_HBVnew %>%
  group_by(age_group) %>%
  tally() %>%
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of patients who are newly diagnosed with HBV" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

# Define a function to replace NA values with 0 in a data frame
replace_na_with_zero <- function(df) {
  df %>% mutate_all(~ ifelse(is.na(.), 0, .))
}

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_HBVnew %>%
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%
    collect() %>%
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of patients who are newly diagnosed with HBV" = n)
  
  # Replace NA values with 0 in the temporary data frame
  temp <- replace_na_with_zero(temp)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_tableAK <- bind_rows(aggregation_tableAK, temp)
  
}

#test 3 - look at table
View(aggregation_tableAK)

# ######INDICATOR 6C######

# Calculate proportion6C, treating NA as 0 in both numerator and denominator
numerator <- ifelse(
  is.na(aggregation_tableAK$"Number of patients who are newly diagnosed with HBV"),
  0,
  aggregation_tableAK$"Number of patients who are newly diagnosed with HBV"
)

denominator <- ifelse(
  is.na(aggregation_tableR$"Number of patients who have a blood test and HBV test"),
  0,
  aggregation_tableR$"Number of patients who have a blood test and HBV test"
)

proportion6C <- numerator / denominator

# Create the indicator6C data frame
indicatorHBV_6C <- data.frame(var = aggregation_tableR$var, proportion6C = proportion6C) %>%
  rename("indicatorHBV_6C: HBV new diagnosis rate" = proportion6C)

# Print the result
print(indicatorHBV_6C)





#####################################################################






###
## to bind cols together at the end, we can use a left join
joined_table <- aggregation_tableC %>% 
  left_join(aggregation_tableD, by = "var") %>% 
  left_join(aggregation_tableE, by = "var") %>% 
  left_join(aggregation_tableF, by = "var") %>% 
  left_join(aggregation_tableG, by = "var") %>% 
  left_join(aggregation_tableH, by = "var") %>% 
  left_join(indicatorHIV_1A, by = "var") %>%
  left_join(aggregation_tableJ, by = "var") %>% 
  left_join(aggregation_tableK, by = "var") %>% 
  left_join(indicatorHIV_1B, by = "var") %>% 
  left_join(aggregation_tableM, by = "var") %>% 
  left_join(aggregation_tableN, by = "var") %>% 
  left_join(indicatorHIV_2A, by = "var") %>% 
  left_join(aggregation_tableP, by = "var") %>% 
  left_join(indicatorHCV_2B, by = "var") %>% 
  left_join(aggregation_tableR, by = "var") %>% 
  left_join(indicatorHBV_2C, by = "var") %>% 
  left_join(aggregation_tableT, by = "var") %>% 
  left_join(indicatorHIV_3A, by = "var") %>% 
  left_join(aggregation_tableV, by = "var") %>% 
  left_join(indicatorHCV_3B, by = "var") %>% 
  left_join(aggregation_tableX, by = "var") %>% 
  left_join(indicatorHBV_3C, by = "var") %>% 
  left_join(aggregation_tableZ, by = "var") %>% 
  left_join(aggregation_tableAA, by = "var") %>% 
  left_join(indicatorHCV_4B, by = "var") %>% 
  left_join(aggregation_tableAC, by = "var") %>% 
  left_join(indicatorHCV_5, by = "var") %>% 
  left_join(aggregation_tableAE, by = "var") %>% 
  left_join(indicatorHCV_7B, by = "var") %>% 
  left_join(aggregation_tableAG, by = "var") %>% 
  left_join(indicatorHCV_6B, by ="var") %>% 
  left_join(aggregation_tableAI, by = "var") %>% 
  left_join(indicatorHBV_7C, by = "var") %>% 
  left_join(aggregation_tableAK, by = "var") %>% 
  left_join(indicatorHBV_6C, by = "var") %>% 
  mutate_all(~ ifelse(is.na(.), "0", .)) %>%  # Replace NA with "0" in all columns
  mutate(var = if_else(is.na(var), "0", var),  # Replace NA with "0" as character in 'var'
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


------------------------

  #for social risk factors
  joined_table_1 <- aggregation_tableAK %>% 
  left_join(aggregation_tableAI, by = "var") %>% 
  left_join(aggregation_tableAG, by = "var") %>% 
  left_join(aggregation_tableZ, by = "var") %>% 
  mutate_all(~ ifelse(is.na(.), "0", .)) %>%  # Replace NA with "0" in all columns
  mutate(var = if_else(is.na(var), "0", var),  # Replace NA with "0" as character in 'var'
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

write.csv(joined_table_1, "MasterTables_export_required.csv")

------------------------------------------------------------










## Adding in HIV data
#NOTE TO USER: may need to update file path with the letter corresponding to your mapped drive
hiv_data_5 <- read_csv(
  "Y:/Evaluations/ED opt out BBV testing evaluation/Analysis/hiv_indicator_tab_5.csv"   
) %>%
  rename(var = Group,
         breakdown = "Breakdown type") %>%
  filter(breakdown %in% c("Total",
                          "Age group",
                          "Ethnic group",
                          "IMD quintile",
                          "Gender")) %>%
  mutate(breakdown = recode(breakdown, "IMD quintile" = "IMD"),
         var = (case_when(breakdown == "Gender" & var == "Not known" ~ "Unknown Gender",
                          breakdown == "Age group" & var == "Not known" ~ "Unknown Age",
                          breakdown == "Ethnic group" & var == "Not known" ~ "Unknown Ethnicity",
                          breakdown == "IMD" & var == "Not known" ~ "Unknown IMD",
                          var != "Not known" ~ var))) %>%
  select(breakdown, var, "Number of patients who test positive for HIV (2022 only)", "Number of patients who test positive for HIV (all)", "New diagnosis (2022 only)") %>%
  mutate(var = recode(var, "1 (most deprived)" = "1",
                      "5 (least deprived)" = "5",
                      "Asian: Indian, Pakistani or Bangladeshi" = "Indian, Pakistani or Bangladeshi",
                      "Mixed" = "Mixed/Multiple",
                      "16-24" = "16 to 24",
                      "25-34" = "25 to 34",
                      "35-49" = "35 to 49",
                      "50-64" = "50 to 64",
                      "65-79" = "65 to 79",
                      "80+" = "80 and over")) %>%
  replace(is.na(.), 0)


# join with joined_table
joined_table_hiv <- joined_table %>%
  left_join(hiv_data_5, by = c("breakdown", "var")) %>%
  rename(hiv_tests_2022 = 'Number of patients who have a blood test and HIV test (up to Dec 22)',
         new_hiv_diags_n = 'New diagnosis (2022 only)', hiv_pos_all_n = 'Number of patients who test positive for HIV (all)',
         hiv_tests_n = 'Number of patients who have a blood test and HIV test') %>%
  mutate(
    proportion7A = hiv_pos_all_n / hiv_tests_n,
    proportion6A = new_hiv_diags_n / hiv_tests_2022) %>%
  rename('Number of patients who have a blood test and HIV test (up to Dec 22)' = hiv_tests_2022,
         'New HIV diagnosis (2022 only)' = new_hiv_diags_n,
         'Number of patients who test positive for HIV (all)' = hiv_pos_all_n,
         'Number of patients who have a blood test and HIV test' = hiv_tests_n,
         'IndicatorHIV_7a: Proportion of patients who test positive for HIV' = proportion7A,
         'IndicatorHIV_6a: HIV new diagnosis rate' = proportion6A) %>%  
  mutate_all(~ ifelse(is.na(.), "0", .)) %>%  # Replace NA with "0" in all columns
  mutate(var = if_else(is.na(var), "0", var),  # Replace NA with "0" as character in 'var'
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

write.csv(joined_table_hiv, "MasterTables_export_OAC.csv")
#Table 4 of MasterTables_export.csv#

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

###############
#Load data for all sites/sentinel sites/london sites/outside london sites

# load all attendees data
attendees_all <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[12mthECDSattendees_includedsitesHIV]
WHERE (arrdate <= '2023-04-07')"
)

# load all attendees data for Sentinel sites
attendees_sentinel <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[12mthECDSattendees_includedSENTINELsitesHIV]
WHERE (arrdate <= '2023-04-07')"
)

# load all attendees data for London sites
attendees_ldn <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[12mthECDSattendees_includedLDNsitesHIV]
WHERE (arrdate <= '2023-04-07')"
)

# load all attendees data for outside London sites
attendees_outside <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[12mthECDSattendees_includedOUTLDNsitesHIV]
WHERE (arrdate <= '2023-04-07')"
)


attendees_all <- attendees_all %>% 
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
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group)) %>% 
  mutate(All = case_when(!is.na(TOKEN_PERSON_ID) ~ "All")) %>%
  replace(is.na(.), 0)



attendees_sentinel <- attendees_sentinel %>% 
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
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group)) %>% 
  mutate(All = case_when(!is.na(TOKEN_PERSON_ID) ~ "All")) %>% 
  replace(is.na(.), 0)



attendees_ldn <- attendees_ldn %>%
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
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group)) %>% 
  mutate(All = case_when(!is.na(TOKEN_PERSON_ID) ~ "All")) %>% 
  replace(is.na(.), 0)




attendees_outside <- attendees_outside %>% 
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
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group)) %>% 
  mutate(All = case_when(!is.na(TOKEN_PERSON_ID) ~ "All")) %>% 
  replace(is.na(.), 0)




##Loop using age group, sex, ethnic group, IMD - ATTENDEES (ALL SITES)

##Generate template table using one of the aggregation variables of the master table
aggregation_table4B <- attendees_all %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of attendees - all sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_all %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of attendees - all sites" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_table4B <- bind_rows(aggregation_table4B, temp) 
  
}

#test 3 - look at table
View(aggregation_table4B)


##Loop using age group, sex, ethnic group, IMD - ATTENDEES (SENTINEL SITES)

##Generate template table using one of the aggregation variables of the master table
aggregation_table4C <- attendees_sentinel %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of attendees - Sentinel sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_sentinel %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of attendees - Sentinel sites" = n)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_table4C <- bind_rows(aggregation_table4C, temp) 
  
}

#test 3 - look at table
View(aggregation_table4C)

##Loop using age group, sex, ethnic group, IMD - ATTENDEES (LONDON SITES)

##Generate template table using one of the aggregation variables of the master table
aggregation_table4D <- attendees_ldn %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of attendees - London sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_ldn %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>% #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of attendees - London sites" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_table4D <- bind_rows(aggregation_table4D, temp) 
  
}

#test 3 - look at table
View(aggregation_table4D)


##Loop using age group, sex, ethnic group, IMD - ATTENDEES (OUTSIDE SITES)

##Generate template table using one of the aggregation variables of the master table
aggregation_table4E <- attendees_outside %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of attendees - Outside London sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_outside %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of attendees - Outside London sites" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_table4E <- bind_rows(aggregation_table4E, temp) 
  
}

#test 3 - look at table
View(aggregation_table4E)



#number of patients attending ED who have a blood test
attendees_bloodsall <- attendees_all %>% filter(ECDS_bloods_any == "Yes")

##Loop using age group, sex, ethnic group, IMD - ATTENDEES WITH A BLOOD TEST (HIV GO LIVE DATES)

##Generate template table using one of the aggregation variables of the master table
aggregation_table4F <- attendees_bloodsall %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of attendees who had a blood test - all sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_bloodsall %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of attendees who had a blood test - all sites" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_table4F <- bind_rows(aggregation_table4F, temp) 
  
}

#test 3 - look at table
View(aggregation_table4F)


#number of patients attending ED who have a blood test
attendees_bloodssentinel <- attendees_sentinel %>% filter(ECDS_bloods_any == "Yes")

##Loop using age group, sex, ethnic group, IMD - ATTENDEES WITH A BLOOD TEST (SENTINEL SITES)

##Generate template table using one of the aggregation variables of the master table
aggregation_table4G <- attendees_bloodssentinel %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of attendees who had a blood test - Sentinel sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_bloodssentinel %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of attendees who had a blood test - Sentinel sites" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_table4G <- bind_rows(aggregation_table4G, temp) 
  
}

#test 3 - look at table
View(aggregation_table4F)



#number of patients attending ED who have a blood test
attendees_bloodsldn <- attendees_ldn %>% filter(ECDS_bloods_any == "Yes")

##Loop using age group, sex, ethnic group, IMD - ATTENDEES WITH A BLOOD TEST (LONDON SITES)

##Generate template table using one of the aggregation variables of the master table
aggregation_table4H <- attendees_bloodsldn %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of attendees who had a blood test - London sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_bloodsldn %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of attendees who had a blood test - London sites" = n)
  
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_table4H <- bind_rows(aggregation_table4H, temp) 
  
}

#test 3 - look at table
View(aggregation_table4H)


#number of patients attending ED who have a blood test
attendees_bloodsoutside <- attendees_outside %>% filter(ECDS_bloods_any == "Yes")

##Loop using age group, sex, ethnic group, IMD - ATTENDEES WITH A BLOOD TEST (OUTSIDE LONDON SITES)

##Generate template table using one of the aggregation variables of the master table
aggregation_table4I <- attendees_bloodsoutside %>% 
  group_by(age_group) %>% 
  tally() %>% 
  collect() %>% #collect from the server after doing anything computationally heavy
  rename(var = age_group) %>%  #rename the agg variable so that it is generic (required for bind_rows)
  rename("Number of attendees who had a blood test - Outside London sites" = n)


## define the variables you want to loop through for master table
vec <- c("Gender", "ethnic_group", "IMD", "All")

#set up for loop
for(i in 1:length(vec)){
  
  ## test 1 print the var you are agg by
  print(vec[i])
  
  #create temp table to store aggregation
  temp <- attendees_bloodsoutside %>% 
    group_by(!!sym(vec[i])) %>% #!!sym used to unquote and translate string into a symbol (a readable var name)
    tally() %>%   
    collect() %>% 
    rename(var = !!sym(vec[i])) %>%  #rename the agg variable so that it is generic (required for bind_rows)
    rename("Number of attendees who had a blood test - Outside London sites" = n)
  
  # test 2 print temp table check its right
  print(temp)
  
  # bind temp table rows to agg table
  aggregation_table4I <- bind_rows(aggregation_table4I, temp) 
  
}

#test 3 - look at table
View(aggregation_table4I)


######INDICATOR 1A######

##ALL SITES
proportion1A_all <- aggregation_table4F$"Number of attendees who had a blood test - all sites" / aggregation_table4B$"Number of attendees - all sites"

indicator1A_all <- data.frame(var = aggregation_table4F$var, proportion1A_all = proportion1A_all)

print(indicator1A_all)

##SENTINEL SITES
proportion1A_sentinel <- aggregation_table4G$"Number of attendees who had a blood test - Sentinel sites" / aggregation_table4C$"Number of attendees - Sentinel sites"

indicator1A_sentinel <- data.frame(var = aggregation_table4F$var, proportion1A_sentinel = proportion1A_sentinel)

print(indicator1A_sentinel)

##LONDON SITES
proportion1A_ldn <- aggregation_table4H$"Number of attendees who had a blood test - London sites" / aggregation_table4D$"Number of attendees - London sites"

indicator1A_ldn <- data.frame(var = aggregation_table4F$var, proportion1A_ldn = proportion1A_ldn)

print(indicator1A_ldn)

##OUTSIDE LONDON SITES
proportion1A_outside <- aggregation_table4I$"Number of attendees who had a blood test - Outside London sites" / aggregation_table4E$"Number of attendees - Outside London sites"

indicator1A_outside <- data.frame(var = aggregation_table4F$var, proportion1A_outside = proportion1A_outside)

print(indicator1A_outside)

###
## to bind cols together at the end, we can use a left join
joined_table4 <- aggregation_table4B %>% 
  left_join(aggregation_table4C, by = "var") %>% 
  left_join(aggregation_table4D, by = "var") %>% 
  left_join(aggregation_table4E, by = "var") %>% 
  left_join(aggregation_table4F, by = "var") %>% 
  left_join(aggregation_table4G, by = "var") %>% 
  left_join(aggregation_table4H, by = "var") %>% 
  left_join(aggregation_table4I, by = "var") %>% 
  left_join(indicator1A_all, by = "var") %>%
  left_join(indicator1A_sentinel, by = "var") %>% 
  left_join(indicator1A_ldn, by = "var") %>% 
  left_join(indicator1A_outside, by = "var") %>% 
  mutate(breakdown = case_when(
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

write.csv(joined_table4, "Mastertables_sitetypes.csv")

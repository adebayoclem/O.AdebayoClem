rm(list=ls())

#LOAD PACKAGES
pacman::p_load(
  pacman,
  tidyverse,
  lubridate,
  readxl,
  writexl,
  DBI,
  odbc,
  arrow,
  janitor,
  reshape2,
  ggplot2,
  scales
)

#ESTABLISH ODBC CONNECTION WITH DATA
Y006 <- odbc::dbConnect(odbc::odbc(),
                        .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;
                             database=Y006_BBV_PID;
                             Encrypt=true;trusted_connection=true",
                        timeout = 60,
                        timezone = Sys.timezone(),
                        timezone_out = Sys.timezone())

#import data
testing <- DBI::dbGetQuery(conn = Y006 , statement = 
"SELECT [TOKEN_PERSON_ID],
       [ARRIVAL_DATE], 
       [SITE],
       [ECDS_bloods_any],
       [PROVIDER_CODE_DERIVED],
       [Provider_derived], 
       [minarrival],
       [included_sites],
       [included_sentinel_sites],
       [Sentinel_sites], 
       [IMD], 
       [live_HCV], 
       [live_HBV], 
       [live_HIV], 
       [HCV], 
       [HBV], 
       [HIV], 
       [HCVresult], 
       [HBVresult],
	     [HIVresult]
from [Y006_BBV_PID].[dbo].[24monthECDSattendances_includedsites]
where (ARRIVAL_DATE <= '2024-03-30')") %>%
  clean_names() 

#ECDS_bloods_any='bloods_any' 

#Number of patients attending ED who have a HIV/HBV/HCV test	- numerator
#Number of patients attending ED who have a blood test - denominator

#change arrival_date to DATE format
testing$arrival_date <-as.Date(testing$arrival_date)

## filter for included_sentinel sites
testing <- testing %>% 
  filter(included_sentinel_sites=='Yes')

#Extract year separately from the date and identify Quarters  
testing <- testing %>% 
  mutate(year=year(arrival_date), 
         month=month(arrival_date),
         Quarter= paste0("Q",ceiling(month / 3))) %>%   
 arrange(arrival_date)


#number of patients attending ED who have a blood test 
attendees_bloods <-testing %>% 
  filter(ecds_bloods_any=="bloods_any")

attendees_bloods <- attendees_bloods %>% 
  mutate(year=year(arrival_date), 
         month=month(arrival_date),
         Quarter= paste0("Q",ceiling(month / 3))) %>%   
  arrange(arrival_date)


### Number of attendances to ED w/ blood test for HIV
hiv_testx <- attendees_bloods %>% 
  filter(live_hiv=='Live') %>%
  group_by(Quarter, year) %>%
  tally() %>% 
  collect() %>%
  rename("blood test (hiv)" = n) 


##Number of attendances to ED w/ blood test +  HIV test
HIV_bloodx <-attendees_bloods %>% 
  filter(live_hiv=='Live' & hiv=='Yes') %>%
  group_by(Quarter, year) %>% 
  tally() %>% 
  collect() %>%
  rename("blood test and HIV test" = n) 
 
num <-ifelse(is.na(hiv_testx$"blood test and HIV test"),
             0,
             hiv_testx$"blood test and HIV test")
denom <-if_else(is.na(HIV_bloodx$"blood test (hiv)"),
                0,
                HIV_bloodx$"blood test (hiv)")

propHIV <-num/denom * 100

HIVx <-data.frame(var=hiv_test$year,hiv_test$Quarter, propHIV=propHIV) 



#################

### attendances with HCV test count
hcv_test <- testing %>% 
  filter( hcv == "Yes") %>%
  group_by(Quarter, year) %>%
  tally() %>% 
  collect() %>%
  rename(test_uptake = n) %>%
  mutate(event = "HCV test")


### attendances with HBV test count
hbv_test<- testing %>% 
  filter( hbv == "Yes") %>%
  group_by(year, Quarter) %>%
  tally() %>% 
  collect() %>%
  rename(test_uptake = n) %>%
  mutate(event = "HBV test")

#bind all 3 BBV together
# total tests =
#(number of individual test)/(Number of patients attending ED who have a blood test) 
test_uptake_all <-rbind(hiv_test, hcv_test, hbv_test) %>% 
mutate(total_tests= sum(test_uptake),
       prop= round(test_uptake/total_tests,2),
      perc = round(prop * 100, 2),
      test_uptake_thousands=round(test_uptake/1e5, 2)) %>%  # dividing by 100,000
  ungroup()
#mutate(year=as.character(test_uptake_ds$year)


test_uptake_ds$year <- as.character(test_uptake_ds$year)

#CREATE GRAPHS

#numbers in thousands
ggplot(test_uptake_all, aes(x= interaction(Quarter, year, sept=""),
                              y=test_uptake_thousands, colour=event, group=event)) +
  geom_line () + 
  geom_point() +
  scale_y_continuous(breaks = seq(0, 1.5, by= 0.2)) +
  labs(title= "Trends in the tests uptake from April 2022- April 2024",
       x= "Years by  Quarters", 
       y= "Number of Test Uptake (100,000)")
  
#proportions
ggplot(test_uptake_all, aes(x= interaction(Quarter, year , sept=""),
                           y=prop, colour=event, group=event)) +
  geom_line () +
  geom_point() +
  labs(title= "Trends in the tests uptake from April 2022- April 2024",
       x= "Years by  Quarters", 
       y= "Proportion of Test Uptake")

#percentage
test_uptake_all$perc <-as.numeric(as.character(test_uptake_all$perc))

included_sites_P<-ggplot(test_uptake_all, aes(x= interaction(Quarter, year , sep=" "),
                           y=perc, colour=event, group=event)) +
  geom_line () +
  geom_point() +
  labs(title= "Trends in the tests uptake from April 2022- April 2024",
       x= "Years by  Quarters", 
       y= "Percentage of Test Uptake (%)")

ggsave("plot.png", plot = included_sites_P, width = 8, height = 6)




aggregate(total_tests ~ Quarter, data= test_uptake_all, sum)


#########################

# load HIV attendees data from Sentinel
attendees_HIV <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT [TOKEN_PERSON_ID],
[live_HIV],
[Sentinel_sites],
[included_sentinel_sites],
[arrdate],
[ECDS_bloods_any],
[HIV],
[SITE]
FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHIV]
WHERE (arrdate <= '2024-03-30')"
)


# load HCV attendees data from Sentinel
attendees_HCV <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT [TOKEN_PERSON_ID],
[live_HCV],
[Sentinel_sites],
[included_sentinel_sites],
[arrdate],
[ECDS_bloods_any],
[HCV],
[SITE]
FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHCV]
WHERE (arrdate <= '	2024-03-30')"
)

# load HBV attendees data from Sentinel
attendees_HBV <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT 
[TOKEN_PERSON_ID],
[live_HBV],
[Sentinel_sites],
[included_sentinel_sites],
[arrdate],
[ECDS_bloods_any],
[HBV],
[SITE]
FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHBV]
WHERE (arrdate <= '	2024-03-30')"
)

#Number of patients attending ED (HIV go live dates)
attendees_HIV <- attendees_HIV %>% 
  mutate(year=year(arrdate), 
         month=month(arrdate),
         Quarter= paste0("Q",ceiling(month / 3))) %>%   
  arrange(arrdate)

HIV_aggregate <-attendees_HIV %>% 
  group_by(Quarter, year) %>% 
  tally() %>% 
  collect() %>%
  rename(test_uptake = n) %>%
  mutate(event = "Number of patients attending ED (HIV go live dates)")
  

#number of patients attending ED who have a blood test (HIV go live dates)
attendees_bloodsHIV <-attendees_HIV %>% 
  filter(ECDS_bloods_any=="Yes")

attendees_bloodsHIV <- attendees_bloodsHIV %>% 
  mutate(year=year(arrdate), 
         month=month(arrdate),
         Quarter= paste0("Q",ceiling(month / 3))) %>%   
  arrange(arrdate)


# Step 1: Merge the two data frames
merged_data <- HIV_aggregate %>%
  inner_join(HIV_blood, by = c("Quarter", "year"), suffix = c("_HIV", "_blood"))

# Step 2: Calculate the uptake percentage
merged_data <- merged_data %>%
  mutate(uptake_percentage = (test_uptake_blood / test_uptake_HIV) * 100)

ggplot(merged_data, aes(x = interaction(year, Quarter, sep = " "), y = uptake_percentage, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Total Test Uptake Over Time",
       x = "Year and Quarter",
       y = "Uptake Percentage (%)") +
  theme_minimal()

########  HBV  ##########

#Number of patients attending ED (HIV go live dates)
attendees_HBV <- attendees_HBV %>% 
  mutate(year=year(arrdate), 
         month=month(arrdate),
         Quarter= paste0("Q",ceiling(month / 3))) %>%   
  arrange(arrdate)

HBV_aggregate <-attendees_HBV %>% 
  group_by(Quarter, year) %>% 
  tally() %>% 
  collect() %>%
  rename(test_uptake = n) %>%
  mutate(event = "Number of patients attending ED (HBV go live dates)")


#number of patients attending ED who have a blood test (HBV go live dates)
attendees_bloodsHBV <-attendees_HBV %>% 
  filter(ECDS_bloods_any=="Yes")

attendees_bloodsHBV <- attendees_bloodsHBV %>% 
  mutate(year=year(arrdate), 
         month=month(arrdate),
         Quarter= paste0("Q",ceiling(month / 3))) %>%   
  arrange(arrdate)

HBV_blood <-attendees_bloodsHBV %>% 
  group_by(Quarter, year) %>% 
  tally() %>% 
  collect() %>%
  rename(test_uptake = n) %>%
  mutate(event ="number of patients attending ED who have a blood test (HBV go live dates)")


# Step 1: Merge the two data frames
merged_dataHBV <- HBV_aggregate %>%
  inner_join(HBV_blood, by = c("Quarter", "year"), suffix = c("_HBV", "_blood"))

# Step 2: Calculate the uptake percentage
merged_dataHBV <- merged_dataHBV %>%
  mutate(uptake_percentage = (test_uptake_blood / test_uptake_HBV) * 100)

ggplot(merged_dataHBV, aes(x = interaction(year, Quarter, sep = " "), y = uptake_percentage, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Total Test Uptake Over Time",
       x = "Year and Quarter",
       y = "Uptake Percentage (%)") +
  theme_minimal()

#######  HCV   ######

#Number of patients attending ED (HIV go live dates)
attendees_HCV <- attendees_HCV %>% 
  mutate(year=year(arrdate), 
         month=month(arrdate),
         Quarter= paste0("Q",ceiling(month / 3))) %>%   
  arrange(arrdate)

HCV_aggregate <-attendees_HCV %>% 
  group_by(Quarter, year) %>% 
  tally() %>% 
  collect() %>%
  rename(test_uptake = n) %>%
  mutate(event = "Number of patients attending ED (HCV go live dates)")


#number of patients attending ED who have a blood test (HCV go live dates)
attendees_bloodsHCV <-attendees_HCV %>% 
  filter(ECDS_bloods_any=="Yes")

attendees_bloodsHCV <- attendees_bloodsHCV %>% 
  mutate(year=year(arrdate), 
         month=month(arrdate),
         Quarter= paste0("Q",ceiling(month / 3))) %>%   
  arrange(arrdate)

HCV_blood <-attendees_bloodsHCV %>% 
  group_by(Quarter, year) %>% 
  tally() %>% 
  collect() %>%
  rename(test_uptake = n) %>%
  mutate(event ="number of patients attending ED who have a blood test (HCV go live dates)")


# Step 1: Merge the two data frames
merged_dataHCV <- HCV_aggregate %>%
  inner_join(HCV_blood, by = c("Quarter", "year"), suffix = c("_HCV", "_blood"))

# Step 2: Calculate the uptake percentage
merged_dataHCV <- merged_dataHCV %>%
  mutate(uptake_percentage = (test_uptake_blood / test_uptake_HCV) * 100)

 ggplot(merged_dataHCV, aes(x = interaction(year, Quarter, sep = " "), y = uptake_percentage, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Total Test Uptake Over Time",
       x = "Year and Quarter",
       y = "Uptake Percentage (%)") +
  theme_minimal()

################## try all together
 
 #Extract year separately from the date and identify Quarters  
 testing <- testing %>% 
   mutate(year=year(arrival_date), 
          month=month(arrival_date),
          Quarter= paste0("Q",ceiling(month / 3))) %>%   
   filter(included_sentinel_sites=='Yes')
   arrange(arrival_date)
 
 ## filter for included_sentinel sites
 testing <- testing %>% 
   filter(included_sentinel_sites=='Yes')
 
 
 #number of patients attending ED who have a blood test (HCV go live dates)
bloods <-testing %>% 
   filter(ecds_bloods_any=="bloods_any")
 
bloods <- bloods %>% 
  mutate(year=year(arrival_date), 
         month=month(arrival_date),
         Quarter= paste0("Q",ceiling(month / 3))) %>%   
  filter(included_sentinel_sites=='Yes')
arrange(arrival_date)

### attendances with HIV test count
hiv_test <- testing %>% 
  filter( hiv == "Yes") %>%
  group_by(year, month) %>%
  tally() %>% 
  collect() %>%
  rename(test_uptake = n) %>%
  mutate(event = "HIV test")


 ### attendances with HCV test count
 hiv_blood <- bloods %>% 
   filter( hiv == "Yes") %>%
   group_by(year, month) %>%
   tally() %>% 
   collect() %>%
   rename(test_uptake = n) %>%
   mutate(event = "HIV test blood")
 
 HIV_ALL <- rbind(hiv_test, hiv_blood)
 
hiv11 <- hiv_test %>%
   inner_join(hiv_blood, by = c("month", "year"), suffix = c("_HBV",
                                                             "_blood"))


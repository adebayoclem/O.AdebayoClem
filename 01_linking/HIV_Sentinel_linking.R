#summary
#included_tests_HIV - linked to attendances (with blood tests)
#included_tests_HIV3 - linked to attendances (no blood tests)
#included_tests_HIV4 - linked to attendees (no blood tests)
#included_tests_HIV5 - linked to attendees (with blood tests)

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
if (!require(DBI)) install.packages("DBI")
if (!require(odbc)) install.packages("odbc")
if (!require(arrow)) install.packages("arrow")

# read in parquet of attendances with blood tests
# attendancesbloodtests<- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendances_with_blood_tests.parquet")

# 5 sites version
attendancesbloodtests<- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendances_blood_tests_5.parquet")

# remove all but data needed
# rm(list= ls()[!(ls() %in% c('attendancesbloodtests'))])

# load connection to Sentinel
Sentinel <- odbc::dbConnect(odbc::odbc(),
                            .connection_string = "driver={SQL Server};server=SQLClusColHFN17\\HFN17;
                             database=NIS_HepatitisSentinel;
                             Encrypt=true;trusted_connection=true",
                            timeout = 60,
                            timezone = Sys.timezone(),
                            timezone_out = Sys.timezone())

#load sites lists
source ("00_setup/sites_lists.R")

# filter ECDS attendances to only include sentinel sites
# attendances_sentinel_bt <- attendancesbloodtests%>%
#   filter (SITE %in%sentinel_sites)

# load HIV tests data from Sentinel
HIV_TESTS_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
SELECT *
FROM [NIS_HepatitisSentinel].[dbo].[EmergencyDepartment_tests]                                
WHERE (HIVtest IS NOT NULL AND SpecimenDate <= '2023-04-07')"
)

# change NHS_NUMBER to the same as in ECDS dataset
HIV_TESTS_SENTINEL <- HIV_TESTS_SENTINEL%>%
  rename("NHS_NUMBER" = "PtNHSNumber")

# Get Sentinel HIV tests with NHS numbers
#Code HIV tests that have an NHS number
HIV_tests_NHS <- HIV_TESTS_SENTINEL %>% mutate(NA_tally = if_else(is.na(NHS_NUMBER),0,1) )

# Select HIV tests that have an NHS number
HIV_tests_NHSn <- HIV_tests_NHS %>% filter(NA_tally == 1) %>% select(NHS_NUMBER, SpecimenDate)

#Create a list of NHS numbers for those who have HIV test recorded
HIV_tests <- as.list(HIV_tests_NHSn[1])

# join NHS number and spec date of HIV tests to the main ECDS attendances with blood tests table
HIV_TESTS_JOINED <- left_join(attendancesbloodtests, HIV_tests_NHSn, by= "NHS_NUMBER")

# format dates                              
HIV_TESTS_JOINED <- HIV_TESTS_JOINED %>%
  mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE, format = '%Y-%m-%d'))%>%
           mutate(INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE, format = '%Y-%m-%d'))%>%
                    mutate(SPECDATE = as.Date(SpecimenDate, format = '%Y-%m-%d'))


#Create column for date range (investigation date + 7) inclusion to check whether
#tests were carried out during person's a+e visit
HIV_TESTS_JOINED <- HIV_TESTS_JOINED%>% 
mutate(max_SPEC_date = ARRIVAL_DATE + 8, 
include = case_when(
(SPECDATE > INVESTIGATION_DATE-1 & SPECDATE < max_SPEC_date)~ "INCLUDE",
FALSE ~ "EXCLUDE"))


# create table of included tests
included_tests_HIV <- HIV_TESTS_JOINED%>%
  filter(include == "INCLUDE" & live_HIV == "LIVE")


# # find number of tests by Trust
# totaltestsbytrust_HIV <- included_tests_HIV %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
#   group_by(Provider_derived) %>%
#   count() %>%
#   select(Provider_derived, n)
# 
# write.csv(totaltestsbytrust_HIV, "hivtestsbytrust.csv")
# 
# 
# # find number of tests by Site
# totaltestsbysite_HIV <- included_tests_HIV %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
#   group_by(SITE) %>%
#   count() %>%
#   select(SITE, n)
# 
# write.csv(totaltestsbysite_HIV, "hivtestsbysite.csv")
# 
# #regroup to find total tests by month
# totaltestsbymonth_HIV <- included_tests_HIV %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, ARRIVAL_MONTH)%>%
#   group_by(ARRIVAL_MONTH)%>%
#   count()%>%
#   select(ARRIVAL_MONTH, n)
# 
# write.csv(totaltestsbymonth_HIV, "HIVtestsbymonth.csv")
# 
# 
# ####### Link HIV tests to attendances (no blood tests)
# 
# attendances <- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendances.parquet")
# 
# attendances_sentinel <- attendances%>%
#   filter (SITE %in%sentinel_sites)
# 
# # join NHS number and spec date of HIV tests to the main ECDS attendances table
# HIV_TESTS_JOINED3 <- left_join(attendances, HIV_tests_NHSn, by= "NHS_NUMBER")
# 
# # format dates                              
# HIV_TESTS_JOINED3 <- HIV_TESTS_JOINED3 %>%
#   mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE, format = '%Y-%m-%d'))%>%
#   mutate(INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE, format = '%Y-%m-%d'))%>%
#   mutate(DATE_RECEIVED = as.Date(SpecimenDate, format = '%Y-%m-%d'))
# 
# 
# #Create column for date range (investigation date + 7) inclusion to check whether
# #tests were carried out during person's a+e visit
# HIV_TESTS_JOINED3 <- HIV_TESTS_JOINED3 %>% 
#   mutate(max_SPEC_date = ARRIVAL_DATE + 8, 
#          include = case_when(
#            (DATE_RECEIVED > INVESTIGATION_DATE-1 & DATE_RECEIVED < max_SPEC_date)~ "INCLUDE",
#            FALSE ~ "EXCLUDE"))
# 
# 
# # create table of included tests
# included_tests_HIV3 <- HIV_TESTS_JOINED3 %>%
#   filter(include == "INCLUDE" & live_HIV == "LIVE")
# 
# 
# # find number of tests by Trust
# totaltestsbytrust_HIV3 <- included_tests_HIV3 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
#   group_by(Provider_derived) %>%
#   count() %>%
#   select(Provider_derived, n)
# 
# write.csv(totaltestsbytrust_HIV3, "HIVtestsbytrust3.csv")
# 
# 
# # find number of tests by Site
# totaltestsbysite_HIV3 <- included_tests_HIV3 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
#   group_by(SITE) %>%
#   count() %>%
#   select(SITE, n)
# 
# write.csv(totaltestsbysite_HIV3, "HIVtestsbysite3.csv")
# 
# 
# ####### HIV tests for attendees (no blood tests)
# # dedup the HIV tests attendances (no blood tests) table by NHS_NUMBER
# 
# included_tests_HIV4 <- included_tests_HIV3%>%
#   distinct(NHS_NUMBER, .keep_all = TRUE)


####### HIV tests for attendees (with blood tests)
# dedup the HIV tests attendances with blood tests table by NHS_NUMBER
included_tests_HIV5 <- included_tests_HIV%>%
  distinct(NHS_NUMBER, .keep_all = TRUE)


# find number of tests by Trust
totaltestsbytrust_HIV5 <- included_tests_HIV5 %>%
  distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
  group_by(Provider_derived) %>%
  count() %>%
  select(Provider_derived, n)

write.csv(totaltestsbytrust_HIV5, "HIVtestsbytrust5.csv")


# find number of tests by Site
totaltestsbysite_HIV5 <- included_tests_HIV5 %>%
  distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
  group_by(SITE) %>%
  count() %>%
  select(SITE, n)

write.csv(totaltestsbysite_HIV5, "HIVtestsbysite5.csv")





################################DIAGNOSES###########################

# # load HIV diagnoses data from Sentinel
# 
# HIV_DIAGNOSES_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
# select * from [SentinelEDApril22]
# where DATE_RECEIVED < '2023-04-01'
# AND HIV like 'positive'"
# )
# 
# # don't use the below, as not all addresses are included in these definitions - keeping for a record of go live dates
# # and ((ADDRESS2 like 'Newham%' AND DATE_RECEIVED >= '2022-04-25') 
# # OR (ADDRESS2 like 'Royal London%' AND DATE_RECEIVED >= '2022-04-25') 
# # OR (ADDRESS2 like 'Whipps%' AND DATE_RECEIVED >= '2022-04-04') 
# # OR (ADDRESS2 like 'Chelsea%' AND DATE_RECEIVED >= '2022-04-01') 
# # OR (ADDRESS2 like 'West Middlesex%' AND DATE_RECEIVED >= '2022-04-01') 
# # OR (ADDRESS2 like 'NORTH MIDDLESEX HOSPITAL' AND DATE_RECEIVED >= '2022-04-01') 
# # OR (ADDRESS2 like 'ST MARY%' AND DATE_RECEIVED >= '2022-08-15')
# # OR (ADDRESS2 like 'Charing%' AND DATE_RECEIVED >='2022-07-01')
# # OR (ADDRESS2 like 'Hillingdon%' AND DATE_RECEIVED >= '2022-07-22')
# # OR (ADDRESS2 like 'Kings College%' AND DATE_RECEIVED >= '2022-10-01')
# # OR (ADDRESS2 like 'King''s College%' AND DATE_RECEIVED >= '2022-10-01')
# # OR (ADDRESS2 like 'St Thomas%' AND DATE_RECEIVED >= '2022-10-01')
# # OR (ADDRESS2 like 'Croydon%' AND DATE_RECEIVED >= '2022-10-01')
# # OR (ADDRESS2 like 'St George%' AND DATE_RECEIVED >= '2022-10-01'))"
# # )
# 
# # create list of sites in sentinel
# sentinel_sites <-c('RYJ02',	'RQM06','RQM01', 'RQM25',	'RJ611',	'RAS01',	'RQXM1',	'RJZ01',	'RAX01',	'R0A02', 'RM326', 'RJZ30',	'R1HNH',	'RW602', 'R0A66', 'RM318',
#                    'RJ253',	'RXH01', 'E0A3H',	'RJ701',	'RYJ01',	'RJ122',	'R1H12',	'RQM91',	'R1HKH',	'R0A07', 'RM325'
# )
# 
# # filter ECDS attendances to only include sentinel sites
# attendances_sentinel <- attendancesbloodtests%>%
#   filter (SITE %in%sentinel_sites)
# 
# 
# # Get Sentinel diagnoses with NHS numbers - separately for each dataset HBV and HCV
# #Code diagnoses that have an NHS number
# HIV_diag_NHS <- HIV_DIAGNOSES_SENTINEL %>% mutate(NA_tally = if_else(is.na(NHS_NUMBER),0,1))
# 
# # Select diagnoses that have an NHS number - include record location and address to look more into these later
# HIV_diag_NHSn <- HIV_diag_NHS %>% filter(NA_tally == 1) %>% select(NHS_NUMBER, DATE_RECEIVED, RECORD_LOCATION, ADDRESS1, ADDRESS2)
# 
# # join NHS number and spec date of HIV diagnoses to ECDS attendances with blood tests table (sentinel sites)
# HIV_DIAG_JOINED <- left_join(attendances_sentinel, HIV_diag_NHSn, by= "NHS_NUMBER")
# 
# # format dates                              
# HIV_DIAG_JOINED <- HIV_DIAG_JOINED%>%
#   mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE),
#          INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE),
#          SPECDATE = as.Date(DATE_RECEIVED, FORMAT = '%Y-%m-%d'))
# 
# #Create column for date range (investigation date + 8) inclusion to check whether
# #tests were carried out during person's a+e visit
# HIV_DIAG_JOINED <- HIV_DIAG_JOINED %>% mutate(max_SPEC_date = ARRIVAL_DATE + 8,
#                                               include = case_when(
#                                                 (SPECDATE > INVESTIGATION_DATE-1 & SPECDATE < max_SPEC_date)~ "INCLUDE",
#                                                 FALSE ~ "EXCLUDE"))
# 
# # create table of included diagnoses
# HIV_included_diagnoses_sentinel <- HIV_DIAG_JOINED %>%
#   filter(include == "INCLUDE")%>%
#   distinct(NHS_NUMBER, ARRIVAL_DATE, .keep_all = TRUE)%>%
#   select (-c(INVESTIGATION_CODE, INVESTIGATION_CODE_Description, InvestigationIdx, CHIEF_COMPLAINT, CHIEF_COMPLAINT_Description, FYEAR, month))
# 
# 


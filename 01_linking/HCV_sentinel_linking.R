#summary
#included_tests_HCV - linked to attendances (with blood tests)
#included_tests_HCV3 - linked to attendances (no blood tests)
#included_tests_HCV4 - linked to attendees (no blood tests)
#included_tests_HCV5 - linked to attendees (with blood tests)

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
attendances_sentinel_bt <- attendancesbloodtests%>%
  filter (SITE %in%sentinel_sites)

# load HCV tests data from Sentinel
HCV_TESTS_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
SELECT *
FROM [NIS_HepatitisSentinel].[dbo].[EmergencyDepartment_tests] 
WHERE (HCVtest IS NOT NULL AND  SpecimenDate <= '2023-04-07')"
)

# change NHS_NUMBER to the same as in ECDS dataset
HCV_TESTS_SENTINEL <- HCV_TESTS_SENTINEL%>%
  rename("NHS_NUMBER" = "PtNHSNumber")


# Get Sentinel HCV tests with NHS numbers
#Code HCV tests that have an NHS number
HCV_tests_NHS <- HCV_TESTS_SENTINEL %>% mutate(NA_tally = if_else(is.na(NHS_NUMBER),0,1) )

# Select HCV tests that have an NHS number
HCV_tests_NHSn <- HCV_tests_NHS %>% filter(NA_tally == 1) %>% select(NHS_NUMBER, SpecimenDate)

#Create a list of NHS numbers for those who have HCV test recorded
HCV_tests <- as.list(HCV_tests_NHSn[1])



# join NHS number and spec date of HCV tests to the main ECDS attendances with blood tests table
HCV_TESTS_JOINED <- left_join(attendances_sentinel_bt, HCV_tests_NHSn, by= "NHS_NUMBER")

# format dates                              
HCV_TESTS_JOINED <- HCV_TESTS_JOINED %>%
  mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE, format = '%Y-%m-%d'))%>%
  mutate(INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE, format = '%Y-%m-%d'))%>%
  mutate(SpecDate = as.Date(SpecimenDate, format = '%Y-%m-%d'))


#Create column for date range (investigation date + 8) inclusion to check whether
#tests were carried out during person's a+e visit
HCV_TESTS_JOINED <- HCV_TESTS_JOINED%>% 
  mutate(max_SPEC_date = ARRIVAL_DATE + 8, 
         include = case_when(
           (SpecDate > INVESTIGATION_DATE-1 & SpecDate < max_SPEC_date)~ "INCLUDE",
           FALSE ~ "EXCLUDE"))


# create table of included tests
included_tests_HCV <- HCV_TESTS_JOINED%>%
  filter(include == "INCLUDE" & live_HCV == "LIVE")

# # find number of tests by Trust
# totaltestsbytrust_HCV <- included_tests_HCV %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
#   group_by(Provider_derived) %>%
#   count() %>%
#   select(Provider_derived, n)
# 
# write.csv(totaltestsbytrust_HCV, "HCVtestsbytrust.csv")
# 
# 
# # find number of tests by Site
# totaltestsbysite_HCV <- included_tests_HCV %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
#   group_by(SITE) %>%
#   count() %>%
#   select(SITE, n)
# 
# write.csv(totaltestsbysite_HCV, "HCVtestsbysite.csv")
# 
# #regroup to find total tests by month
# totaltestsbymonth_HCV <- included_tests_HCV %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, ARRIVAL_MONTH)%>%
#   group_by(ARRIVAL_MONTH)%>%
#   count()%>%
#   select(ARRIVAL_MONTH, n)
# 
# write.csv(totaltestsbymonth_HCV, "HCVtestsbymonth.csv")



####### Link HCV tests to attendances (regardless of whether or not they had blood tests)

# attendances <- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendances.parquet")
# 
# attendances_sentinel <- attendances%>%
#   filter (SITE %in%sentinel_sites)
# 
# # join NHS number and spec date of HBV tests to the main ECDS attendances with blood tests table
# HCV_TESTS_JOINED3 <- left_join(attendances, HCV_tests_NHSn, by= "NHS_NUMBER")
# 
# # format dates                              
# HCV_TESTS_JOINED3 <- HCV_TESTS_JOINED3 %>%
#   mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE, format = '%Y-%m-%d'))%>%
#   mutate(INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE, format = '%Y-%m-%d'))%>%
#   mutate(SpecDate = as.Date(SpecimenDate, format = '%Y-%m-%d'))
# 
# 
# #Create column for date range (investigation date + 8) inclusion to check whether
# #tests were carried out during person's a+e visit 
# HCV_TESTS_JOINED3 <- HCV_TESTS_JOINED3 %>% 
#   mutate(max_SPEC_date = ARRIVAL_DATE + 8, 
#          include = case_when(
#            (SpecDate > INVESTIGATION_DATE-1 & SpecDate < max_SPEC_date)~ "INCLUDE",
#            FALSE ~ "EXCLUDE"))
# 
# 
# # create table of included tests
# included_tests_HCV3 <- HCV_TESTS_JOINED3 %>%
#   filter(include == "INCLUDE" & live_HCV == "LIVE")
# 
# 
# # find number of tests by Trust
# totaltestsbytrust_HCV3 <- included_tests_HCV3 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
#   group_by(Provider_derived) %>%
#   count() %>%
#   select(Provider_derived, n)
# 
# write.csv(totaltestsbytrust_HCV3, "HCVtestsbytrust3.csv")
# 
# 
# # find number of tests by Site
# totaltestsbysite_HCV3 <- included_tests_HCV3 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
#   group_by(SITE) %>%
#   count() %>%
#   select(SITE, n)
# 
# write.csv(totaltestsbysite_HCV3, "HCVtestsbysite3.csv")


####### Attendees that had a HCV test (regardless of whether they had a blood test)
######  dedup the attendances with a HCV test table by NHS number

# included_tests_HCV4 <- included_tests_HCV3%>%
#   distinct(NHS_NUMBER, .keep_all = TRUE)
# 
# # find number of tests by Trust
# totaltestsbytrust_HCV4 <- included_tests_HCV4 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
#   group_by(Provider_derived) %>%
#   count() %>%
#   select(Provider_derived, n)
# 
# write.csv(totaltestsbytrust_HCV4, "HCVtestsbytrust4.csv")
# 
# 
# # find number of tests by Site
# totaltestsbysite_HCV4 <- included_tests_HCV4 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
#   group_by(SITE) %>%
#   count() %>%
#   select(SITE, n)
# 
# write.csv(totaltestsbysite_HCV4, "HCVtestsbysite4.csv")


####### HCV tests for attendees (with blood tests)
#### dedup the HCV tests for attendances with blood tests table by NHS number
included_tests_HCV5 <- included_tests_HCV%>%
  distinct(NHS_NUMBER, .keep_all = TRUE)


# # find number of tests by Trust
# totaltestsbytrust_HCV5 <- included_tests_HCV5 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
#   group_by(Provider_derived) %>%
#   count() %>%
#   select(Provider_derived, n)
# 
# write.csv(totaltestsbytrust_HCV5, "HCVtestsbytrust5.csv")
# 
# 
# # find number of tests by Site
# totaltestsbysite_HCV5 <- included_tests_HCV5 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
#   group_by(SITE) %>%
#   count() %>%
#   select(SITE, n)
# 
# write.csv(totaltestsbysite_HCV5, "HCVtestsbysite5.csv")
# 









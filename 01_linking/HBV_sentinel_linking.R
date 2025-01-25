#summary
#included_tests_HBV - linked to attendances (with blood tests)
#included_tests_HBV3 - linked to attendances (no blood tests)
#included_tests_HBV4 - linked to attendees (no blood tests)
#included_tests_HBV5 - linked to attendees (with blood tests)

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

# load HBV tests data from Sentinel
HBV_TESTS_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
SELECT *
FROM [NIS_HepatitisSentinel].[dbo].[EmergencyDepartment_tests]                                  
WHERE (HBVtest IS NOT NULL AND SpecimenDate <= '2023-04-07')"
)

# change NHS_NUMBER to the same as in ECDS dataset
HBV_TESTS_SENTINEL <- HBV_TESTS_SENTINEL%>%
  rename("NHS_NUMBER" = "PtNHSNumber")

# Get Sentinel HBV tests with NHS numbers
#Code HBV tests that have an NHS number
HBV_tests_NHS <- HBV_TESTS_SENTINEL %>% mutate(NA_tally = if_else(is.na(NHS_NUMBER),0,1) )

# Select HBV tests that have an NHS number
HBV_tests_NHSn <- HBV_tests_NHS %>% filter(NA_tally == 1) %>% select(NHS_NUMBER, SpecimenDate)

#Create a list of NHS numbers for those who have HBV test recorded
HBV_tests <- as.list(HBV_tests_NHSn[1])

# join NHS number and spec date of HBV tests to the main ECDS attendances with blood tests table
HBV_TESTS_JOINED <- left_join(attendancesbloodtests, HBV_tests_NHSn, by= "NHS_NUMBER")

# format dates                              
HBV_TESTS_JOINED <- HBV_TESTS_JOINED %>%
  mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE, format = '%Y-%m-%d'))%>%
  mutate(INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE, format = '%Y-%m-%d'))%>%
  mutate(SpecDate = as.Date(SpecimenDate, format = '%Y-%m-%d'))


#Create column for date range (investigation date + 7) inclusion to check whether
#tests were carried out during person's a+e visit
HBV_TESTS_JOINED <- HBV_TESTS_JOINED%>% 
  mutate(max_SPEC_date = ARRIVAL_DATE + 8, 
         include = case_when(
           (SpecDate > INVESTIGATION_DATE-1 & SpecDate < max_SPEC_date)~ "INCLUDE",
           FALSE ~ "EXCLUDE"))


# create table of included tests
included_tests_HBV <- HBV_TESTS_JOINED%>%
  filter(include == "INCLUDE" & live_HBV == "LIVE")


# # find number of tests by Trust
# totaltestsbytrust_HBV <- included_tests_HBV %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
#   group_by(Provider_derived) %>%
#   count() %>%
#   select(Provider_derived, n)
# 
# write.csv(totaltestsbytrust_HBV, "HBVtestsbytrust.csv")
# 
# 
# # find number of tests by Site
# totaltestsbysite_HBV <- included_tests_HBV %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
#   group_by(SITE) %>%
#   count() %>%
#   select(SITE, n)
# 
# write.csv(totaltestsbysite_HBV, "HBVtestsbysite.csv")

# #regroup to find total tests by month
# totaltestsbymonth_HBV <- included_tests_HBV %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, ARRIVAL_MONTH)%>%
#   group_by(ARRIVAL_MONTH)%>%
#   count()%>%
#   select(ARRIVAL_MONTH, n)
# 
# write.csv(totaltestsbymonth_HBV, "HBVtestsbymonth.csv")
# 
# ####### Link HBV tests to attendances (no blood tests)
# 
# attendances <- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendances.parquet")
# 
# attendances_sentinel <- attendances%>%
#   filter (SITE %in%sentinel_sites)
# 
# # join NHS number and spec date of HBV tests to the main ECDS attendances table
# HBV_TESTS_JOINED3 <- left_join(attendances, HBV_tests_NHSn, by= "NHS_NUMBER")
# 
# # format dates                              
# HBV_TESTS_JOINED3 <- HBV_TESTS_JOINED3 %>%
#   mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE, format = '%Y-%m-%d'))%>%
#   mutate(INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE, format = '%Y-%m-%d'))%>%
#   mutate(SpecDate = as.Date(SpecimenDate, format = '%Y-%m-%d'))
# 
# 
# #Create column for date range (investigation date + 7) inclusion to check whether
# #tests were carried out during person's a+e visit
# HBV_TESTS_JOINED3 <- HBV_TESTS_JOINED3 %>% 
#   mutate(max_SPEC_date = ARRIVAL_DATE + 8, 
#          include = case_when(
#            (SpecDate > INVESTIGATION_DATE-1 & SpecDate < max_SPEC_date)~ "INCLUDE",
#            FALSE ~ "EXCLUDE"))
# 
# 
# # create table of included tests
# included_tests_HBV3 <- HBV_TESTS_JOINED3 %>%
#   filter(include == "INCLUDE" & live_HBV == "LIVE")
# 

# # find number of tests by Trust
# totaltestsbytrust_HBV3 <- included_tests_HBV3 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
#   group_by(Provider_derived) %>%
#   count() %>%
#   select(Provider_derived, n)
# 
# write.csv(totaltestsbytrust_HBV3, "HBVtestsbytrust3.csv")
# 
# 
# # find number of tests by Site
# totaltestsbysite_HBV3 <- included_tests_HBV3 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
#   group_by(SITE) %>%
#   count() %>%
#   select(SITE, n)
# 
# write.csv(totaltestsbysite_HBV3, "HBVtestsbysite3.csv")


####### HBV tests by attendees (no blood tests)
# dedup the HBV tests attendances (no blood tests) table by NHS_NUMBER

# included_tests_HBV4 <- included_tests_HBV3%>%
#   distinct(NHS_NUMBER, .keep_all = TRUE)


# # find number of tests by Trust
# totaltestsbytrust_HBV4 <- included_tests_HBV4 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
#   group_by(Provider_derived) %>%
#   count() %>%
#   select(Provider_derived, n)
# 
# write.csv(totaltestsbytrust_HBV4, "HBVtestsbytrust4.csv")
# 
# 
# # find number of tests by Site
# totaltestsbysite_HBV4 <- included_tests_HBV4 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
#   group_by(SITE) %>%
#   count() %>%
#   select(SITE, n)
# 
# write.csv(totaltestsbysite_HBV4, "HBVtestsbysite4.csv")


####### HBV tests for attendees (with blood tests)
# dedup the HBV tests attendances with blood tests table by NHS_NUMBER
included_tests_HBV5 <- included_tests_HBV%>%
  distinct(NHS_NUMBER, .keep_all = TRUE)


# # find number of tests by Trust
# totaltestsbytrust_HBV5 <- included_tests_HBV5 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
#   group_by(Provider_derived) %>%
#   count() %>%
#   select(Provider_derived, n)
# 
# write.csv(totaltestsbytrust_HBV5, "HBVtestsbytrust5.csv")
# 
# 
# # find number of tests by Site
# totaltestsbysite_HBV5 <- included_tests_HBV5 %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
#   group_by(SITE) %>%
#   count() %>%
#   select(SITE, n)
# 
# write.csv(totaltestsbysite_HBV5, "HBVtestsbysite5.csv")







###HBV

# Links HBV diagnosis table to ECDS attendances with a blood test and 
# then dedups to one record per NHS number - to be linked onto attendees table

##Sentinel linking script
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

# remove all but data needed - leave commented out
#rm(list= ls()[!(ls() %in% c('attendancesbloodtests'))])

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

# create list of sites in sentinel
sentinel_sites <-c('RYJ02',	'RQM06','RQM01', 'RQM25',	'RJ611',	'RAS01',	'RQXM1',	'RJZ01',	'RAX01',	'R0A02', 'RM326', 'RJZ30',	'R1HNH',	'RW602', 'R0A66', 'RM318',
                   'RJ253',	'RXH01', 'E0A3H',	'RJ701',	'RYJ01',	'RJ122',	'R1H12',	'RQM91',	'R1HKH',	'R0A07', 'RM325'
)

# filter ECDS attendances to only include sentinel sites
attendances_sentinel_bt <- attendancesbloodtests%>%
  filter (SITE %in%sentinel_sites)

attendances_sentinel_bt <- attendancesbloodtests%>%
  filter (SITE %in%five_included_sites)

#### linking to attendances with a blood test ########################################################


#Code diagnoses that have an NHS number
HBV_diag_NHS <- HBV_DIAGNOSES_SENTINEL %>% mutate(NA_tally = if_else(is.na(NHSNumber),0,1))

# Select diagnoses that have an NHS number
HBV_diag_NHSn <- HBV_diag_NHS %>% filter(NA_tally == 1) 

# join NHS number and spec date of HCV diagnoses to ECDS attendances with blood tests table (sentinel sites)
HBV_DIAG_JOINED <- left_join(attendancesbloodtests, HBV_diag_NHSn, by= c("NHS_NUMBER" = "NHSNumber"))
HBV_DIAG_JOINED <- left_join(attendances_sentinel_bt, HBV_diag_NHSn, by= c("NHS_NUMBER" = "NHSNumber"))
# mutate dates
HBV_DIAG_JOINED <- HBV_DIAG_JOINED%>%
  mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE),
         INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE),
         SPECDATE = as.Date(FirstHBVtestdate, FORMAT = '%Y-%m-%d'))

#Create column for date range (investigation date + 8) inclusion to check whether
#tests were carried out during person's a+e visit

HBV_DIAG_JOINED <- HBV_DIAG_JOINED %>% mutate(max_SPEC_date = ARRIVAL_DATE + 8,
                                              include = case_when(
                                                (SPECDATE > INVESTIGATION_DATE-1 & SPECDATE < max_SPEC_date)~ "INCLUDE",
                                                FALSE ~ "EXCLUDE"))


# Dedup to one record per NHS number and remove columns not needed
HBV_included_diagnoses_sentinel <- HBV_DIAG_JOINED %>%
  filter(include == "INCLUDE" & live_HBV == 'LIVE')%>%
  distinct(NHS_NUMBER, .keep_all = TRUE)%>%
  select (-c(INVESTIGATION_CODE, INVESTIGATION_CODE_Description, InvestigationIdx, 
             CHIEF_COMPLAINT, CHIEF_COMPLAINT_Description, FYEAR.x,
             PROVIDER_CODE_DERIVED, INVESTIGATION_DATE,
             AGE_AT_ARRIVAL, LSOA_2011, ETHNIC_CATEGORY, London_Provider_ICS, Provider_derived ))


# # find number of HBsAg positives by Site
# totaldiagnosesbysite_HBV <- HBV_included_diagnoses_sentinel %>%
#   filter(HBsAg == 'Positive' | HBsAg == "Reactive")%>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, SITE)%>%
#   group_by(SITE) %>%
#   count() %>%
#   select(SITE, n)
# 
# 
# write.csv(totaldiagnosesbysite_HBV, "HBVdiagnosesbysite.csv")
# 
# #regroup to find total diagnoses by month
# totaldiagnosesbymonth_HBV <- HBV_included_diagnoses_sentinel %>%
#   distinct(ARRIVAL_DATE, NHS_NUMBER, month)%>%
#   group_by(month)%>%
#   count()%>%
#   select(month, n)
# 
# write.csv(totaldiagnosesbymonth_HBV, "HBVdiagnosesbymonth.csv")




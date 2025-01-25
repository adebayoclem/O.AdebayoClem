if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
if (!require(DBI)) install.packages("DBI")
if (!require(odbc)) install.packages("odbc")
if (!require(arrow)) install.packages("arrow")

# read in parquet of attendances with blood tests
attendancesbloodtests<- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendances_with_blood_tests.parquet")

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

# load HBV diagnosis data from Sentinel
HBV_DIAGNOSES_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
select * from SentinelApril2022v3
where DATE_RECEIVED < '2023-02-01'
AND HBV like 'positive'"
)

# don't do this because not all addresses are included in these definitions - keeping for a record of go live dates
# and ((ADDRESS2 like 'Newham%' AND DATE_RECEIVED >= '2022-04-25') 
# OR (ADDRESS2 like 'Royal London%' AND DATE_RECEIVED >= '2022-04-25') 
# OR (ADDRESS2 like 'Whipps%' AND DATE_RECEIVED >= '2022-04-04') 
# OR (ADDRESS2 like 'Chelsea%' AND DATE_RECEIVED >= '2022-04-01') 
# OR (ADDRESS2 like 'West Middlesex%' AND DATE_RECEIVED >= '2022-04-01') 
# OR (ADDRESS2 like 'NORTH MIDDLESEX HOSPITAL' AND DATE_RECEIVED >= '2022-04-01') 
# OR (ADDRESS2 like 'ST MARY%' AND DATE_RECEIVED >= '2022-08-15')
# OR (ADDRESS2 like 'Charing%' AND DATE_RECEIVED >='2022-07-01')
# OR (ADDRESS2 like 'Hillingdon%' AND DATE_RECEIVED >= '2022-07-22')
# OR (ADDRESS2 like 'Kings College%' AND DATE_RECEIVED >= '2022-10-01')
# OR (ADDRESS2 like 'King''s College%' AND DATE_RECEIVED >= '2022-10-01')
# OR (ADDRESS2 like 'St Thomas%' AND DATE_RECEIVED >= '2022-10-01')
# OR (ADDRESS2 like 'Croydon%' AND DATE_RECEIVED >= '2022-10-01')
# OR (ADDRESS2 like 'St George%' AND DATE_RECEIVED >= '2022-10-01'))"
# )

HCV_DIAGNOSES_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
select * from SentinelApril2022v3
where DATE_RECEIVED < '2023-02-01'
AND HCV like 'positive'"
)

# and ((ADDRESS2 like 'Newham%' AND DATE_RECEIVED >= '2022-04-01') 
# OR (ADDRESS2 like 'Royal London%' AND DATE_RECEIVED >= '2022-04-01') 
# OR (ADDRESS2 like 'Whipps%' AND DATE_RECEIVED >= '2022-04-04') 
# OR (ADDRESS2 like 'Chelsea%' AND DATE_RECEIVED >= '2022-04-01') 
# OR (ADDRESS2 like 'West Middlesex%' AND DATE_RECEIVED >= '2022-04-01') 
# OR (ADDRESS2 like 'NORTH MIDDLESEX HOSPITAL' AND DATE_RECEIVED >= '2022-04-01') 
# OR (ADDRESS2 like 'ST MARY%' AND DATE_RECEIVED >= '2022-08-15')
# OR (ADDRESS2 like 'Charing%' AND DATE_RECEIVED >='2022-07-01')
# OR (ADDRESS2 like 'Hillingdon%' AND DATE_RECEIVED >= '2022-07-22')
# OR (ADDRESS2 like 'Kings College%' AND DATE_RECEIVED >= '2022-10-01')
# OR (ADDRESS2 like 'King''s College%' AND DATE_RECEIVED >= '2022-10-01')
# OR (ADDRESS2 like 'St Thomas%' AND DATE_RECEIVED >= '2022-10-01')
# OR (ADDRESS2 like 'Croydon%' AND DATE_RECEIVED >= '2022-10-01')
# OR (ADDRESS2 like 'St George%' AND DATE_RECEIVED >= '2022-10-01'))"
# )

# create list of sites in sentinel
sentinel_sites <-c('RYJ02',	'RQM06','RQM01', 'RQM25',	'RJ611',	'RAS01',	'RQXM1',	'RJZ01',	'RAX01',	'R0A02', 'RM326', 'RJZ30',	'R1HNH',	'RW602', 'R0A66', 'RM318',
                   'RJ253',	'RXH01', 'E0A3H',	'RJ701',	'RYJ01',	'RJ122',	'R1H12',	'RQM91',	'R1HKH',	'R0A07', 'RM325'
)

# filter ECDS attendances to only include sentinel sites
attendances_sentinel <- attendancesbloodtests%>%
  filter (SITE %in%sentinel_sites)


# Get Sentinel diagnoses with NHS numbers - separately for each dataset HBV and HCV
#Code diagnoses that have an NHS number
HBV_diag_NHS <- HBV_DIAGNOSES_SENTINEL %>% mutate(NA_tally = if_else(is.na(NHS_NUMBER),0,1))
HCV_diag_NHS <- HCV_DIAGNOSES_SENTINEL %>% mutate(NA_tally = if_else(is.na(NHS_NUMBER),0,1))

# Select diagnoses that have an NHS number - include record location and address to look more into these later
HBV_diag_NHSn <- HBV_diag_NHS %>% filter(NA_tally == 1) %>% select(NHS_NUMBER, DATE_RECEIVED, RECORD_LOCATION, ADDRESS1, ADDRESS2)
HCV_diag_NHSn <- HCV_diag_NHS %>% filter(NA_tally == 1) %>% select(NHS_NUMBER, DATE_RECEIVED, RECORD_LOCATION, ADDRESS1, ADDRESS2)

# join NHS number and spec date of HBV/HCV diagnoses to ECDS attendances with blood tests table (sentinel sites)
HBV_DIAG_JOINED <- left_join(attendances_sentinel, HBV_diag_NHSn, by= "NHS_NUMBER")
HCV_DIAG_JOINED <- left_join(attendances_sentinel, HCV_diag_NHSn, by= "NHS_NUMBER")

# format dates                              
HBV_DIAG_JOINED <- HBV_DIAG_JOINED%>%
  mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE),
         INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE),
         SPECDATE = as.Date(DATE_RECEIVED, FORMAT = '%Y-%m-%d'))

HCV_DIAG_JOINED <- HCV_DIAG_JOINED%>%
  mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE),
         INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE),
         SPECDATE = as.Date(DATE_RECEIVED, FORMAT = '%Y-%m-%d'))

#Create column for date range (investigation date + 8) inclusion to check whether
#tests were carried out during person's a+e visit
HBV_DIAG_JOINED <- HBV_DIAG_JOINED %>% mutate(max_SPEC_date = ARRIVAL_DATE + 8,
                                              include = case_when(
                                                (SPECDATE > INVESTIGATION_DATE-1 & SPECDATE < max_SPEC_date)~ "INCLUDE",
                                                FALSE ~ "EXCLUDE"))
                                              

HCV_DIAG_JOINED <- HCV_DIAG_JOINED %>% mutate(max_SPEC_date = ARRIVAL_DATE + 8,
                                              include = case_when(
                                                (SPECDATE > INVESTIGATION_DATE-1 & SPECDATE < max_SPEC_date)~ "INCLUDE",
                                                FALSE ~ "EXCLUDE"))
                                              


# create table of included diagnoses
HBV_included_diagnoses_sentinel <- HBV_DIAG_JOINED %>%
  filter(include == "INCLUDE")%>%
  distinct(NHS_NUMBER, ARRIVAL_DATE, .keep_all = TRUE)%>%
  select (-c(INVESTIGATION_CODE, INVESTIGATION_CODE_Description, InvestigationIdx, CHIEF_COMPLAINT, CHIEF_COMPLAINT_Description, FYEAR, month))

HCV_included_diagnoses_sentinel <- HCV_DIAG_JOINED %>%
  filter(include == "INCLUDE")%>%
  distinct(NHS_NUMBER, ARRIVAL_DATE, .keep_all = TRUE)%>%
  select (-c(INVESTIGATION_CODE, INVESTIGATION_CODE_Description, InvestigationIdx, CHIEF_COMPLAINT, CHIEF_COMPLAINT_Description, FYEAR, month))



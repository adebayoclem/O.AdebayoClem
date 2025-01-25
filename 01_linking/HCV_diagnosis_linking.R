###HCV

# Links HCV diagnosis table to ECDS attendances with a blood test and 
# then dedups to one record per NHS number - to be linked onto attendees table

##Sentinel linking script
if (!require(tidyverse)) install.packAges("tidyverse")
if (!require(lubridate)) install.packAges("lubridate")
if (!require(readxl)) install.packAges("readxl")
if (!require(DBI)) install.packAges("DBI")
if (!require(odbc)) install.packAges("odbc")
if (!require(arrow)) install.packAges("arrow")

# read in parquet of attendances with blood tests
# attendancesbloodtests<- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendances_with_blood_tests.parquet")

# 5 sites version
attendancesbloodtests<- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendances_blood_tests_5.parquet")


# remove all but data needed
#rm(list= ls()[!(ls() %in% c('attendancesbloodtests'))])

# load connection to Sentinel
Sentinel <- odbc::dbConnect(odbc::odbc(),
                            .connection_string = "driver={SQL Server};server=SQLClusColHFN17\\HFN17;
                             database=NIS_HepatitisSentinel;
                             Encrypt=true;trusted_connection=true",
                            timeout = 60,
                            timezone = Sys.timezone(),
                            timezone_out = Sys.timezone())


# Load the HCV diagnoses table from Sentinel
HCV_DIAGNOSES_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
select * from SentinelEDApril22_HCVnew
where FirstHCVtestdate <= '2023-04-07'AND (antiHCV like 'Positive' or HCVRNA like 'Positive')")


# create list of sites in sentinel
sentinel_sites <-c('RYJ02',	'RQM06','RQM01', 'RQM25',	'RJ611',	'RAS01',	'RQXM1',	'RJZ01',	'RAX01',	'R0A02', 'RM326', 'RJZ30',	'R1HNH',	'RW602', 'R0A66', 'RM318',
                   'RJ253',	'RXH01', 'E0A3H',	'RJ701',	'RYJ01',	'RJ122',	'R1H12',	'RQM91',	'R1HKH',	'R0A07', 'RM325'
)

# filter ECDS attendances to only include sentinel sites
attendances_sentinel_bt <- attendancesbloodtests%>%
  filter (SITE %in%sentinel_sites)


## data cleaning for HCV_DIAGNOSES_SENTINEL ########

# format dates  

HCV_DIAG_SENTINEL <- HCV_DIAGNOSES_SENTINEL %>% 
  mutate(antiHCVdate =as.Date(antiHCVdate, format = '%Y-%m-%d'),
         HCVRNAdate = as.Date(HCVRNAdate, format = '%Y-%m-%d'),
         FirstHCVtestdate = as.Date(FirstHCVtestdate, format = '%Y-%m-%d'),
         EED = as.Date(EED, format = '%Y-%m-%d'),
         preRxdate = as.Date(preRxdate, format = '%Y-%m-%d'),
         postRxdate = as.Date(postRxdate, format = '%Y-%m-%d'),
         mincreatedate = as.Date(mincreatedate, format = '%Y-%m-%d'),
         minreferraldate = as.Date(minreferraldate, format = '%Y-%m-%d'),
         earliestRxdate = as.Date(earliestRxdate, format = '%Y-%m-%d'),
         minsentineldate = as.Date(minsentineldate, format = '%Y-%m-%d'),
         DOB = as.Date(DOB, format = '%Y-%m-%d')) 

# lower case 
HCV_DIAG_SENTINEL <- mutate(HCV_DIAG_SENTINEL, antiHCV = tolower(antiHCV)) 
HCV_DIAG_SENTINEL <- mutate(HCV_DIAG_SENTINEL, HCVRNA = tolower(HCVRNA)) 

# Recode PWID

HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(PWID_2 = case_when(PWID == "Current/RecentPWID" | PWID == "Yes" ~ "Current/Recent PWID", # is yes current or past?
                            PWID == "PastPWID" ~ "Past PWID",
                            PWID == "NeverPWID" | PWID == "No" | PWID == "Never PWID" ~ "Never PWID"))  


# Recode some new that were not correctly defined and create rna_positive_new field

HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(Newdiag = case_when(HCVRNA == 'positive' & Newdiag == 'Yes' & (preRxdate < HCVRNAdate | EED < HCVRNAdate) ~ "", TRUE ~ Newdiag))

HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(rna_positive_new = case_when(HCVRNA == 'positive' & Newdiag == 'Yes' ~ "New",
                                      TRUE ~ ""))

#### linking to attendances with a blood test ########################################################


#Code diagnoses that have an NHS number
HCV_diag_NHS <- HCV_DIAG_SENTINEL %>% mutate(NA_tally = if_else(is.na(NHSNumber),0,1))

# Select diagnoses that have an NHS number
HCV_diag_NHSn <- HCV_diag_NHS %>% filter(NA_tally == 1) 

# join NHS number and spec date of HCV diagnoses to ECDS attendances with blood tests table (sentinel sites)
HCV_DIAG_JOINED <- left_join(attendancesbloodtests, HCV_diag_NHSn, by= c("NHS_NUMBER" = "NHSNumber"))

# mutate dates
HCV_DIAG_JOINED <- HCV_DIAG_JOINED%>%
  mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE),
         INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE),
         SPECDATE = as.Date(FirstHCVtestdate, FORMAT = '%Y-%m-%d'))

#Create column for date range (investigation date + 8) inclusion to check whether
#tests were carried out during person's a+e visit

HCV_DIAG_JOINED <- HCV_DIAG_JOINED %>% mutate(max_SPEC_date = ARRIVAL_DATE + 8,
                                              include = case_when(
                                                (SPECDATE > INVESTIGATION_DATE-1 & SPECDATE < max_SPEC_date)~ "INCLUDE",
                                                FALSE ~ "EXCLUDE"))


# Dedup to one record per NHS number and remove columns not needed
HCV_included_diagnoses_sentinel <- HCV_DIAG_JOINED %>%
  filter(include == "INCLUDE" & live_HCV == 'LIVE')%>%
  distinct(NHS_NUMBER, .keep_all = TRUE)%>%
  select (-c(INVESTIGATION_CODE, INVESTIGATION_CODE_Description, InvestigationIdx, 
             CHIEF_COMPLAINT, CHIEF_COMPLAINT_Description, FYEAR.x, month,
             PROVIDER_CODE_DERIVED, SITE, ARRIVAL_MONTH, ARRIVAL_DATE, INVESTIGATION_DATE,
             AGE_AT_ARRIVAL, LSOA_2011, ETHNIC_CATEGORY, London_Provider_ICS, Provider_derived ))

###HCV

# Brings in HCV diagnosis table and does some cleaning
# unlinked so includes all HCV diagnoses coded as ED for sentinel sites

##Load packages
if (!require(tidyverse)) install.packAges("tidyverse")
if (!require(lubridate)) install.packAges("lubridate")
if (!require(readxl)) install.packAges("readxl")
if (!require(DBI)) install.packAges("DBI")
if (!require(odbc)) install.packAges("odbc")
if (!require(arrow)) install.packAges("arrow")


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
  mutate(Newdiag = case_when(HCVRNA == 'positive' & Newdiag == 'Yes' & (preRxdate < HCVRNAdate | EED < HCVRNAdate) ~ "",
                             HCVRNA == 'positive' & is.na (preRxdate) & is.na(EED)& is.na(minsentineldate)~ "Yes",
                             TRUE ~ Newdiag))

HCV_DIAG_SENTINEL <- HCV_DIAG_SENTINEL %>% 
  mutate(rna_positive_new = case_when(HCVRNA == 'positive' & Newdiag == 'Yes' ~ "New",
                                      TRUE ~ ""))

### add go live dates

HCV_DIAG_SENTINEL<- HCV_DIAG_SENTINEL%>%
  mutate(live_HCV = case_when ((Hospital ==  'Charing Cross Hospital' & FirstHCVtestdate >= '2022-07-01') 
                               |(Hospital == 'Chelsea & Westminster Hospital'& FirstHCVtestdate >= '2022-04-01')
                               |(Hospital == 'Croydon University Hospital'& FirstHCVtestdate >= '2023-03-20')
                               |(Hospital == 'Hillingdon Hospital'& FirstHCVtestdate >= '2022-07-22')
                               |(Hospital == 'Homerton University Hospital'& FirstHCVtestdate >= '2022-09-12')
                               |(Hospital == 'Kings College Hospital'& FirstHCVtestdate >= '2022-11-16')
                               |(Hospital == 'Kingston Hospital'& FirstHCVtestdate >= '2023-04-24')
                               |(Hospital == 'Newham General Hospital'& FirstHCVtestdate >= '2022-04-01')
                               |(Hospital == 'Royal Sussex County Hospital'& FirstHCVtestdate >= '2023-03-06')
                               |(Hospital == 'St Georges Hospital'& FirstHCVtestdate >= '2022-11-17')
                               |(Hospital == 'St Marys Hospital'& FirstHCVtestdate >= '2022-08-15')
                               |(Hospital == 'St Thomas Hospital'& FirstHCVtestdate >= '2022-11-01')
                               |(Hospital == 'The Royal London Hospital'& FirstHCVtestdate >= '2022-04-01')
                               |(Hospital == 'West Middlesex University Hospital'& FirstHCVtestdate >= '2022-04-01')
                               |(Hospital == 'Whipps Cross University Hospital'& FirstHCVtestdate >= '2022-04-04')
                               ~ "LIVE",
                               TRUE~ "NOT LIVE"))%>%
  filter(live_HCV == 'LIVE')

# # find number of RNA positives by Site
# totaldiagnosesbysite_HCVrna <- HCV_DIAG_SENTINEL %>%
#   filter(HCVRNA == 'positive')%>%
#   distinct(FirstHCVtestdate, NHSNumber, Hospital)%>%
#   group_by(Hospital) %>%
#   count() %>%
#   select(Hospital, n)
# 
# write.csv(totaldiagnosesbysite_HCVrna, "HCVrnadiagnoses_site_not_linked.csv")
# 
# # find number of anti-HCV positives by Site
# totaldiagnosesbysite_antiHCV <- HCV_DIAG_SENTINEL %>%
#   filter(antiHCV == 'positive')%>%
#   distinct(FirstHCVtestdate, NHSNumber, Hospital)%>%
#   group_by(Hospital) %>%
#   count() %>%
#   select(Hospital, n)
# 
# write.csv(totaldiagnosesbysite_antiHCV, "antiHCVdiagnoses_site_not_linked.csv")










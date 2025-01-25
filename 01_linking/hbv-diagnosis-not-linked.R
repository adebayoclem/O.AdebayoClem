###HBV

# Brings in HBV diagnosis table - unlinked

##Sentinel linking script
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
if (!require(DBI)) install.packages("DBI")
if (!require(odbc)) install.packages("odbc")
if (!require(arrow)) install.packages("arrow")


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

# format dates  
HBV_DIAGNOSES_SENTINEL <- HBV_DIAGNOSES_SENTINEL %>% 
  mutate(HBsAGdate =as.Date(HBsAGdate, format = '%Y-%m-%d'),
         HBV_DNAdate = as.Date(HBV_DNAdate, format = '%Y-%m-%d'),
         HBcIgMdate  = as.Date(HBcIgMdate , format = '%Y-%m-%d'),
         HBctotaldate  = as.Date(HBctotaldate, format = '%Y-%m-%d'),
         DOB = as.Date(DOB, format = '%Y-%m-%d'),
         FirstHBVtestdate = as.Date(FirstHBVtestdate, format = '%Y-%m-%d')) 


### add go live dates

HBV_DIAGNOSES_SENTINEL<- HBV_DIAGNOSES_SENTINEL%>%
  mutate(live_HBV = case_when ((Hospital ==  'Charing Cross Hospital' & FirstHBVtestdate >= '2022-07-01') 
                               |(Hospital == 'Chelsea & Westminster Hospital'& FirstHBVtestdate >= '2022-04-01')
                               |(Hospital == 'Croydon University Hospital'& FirstHBVtestdate >= '2023-03-20')
                               |(Hospital == 'Hillingdon Hospital'& FirstHBVtestdate >= '2022-07-22')
                               |(Hospital == 'Homerton University Hospital'& FirstHBVtestdate >= '2022-09-12')
                               |(Hospital == 'Kings College Hospital'& FirstHBVtestdate >= '2022-11-16')
                               |(Hospital == 'Kingston Hospital'& FirstHBVtestdate >= '2023-04-24')
                               |(Hospital == 'Newham General Hospital'& FirstHBVtestdate >= '2022-04-25')
                               |(Hospital == 'St Georges Hospital'& FirstHBVtestdate >= '2022-11-17')
                               |(Hospital == 'St Marys Hospital'& FirstHBVtestdate >= '2022-08-15')
                               |(Hospital == 'St Thomas Hospital'& FirstHBVtestdate >= '2022-11-01')
                               |(Hospital == 'The Royal London Hospital'& FirstHBVtestdate >= '2022-04-25')
                               |(Hospital == 'West Middlesex University Hospital'& FirstHBVtestdate >= '2022-04-01')
                               |(Hospital == 'Whipps Cross University Hospital'& FirstHBVtestdate >= '2022-04-25')
                               ~ "LIVE",
                               TRUE~ "NOT LIVE"))%>%
  filter(live_HBV == 'LIVE')


                


# find number of HBsAg positives by Site
totaldiagnosesbysite_HBV <- HBV_DIAGNOSES_SENTINEL %>%
  distinct(FirstHBVtestdate, NHSNumber, Hospital)%>%
  group_by(Hospital) %>%
  count() %>%
  select(Hospital, n)

write.csv(totaldiagnosesbysite_HBV, "hbv-diag-unlinked-site.csv")




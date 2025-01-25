# This script connects to source databases and sets up data tables for analysis
# Data tables created are:
# ECDS_2223_main - main ECDS data table
# ECDS_JOIN_PII - ECDS_2223_main joined with PII - main data table used for ECDS data analysis and linkage
# HIV_tests, HCV_tests and HBV_tests - all HIV, HCV or HBV tests in Sentinel since March 2022 (NOT limited to ED only - all tests)
# HCV_cases - all HCV cases in SGSS since April 2022 (NOT limited to ED only - all cases) - need to add equivalent for HBV ??
# HCVED and HBVED - HCV / HBV cases in SGSS selected by site codes - need to check these are up to date
# HCV_treatment_notif and HCV_treatment_patients - treatment data - WIP


#load or install required packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
if (!require(DBI)) install.packages("DBI")
if (!require(odbc)) install.packages("odbc")
if (!require(arrow)) install.packages("arrow")


# Establish ODBC connections -----------------------------------------------

#Access to Y006 and sentinel needed

#ESTABLISH ODBC CONNECTION WITH DATA
Y006 <- odbc::dbConnect(odbc::odbc(),
                           .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;
                             database=Y006_BBV_PID;
                             Encrypt=true;trusted_connection=true",
                           timeout = 60,
                           timezone = Sys.timezone(),
                           timezone_out = Sys.timezone())



ECDS <- odbc::dbConnect(odbc::odbc(),
                        .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;
                             database=ECDS;
                             Encrypt=true;trusted_connection=true",
                        timeout = 60,
                        timezone = Sys.timezone(),
                        timezone_out = Sys.timezone())



Sentinel <- odbc::dbConnect(odbc::odbc(),
                        .connection_string = "driver={SQL Server};server=SQLClusColHFN17\\HFN17;
                             database=NIS_HepatitisSentinel;
                             Encrypt=true;trusted_connection=true",
                        timeout = 60,
                        timezone = Sys.timezone(),
                        timezone_out = Sys.timezone())

NIS_Hepatitis <- odbc::dbConnect(odbc::odbc(),
                                 .connection_string = "driver={SQL Server};server=SQLClusColHFN17\\HFN17;
                             database=NIS_Hepatitis;
                             Encrypt=true;trusted_connection=true",
                                 timeout = 60,
                                 timezone = Sys.timezone(),
                                 timezone_out = Sys.timezone())

#ESTABLISH ODBC CONNECTION FOR LSOA - IMD LOOKUPS
Lookup <- odbc::dbConnect(odbc::odbc(),
                          .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;
                             database=LookupsShared;
                             Encrypt=true;trusted_connection=true",
                          timeout = 60,
                          timezone = Sys.timezone(),
                          timezone_out = Sys.timezone())





# Run queries to get data tables ------------------------------------------

####ECDS DATA ####

##Main ECDS query
##This will take a while as approx 4.5m rows (approx 30 mins)
##As of March 14, Nov is latest data period

##Only need to run this query if there is new data - otherwise stored in parquet format in //filepor10/datalake$/Y006_BBV_PID/ED evaluation

#source("ECDS_main_query.R")

##to rewrite file once query run
#ECDS_2223_main %>%
#write_dataset(path = "//filepor10/datalake$/Y006_BBV_PID/ED evaluation/attendances/ECDS_main", format = "parquet")
#write_parquet(ECDS_2223_main, "//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/ECDS_2223_main.parquet" )

#ECDS_2223_main <- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/ECDS_223_main.parquet")

##Query to get PII for linkage
#ECDS_PII <- DBI::dbGetQuery(conn = Y006, statement = "SELECT
#[FYEAR]
#,[rowid]
#,[NHS_NUMBER]
#,[BIRTH_DATE]
#,[POSTCODE]
#FROM dbo.vECDS_PID
#WHERE FYEAR = '2223' "
#)


##Join PII onto ECDS table
#ECDS_JOIN_PII <- left_join(ECDS_2223_main, ECDS_PII, by=c("FYEAR" = "FYEAR",
#"PheKey" = "rowid"))
#read in IMD lookup
#LookupIMD <- Lookup %>%
#tbl( "vSocioDemog_LSOA11") %>%
#select(LSOA11CD, IMD2019_Deciles_LSOA11_withinUTLA21) %>%
#rename(LSOA_2011 = LSOA11CD, IMD_19 = IMD2019_Deciles_LSOA11_withinUTLA21)

#LookupIMD <- data.frame(LookupIMD)

#ECDS_JOIN_PII <- ECDS_JOIN_PII %>%
#left_join(LookupIMD, by = "LSOA_2011")

##to rewrite file once query run
#ECDS_JOIN_PII %>%
#write_dataset(path = "//filepor10/datalake$/Y006_BBV_PID/ED evaluation/attendances/ECDS_PII", format = "parquet")
#write_parquet(ECDS_JOIN_PII, "//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/ECDS_JOIN_PII.parquet" )

#to save time can read in data in a parquet format rather than running the query every time - if no new data 
#ECDS_JOIN_PII <- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/attendances/ECDS_JOIN_PII/part-0.parquet")
ECDS_JOIN_PII <- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/ECDS_JOIN_PII.parquet")


####Sentinel####

#Create table of HIV tests carried out in ED in participating sites from April 2022 to Mar 2023
#Sometimes these column names change so query may need amending
HIV_TESTS_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
SELECT [PATIENT_ID]
,[RECORD_LOCATION]
,[DUPE_SAMPLE]
,[DATETEST]
,MONTH([DATETEST]) TESTMONTH
,YEAR([DATETEST]) year
,[SEX]
,[AGE]
,[NHS_NUMBER]
,[AGE_GROUP]
,[SUM_ETHNICITY]
,[SPECIALITY]
,[SPECGROUP]
,[PHLS]
,[PCT_NAME]
,[SAMPLETYPE]
,[include]
,[Exclude]
,[Age1]
,[Ethnicity]
,ADDRESS
,Postcode
FROM [NIS_HepatitisSentinel].[dbo].[HIVTESTS]
WHERE ((SPECIALITY = 'CASUALAE'
or (SPECGROUP = 'HOSPITAL' and (SPECIALITY = 'other' or SPECIALITY = 'wardunk')))
AND ((ADDRESS like 'Newham%' AND DATETEST >= '2022-04-01') 
OR (ADDRESS like 'Royal London%' AND DATETEST >= '2022-04-01') 
OR (ADDRESS like 'Whipps%' AND DATETEST >= '2022-04-04') 
OR (ADDRESS like 'Homerton%' AND DATETEST >= '2022-04-01') 
OR (ADDRESS like 'Chelsea%' AND DATETEST >= '2022-04-01') 
OR (ADDRESS like 'West Middlesex%' AND DATETEST >= '2022-04-01') 
OR (ADDRESS like 'NORTH MIDDLESEX HOSPITAL' AND DATETEST >= '2022-04-01') 
OR (ADDRESS like 'ST MARY%' AND DATETEST >= '2022-04-01')
OR (ADDRESS like 'Charing%' AND DATETEST >='2022-04-01')
OR (ADDRESS like 'Hillingdon%' AND DATETEST >= '2022-07-22')
OR (ADDRESS like 'Kings College%' AND DATETEST >= '2022-04-01')
OR (ADDRESS like 'Princess Royal%' AND DATETEST >= '2022-04-21')
OR (ADDRESS like 'St Thomas%' AND DATETEST >= '2022-04-01')
OR (ADDRESS like 'Croydon%' AND DATETEST >= '2022-04-01')
OR (ADDRESS like 'St George%' AND DATETEST >= '2022-04-01')
OR (ADDRESS like 'Kingston%' AND DATETEST >= '2022-04-01')
OR (ADDRESS like 'Royal Sussex%' AND DATETEST >= '2022-04-01')
OR (ADDRESS like 'Manchester Royal%' AND DATETEST >= '2022-04-01')
OR (ADDRESS like '%NORTH M/C GENERAL HOSPITAL%' AND DATETEST >= '2022-04-01')
OR (ADDRESS like '%NORTH MANCHESTER GENERAL HOSPITAL%'AND DATETEST >= '2022-04-01')
OR (ADDRESS like 'Wythenshawe%' AND DATETEST >= '2022-04-01'))
AND DATETEST <= '2023-03-31')"
)

##recode age groups
HIV_TESTS_SENTINEL <- HIV_TESTS_SENTINEL %>%
  mutate(AGE_GROUP2 = case_when(AGE >=15 & AGE <= 24 ~ "15-24",
                                AGE >=25 & AGE <= 34 ~ "25-34",
                                AGE >=35 & AGE <= 44 ~ "35-44",
                                AGE >=45 & AGE <= 54 ~ "45-54",
                                AGE >=55 & AGE <= 64 ~ "55-64",
                                AGE >=65 & AGE <= 74 ~ "65-74",
                                AGE >=75 & AGE <= 84 ~ "75-84",
                                AGE >=85 & AGE <= 94 ~ "85-94",
                                AGE > 94 ~ "95+"))

# HCV

HCV_tests <- DBI::dbGetQuery(conn = Sentinel , statement = "
SELECT
[PATID]
,[RECORD_LOCATION]
,[DUPE_SAMPLE]
,[DATETEST]
,MONTH([DATETEST]) TESTMONTH
,YEAR([DATETEST]) year
,[SEX]
,[AGE]
,[NHSNUMBER]
,[AGE_GROUP]
,[ETHNICITY]
,[SPECIALITY]
,[SPECGROUP]
,[PHLS2]
,[PCT_NAME]
,[SAMPLETYPE]
,[include]
,[Exclude]
,[Age1]
,ADDRESS
,Postcode
FROM [NIS_HepatitisSentinel].[dbo].[HCVTESTS]
WHERE (((SPECIALITY = 'CASUALAE')
or (SPECGROUP = 'HOSPITAL' and (SPECIALITY = 'other' or SPECIALITY = 'wardunk')))
AND (ADDRESS like 'Newham%' 
OR ADDRESS like 'Royal London%'
OR ADDRESS like 'Whipps%'
OR ADDRESS like 'Homerton%'
OR ADDRESS like 'Chelsea%'
OR ADDRESS like 'West Middlesex%'
OR ADDRESS like 'NORTH MIDDLESEX HOSPITAL'
OR ADDRESS like 'ST MARY%'
OR ADDRESS like 'Charing%'
OR ADDRESS like 'Hillingdon%'
OR ADDRESS like 'Kings College%'
OR ADDRESS like 'Princess Royal%'
OR ADDRESS like 'St Thomas%'
OR ADDRESS like 'Croydon%'
OR ADDRESS like 'St George%'
OR ADDRESS like 'Kingston%'
OR ADDRESS like 'Royal Sussex%'
OR ADDRESS like 'Manchester Royal%'
OR ADDRESS like '%NORTH M/C GENERAL HOSPITAL%'
or ADDRESS like '%NORTH MANCHESTER GENERAL HOSPITAL%'
OR ADDRESS like 'Wythenshawe%')
AND DATETEST BETWEEN '2022-01-04' AND '2023-03-31')"                           
)

#find trust by pct_name- wont be exact match
##
HBV_tests <- DBI::dbGetQuery(conn = Sentinel , statement = "SELECT
a.[PATIENT_ID]
,a.[DUPE_SAMPLE]
,b.NHS_NUMBER
,a.[DATETEST]
,a.[PCT_NAME]
,a.[Resultoverview2]
,MONTH([DATETEST]) TESTMONTH
FROM [NIS_HepatitisSentinel].[dbo].[HBVTESTS] as a
LEFT JOIN [NIS_HepatitisSentinel].[dbo].[PATIENTIDENTIFIERS_Nov2022] as b
ON a.PATIENT_ID = b.PATIENT_ID
WHERE DATETEST > '2022-02-28' AND SPECGROUP = 'CASUALAE' "                           
)

####Hepatitis cases####


HCV_cases <- DBI::dbGetQuery(conn = NIS_Hepatitis, statement = "SELECT
[NID]
,[FINALID]
,[DUPID]
,[DONOTUSE]
,[DateEntered]
,[NHS_Number]
,[Test_Method]
,[RequestingOrganisationType]
FROM [NIS_Hepatitis].[dbo].[HCV_ALL]
WHERE SpecimenDate > '2022/03/31' and Speciality_of_test = 'emergency department'")


##this is still under construction

HCVED <- DBI::dbGetQuery(conn = NIS_Hepatitis, statement = "SELECT*
FROM [NIS_Hepatitis].[dbo].[HCVED]
WHERE ([SiteCode] LIKE 'RAL%' 
    OR [SiteCode] LIKe 'RAP%' 
    OR [SiteCode] LIKE 'RRV%'
    OR [SiteCode] LIKE 'RKE%' 
    OR [SiteCode] LIKE 'R1H%' 
    OR [SiteCode] LIKe 'RF4%' 
    OR [SiteCode] LIKE 'RQX%' 
    OR [SiteCode] LIKE 'RQM%' 
    OR [SiteCode] LIKE 'RYJ%' 
    OR [SiteCode] LIKE 'R1K%' 
    OR [SiteCode] LIKE 'RAS%' 
    OR [SiteCode] LIKE'RJZ%' 
    OR [SiteCode] LIKE'RJ2%' 
    OR [SiteCode] LIKE 'RJ1%' 
    OR [SiteCode] LIKE 'RJ6%' 
    OR [SiteCode] LIKE 'RJ7%' 
    OR [SiteCode] LIKE 'RVR%' 
    OR [SiteCode] LIKE 'RAX%' 
    OR [SiteCode] LIKE 'RTP%' 
    OR [SiteCode] LIKE 'R0A%' 
    OR [SiteCode] LIKE 'RM3%'
    OR [SiteCode] LIKE 'RXL%')
 AND [Age] > 15")


#this needs a play around with - currently not pulling in - mostly ignore these until fixed 
#23/05 - this section is now running without errors
HBVED <- DBI::dbGetQuery(conn = NIS_Hepatitis, statement = "SELECT 
SUBSTRING([SiteCode],1,len([SiteCode])-2) AS PROVIDER_CODE_DERIVED
,[SiteCode]
,[SiteName]
,[Age]
,[SEX]
,[NHS_Number]
,[Test_Method]
,[Earliesteventdate]
,[Newdiagnosis]

FROM [NIS_Hepatitis].[dbo].[HBVED]

WHERE ([SiteCode] LIKE 'RAL%' 
    OR [SiteCode] LIKe 'RAP%' 
    OR [SiteCode] LIKE 'RRV%'
    OR [SiteCode] LIKE 'RKE%' 
    OR [SiteCode] LIKE 'R1H%' 
    OR [SiteCode] LIKe 'RF4%' 
    OR [SiteCode] LIKE 'RQX%' 
    OR [SiteCode] LIKE 'RQM%' 
    OR [SiteCode] LIKE 'RYJ%' 
    OR [SiteCode] LIKE 'R1K%' 
    OR [SiteCode] LIKE 'RAS%' 
    OR [SiteCode] LIKE 'RJZ%' 
    OR [SiteCode] LIKE 'RJ2%' 
    OR [SiteCode] LIKE 'RJ1%' 
    OR [SiteCode] LIKE 'RJ6%' 
    OR [SiteCode] LIKE 'RJ7%' 
    OR [SiteCode] LIKE 'RVR%' 
    OR [SiteCode] LIKE 'RAX%' 
    OR [SiteCode] LIKE 'RTP%' 
    OR [SiteCode] LIKE 'R0A%' 
    OR [SiteCode] LIKE 'RM3%'
    OR [SiteCode] LIKE 'RXL%')
  AND [Age] > 15") %>%
  mutate(Provider_derived = case_when(PROVIDER_CODE_DERIVED == "RTP" ~ "UNIVERSITY HOSPITALS SUSSEX NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "R0A" ~ "MANCHESTER UNIVERSITY NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RM3" ~ "SALFORD ROYAL NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RXL" ~ "BLACKPOOL TEACHING HOSPITALS NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RRV" ~ "UNIVERSITY COLLEGE LONDON HOSPITALS NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RP4" ~ "GREAT ORMOND STREET HOSPITAL FOR CHILDREN NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RAL" ~ "ROYAL FREE LONDON NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RP6" ~ "MOORFIELDS EYE HOSPITAL NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RAN" ~ "ROYAL NATIONAL ORTHOPAEDIC HOSPITAL NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RAP" ~ "NORTH MIDDLESEX UNIVERSITY HOSPITAL NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RRP" ~ "BARNET, ENFIELD AND HARINGEY MENTAL HEALTH NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RKE" ~ "THE WHITTINGTON HOSPITAL NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RNK" ~ "TAVISTOCK AND PORTMAN NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "R1H" ~ "BARTS HEALTH NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RF4" ~ "BARKING, HAVERING AND REDBRIDGE UNIVERSITY HOSPITALS NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RWK" ~ "EAST LONDON NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RQX" ~ "HOMERTON UNIVERSITY HOSPITAL NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RAT" ~ "NORTH EAST LONDON NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RYJ" ~ "IMPERIAL COLLEGE HEALTHCARE NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RT3" ~ "ROYAL BROMPTON & HAREFIELD NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RQM" ~ "CHELSEA AND WESTMINSTER HOSPITAL NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RKL" ~ "WEST LONDON MENTAL HEALTH NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RV3" ~ "CENTRAL AND NORTH WEST LONDON NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RAS" ~ "THE HILLINGDON HOSPITALS NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RYX" ~ "CENTRAL LONDON COMMUNITY HEALTHCARE NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RJ1" ~ "GUYS AND ST THOMAS NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RJZ" ~ "KINGS COLLEGE HOSPITAL NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RV5" ~ "SOUTH LONDON AND MAUDSLEY NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RJ2" ~ "LEWISHAM HEALTHCARE NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RPG" ~ "OXLEAS NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RJ7" ~ "ST GEORGES HEALTHCARE NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RVR" ~ "EPSOM AND ST HELIER UNIVERSITY HOSPITALS NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RQY" ~ "SOUTH WEST LONDON AND ST GEORGES MENTAL HEALTH NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RJ6" ~ "CROYDON HEALTH SERVICES NHS TRUST",
                                      PROVIDER_CODE_DERIVED == "RAX" ~ "KINGSTON HOSPITAL NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "RPY" ~ "THE ROYAL MARSDEN NHS FOUNDATION TRUST",
                                      PROVIDER_CODE_DERIVED == "R1K" ~ "LONDON NORTH WEST UNIVERSITY HEALTHCARE NHS TRUST"),
         Provider_derived)



####treatment data#####

#will need to link to HCV cases

#notifications - multiple records by patient - see sop for hepatitis linkage and also Ruth
HCV_treatment_notif <- DBI::dbGetQuery(conn = NIS_Hepatitis, statement = "SELECT*
FROM [NIS_Hepatitis].[dbo].[HCV_RX_Notification_Table]")

#individual patients 
HCV_treatment_patients <- DBI::dbGetQuery(conn = NIS_Hepatitis, statement = "SELECT*
                                            FROM [NIS_Hepatitis].[dbo].[HCV_RX_Patient_Table]")
  
  

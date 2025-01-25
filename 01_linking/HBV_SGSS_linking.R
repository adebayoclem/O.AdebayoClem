if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
if (!require(DBI)) install.packages("DBI")
if (!require(odbc)) install.packages("odbc")
if (!require(arrow)) install.packages("arrow")

rm(list= ls()[!(ls() %in% c('attendancesbloodtests'))])
# read in parquet of attendances with blood tests
attendancesbloodtests<- read_parquet("//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/attendances_with_blood_tests.parquet")

# load connection to SGSS
NIS_Hepatitis <- odbc::dbConnect(odbc::odbc(),
                                 .connection_string = "driver={SQL Server};server=SQLClusColHFN17\\HFN17;
                             database=NIS_Hepatitis;
                             Encrypt=true;trusted_connection=true",
                                 timeout = 60,
                                 timezone = Sys.timezone(),
                                 timezone_out = Sys.timezone())


# load HBV diagnosis data from SGSS
#V1 - filtered for relevant site codes and speciality of test is hospital or ED
HBVED <- DBI::dbGetQuery(conn = NIS_Hepatitis, statement = "SELECT 
SUBSTRING([SiteCode],1,len([SiteCode])-2) AS PROVIDER_CODE_DERIVED
,[SiteCode]
,[SiteName]
,[SpecimenDate]
,[Speciality_of_test]
,[Setting_of_test]
,[Age]
,[SEX]
,[NHS_Number]
,[ETHcollated]
,[Test_Method]
,[Earliesteventdate]
,[Newdiagnosis]

FROM [NIS_Hepatitis].[dbo].[HBVEDv4]
WHERE (([SiteCode] LIKE 'RAL%' 
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
  AND [Age] > 15 AND ([Speciality_of_test] LIKE 'Hospital' or [Speciality_of_test] LIKE 'Emergency Department')  AND [SpecimenDate] > '2022-03-31')"
)%>%
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
 

  
# Get SGSS diagnoses with NHS numbers
#Code HBV diagnoses that have an NHS number
HBV_diag_NHS <- HBVED %>% mutate(NA_tally = if_else(is.na(NHS_Number),0,1) )

# Select HBV diagnoses that have an NHS number
HBV_diag_NHSn <- HBV_diag_NHS %>% filter(NA_tally == 1) %>% select(NHS_Number, SpecimenDate)

# join NHS number and spec date of HBV diagnoses to the main ECDS attendances with blood tests table
HBV_DIAG_JOINED <- left_join(attendancesbloodtests, HBV_diag_NHSn, by= c("NHS_NUMBER" = "NHS_Number"))

# format dates                              
HBV_DIAG_JOINED <- HBV_DIAG_JOINED%>%
  mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE),
         INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE),
         SPECDATE = as.Date(SpecimenDate), FORMAT = '%Y-%m-%d')


#Create column for date range (investigation date + 8) inclusion to check whether
#tests were carried out during person's a+e visit
HBV_DIAG_JOINED <- HBV_DIAG_JOINED %>% mutate(max_SPEC_date = ARRIVAL_DATE + 8,
                                              include = case_when(
                                                (SPECDATE > INVESTIGATION_DATE-1 & SPECDATE < max_SPEC_date)~ "INCLUDE",
                                                FALSE ~ "EXCLUDE"))

# create table of included diagnoses
included_diagnoses <- HBV_DIAG_JOINED %>%
  filter(include == "INCLUDE")


# find number of diagnoses by Trust
totaldiagnosesbytrust <- included_diagnoses %>%
  distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived, .keep_all = TRUE)%>%
  group_by(Provider_derived) %>%
  count() %>%
  select(Provider_derived, n)



# second version - no limits on site or setting of test
HBVED2 <- DBI::dbGetQuery(conn = NIS_Hepatitis, statement = "SELECT 
SUBSTRING([SiteCode],1,len([SiteCode])-2) AS PROVIDER_CODE_DERIVED
,[SiteCode]
,[SiteName]
,[SpecimenDate]
,[Speciality_of_test]
,[Setting_of_test]
,[Age]
,[SEX]
,[NHS_Number]
,[ETHcollated]
,[Test_Method]
,[Earliesteventdate]
,[Newdiagnosis]

FROM [NIS_Hepatitis].[dbo].[HBVEDv4]
WHERE ((len([SiteCode])>1)
  AND [Age] > 15  AND [SpecimenDate] > '2022-03-31')"
)%>%
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


# Get SGSS diagnoses with NHS numbers
#Code HBV diagnoses that have an NHS number
HBV_diag_NHS2 <- HBVED2 %>% mutate(NA_tally = if_else(is.na(NHS_Number),0,1) )

# Select HBV diagnoses that have an NHS number
HBV_diag_NHSn2 <- HBV_diag_NHS2 %>% filter(NA_tally == 1) %>% select(NHS_Number, SpecimenDate, Speciality_of_test, Setting_of_test)


# join NHS number and spec date of HBV diagnoses to the main ECDS attendances with blood tests table
HBV_DIAG_JOINED2 <- left_join(attendancesbloodtests, HBV_diag_NHSn2, by= c("NHS_NUMBER" = "NHS_Number"))


# format dates                              
HBV_DIAG_JOINED2 <- HBV_DIAG_JOINED2%>%
  mutate(ARRIVAL_DATE = as.Date(ARRIVAL_DATE),
         INVESTIGATION_DATE = as.Date(INVESTIGATION_DATE),
         SPECDATE = as.Date(SpecimenDate), FORMAT = '%Y-%m-%d')

HBV_DIAG_JOINED2 <- HBV_DIAG_JOINED2%>%
  mutate(SPECDATE2 = as.Date(SpecimenDate.y), FORMAT = '%Y-%m-%d')


#Create column for date range (investigation date + 8) inclusion to check whether
#tests were carried out during person's a+e visit
HBV_DIAG_JOINED2 <- HBV_DIAG_JOINED2 %>% mutate(max_SPEC_date = ARRIVAL_DATE + 8,
                           include = case_when(
                             (SPECDATE > INVESTIGATION_DATE-1 & SPECDATE < max_SPEC_date)~ "INCLUDE",
                             FALSE ~ "EXCLUDE"))

HBV_DIAG_JOINED2 <- HBV_DIAG_JOINED2 %>% mutate(include2 = case_when
                                                ((SPECDATE2 > INVESTIGATION_DATE-1 & SPECDATE2 < max_SPEC_date)~ "INCLUDE",
                                                FALSE ~ "EXCLUDE"))


# create table of included diagnoses
included_diagnoses2 <- HBV_DIAG_JOINED2 %>%
  filter(include == "INCLUDE")



# find number of diagnoses by Trust
totaldiagnosesbytrust2 <- included_diagnoses2 %>%
  distinct(ARRIVAL_DATE, NHS_NUMBER, Provider_derived)%>%
  group_by(Provider_derived) %>%
  count() %>%
  select(Provider_derived, n)

# investigating sites for diagnoses from wider definition
noned <- HBV_DIAG_JOINED2%>%
  filter (include == 'INCLUDE')

nonednhsno <- noned%>%
  select(NHS_NUMBER, SpecimenDate, include)

nonedhbv <- left_join (HBVED2, nonednhsno, by = c("NHS_Number" = "NHS_NUMBER", "SpecimenDate" = "SpecimenDate"))%>%
  filter (!is.na(include))%>%
  distinct(SiteCode, NHS_Number, SpecimenDate, SiteName, Speciality_of_test, Setting_of_test)

noned_grouped <- nonedhbv%>%
  filter(!Speciality_of_test %in% c("Emergency Department", "Hospital"))%>%
  group_by(SiteCode, SiteName, Setting_of_test, Speciality_of_test)%>%
  count()%>%
  select(SiteCode, SiteName, Setting_of_test, Speciality_of_test, n)

write.csv(noned_grouped, "Non-ED-HBV-diagnoses.csv")



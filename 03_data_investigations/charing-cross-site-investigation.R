# filtering for Charing Cross attendances where specimen date is Nov 2022
# use included_diagnoses table made in Data_prep_SGSS script
charing_cross <- included_diagnoses%>%
  filter (SITE == "RYJ02", SPECIMEN_DATE > '2022-10-30' & SPECIMEN_DATE < '2022-12-01')%>%
  select(TOKEN_PERSON_ID, ARRIVAL_DATE,INVESTIGATION_DATE, AGE_AT_ARRIVAL, ETHNIC_CATEGORY_Description, NHS_NUMBER, IMD_19, age_group, age_group2, SPECIMEN_DATE)

# format specimen date
SGSS_HCV_CASES <- SGSS_HCV_CASES%>%
mutate(
  SPECIMEN_DATE = as.Date(SPECIMEN_DATE), FORMAT = '%Y-%m-%d'
)


# join to the SGSS data to get all the info about the diagnoses
charing_cross_joined <- left_join (charing_cross, SGSS_HCV_CASES, by = c("NHS_NUMBER" = "NHS_NUMBER", "SPECIMEN_DATE" = "SPECIMEN_DATE"))%>%
  distinct(NHS_NUMBER, SPECIMEN_DATE, .keep_all = TRUE)

# select just the NHS number and specimen dates of the included diagnoses to link back to Sentinel
charing_cross_nhsno <- charing_cross_joined %>%
  select(NHS_NUMBER, SPECIMEN_DATE)

# join the NHS numbers to the Sentinel data
# load connection to Sentinel
Sentinel <- odbc::dbConnect(odbc::odbc(),
                            .connection_string = "driver={SQL Server};server=SQLClusColHFN17\\HFN17;
                             database=NIS_HepatitisSentinel;
                             Encrypt=true;trusted_connection=true",
                            timeout = 60,
                            timezone = Sys.timezone(),
                            timezone_out = Sys.timezone())

HCV_DIAGNOSES_SENTINEL <- DBI::dbGetQuery(conn = Sentinel , statement = "
select * from SentinelApril2022v2
where DATE_RECEIVED < '2023-04-01'
AND HCVPositive like 'positive'"
)

# left join the SGSS diagnoses to the Sentinel data
joined_both <- left_join(HCV_DIAGNOSES_SENTINEL, charing_cross_nhsno, by = "NHS_NUMBER")%>%
  filter(!is.na(SPECIMEN_DATE))%>%
distinct(NHS_NUMBER, SPECIMEN_DATE, .keep_all = TRUE)





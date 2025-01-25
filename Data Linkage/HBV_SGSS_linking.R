# Import required libraries
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
if (!require(DBI)) install.packages("DBI")
if (!require(odbc)) install.packages("odbc")
if (!require(arrow)) install.packages("arrow")

rm(list= ls()[!(ls() %in% c('clinical_records'))])

# Read source data
clinical_records <- read_parquet("PATH/TO/DATA/clinical_records.parquet")

# Database connection
clinical_db <- odbc::dbConnect(odbc::odbc(),
                            .connection_string = "driver={SQL Server};server=DB_SERVER;
                            database=CLINICAL_DATABASE;
                            Encrypt=true;trusted_connection=true",
                            timeout = 60,
                            timezone = Sys.timezone(),
                            timezone_out = Sys.timezone())

# Query clinical data - Version 1 (filtered)
clinical_data_v1 <- DBI::dbGetQuery(conn = clinical_db, statement = "SELECT 
SUBSTRING([LocationCode],1,len([LocationCode])-2) AS PROVIDER_CODE
,[LocationCode]
,[LocationName]
,[TestDate]
,[TestSpeciality]
,[TestSetting]
,[Age]
,[SEX]
,[PATIENT_ID]
,[DemographicCode]
,[TestMethod]
,[InitialEventDate]
,[DiagnosisStatus]

FROM [CLINICAL_DATABASE].[dbo].[CLINICAL_TABLE]
WHERE (([LocationCode] LIKE 'LOC1%' 
    OR [LocationCode] LIKE 'LOC2%'
    [... additional location codes ...]
    OR [LocationCode] LIKE 'LOC40%')
  AND [Age] > 15 
  AND ([TestSpeciality] LIKE 'SPEC1' or [TestSpeciality] LIKE 'SPEC2')  
  AND [TestDate] > '2022-03-31')"
) %>%
  mutate(provider_name = case_when(
    PROVIDER_CODE == "LOC1" ~ "HEALTHCARE_PROVIDER_1",
    PROVIDER_CODE == "LOC2" ~ "HEALTHCARE_PROVIDER_2",
    [... additional provider mappings ...]
    PROVIDER_CODE == "LOC40" ~ "HEALTHCARE_PROVIDER_40"
  ))

# Process records with patient IDs
clinical_records_with_id <- clinical_data_v1 %>% 
  mutate(has_id = if_else(is.na(PATIENT_ID), 0, 1))

# Filter for valid IDs
valid_records <- clinical_records_with_id %>% 
  filter(has_id == 1) %>% 
  select(PATIENT_ID, TestDate)

# Link records
linked_records <- left_join(clinical_records, valid_records, 
                          by = c("PATIENT_ID" = "PATIENT_ID"))

# Standardize dates
linked_records <- linked_records %>%
  mutate(VISIT_DATE = as.Date(VISIT_DATE),
         TEST_DATE = as.Date(TEST_DATE),
         SPECIMEN_DATE = as.Date(TestDate), FORMAT = '%Y-%m-%d')

# Define inclusion criteria
linked_records <- linked_records %>% 
  mutate(max_test_date = VISIT_DATE + 8,
         inclusion_status = case_when(
           (SPECIMEN_DATE > TEST_DATE-1 & SPECIMEN_DATE < max_test_date) ~ "INCLUDE",
           FALSE ~ "EXCLUDE"))

# Filter included records
included_records <- linked_records %>%
  filter(inclusion_status == "INCLUDE")

# Analysis by provider
provider_analysis <- included_records %>%
  distinct(VISIT_DATE, PATIENT_ID, provider_name, .keep_all = TRUE) %>%
  group_by(provider_name) %>%
  count() %>%
  select(provider_name, n)

# Version 2 - Unfiltered query
clinical_data_v2 <- DBI::dbGetQuery(conn = clinical_db, 
  [... similar query without location/specialty filters ...]
)

[... repeat similar processing steps for v2 data ...]

# Additional analysis
specialty_analysis <- non_standard_records %>%
  filter(!TestSpeciality %in% c("SPEC1", "SPEC2")) %>%
  group_by(LocationCode, LocationName, TestSetting, TestSpeciality) %>%
  count() %>%
  select(LocationCode, LocationName, TestSetting, TestSpeciality, n)

write.csv(specialty_analysis, "specialty_analysis.csv")
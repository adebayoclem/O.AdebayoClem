### CONDITION_A LINKAGE ANALYSIS

# Import required libraries
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
if (!require(DBI)) install.packages("DBI")
if (!require(odbc)) install.packages("odbc")
if (!require(arrow)) install.packages("arrow")

# Read processed data
source_data <- read_parquet("PATH_TO_DATA/processed_records.parquet")

# Database connection setup
db_conn <- odbc::dbConnect(odbc::odbc(),
                        .connection_string = "driver={SQL Server};server=DB_SERVER;
                         database=CLINICAL_DB;
                         Encrypt=true;trusted_connection=true",
                        timeout = 60,
                        timezone = Sys.timezone(),
                        timezone_out = Sys.timezone())

# Load diagnosis data
condition_diagnoses <- DBI::dbGetQuery(conn = db_conn, statement = "
select * from DIAGNOSIS_TABLE
where FirstTestDate <= '2023-04-07' AND (TestResult like 'Positive' or TestResult like 'Reactive')")

# Define included locations
included_locations <- c('LOC001', 'LOC002', 'LOC003', 'LOC004', 'LOC005', 'LOC006',
                     'LOC007', 'LOC008', 'LOC009', 'LOC010', 'LOC011', 'LOC012',
                     'LOC013', 'LOC014', 'LOC015', 'LOC016', 'LOC017', 'LOC018',
                     'LOC019', 'LOC020', 'LOC021', 'LOC022', 'LOC023', 'LOC024',
                     'LOC025', 'LOC026', 'LOC027')

# Filter data for included locations
filtered_records <- source_data %>%
  filter(LOCATION %in% included_locations)

# Process diagnoses with identifiers
diagnoses_with_id <- condition_diagnoses %>% 
  mutate(has_identifier = if_else(is.na(PATIENT_ID), 0, 1))

# Filter records with valid identifiers
valid_diagnoses <- diagnoses_with_id %>% 
  filter(has_identifier == 1)

# Link diagnoses to attendance records
linked_records <- left_join(filtered_records, valid_diagnoses, 
                          by = c("PATIENT_ID" = "PATIENT_ID"))

# Standardize dates
linked_records <- linked_records %>%
  mutate(VISIT_DATE = as.Date(VISIT_DATE),
         TEST_DATE = as.Date(TEST_DATE),
         DIAGNOSIS_DATE = as.Date(FirstTestDate, FORMAT = '%Y-%m-%d'))

# Define inclusion criteria
linked_records <- linked_records %>% 
  mutate(max_test_date = VISIT_DATE + 8,
         inclusion_status = case_when(
           (DIAGNOSIS_DATE > TEST_DATE-1 & DIAGNOSIS_DATE < max_test_date) ~ "INCLUDE",
           FALSE ~ "EXCLUDE"))

# Create final dataset
final_diagnoses <- linked_records %>%
  filter(inclusion_status == "INCLUDE" & condition_status == 'ACTIVE') %>%
  distinct(PATIENT_ID, .keep_all = TRUE) %>%
  select(-c(TEST_CODE, TEST_DESCRIPTION, TEST_INDEX, 
            PRESENTING_COMPLAINT, COMPLAINT_DESCRIPTION, FISCAL_YEAR,
            PROVIDER_CODE, TEST_DATE,
            PATIENT_AGE, GEOGRAPHIC_CODE, DEMOGRAPHIC_CODE, REGION_CODE, PROVIDER_CODE))

# Optional analysis code (commented out)
# diagnoses_by_location <- final_diagnoses %>%
#   filter(TestResult == 'Positive' | TestResult == "Reactive") %>%
#   distinct(VISIT_DATE, PATIENT_ID, LOCATION) %>%
#   group_by(LOCATION) %>%
#   count() %>%
#   select(LOCATION, n)
# 
# write.csv(diagnoses_by_location, "diagnoses_by_location.csv")
# 
# diagnoses_by_month <- final_diagnoses %>%
#   distinct(VISIT_DATE, PATIENT_ID, month) %>%
#   group_by(month) %>%
#   count() %>%
#   select(month, n)
# 
# write.csv(diagnoses_by_month, "diagnoses_by_month.csv")
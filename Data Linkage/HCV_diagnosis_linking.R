### CONDITION_A ANALYSIS

# Import required libraries
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
if (!require(DBI)) install.packages("DBI")
if (!require(odbc)) install.packages("odbc")
if (!require(arrow)) install.packages("arrow")

# Load source data
clinical_records <- read_parquet("PATH/TO/DATA/clinical_records.parquet")

# Database connection
clinical_db <- odbc::dbConnect(odbc::odbc(),
                            .connection_string = "driver={SQL Server};server=DB_SERVER;
                            database=CLINICAL_DB;
                            Encrypt=true;trusted_connection=true",
                            timeout = 60,
                            timezone = Sys.timezone(),
                            timezone_out = Sys.timezone())

# Query diagnosis data
clinical_diagnoses <- DBI::dbGetQuery(conn = clinical_db, statement = "
select * from DIAGNOSIS_TABLE
where FirstTestDate <= '2023-04-07' AND (TEST_A like 'Positive' or TEST_B like 'Positive')")

# Define included locations
included_locations <- c('LOC001', 'LOC002', 'LOC003', 'LOC004', 'LOC005',
                     'LOC006', 'LOC007', 'LOC008', 'LOC009', 'LOC010',
                     [...additional location codes...])

# Filter records by location
filtered_records <- clinical_records %>%
  filter(LOCATION %in% included_locations)

# Clean diagnosis data
clean_diagnoses <- clinical_diagnoses %>% 
  mutate(test_a_date = as.Date(test_a_date, format = '%Y-%m-%d'),
         test_b_date = as.Date(test_b_date, format = '%Y-%m-%d'),
         first_test_date = as.Date(first_test_date, format = '%Y-%m-%d'),
         initial_event_date = as.Date(initial_event_date, format = '%Y-%m-%d'),
         pre_treatment_date = as.Date(pre_treatment_date, format = '%Y-%m-%d'),
         post_treatment_date = as.Date(post_treatment_date, format = '%Y-%m-%d'),
         min_create_date = as.Date(min_create_date, format = '%Y-%m-%d'),
         min_referral_date = as.Date(min_referral_date, format = '%Y-%m-%d'),
         earliest_treatment_date = as.Date(earliest_treatment_date, format = '%Y-%m-%d'),
         min_record_date = as.Date(min_record_date, format = '%Y-%m-%d'),
         birth_date = as.Date(birth_date, format = '%Y-%m-%d')) 

# Standardize test results
clean_diagnoses <- mutate(clean_diagnoses, test_a = tolower(test_a)) 
clean_diagnoses <- mutate(clean_diagnoses, test_b = tolower(test_b)) 

# Recode risk factors
clean_diagnoses <- clean_diagnoses %>% 
  mutate(risk_factor_group = case_when(
    risk_factor == "Current/Recent" | risk_factor == "Yes" ~ "Current/Recent", 
    risk_factor == "Past" ~ "Past",
    risk_factor == "Never" | risk_factor == "No" ~ "Never"))  

# Validate diagnosis status
clean_diagnoses <- clean_diagnoses %>% 
  mutate(diagnosis_status = case_when(
    test_b == 'positive' & status == 'New' & 
    (pre_treatment_date < test_b_date | initial_event_date < test_b_date) ~ "", 
    TRUE ~ status))

clean_diagnoses <- clean_diagnoses %>% 
  mutate(positive_new_case = case_when(
    test_b == 'positive' & status == 'New' ~ "New",
    TRUE ~ ""))

# Process records with identifiers
diagnoses_with_id <- clean_diagnoses %>% 
  mutate(has_id = if_else(is.na(PATIENT_ID), 0, 1))

# Filter for valid identifiers
valid_diagnoses <- diagnoses_with_id %>% 
  filter(has_id == 1) 

# Link records
linked_records <- left_join(clinical_records, valid_diagnoses, 
                          by = c("PATIENT_ID" = "PATIENT_ID"))

# Standardize dates
linked_records <- linked_records %>%
  mutate(VISIT_DATE = as.Date(VISIT_DATE),
         TEST_DATE = as.Date(TEST_DATE),
         SPECIMEN_DATE = as.Date(first_test_date, FORMAT = '%Y-%m-%d'))

# Define inclusion criteria
linked_records <- linked_records %>% 
  mutate(max_test_date = VISIT_DATE + 8,
         inclusion_status = case_when(
           (SPECIMEN_DATE > TEST_DATE-1 & SPECIMEN_DATE < max_test_date) ~ "INCLUDE",
           FALSE ~ "EXCLUDE"))

# Create final dataset
final_records <- linked_records %>%
  filter(inclusion_status == "INCLUDE" & record_status == 'ACTIVE') %>%
  distinct(PATIENT_ID, .keep_all = TRUE) %>%
  select(-c(TEST_CODE, TEST_DESCRIPTION, TEST_INDEX, 
           COMPLAINT, COMPLAINT_DESCRIPTION, FISCAL_YEAR, MONTH,
           PROVIDER_CODE, LOCATION, VISIT_MONTH, VISIT_DATE, TEST_DATE,
           PATIENT_AGE, GEOGRAPHIC_CODE, DEMOGRAPHIC_CODE, REGION_CODE, PROVIDER_NAME))
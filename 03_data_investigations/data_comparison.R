# Clear the environment
rm(list=ls())

# Load packages
pacman::p_load(
  pacman,
  tidyverse,
  lubridate,
  readxl,
  writexl,
  DBI,
  odbc,
  arrow,
  janitor,
  reshape2
)

# Establish ODBC connection with data
Y006 <- odbc::dbConnect(odbc::odbc(),
                        .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;
                             database=Y006_BBV_PID;
                             Encrypt=true;trusted_connection=true",
                        timeout = 60,
                        timezone = Sys.timezone(),
                        timezone_out = Sys.timezone())

# Load attendances data
attendances24M <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT [SITE], 
[ECDS_bloods_any], 
[included_sites], 
[sentinel_sites],
[HIV], 
[HCV], 
[HBV],
[HCVAb],
[HCVPCR],
[HBVAg],
[HIVresult],
arrival_date
FROM [Y006_BBV_PID].[dbo].[24monthECDSattendances]
WHERE (ARRIVAL_DATE <= '2024-01-07')"
) %>%
  clean_names() %>%
  rename(site_code = site) 

# Use Included Sentinel sites to check the attendances and blood tests for those who are not in sentinel yet
included_sites <- attendances24M %>% 
  filter(included_sites == "Yes")

# Import site codes
site_codes <- read_xlsx("Data investigation_24month/sitecodelookup2024_final.xlsx") %>%
  clean_names() %>%
  mutate(site_hospital_name = toupper(hospital))

# Change site name according to NHSE document
site_codes <- site_codes %>%
  mutate(site_hospital_name = ifelse(site_hospital_name == "CHELSEA & WESTMINSTER HOSPITAL", "CHELSEA AND WESTMINSTER HOSPITAL",
                                     ifelse(site_hospital_name == "KING'S COLLEGE HOSPITAL", "KING'S COLLEGE HOSPITAL (DENMARK HILL)",
                                            site_hospital_name)))

# Join site codes with sentinel sites
included_sites <- included_sites %>% 
  full_join(site_codes, by = 'site_code') %>% 
  select(1:12, 16)

A <- included_sites %>% 
  group_by(site_code, site_hospital_name) %>% 
  tally() %>% 
  arrange(site_hospital_name)

### Attendance count 
sentinel_tbl <- included_sites %>% 
  group_by(site_code, site_hospital_name) %>%
  tally() %>% 
  collect() %>%
  rename(ecds_total = n) %>%
  mutate(event = "Attendance")

### Attendances with blood test count
sentinel_blood_test_tbl <- included_sites %>% 
  filter(ecds_bloods_any == "bloods_any") %>% 
  group_by(site_code, site_hospital_name) %>% 
  tally() %>% 
  collect() %>% 
  rename(ecds_total = n) %>% 
  mutate(event = "Blood test")

# Compare blood test to attendance table to allow labeling of sites with 0 blood tests
zero_sites <- anti_join(sentinel_tbl, sentinel_blood_test_tbl, by = c("site_code", "site_hospital_name")) %>% 
  select(site_code, site_hospital_name) %>% 
  mutate(ecds_total = 0,
         event = "Blood test")

sentinel_blood_test_tbl <- sentinel_blood_test_tbl %>% 
  rbind(zero_sites)

### Attendances with HIV test count
sentinel_hiv_test_tbl <- included_sites %>% 
  filter(ecds_bloods_any == "bloods_any" & hiv == "Yes") %>%
  group_by(site_code, site_hospital_name) %>%
  tally() %>% 
  collect() %>%
  rename(ecds_total = n) %>%
  mutate(event = "HIV test")

zero_sites <- anti_join(sentinel_tbl, sentinel_hiv_test_tbl, by = c("site_code", "site_hospital_name")) %>%
  select(site_code, site_hospital_name) %>%
  mutate(ecds_total = 0,
         event = "HIV test")

sentinel_hiv_test_tbl <- sentinel_hiv_test_tbl %>%
  rbind(zero_sites)

### Attendances with HCV test count
sentinel_hcv_test_tbl <- included_sites %>% 
  filter(ecds_bloods_any == "bloods_any" & hcv == "Yes") %>%
  group_by(site_code, site_hospital_name) %>%
  tally() %>% 
  collect() %>%
  rename(ecds_total = n) %>%
  mutate(event = "HCV test")

zero_sites <- anti_join(sentinel_tbl, sentinel_hcv_test_tbl, by = c("site_code", "site_hospital_name")) %>%
  select(site_code, site_hospital_name) %>%
  mutate(ecds_total = 0,
         event = "HCV test")

sentinel_hcv_test_tbl <- sentinel_hcv_test_tbl %>%
  rbind(zero_sites)

### Attendances with HBV test count
sentinel_hbv_test_tbl <- included_sites %>% 
  filter(ecds_bloods_any == "bloods_any" & hbv == "Yes") %>%
  group_by(site_code, site_hospital_name) %>%
  tally() %>% 
  collect() %>%
  rename(ecds_total = n) %>%
  mutate(event = "HBV test")

zero_sites <- anti_join(sentinel_tbl, sentinel_hbv_test_tbl, by = c("site_code", "site_hospital_name")) %>%
  select(site_code, site_hospital_name) %>%
  mutate(ecds_total = 0,
         event = "HBV test")

sentinel_hbv_test_tbl <- sentinel_hbv_test_tbl %>%
  rbind(zero_sites)

### Attendances with HCV diagnosis count
sentinel_hcv_diag_tbl <- included_sites %>% 
  filter(ecds_bloods_any == "bloods_any" & hcvpcr == "Positive") %>%
  group_by(site_code, site_hospital_name) %>%
  tally() %>% 
  collect() %>%
  rename(ecds_total = n) %>%
  mutate(event = "HCV diagnosis") 

zero_sites <- anti_join(sentinel_tbl, sentinel_hcv_diag_tbl, by = c("site_code", "site_hospital_name")) %>%
  select(site_code, site_hospital_name) %>%
  mutate(ecds_total = 0,
         event = "HCV diagnosis")

sentinel_hcv_diag_tbl <- sentinel_hcv_diag_tbl %>%
  rbind(zero_sites)

### Attendances with HBV diagnosis count
sentinel_hbv_diag_tbl <- included_sites %>% 
  filter(ecds_bloods_any == "bloods_any" & hbv_ag == "Positive") %>%
  group_by(site_code, site_hospital_name) %>%
  tally() %>% 
  collect() %>%
  rename(ecds_total = n) %>%
  mutate(event = "HBV diagnosis")

zero_sites <- anti_join(sentinel_tbl, sentinel_hbv_diag_tbl, by = c("site_code", "site_hospital_name")) %>%
  select(site_code, site_hospital_name) %>%
  mutate(ecds_total = 0,
         event = "HBV diagnosis")

sentinel_hbv_diag_tbl <- sentinel_hbv_diag_tbl %>%
  rbind(zero_sites)

# Bind all the tables
sentinel_all_numbers <- bind_rows(
  sentinel_tbl,
  sentinel_blood_test_tbl,
  sentinel_hiv_test_tbl,
  sentinel_hcv_test_tbl,
  sentinel_hbv_test_tbl,
  sentinel_hcv_diag_tbl,
  sentinel_hbv_diag_tbl
)

# Import NHSE file
nhse_numbers <- read_csv("Data investigation_24month/NHSE_dashboard_160524.csv") %>%
  clean_names() %>%
  select(metric_name,
         site_hospital_name,
         year_of_reporting_period_start,
         month_of_reporting_period_start,
         metric_or_proportion_integer)

# Concatenate the month and year into one field 
nhse_numbers$date <- with(
  nhse_numbers, 
  sprintf("%d-%02s", 
          year_of_reporting_period_start,
          month_of_reporting_period_start))

# Remove dates after March 2023
nhse_numbers <- nhse_numbers %>%
  filter(
    ( !date %in% c('2024-January', '2024-February', '2024-March', '2024-April')) & 
      (metric_name %in% c( # filter out unnecessary events
        "BBV attending ED (adults)",
        "BBV ED attendances with blood tests",
        "ED attendances with blood tests minus blocked",
        "Hep B antigen tests performed (BBV)",
        "Hep C antibody tests performed",
        "Hep C RNA tests performed",
        "HIV tests performed",
        "Positive Hep B antigen tests",
        "Positive Hep C antibody tests",
        "Positive Hep C RNA tests (BBV)",
        "Reactive HIV tests" ))) %>%
  mutate(metric_or_proportion_integer = ifelse(is.na(metric_or_proportion_integer), 0, metric_or_proportion_integer),  # make NA values 0
         site_hospital_name = toupper(site_hospital_name))

# Calculate totals for the year
nhse_totals_tbl <- nhse_numbers %>%
  group_by(metric_name) %>%
  mutate (total = sum(metric_or_proportion_integer)) %>%
  distinct(metric_name, total)

# Bring in site codes
nhse_numbers_by_site <- nhse_numbers %>%
  left_join(site_codes, by = "site_hospital_name")

# Make a table of attendances
nhse_attendances_tbl <- nhse_numbers_by_site %>%
  filter(metric_name == "BBV attending ED (adults)") %>%
  select(site_code, metric_or_proportion_integer, site_hospital_name) %>%
  group_by(site_code, site_hospital_name) %>%
  mutate(nhse_total = sum(metric_or_proportion_integer)) %>%
  select(-metric_or_proportion_integer) %>%
  ungroup() %>%
  distinct() %>%
  mutate(event = "Attendance")

# Make a table of blood tests
nhse_blood_test_tbl <- nhse_numbers_by_site %>%
  filter(metric_name == "BBV ED attendances with blood tests") %>%
  select(site_code, metric_or_proportion_integer, site_hospital_name) %>%
  group_by(site_code, site_hospital_name) %>%
  mutate(nhse_total = sum(metric_or_proportion_integer)) %>%
  select(-metric_or_proportion_integer) %>%
  ungroup() %>%
  distinct() %>%
  mutate(event = "Blood test")

# Make a table of HIV tests
nhse_hiv_test_tbl <- nhse_numbers_by_site %>%
  filter(metric_name == "HIV tests performed") %>%
  select(site_code, metric_or_proportion_integer, site_hospital_name) %>%
  group_by(site_code, site_hospital_name) %>%
  mutate(nhse_total = sum(metric_or_proportion_integer)) %>%
  select(-metric_or_proportion_integer) %>%
  ungroup() %>%
  distinct() %>%
  mutate(event = "HIV test")

# Make a table of HCV tests
nhse_hcv_test_tbl <- nhse_numbers_by_site %>%
  filter(metric_name == "Hep C antibody tests performed") %>%
  select(site_code, metric_or_proportion_integer, site_hospital_name) %>%
  group_by(site_code, site_hospital_name) %>%
  mutate(nhse_total = sum(metric_or_proportion_integer)) %>%
  select(-metric_or_proportion_integer) %>%
  ungroup() %>%
  distinct() %>%
  mutate(event = "HCV test")

# Make a table of HBV tests
nhse_hbv_test_tbl <- nhse_numbers_by_site %>%
  filter(metric_name == "Hep B antigen tests performed (BBV)") %>%
  select(site_code, metric_or_proportion_integer, site_hospital_name) %>%
  group_by(site_code, site_hospital_name) %>%
  mutate(nhse_total = sum(metric_or_proportion_integer)) %>%
  select(-metric_or_proportion_integer) %>%
  ungroup() %>%
  distinct() %>%
  mutate(event = "HBV test")

# Make a table of HCV diagnoses
nhse_hcv_diag_tbl <- nhse_numbers_by_site %>%
  filter(metric_name == "Positive Hep C RNA tests (BBV)") %>%
  select(site_code, metric_or_proportion_integer, site_hospital_name) %>%
  group_by(site_code, site_hospital_name) %>%
  mutate(nhse_total = sum(metric_or_proportion_integer)) %>%
  select(-metric_or_proportion_integer) %>%
  ungroup() %>%
  distinct() %>%
  mutate(event = "HCV diagnosis")

# Make a table of HBV diagnoses
nhse_hbv_diag_tbl <- nhse_numbers_by_site %>%
  filter(metric_name == "Positive Hep B antigen tests") %>%
  select(site_code, metric_or_proportion_integer, site_hospital_name) %>%
  group_by(site_code, site_hospital_name) %>%
  mutate(nhse_total = sum(metric_or_proportion_integer)) %>%
  select(-metric_or_proportion_integer) %>%
  ungroup() %>%
  distinct() %>%
  mutate(event = "HBV diagnosis")

# Join tables
nhse_all_numbers <- bind_rows(
  nhse_attendances_tbl,
  nhse_blood_test_tbl,
  nhse_hbv_diag_tbl,
  nhse_hbv_test_tbl,
  nhse_hcv_diag_tbl,
  nhse_hcv_test_tbl,
  nhse_hiv_test_tbl
)

# Join ECDS and NHSE data 
sentinel_nhse_compared <- left_join(
  sentinel_all_numbers,
  nhse_all_numbers,
  by = join_by("site_code" == "site_code", "event", "site_hospital_name")) %>%
  select(site_code,
         event,
         site_hospital_name,
         nhse_total,
         ecds_total) %>%
  mutate(perc_diff = (ecds_total - nhse_total)/nhse_total*100,
         abs_diff = (ecds_total - nhse_total)) %>% # calc percentage and absolute difference
  mutate(perc_diff = sqrt(perc_diff^2), # square and then square root to make all values positive
         abs_diff = sqrt(abs_diff^2)) 

# Add inclusion grade (30% difference added as arbitrary cut-off)
sentinel_nhse_compared <- sentinel_nhse_compared %>%
  mutate(include = if_else(perc_diff < 30, "YES", "NO", "INVESTIGATE"))

# Write the combined data to an Excel file
#write_xlsx(sentinel_nhse_compared,
#           "04_sentinel_nhse_compared_Raw_RP2024.xlsx")

# Additional script needed for the site level report
# Filter for sites with 'YES' for any of the specified conditions
included_sites <- sentinel_nhse_compared %>%
  filter(include == "YES") %>%
  select(
    site_code,
    event,
    include
  ) %>%
  pivot_wider(names_from = event, values_from = include, values_fill = "NO") %>%
  rename(
    include_Attendance = `Attendance`,
    `include_Blood test` = `Blood test`,
    `include_HIV test` = `HIV test`,
    `include_HCV test` = `HCV test`,
    `include_HBV test` = `HBV test`,
    `include_HCV diagnosis` = `HCV diagnosis`,
    `include_HBV diagnosis` = `HBV diagnosis`
  )

view(included_sites)

# Save the list of site codes that meet any of the inclusion criteria
write.csv(included_sites, "included_sites.csv", row.names = FALSE)

##### EXPLORING AND CLEANING DATA FOR PROBABILISTIC LINKAGE #####

# IMPORT LIBRARIES
pacman::p_load(
  pacman,
  tidyverse,
  readxl,
  writexl,
  DBI,
  odbc,
  knitr,
  data.table,
  kableExtra,
  janitor,
  report,
  arrow
)

# LOAD ECDS DATA
## establish ODBC connection
Y006 <- odbc::dbConnect(
  odbc::odbc(),
  .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;
                             database=Y006_BBV_PID;
                             Encrypt=true;trusted_connection=true",
  timeout = 60,
  timezone = Sys.timezone(),
  timezone_out = Sys.timezone()
)

## read in data from SQL
ECDS <- DBI::dbGetQuery(
  conn = Y006 ,
  statement = "SELECT [TOKEN_PERSON_ID]
  ,[ARRIVAL_DATE]
  ,[BIRTH_DATE]
  ,[NHS_NUMBER]
  ,[PROVIDER_CODE_DERIVED]
  ,[POSTCODE]
  ,[AGE_AT_ARRIVAL]
  ,[Sex]
  FROM [Y006_BBV_PID].[dbo].[12monthECDS_JE]
  WHERE (ARRIVAL_DATE <= '2023-04-07')"
) %>% 
  clean_names()

# INVESTIGATE DATA
## Number of observations, missing values, %_complete, distinct values for each column
n_obs <- nrow(ECDS)
missing_values <- colSums(is.na(ECDS))
distinct_values <- sapply(ECDS, function(x)
  length(unique(x)))
complete_perc <- round((((
  n_obs - missing_values
) / n_obs) * 100), 2)

## Create a summary data frame
ECDS_summary <- data.frame(
  n = n_obs,
  missing = missing_values,
  perc_complete = complete_perc,
  distinct = distinct_values
)

## print
kable(head(ECDS, n = 5), caption = 'ECDS: Top 5 rows') %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = F,
    position = "left"
  )

ECDS_summary %>%
  kbl(caption = 'Summary: ECDS') %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = F,
    position = "left"
  )

# READ IN SGSS DATA
SGSS <-
  DBI::dbGetQuery(
    conn = Y006 ,
    statement = "SELECT [SpecimenNumber]
,[SpecimenDate]
,[Sex]
,[PtNHSNumber]
,[Age]
,[Postcode]
,[Providercode]
,[Dateofbirth]
FROM [Y006_BBV_PID].[dbo].[EmergencyDepartment_tests]
WHERE (SpecimenDate <= '2023-04-07')"
  ) %>%
  clean_names()

# INVESTIGATE DATA
## Number of observations, missing values, %_complete, distinct values for each column
n_obs <- nrow(SGSS)
missing_values <- colSums(is.na(SGSS))
distinct_values <- sapply(SGSS, function(x)
  length(unique(x)))
complete_perc <- round((((
  n_obs - missing_values
) / n_obs) * 100), 2)

## Create a summary data frame
SGSS_summary <- data.frame(
  n = n_obs,
  missing = missing_values,
  perc_complete = complete_perc,
  distinct = distinct_values
)

## print
kable(head(SGSS, n = 5), caption = 'SGSS: Top 5 rows ') %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = F,
    position = "left"
  )


SGSS_summary %>%
  kbl(caption = 'Summary: SGSS') %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = F,
    position = "left"
  )

# STANDARDISE COMMON COL NAMES
SGSS_new_names <- SGSS %>%
  rename(
    birth_date = dateofbirth,
    date = specimen_date,
    id = specimen_number,
    nhs_number = pt_nhs_number,
    provider_code = providercode
  )

ECDS_new_names <- ECDS %>%
   rename(
     id = token_person_id,
     date = arrival_date,
     provider_code = provider_code_derived,
     age = age_at_arrival)

# CLEAN INDEX DATES
ECDS_clean_date <- ECDS_new_names %>%
  mutate(date = as.Date(date)) %>%
  mutate(date_int = as.numeric(as.Date(date)))

SGSS_clean_date <- SGSS_new_names %>%
  mutate(date = as.Date(date)) %>%
  mutate(date_int = as.numeric(as.Date(date)))

# CLEAN POSTCODES
## make postcodes upper case
ECDS_clean_pcode <- ECDS_clean_date %>%
  mutate(postcode = toupper(postcode))

## get rid of double spaces
ECDS_clean_pcode$postcode <-
  gsub("  ", " ", ECDS_clean_pcode$postcode)
## correct any mixed up codes
ECDS_clean_pcode$postcode <-
  gsub("^([0-9][0-9A-Z]+) ([0-9A-Z]+)$",
       "\\2 \\1",
       ECDS_clean_pcode$postcode)
## get rid of all spaces
ECDS_clean_pcode$postcode <-
  gsub(" ", "", ECDS_clean_pcode$postcode)

## make postcodes upper case
SGSS_clean_pcode <- SGSS_clean_date %>%
  mutate(postcode = toupper(postcode))
## get rid of all spaces
SGSS_clean_pcode$postcode <-
  gsub(" ", "", SGSS_clean_pcode$postcode)


# CLEAN DOBS
## in both datasets, DOBS are strings so need to make them datetime
ECDS_clean_pcode_dob <- ECDS_clean_pcode %>%
  mutate(birth_date = as.Date(birth_date)) %>%
  mutate(birth_date_int = as.numeric(as.Date(birth_date)))

## same again for SGSS
SGSS_clean_pcode_dob <- SGSS_clean_pcode %>%
  mutate(birth_date = as.Date(birth_date)) %>%
  mutate(birth_date_int = as.numeric(as.Date(birth_date)))

## replace invalid DOBs with NA
ECDS_clean_pcode_dob <- ECDS_clean_pcode_dob %>%
  mutate(year_of_birth = as.integer(format(birth_date, format="%Y")),
         month_of_birth = as.integer(format(birth_date, format="%m"))) %>%
  mutate(birth_date = if_else((year_of_birth >= 1910 & year_of_birth < 2022), birth_date, NA, NA))

## same again for SGSS
SGSS_clean_pcode_dob <- SGSS_clean_pcode_dob %>%
  mutate(year_of_birth = as.integer(format(birth_date, format="%Y")),
         month_of_birth = as.integer(format(birth_date, format="%m"))) %>%
  mutate(birth_date = if_else((year_of_birth >= 1910 & year_of_birth < 2022), birth_date, NA, NA))

# CLEAN SEX
## in ECDS: MALE, FEMALE; in SGSS: 1, 2
SGSS_clean <- SGSS_clean_pcode_dob %>%
  mutate(sex = case_when(sex == 1 ~ "MALE",
                         sex == 2 ~ "FEMALE",
                         is.na(sex) ~ "MISSING"))

ECDS_clean<- ECDS_clean_pcode_dob %>%
  mutate(sex = ifelse(is.na(sex), "MISSING", sex))

# CHECK MISSINGNESS OF NHS NUM, DOB, SEX, POSTCODE
ECDS_clean2 <- ECDS_clean %>%
  mutate(
    nhs_num_complete = ifelse(is.na(nhs_number), 0, 1),
    dob_complete = ifelse(is.na(birth_date), 0, 1),
    sex_complete = ifelse(is.na(sex), 0, 1),
    pcode_complete = ifelse(is.na(postcode), 0, 1)
  ) %>%
  unite(
    missing_pattern,
    c(nhs_num_complete, dob_complete, sex_complete, pcode_complete),
    sep = ""
  )

ECDS_clean2 %>%
  group_by(missing_pattern) %>%
  tally()

## same again for SGSS
SGSS_clean2 <- SGSS_clean %>%
  mutate(
    nhs_num_complete = ifelse(is.na(nhs_number), 0, 1),
    dob_complete = ifelse(is.na(birth_date), 0, 1),
    sex_complete = ifelse(is.na(sex), 0, 1),
    pcode_complete = ifelse(is.na(postcode), 0, 1)
  ) %>%
  unite(
    missing_pattern,
    c(nhs_num_complete, dob_complete, sex_complete, pcode_complete),
    sep = ""
  ) 

SGSS_clean2 %>%
  group_by(missing_pattern) %>%
  tally()

# REORDER COLUMNS
ECDS_clean <- ECDS_clean %>%
  select(id,
         date,
         date_int,
         nhs_number,
         birth_date,
         birth_date_int,
         year_of_birth,
         month_of_birth,
         postcode,
         age,
         sex,
         provider_code
         )

SGSS_clean <- SGSS_clean %>%
  select(id,
         date,
         date_int,
         nhs_number,
         birth_date,
         birth_date_int,
         year_of_birth,
         month_of_birth,
         postcode,
         age,
         sex,
         provider_code
  )

# CHANGE COLUMN NAMES TO TRACK ORIGIN

## save datasets with old names first
SGSS_clean_nop <- SGSS_clean
ECDS_clean_nop <- ECDS_clean

## Get the column names
column_names <- names(ECDS_clean)
P_column_names <- data.frame(Original = column_names)
new_names <- paste("P", column_names, sep = "_")
P_column_names$Updated <- new_names

## Update 
names(ECDS_clean) <- new_names

## Again for SGSS
# Get the column names
column_names <- names(SGSS_clean)
L_column_names <- data.frame(Original = column_names)
new_names <- paste("L", column_names, sep = "_")
L_column_names$Updated <- new_names

# Update 
names(SGSS_clean) <- new_names

# SAVE AS PARQUET FILES
# write_parquet(
#   ECDS_clean_nop,
#   "Y:/NEW NCSP/RESEARCH AND DEVELOPMENT/J Edney/ED opt out/ed-monitoring/ECDS_parquet.parquet"
# )
# 
# write_parquet(
#   SGSS_clean_nop,
#   "Y:/NEW NCSP/RESEARCH AND DEVELOPMENT/J Edney/ED opt out/ed-monitoring/SGSS_parquet.parquet"
# )
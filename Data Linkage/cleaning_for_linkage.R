##### DATA PREPARATION AND CLEANING FOR LINKAGE ANALYSIS #####

# IMPORT LIBRARIES
pacman::p_load(
  pacman, tidyverse, readxl, writexl, DBI, odbc, knitr,
  data.table, kableExtra, janitor, report, arrow
)

# LOAD PRIMARY DATASET
## establish connection
db_connection <- odbc::dbConnect(
  odbc::odbc(),
  .connection_string = "driver={SQL Server};server=SERVER_PLACEHOLDER;
                       database=DB_NAME;
                       Encrypt=true;trusted_connection=true",
  timeout = 60,
  timezone = Sys.timezone(),
  timezone_out = Sys.timezone()
)

## read primary dataset
dataset_a <- DBI::dbGetQuery(
  conn = db_connection,
  statement = "SELECT [PERSON_ID]
  ,[EVENT_DATE]
  ,[BIRTH_DATE]
  ,[ID_NUMBER]
  ,[LOCATION_CODE]
  ,[AREA_CODE]
  ,[AGE_AT_EVENT]
  ,[Sex]
  FROM [DB_NAME].[dbo].[TABLE_A]
  WHERE (EVENT_DATE <= '2023-04-07')"
) %>% 
  clean_names()

# INVESTIGATE DATA
## Calculate statistics
n_obs <- nrow(dataset_a)
missing_values <- colSums(is.na(dataset_a))
distinct_values <- sapply(dataset_a, function(x)
  length(unique(x)))
complete_perc <- round((((n_obs - missing_values) / n_obs) * 100), 2)

## Create summary
dataset_a_summary <- data.frame(
  n = n_obs,
  missing = missing_values,
  perc_complete = complete_perc,
  distinct = distinct_values
)

## print summaries
kable(head(dataset_a, n = 5), caption = 'Dataset A: Sample') %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = F,
    position = "left"
  )

dataset_a_summary %>%
  kbl(caption = 'Summary: Dataset A') %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = F,
    position = "left"
  )

# READ SECONDARY DATASET
dataset_b <- DBI::dbGetQuery(
  conn = db_connection,
  statement = "SELECT [RECORD_ID]
,[RECORD_DATE]
,[Sex]
,[PERSON_NUMBER]
,[Age]
,[AREA_CODE]
,[SITE_CODE]
,[DOB]
FROM [DB_NAME].[dbo].[TABLE_B]
WHERE (RECORD_DATE <= '2023-04-07')"
) %>%
  clean_names()

# INVESTIGATE SECONDARY DATASET
## Calculate statistics
n_obs <- nrow(dataset_b)
missing_values <- colSums(is.na(dataset_b))
distinct_values <- sapply(dataset_b, function(x)
  length(unique(x)))
complete_perc <- round((((n_obs - missing_values) / n_obs) * 100), 2)

## Create summary
dataset_b_summary <- data.frame(
  n = n_obs,
  missing = missing_values,
  perc_complete = complete_perc,
  distinct = distinct_values
)

## print summaries
kable(head(dataset_b, n = 5), caption = 'Dataset B: Sample') %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = F,
    position = "left"
  )

dataset_b_summary %>%
  kbl(caption = 'Summary: Dataset B') %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = F,
    position = "left"
  )

# STANDARDIZE COLUMN NAMES
dataset_b_standardized <- dataset_b %>%
  rename(
    birth_date = dob,
    date = record_date,
    id = record_id,
    person_id = person_number,
    location_code = site_code
  )

dataset_a_standardized <- dataset_a %>%
   rename(
     id = person_id,
     date = event_date,
     location_code = location_code,
     age = age_at_event)

# CLEAN DATES
dataset_a_clean_date <- dataset_a_standardized %>%
  mutate(date = as.Date(date)) %>%
  mutate(date_int = as.numeric(as.Date(date)))

dataset_b_clean_date <- dataset_b_standardized %>%
  mutate(date = as.Date(date)) %>%
  mutate(date_int = as.numeric(as.Date(date)))

# CLEAN AREA CODES
## standardize format
dataset_a_clean_area <- dataset_a_clean_date %>%
  mutate(area_code = toupper(area_code))

## remove spaces
dataset_a_clean_area$area_code <- gsub("  ", " ", dataset_a_clean_area$area_code)
## standardize format
dataset_a_clean_area$area_code <- gsub("^([0-9][0-9A-Z]+) ([0-9A-Z]+)$",
       "\\2 \\1",
       dataset_a_clean_area$area_code)
## remove all spaces
dataset_a_clean_area$area_code <- gsub(" ", "", dataset_a_clean_area$area_code)

## repeat for dataset B
dataset_b_clean_area <- dataset_b_clean_date %>%
  mutate(area_code = toupper(area_code))
dataset_b_clean_area$area_code <- gsub(" ", "", dataset_b_clean_area$area_code)

# CLEAN BIRTH DATES
## standardize date format
dataset_a_clean_birth <- dataset_a_clean_area %>%
  mutate(birth_date = as.Date(birth_date)) %>%
  mutate(birth_date_int = as.numeric(as.Date(birth_date)))

dataset_b_clean_birth <- dataset_b_clean_area %>%
  mutate(birth_date = as.Date(birth_date)) %>%
  mutate(birth_date_int = as.numeric(as.Date(birth_date)))

## validate birth dates
dataset_a_clean_birth <- dataset_a_clean_birth %>%
  mutate(year_of_birth = as.integer(format(birth_date, format="%Y")),
         month_of_birth = as.integer(format(birth_date, format="%m"))) %>%
  mutate(birth_date = if_else((year_of_birth >= 1910 & year_of_birth < 2022), birth_date, NA, NA))

dataset_b_clean_birth <- dataset_b_clean_birth %>%
  mutate(year_of_birth = as.integer(format(birth_date, format="%Y")),
         month_of_birth = as.integer(format(birth_date, format="%m"))) %>%
  mutate(birth_date = if_else((year_of_birth >= 1910 & year_of_birth < 2022), birth_date, NA, NA))

# STANDARDIZE SEX
dataset_b_clean <- dataset_b_clean_birth %>%
  mutate(sex = case_when(sex == 1 ~ "MALE",
                         sex == 2 ~ "FEMALE",
                         is.na(sex) ~ "MISSING"))

dataset_a_clean <- dataset_a_clean_birth %>%
  mutate(sex = ifelse(is.na(sex), "MISSING", sex))

# CHECK COMPLETENESS OF KEY FIELDS
dataset_a_complete <- dataset_a_clean %>%
  mutate(
    id_complete = ifelse(is.na(person_id), 0, 1),
    birth_complete = ifelse(is.na(birth_date), 0, 1),
    sex_complete = ifelse(is.na(sex), 0, 1),
    area_complete = ifelse(is.na(area_code), 0, 1)
  ) %>%
  unite(
    missing_pattern,
    c(id_complete, birth_complete, sex_complete, area_complete),
    sep = ""
  )

dataset_a_complete %>%
  group_by(missing_pattern) %>%
  tally()

dataset_b_complete <- dataset_b_clean %>%
  mutate(
    id_complete = ifelse(is.na(person_id), 0, 1),
    birth_complete = ifelse(is.na(birth_date), 0, 1),
    sex_complete = ifelse(is.na(sex), 0, 1),
    area_complete = ifelse(is.na(area_code), 0, 1)
  ) %>%
  unite(
    missing_pattern,
    c(id_complete, birth_complete, sex_complete, area_complete),
    sep = ""
  ) 

dataset_b_complete %>%
  group_by(missing_pattern) %>%
  tally()

# STANDARDIZE COLUMN ORDER
dataset_a_ordered <- dataset_a_clean %>%
  select(id,
         date,
         date_int,
         person_id,
         birth_date,
         birth_date_int,
         year_of_birth,
         month_of_birth,
         area_code,
         age,
         sex,
         location_code
         )

dataset_b_ordered <- dataset_b_clean %>%
  select(id,
         date,
         date_int,
         person_id,
         birth_date,
         birth_date_int,
         year_of_birth,
         month_of_birth,
         area_code,
         age,
         sex,
         location_code
  )

# ADD DATASET IDENTIFIERS TO COLUMN NAMES
## save original versions
dataset_b_original <- dataset_b_ordered
dataset_a_original <- dataset_a_ordered

## rename dataset A columns
a_columns <- names(dataset_a_ordered)
a_column_map <- data.frame(Original = a_columns)
a_new_names <- paste("A", a_columns, sep = "_")
a_column_map$Updated <- a_new_names
names(dataset_a_ordered) <- a_new_names

## rename dataset B columns
b_columns <- names(dataset_b_ordered)
b_column_map <- data.frame(Original = b_columns)
b_new_names <- paste("B", b_columns, sep = "_")
b_column_map$Updated <- b_new_names
names(dataset_b_ordered) <- b_new_names

# SAVE PROCESSED DATA
# write_parquet(
#   dataset_a_original,
#   "OUTPUT_PATH/dataset_a.parquet"
# )
# 
# write_parquet(
#   dataset_b_original,
#   "OUTPUT_PATH/dataset_b.parquet"
# )
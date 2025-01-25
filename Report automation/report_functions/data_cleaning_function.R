clean_data <- function(data) {
  library(dplyr)
  library(stringr)
  
  # Check if data is NULL or empty
  if (is.null(data) || nrow(data) == 0) {
    warning("Input data is NULL or empty. Returning NULL.")
    return(NULL)
  }
  
  # Check if required columns exist
  required_columns <- c("IMD", "Sex", "ethnic_group", "age_group", "age")
  missing_columns <- setdiff(required_columns, colnames(data))
  
  if (length(missing_columns) > 0) {
    warning(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
    # Add missing columns with NA values
    for (col in missing_columns) {
      data[[col]] <- NA
    }
  }
  
  # Clean the data
  cleaned_data <- data %>%
    mutate(
      # Handle missing or unknown IMD values
      IMD = if_else(is.na(IMD), "Unknown IMD", as.character(IMD)),
      
      # Replace missing or unknown Gender values
      Gender = if_else(is.na(Sex), "Unknown Gender", as.character(Sex)),
      
      # Replace missing or unknown ethnic group values
      ethnic_group = if_else(is.na(ethnic_group) | ethnic_group == "Unknown", "Unknown Ethnicity", as.character(ethnic_group)),
      
      # Handle missing or unknown age group values
      age_group = if_else(is.na(age_group) | age_group == "Unknown Age Group", NA_character_, as.character(age_group))
    ) %>%
    mutate(All = case_when(!is.na(age) & age >= 0 ~ "All", TRUE ~ NA_character_)) %>%
    
    # Map Gender values to more descriptive terms
    mutate(Gender = case_when(
      Gender == "FEMALE" ~ "Women",
      Gender == "MALE" ~ "Men",
      TRUE ~ Gender
    )) %>%
    
    # Process age group based on the specified categories
    mutate(
      age_group = case_when(
        age >= 0 & age <= 24 ~ "16 to 24",
        age >= 25 & age <= 34 ~ "25 to 34",
        age >= 35 & age <= 49 ~ "35 to 49",
        age >= 50 & age <= 64 ~ "50 to 64",
        age >= 65 & age <= 79 ~ "65 to 79",
        age >= 80 ~ "80 and over",
        TRUE ~ "Unknown Age Group"
      )
    ) %>%
    mutate(
      age_group = str_replace_all(age_group, "-", " to "),
      age_group = str_replace_all(age_group, "80\\+", "80 and over"),
      age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3"),
      age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group)
    ) %>%
    
    # Standardize IMD categories
    mutate(IMD = case_when(
      IMD == "1" ~ "IMD quintile 1 (most deprived)",
      IMD == "2" ~ "IMD quintile 2",
      IMD == "3" ~ "IMD quintile 3",
      IMD == "4" ~ "IMD quintile 4",
      IMD == "5" ~ "IMD quintile 5 (least deprived)",
      TRUE ~ IMD
    )) %>%
    
    # Filter out rows with missing age group values
    filter(!is.na(age_group)) 
  
  # Check if any data remains after cleaning
  if (nrow(cleaned_data) == 0) {
    warning("No data remains after cleaning. Returning NULL.")
    return(NULL)
  }
  
  return(cleaned_data)
}


# Improved attendees_characteristics function
attendees_characteristics <- function(df) {
  library(dplyr)
  
  # Ensure input data is not empty
  if (is.null(df) || nrow(df) == 0) {
    warning("Input data is NULL or empty. Returning an empty dataframe.")
    return(data.frame())
  }
  
  # Check if required columns exist
  required_columns <- c("age_group", "Gender", "IMD", "ethnic_group")
  missing_columns <- setdiff(required_columns, colnames(df))
  
  if (length(missing_columns) > 0) {
    stop(paste("The input dataframe is missing required columns:", 
               paste(missing_columns, collapse = ", ")))
  }
  
  # Group and summarize attendees by demographics
  df_processed <- df %>%
    group_by(age_group, Gender, IMD, ethnic_group) %>%
    summarise(Total_Attendees = n(), .groups = 'drop')
  
  return(df_processed)
}
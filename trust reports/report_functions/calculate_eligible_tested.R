calculate_eligible_tested <- function(data, demographic, bbv_type) {
  library(dplyr)
  
  if (nrow(data) == 0) return(data.frame())
  
  if (bbv_type == "HIV") {
    result <- data %>%
      filter(ECDS_bloods_any == "Yes" & HIV == "Yes")
  } else if (bbv_type == "HCV") {
    result <- data %>%
      filter(ECDS_bloods_any == "Yes" & HCV == "Yes")
  } else if (bbv_type == "HBV") {
    result <- data %>%
      filter(ECDS_bloods_any == "Yes" & HBV == "Yes")
  } else {
    result <- data # Default case if invalid BBV type provided
  }
  
  result <- result %>%
    group_by({{demographic}}) %>%
    summarise(
      Eligible_Count = n(),
      .groups = 'drop'
    ) %>%
    ungroup()
  
  return(result)
}
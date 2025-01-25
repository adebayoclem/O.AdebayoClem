calculate_blood_demographics <- function(data, demographic_var) {
  if (nrow(data) == 0) return(data.frame())
  # Filter for only those who had blood tests first
  blood_test_data <- data %>% 
    filter(ECDS_bloods_any == "Yes")
  
  result <- blood_test_data %>%
    group_by(!!sym(demographic_var)) %>%
    summarise(
      Blood_Test_Count = n(),
      .groups = 'drop'  # Ensures the data is ungrouped automatically after summarise
    ) %>%
    mutate(
      Demographic = demographic_var,  # Set demographic label
      Category = as.character(!!sym(demographic_var))  # Convert to character to ensure consistency
    )
  
  return(result)
}
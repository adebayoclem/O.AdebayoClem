calculate_proportion_tested <- function(data, demographic_var) {
  if (nrow(data) == 0) return(data.frame())
  result <- data %>%
    group_by(!!sym(demographic_var)) %>%
    summarise(
      Total_Attendees = n(),
      Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
      Proportion_Tested = Tested_Attendees / Total_Attendees,
      .groups = 'drop'  # Ensures the data is ungrouped automatically after summarise
    ) %>%
    mutate(
      Demographic = demographic_var,  # Set demographic label
      Category = as.character(!!sym(demographic_var))  # Convert to character to ensure consistency
    )
  
  return(result)
}

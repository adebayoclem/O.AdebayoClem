proportion_diagnosed_HCV <- function(data, demographic_var) {
  if (nrow(data) == 0) return(data.frame())
  
  result <- data %>%
    group_by(!!sym(demographic_var)) %>%
    summarise(
      Diagnosed_Attendees = sum(rna_positive_new == "New", na.rm = TRUE),
      Blood_Tested_Attendees = sum(ECDS_bloods_any == "Yes" & HCV == "Yes", na.rm = TRUE),
      Proportion_Tested = Diagnosed_Attendees / Blood_Tested_Attendees,
      .groups = 'drop'  # Ensures the data is ungrouped automatically after summarise
    ) %>%
    mutate(
      Demographic = demographic_var,  # Set demographic label
      Category_Value = as.character(!!sym(demographic_var))  # Convert to character to ensure consistency
    )
  
  # Filter out "Unknown" categories if needed
  if (demographic_var == "IMD") {
    result <- result %>% filter(Category_Value != "Unknown IMD")
  } else if (demographic_var == "ethnic_group") {
    result <- result %>% filter(!Category_Value %in% c("Unknown Ethnicity", "Other"))
  }
  
  return(result)
}

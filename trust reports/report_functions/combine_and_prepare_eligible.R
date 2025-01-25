combine_and_prepare_eligible <- function(data_frames, labels) {
  
  # Check if the list of data frames is empty or if all data frames inside are empty
    if (length(data_frames) == 0 || all(sapply(data_frames, nrow) == 0)) {
    return(data.frame())
  } 
  
   # Bind rows and convert demographic data to character to prevent type mismatch
  combined <- bind_rows(mapply(mutate, data_frames, Demographic = labels, SIMPLIFY = FALSE)) %>%
    mutate(across(c(age_group, Gender, ethnic_group, IMD), as.character)) %>%
    pivot_longer(
      cols = c(age_group, Gender, ethnic_group, IMD), 
      names_to = "Category", 
      values_to = "Category_Value", 
      values_drop_na = TRUE
    ) %>%
    filter(!(Category_Value %in% c("Unknown Gender", "Unknown Ethnicity", "Other", "Unknown IMD", "Unknown Age Group"))) %>%
    select(Demographic, Category_Value, Proportion_Tested, CI_Lower, CI_Upper)
  return(combined)
}

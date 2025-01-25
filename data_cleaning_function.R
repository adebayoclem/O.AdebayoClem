

clean_data <- function(data) {
  library(dplyr)
  library(stringr)
  
  cleaned_data <- data %>%
    mutate(
      IMD = if_else(is.na(IMD), "Unknown IMD", IMD),
      Gender = if_else(is.na(Sex), "Unknown Gender", Sex),
      ethnic_group = if_else(is.na(ethnic_group) | ethnic_group == "Unknown", "Unknown Ethnicity", ethnic_group)
    ) %>%
    mutate(All = case_when(age >= 0 ~ "All")) %>%
    mutate(Gender = replace(Gender, Gender == "FEMALE", "Women")) %>%
    mutate(Gender = replace(Gender, Gender == "MALE", "Men")) %>%
    mutate(
      age_group = str_replace_all(age_group, "-", " to "),
      age_group = str_replace_all(age_group, "80\\+", "80 and over"),
      age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")
    ) %>%
    mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group))
  
  return(cleaned_data)
}

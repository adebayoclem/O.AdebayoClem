count_new_diag <- function(data) {
  if (nrow(data) == 0) return(data.frame())
  data %>%
    mutate(
      IMD = if_else(is.na(IMD), "Unknown IMD", IMD),
      Gender = if_else(is.na(Sex), "Unknown Gender", Sex),
      ethnic_group = if_else(is.na(ethnic_group) | ethnic_group == "Unknown", "Unknown Ethnicity", ethnic_group),
      All = case_when(age >= 0 ~ "All"),
      Gender = replace(Gender, Gender == "FEMALE", "Women"),
      Gender = replace(Gender, Gender == "MALE", "Men"),
      age_group = str_replace_all(age_group, "-", " to "),
      age_group = str_replace_all(age_group, "80\\+", "80 and over"),
      age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3"),
      age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group),
      # Convert all columns to character to ensure consistency
      age_group = as.character(age_group),
      Gender = as.character(Gender),
      ethnic_group = as.character(ethnic_group),
      IMD = as.character(IMD)
    ) %>%
    count(age_group, Gender, ethnic_group, IMD) %>%
    pivot_longer(
      cols = c(age_group, Gender, ethnic_group, IMD),
      names_to = "Demographic",
      values_to = "Category"
    ) %>%
    filter(
      Category != "Unknown Gender",
      Category != "Other",
      Category != "Unknown Ethnicity",
      Category != "Unknown IMD"
    )
}
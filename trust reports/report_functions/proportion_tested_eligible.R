proportion_tested_eligible <- function(data, bbv, demographic) {
  library(dplyr)
  if (nrow(data) == 0) return(data.frame())
  z <- 1.96  # 95% confidence level
  
  result <- data %>%
    group_by({{demographic}}) %>%
    summarise(
      Eligible_Attendees = sum({{bbv}} == "Yes", na.rm = TRUE),
      Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
      Proportion_Tested = ifelse(Tested_Attendees > 0, Eligible_Attendees / Tested_Attendees, 0),
      CI_Lower = ifelse(Eligible_Attendees > 0,
                        (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
                           z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
                          (1 + (z^2)/Eligible_Attendees),
                        0),
      CI_Upper = ifelse(Eligible_Attendees > 0,
                        (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
                           z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
                          (1 + (z^2)/Eligible_Attendees),
                        0)
    ) %>%
    ungroup()
  
  return(result)
}


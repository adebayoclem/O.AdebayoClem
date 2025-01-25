#load or install required packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
if (!require(DBI)) install.packages("DBI")
if (!require(odbc)) install.packages("odbc")
if (!require(arrow)) install.packages("arrow")
if (!require(pacman)) install.packages("pacman")
if (!require(EpiFunc)) install.packages("EpiFunc")
if (!require(janitor)) install.packages("janitor")
if (!require(gtsummary)) install.packages("gtsummary")
if (!require(scales)) install.packages("scales")

# Establish ODBC connections -----------------------------------------------

#ESTABLISH ODBC CONNECTION WITH DATA
Y006 <- odbc::dbConnect(odbc::odbc(),
                        .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;
                             database=Y006_BBV_PID;
                             Encrypt=true;trusted_connection=true",
                        timeout = 60,
                        timezone = Sys.timezone(),
                        timezone_out = Sys.timezone())





# load BBV attendees data from Sentinel 
# load HIV attendees data from Sentinel
attendees_HIV_Site1 <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[12mthECDSattendees_included5sitesHIV]
WHERE (Site <= 'R1HNH')"
)

# load HCV attendees data from Sentinel
attendees_HCV_Site1 <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[12mthECDSattendees_included5sitesHCV]
WHERE (Site <= 'R1HNH')"
)

# load HBV attendees data from Sentinel
attendees_HBV_Site1 <- DBI::dbGetQuery(conn = Y006 , statement = "
SELECT *
FROM [Y006_BBV_PID].[dbo].[12mthECDSattendees_included5sitesHBV]
WHERE (Site <= 'R1HNH')"
)

attendees_HIV_Site1 <- attendees_HIV_Site1 %>% 
 mutate(IMD = if_else(
   is.na(IMD), 
   "Unknown IMD", 
   IMD, 
   IMD),
   Gender = if_else( # Replace Sex with Gender
     is.na(Sex), 
     "Unknown Gender", 
     Sex, 
     Sex),
   ethnic_group = if_else(
     (is.na(ethnic_group) |
        ethnic_group == "Unknown"), 
     "Unknown Ethnicity", 
     ethnic_group,
     ethnic_group)) %>% 
 mutate(All = case_when(age >= 0 ~ "All")) %>%
 mutate(Gender = replace(Gender, Gender=="FEMALE", "Women")) %>% # Replace Female
 mutate(Gender = replace(Gender, Gender=="MALE", "Men")) %>%    # Replace Male
 mutate(age_group = str_replace_all(age_group, "-", " to "),
        age_group = str_replace_all(age_group, "80\\+", "80 and over"),
        age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>%
 mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group))

attendees_HBV_Site1 <- attendees_HBV_Site1 %>% 
  mutate(IMD = if_else(
    is.na(IMD), 
    "Unknown IMD", 
    IMD, 
    IMD),
    Gender = if_else( # Replace Sex with Gender
      is.na(Sex), 
      "Unknown Gender", 
      Sex, 
      Sex),
    ethnic_group = if_else(
      (is.na(ethnic_group) |
         ethnic_group == "Unknown"), 
      "Unknown Ethnicity", 
      ethnic_group,
      ethnic_group)) %>% 
  mutate(All = case_when(age >= 0 ~ "All")) %>%
  mutate(Gender = replace(Gender, Gender=="FEMALE", "Women")) %>% # Replace Female
  mutate(Gender = replace(Gender, Gender=="MALE", "Men")) %>%    # Replace Male
  mutate(age_group = str_replace_all(age_group, "-", " to "),
         age_group = str_replace_all(age_group, "80\\+", "80 and over"),
         age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>%
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group))

attendees_HCV_Site1 <- attendees_HCV_Site1 %>% 
  mutate(IMD = if_else(
    is.na(IMD), 
    "Unknown IMD", 
    IMD, 
    IMD),
    Gender = if_else( # Replace Sex with Gender
      is.na(Sex), 
      "Unknown Gender", 
      Sex, 
      Sex),
    ethnic_group = if_else(
      (is.na(ethnic_group) |
         ethnic_group == "Unknown"), 
      "Unknown Ethnicity", 
      ethnic_group,
      ethnic_group)) %>% 
  mutate(All = case_when(age >= 0 ~ "All")) %>%
  mutate(Gender = replace(Gender, Gender=="FEMALE", "Women")) %>% # Replace Female
  mutate(Gender = replace(Gender, Gender=="MALE", "Men")) %>%    # Replace Male
  mutate(age_group = str_replace_all(age_group, "-", " to "),
         age_group = str_replace_all(age_group, "80\\+", "80 and over"),
         age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>%
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group))


# Loading necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(forcats)  # For reordering factors
 
attendees_HIV_Site1_regroup <- attendees_HIV_Site1 %>%
  group_by(age_group, Gender, IMD, ethnic_group) %>%
  summarise(Total_Attendees = n(), .groups = 'drop')

print(head(attendees_HIV_Site1_regroup))

# Reshaping the data for a combined plot
attendees_long_HIV_Site1 <- attendees_HIV_Site1_regroup %>%
  pivot_longer(cols = -Total_Attendees, names_to = "Demographic", values_to = "Category")

# Drop NA values and unwanted categories before plotting
attendees_long_HIV_Site1 <- attendees_long_HIV_Site1 %>%
  filter(!is.na(Category)) %>%
  filter(!(Category %in% c("Unknown Gender", "Unknown Ethnicity", "Other", "Unknown IMD")))

print(head(attendees_long_HIV_Site1))

# Reorder the factors within each demographic subgroup
attendees_long_HIV_Site1 <- attendees_long_HIV_Site1 %>%
  mutate(Category = case_when(
    Demographic == "age_group" ~ fct_relevel(Category, 
                                             "16 to 24", "25 to 34", "35 to 49", "50 to 64", 
                                             "65 to 79", "80 and over"),
    Demographic == "ethnic_group" ~ fct_reorder(Category, as.numeric(as.factor(Category))),
    TRUE ~ fct_reorder(Category, Total_Attendees)
  ))
 
 # Improved Visualisation without headers for each demographic subgroup
 ggplot(attendees_long_HIV_Site1, aes(x = Category, y = Total_Attendees)) +
   geom_bar(stat = "identity", fill = "darkblue", show.legend = FALSE) +
   coord_flip() +
   theme_minimal() +
   theme(
     axis.title.y = element_blank(),
     axis.text.y = element_text(size = 10),
     plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
     strip.text.x = element_blank(),
     strip.background = element_blank(),
     panel.grid.major.x = element_line(size = 0.5, color = "grey"),
     panel.grid.major.y = element_blank(),
     panel.spacing = unit(1, "lines")
   ) +
   labs(
     title = "All Attendees by Demographics (Site R1HNH)",
     x = "Total Attendees",
     y = ""
   ) +
   facet_wrap(~ Demographic, scales = "free_y", ncol = 1) +
   scale_y_continuous(labels = label_number())
 
 ---

   attendees_HCV_Site1_regroup <- attendees_HCV_Site1 %>%
   group_by(age_group, Gender, IMD, ethnic_group) %>%
   summarise(Total_Attendees = n(), .groups = 'drop')
 
 print(head(attendees_HCV_Site1_regroup))
 
 # Reshaping the data for a combined plot
 attendees_long_HCV_Site1 <- attendees_HCV_Site1_regroup %>%
   pivot_longer(cols = -Total_Attendees, names_to = "Demographic", values_to = "Category")
 
 # Drop NA values and unwanted categories before plotting
 attendees_long_HCV_Site1 <- attendees_long_HCV_Site1 %>%
   filter(!is.na(Category)) %>%
   filter(!(Category %in% c("Unknown Gender", "Unknown Ethnicity", "Other", "Unknown IMD")))
 
 print(head(attendees_long_HCV_Site1))
 
 # Reorder the factors within each demographic subgroup
 attendees_long_HCV_Site1 <- attendees_long_HCV_Site1 %>%
   mutate(Category = case_when(
     Demographic == "age_group" ~ fct_relevel(Category, 
                                              "16 to 24", "25 to 34", "35 to 49", "50 to 64", 
                                              "65 to 79", "80 and over"),
     Demographic == "ethnic_group" ~ fct_reorder(Category, as.numeric(as.factor(Category))),
     TRUE ~ fct_reorder(Category, Total_Attendees)
   ))
 
 # Improved Visualisation without headers for each demographic subgroup
 ggplot(attendees_long_HCV_Site1, aes(x = Category, y = Total_Attendees)) +
   geom_bar(stat = "identity", fill = "darkblue", show.legend = FALSE) +
   coord_flip() +
   theme_minimal() +
   theme(
     axis.title.y = element_blank(),
     axis.text.y = element_text(size = 10),
     plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
     strip.text.x = element_blank(),
     strip.background = element_blank(),
     panel.grid.major.x = element_line(size = 0.5, color = "grey"),
     panel.grid.major.y = element_blank(),
     panel.spacing = unit(1, "lines")
   ) +
   labs(
     title = "All Attendees by Demographics (Site R1HNH)",
     x = "Total Attendees",
     y = ""
   ) +
   facet_wrap(~ Demographic, scales = "free_y", ncol = 1) +
   scale_y_continuous(labels = label_number())

#Proportion of Attendees with a Blood test by Demographics (HIV go live dates)
 
 # Proportion of Attendees tested by age_group
 
proportion_tested_by_age_HIV <- attendees_HIV_Site1 %>%
   group_by(age_group) %>%
   summarise(
     Total_Attendees = n(),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Tested_Attendees / Total_Attendees
   ) %>%
   ungroup()
 
 # Proportion of Attendees tested by Gender

proportion_tested_by_gender_HIV <- attendees_HIV_Site1 %>%
   group_by(Gender) %>%
   summarise(
     Total_Attendees = n(),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Tested_Attendees / Total_Attendees
   ) %>%
   ungroup()
 
 # Proportion of Attendees tested by Ethnic Group

proportion_tested_by_ethnic_group_HIV <- attendees_HIV_Site1 %>%
   group_by(ethnic_group) %>%
   summarise(
     Total_Attendees = n(),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Tested_Attendees / Total_Attendees
   ) %>%
   ungroup()

# Proportion of Attendees tested by IMD

 proportion_tested_by_IMD_HIV <- attendees_HIV_Site1 %>%
   group_by(IMD) %>%
   summarise(
     Total_Attendees = n(),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Tested_Attendees / Total_Attendees
   ) %>%
   ungroup()
 
# Combine all the dataframes
combined_proportions_tested_HIV <- bind_rows(
   mutate(proportion_tested_by_age_HIV, Demographic = "Age Group"),
   mutate(proportion_tested_by_gender_HIV, Demographic = "Gender"),
   mutate(proportion_tested_by_ethnic_group_HIV, Demographic = "Ethnic Group"),
   mutate(proportion_tested_by_IMD_HIV, Demographic = "IMD")
 )
 
 # Pivot the combined dataframe for visualization
 combined_proportions_long_tested_HIV <- combined_proportions_tested_HIV %>%
   pivot_longer(cols = c(age_group, Gender, ethnic_group, IMD), 
                names_to = "Category", values_to = "Category_Value", 
                values_drop_na = TRUE) %>%
   filter(!(Category_Value %in% c("Unknown Gender", "Unknown Ethnicity", "Other", "Unknown IMD"))) %>%
   select(Demographic, Category_Value, Proportion_Tested)
 
 # Visualize the combined data
 ggplot(combined_proportions_long_tested_HIV, aes(x = Category_Value, y = Proportion_Tested)) +
   geom_bar(stat = "identity", fill = "darkblue", show.legend = FALSE) +
   coord_flip() +
   theme_minimal() +
   theme(
     axis.title.y = element_blank(),
     axis.text.y = element_text(size = 10),
     plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
     strip.text.x = element_blank(),
     strip.background = element_blank(),
     panel.grid.major.x = element_line(size = 0.5, color = "grey"),
     panel.grid.major.y = element_blank(),
     panel.spacing = unit(1, "lines")
   ) +
   labs(
     title = "Proportion of Attendees with a Blood Test by Demographics (Site R1HNH)",
     x = "Demographic Category",
     y = "Proportion Tested (%)"
   ) +
   facet_wrap(~ Demographic, scales = "free_y", ncol = 1) +
   scale_y_continuous(labels = scales::label_percent())
 

 #Proportion of eligible attendees with a HIV, HCV or HBV test by demographics (3 series, one for each BBV)
 #Proportion of eligible attendees with a HIV test by Gender
 # Assuming a 95% confidence level
 z <- 1.96
 proportion_of_eligible_tested_HIV_gender <- attendees_HIV_Site1 %>%
   group_by(Gender) %>%
   summarise(
     Eligible_Attendees = sum(HIV == "Yes", na.rm = TRUE),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Eligible_Attendees / Tested_Attendees,
     CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees),
     CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees)
   ) %>%
   ungroup()
 
 #Proportion of eligible attendees with a HIV test by Age group
 proportion_of_eligible_tested_HIV_agegroup <- attendees_HIV_Site1 %>%
   group_by(age_group) %>%
   summarise(
     Eligible_Attendees = sum(HIV == "Yes", na.rm = TRUE),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Eligible_Attendees / Tested_Attendees,
     CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees),
     CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees)
   ) %>%
   ungroup()

 #Proportion of eligible attendees with a HIV test by Ethnic group
 proportion_of_eligible_tested_HIV_ethnicgroup <- attendees_HIV_Site1 %>%
   group_by(ethnic_group) %>%
   summarise(
     Eligible_Attendees = sum(HIV == "Yes", na.rm = TRUE),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Eligible_Attendees / Tested_Attendees,
     CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees),
     CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees)
   ) %>%
   ungroup()

 #Proportion of eligible attendees with a HIV test by IMD 
 proportion_of_eligible_tested_HIV_IMD <- attendees_HIV_Site1 %>%
   group_by(IMD) %>%
   summarise(
     Eligible_Attendees = sum(HIV == "Yes", na.rm = TRUE),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Eligible_Attendees / Tested_Attendees,
     CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees),
     CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees)
   ) %>%
   ungroup()

 # Combine all the dataframes
 combined_proportions_eligible_tested_HIV <- bind_rows(
   mutate(proportion_of_eligible_tested_HIV_agegroup, Demographic = "Age Group"),
   mutate(proportion_of_eligible_tested_HIV_gender, Demographic = "Gender"),
   mutate(proportion_of_eligible_tested_HIV_ethnicgroup, Demographic = "Ethnic Group"),
   mutate(proportion_of_eligible_tested_HIV_IMD, Demographic = "IMD")
 )
 
 # Pivot the combined dataframe for visualization
 combined_proportions_long_eligible_tested_HIV <- combined_proportions_eligible_tested_HIV %>%
   pivot_longer(cols = c(age_group, Gender, ethnic_group, IMD), 
                names_to = "Category", values_to = "Category_Value", 
                values_drop_na = TRUE) %>%
   filter(!(Category_Value %in% c("Unknown Gender", "Unknown Ethnicity", "Other", "Unknown IMD"))) %>%
   select(Demographic, Category_Value, Proportion_Tested, CI_Lower, CI_Upper)
 
 # Visualize the combined data with confidence intervals
 ggplot(combined_proportions_long_eligible_tested_HIV, aes(x = Category_Value, y = Proportion_Tested)) +
   geom_bar(stat = "identity", position = position_dodge(), fill = "darkblue") +
   geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, position = position_dodge(0.9)) +
   geom_text(aes(label = sprintf("%.2f", Proportion_Tested), y = Proportion_Tested + 0.02), 
             position = position_dodge(0.9), vjust = -0.5, size = 3, color = "black") +
   coord_flip() +
   theme_minimal() +
   theme(
     axis.title.y = element_blank(),
     axis.text.y = element_text(size = 10),
     plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
     strip.text.x = element_blank(),
     strip.background = element_blank(),
     panel.grid.major.x = element_line(size = 0.5, color = "grey"),
     panel.grid.major.y = element_blank(),
     panel.spacing = unit(1, "lines")
   ) +
   labs(
     title = "Proportion of eligible Attendees with a HIV Test by Demographics (Site R1HNH)",
     x = "Demographic Category",
     y = "Proportion Tested"
   ) +
   facet_wrap(~ Demographic, scales = "free_y", ncol = 1) +
   scale_y_continuous()
 --- 
 #Proportion of eligible attendees with a HBV test by Gender

# Assuming a 95% confidence level
 z <- 1.96
 
 # Add the confidence interval calculation for one of your groupings, for example, for Gender
 proportion_of_eligible_tested_HBV_gender <- attendees_HBV_Site1 %>%
   group_by(Gender) %>%
   summarise(
     Eligible_Attendees = sum(HBV == "Yes", na.rm = TRUE),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Eligible_Attendees / Tested_Attendees,
     CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees),
     CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees)
   ) %>%
   ungroup()
 
 
 #Proportion of eligible attendees with a HBV test by Age group
 proportion_of_eligible_tested_HBV_agegroup <- attendees_HBV_Site1 %>%
   group_by(age_group) %>%
   summarise(
     Eligible_Attendees = sum(HBV == "Yes", na.rm = TRUE),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Eligible_Attendees / Tested_Attendees,
     CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees),
     CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees)
   ) %>%
   ungroup()
 
 #Proportion of eligible attendees with a HBV test by Ethnic group
 proportion_of_eligible_tested_HBV_ethnicgroup <- attendees_HBV_Site1 %>%
   group_by(ethnic_group) %>%
   summarise(
     Eligible_Attendees = sum(HBV == "Yes", na.rm = TRUE),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Eligible_Attendees / Tested_Attendees,
     CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees),
     CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees)
   ) %>%
   ungroup()
 
 #Proportion of eligible attendees with a HBV test by IMD 
 proportion_of_eligible_tested_HBV_IMD <- attendees_HBV_Site1 %>%
   group_by(IMD) %>%
   summarise(
     Eligible_Attendees = sum(HBV == "Yes", na.rm = TRUE),
     Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
     Proportion_Tested = Eligible_Attendees / Tested_Attendees,
     CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees),
     CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
                   z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
       (1 + (z^2)/Eligible_Attendees)
   ) %>%
   ungroup()
 
 # Combine all the dataframes
 combined_proportions_eligible_tested_HBV <- bind_rows(
   mutate(proportion_of_eligible_tested_HBV_agegroup, Demographic = "Age Group"),
   mutate(proportion_of_eligible_tested_HBV_gender, Demographic = "Gender"),
   mutate(proportion_of_eligible_tested_HBV_ethnicgroup, Demographic = "Ethnic Group"),
   mutate(proportion_of_eligible_tested_HBV_IMD, Demographic = "IMD")
 )
 
 # Pivot the combined dataframe for visualization
 combined_proportions_long_eligible_tested_HBV <- combined_proportions_eligible_tested_HBV %>%
   pivot_longer(cols = c(age_group, Gender, ethnic_group, IMD), 
                names_to = "Category", values_to = "Category_Value", 
                values_drop_na = TRUE) %>%
   filter(!(Category_Value %in% c("Unknown Gender", "Unknown Ethnicity", "Other", "Unknown IMD"))) %>%
   select(Demographic, Category_Value, Proportion_Tested, CI_Lower, CI_Upper)
 
 # Visualize the combined data
 ggplot(combined_proportions_long_eligible_tested_HBV, aes(x = Category_Value, y = Proportion_Tested)) +
   geom_bar(stat = "identity", fill = "darkblue") +
   geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
   coord_flip() +
   theme_minimal() +
   theme(
     axis.title.y = element_blank(),
     axis.text.y = element_text(size = 10),
     plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
     strip.text.x = element_blank(),
     strip.background = element_blank(),
     panel.grid.major.x = element_line(size = 0.5, color = "grey"),
     panel.grid.major.y = element_blank(),
     panel.spacing = unit(1, "lines"),
     axis.text.x = element_text(angle = 45, hjust = 1)
   ) +
   labs(
     title = "Proportion of eligible Attendees with a HBV Test by Demographics (Site R1HNH)",
     x = "Demographic Category",
     y = "Proportion Tested"
   ) +
   facet_wrap(~ Demographic, scales = "free_y", ncol = 1) +
   scale_y_continuous()
 
---

#Proportion of eligible attendees with a HCV test by Gender
proportion_of_eligible_tested_HCV_gender <- attendees_HCV_Site1 %>%
group_by(Gender) %>%
summarise(
 Eligible_Attendees = sum(HCV == "Yes", na.rm = TRUE),
 Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
 Proportion_Tested = Eligible_Attendees / Tested_Attendees,
 CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
               z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
   (1 + (z^2)/Eligible_Attendees),
 CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
               z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
   (1 + (z^2)/Eligible_Attendees)
) %>%
   ungroup()

#Proportion of eligible attendees with a HCV test by Age group
proportion_of_eligible_tested_HCV_agegroup <- attendees_HCV_Site1 %>%
group_by(age_group) %>%
summarise(
 Eligible_Attendees = sum(HCV == "Yes", na.rm = TRUE),
 Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
 Proportion_Tested = Eligible_Attendees / Tested_Attendees,
 CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
               z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
   (1 + (z^2)/Eligible_Attendees),
 CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
               z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
   (1 + (z^2)/Eligible_Attendees)
) %>%
  ungroup()

#Proportion of eligible attendees with a HCV test by Ethnic group
proportion_of_eligible_tested_HCV_ethnicgroup <- attendees_HCV_Site1 %>%
group_by(ethnic_group) %>%
summarise(
 Eligible_Attendees = sum(HCV == "Yes", na.rm = TRUE),
 Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
 Proportion_Tested = Eligible_Attendees / Tested_Attendees,
 CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
               z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
   (1 + (z^2)/Eligible_Attendees),
 CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
               z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
   (1 + (z^2)/Eligible_Attendees)
) %>%
  ungroup()

#Proportion of eligible attendees with a HCV test by IMD 
proportion_of_eligible_tested_HCV_IMD <- attendees_HCV_Site1 %>%
group_by(IMD) %>%
summarise(
 Eligible_Attendees = sum(HCV == "Yes", na.rm = TRUE),
 Tested_Attendees = sum(ECDS_bloods_any == "Yes", na.rm = TRUE),
 Proportion_Tested = Eligible_Attendees / Tested_Attendees,
 CI_Lower = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) - 
               z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
   (1 + (z^2)/Eligible_Attendees),
 CI_Upper = (Proportion_Tested + (z^2)/(2*Eligible_Attendees) + 
               z*sqrt((Proportion_Tested*(1 - Proportion_Tested))/Eligible_Attendees + (z^2)/(4*Eligible_Attendees^2))) / 
   (1 + (z^2)/Eligible_Attendees)
) %>%
  ungroup()



# Combine all the dataframes
combined_proportions_eligible_tested_HCV <- bind_rows(
mutate(proportion_of_eligible_tested_HCV_agegroup, Demographic = "Age Group"),
mutate(proportion_of_eligible_tested_HCV_gender, Demographic = "Gender"),
mutate(proportion_of_eligible_tested_HCV_ethnicgroup, Demographic = "Ethnic Group"),
mutate(proportion_of_eligible_tested_HCV_IMD, Demographic = "IMD")
)

# Pivot the combined dataframe for visualization
combined_proportions_long_eligible_tested_HCV <- combined_proportions_eligible_tested_HCV %>%
pivot_longer(cols = c(age_group, Gender, ethnic_group, IMD), 
            names_to = "Category", values_to = "Category_Value", 
            values_drop_na = TRUE) %>%
filter(!(Category_Value %in% c("Unknown Gender", "Unknown Ethnicity", "Other", "Unknown IMD"))) %>%
  select(Demographic, Category_Value, Proportion_Tested, CI_Lower, CI_Upper)

# Visualize the combined data
ggplot(combined_proportions_long_eligible_tested_HCV, aes(x = Category_Value, y = Proportion_Tested)) +
geom_bar(stat = "identity", fill = "darkblue", show.legend = FALSE) +
geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
coord_flip() +
theme_minimal() +
theme(
 axis.title.y = element_blank(),
 axis.text.y = element_text(size = 10),
 plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
 strip.text.x = element_blank(),
 strip.background = element_blank(),
 panel.grid.major.x = element_line(size = 0.5, color = "grey"),
 panel.grid.major.y = element_blank(),
 panel.spacing = unit(1, "lines")
) +
labs(
 title = "Proportion of eligible Attendees with a HCV Test by Demographics (Site R1HNH)",
 x = "Demographic Category",
 y = "Proportion Tested"
) +
facet_wrap(~ Demographic, scales = "free_y", ncol = 1) +
scale_y_continuous() 

---
# Combining all test dataframes
  combined_all_tests <- bind_rows(
    combined_proportions_long_eligible_tested_HIV, 
    combined_proportions_long_eligible_tested_HBV, 
    combined_proportions_long_eligible_tested_HCV
  )

# Check the combined dataframe
head(combined_all_tests)

  
# Combine the dataframes with confidence intervals
combined_all_tests_with_ci <- bind_rows(
  combined_proportions_long_eligible_tested_HIV %>% mutate(Test_Type = "HIV"),
  combined_proportions_long_eligible_tested_HBV %>% mutate(Test_Type = "HBV"),
  combined_proportions_long_eligible_tested_HCV %>% mutate(Test_Type = "HCV")
)

head(combined_all_tests_with_ci)

# Define a custom blue color palette
blue_palette <- c("#1f77b4", "#aec7e8", "#4c78a8")  

# Create the plot with the custom palette and error bars
ggplot(combined_all_tests_with_ci, aes(x = Category_Value, y = Proportion_Tested, fill = Test_Type)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_text(aes(y = CI_Upper, label = sprintf("%.2f", CI_Upper)), vjust = -0.5, size = 3, color = "black", position = position_dodge(width = 0.9)) +
  coord_flip() +
  scale_fill_manual(values = blue_palette) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, color = "lightgrey"),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  labs(
    title = "Proportion of Eligible Attendees with BBV Tests by Demographics (Site R1HNH)",
    x = "Demographic Category",
    y = "Proportion Tested"
  ) +
  facet_grid(Demographic ~ Test_Type, scales = "free_y", space = "free_y") +
  scale_y_continuous(labels = scales::label_percent())


---
#Proportion diagnosed positive for HCV by age group
Proportion_diagnosed_by_age_HCV <- attendees_HCV_Site1 %>%
  group_by(age_group) %>%
  summarise(
    Diagnosed_Attendees = sum(rna_positive_new == "New", na.rm = TRUE),
    Blood_Tested_Attendees = sum(ECDS_bloods_any == "Yes" & HCV == "Yes", na.rm = TRUE),
    Proportion_Tested = Diagnosed_Attendees / Blood_Tested_Attendees
  ) %>%
  ungroup()

#Proportion diagnosed positive for HCV by Gender
Proportion_diagnosed_by_gender_HCV <- attendees_HCV_Site1 %>%
  group_by(Gender) %>%
  summarise(
    Diagnosed_Attendees = sum(rna_positive_new == "New", na.rm = TRUE),
    Blood_Tested_Attendees = sum(ECDS_bloods_any == "Yes" & HCV == "Yes", na.rm = TRUE),
    Proportion_Tested = Diagnosed_Attendees / Blood_Tested_Attendees
  ) %>%
  ungroup()

#Proportion diagnosed positive for HCV by Ethnic group
Proportion_diagnosed_by_Ethnic_Group_HCV <- attendees_HCV_Site1 %>%
  group_by(ethnic_group) %>%
  summarise(
    Diagnosed_Attendees = sum(rna_positive_new == "New", na.rm = TRUE),
    Blood_Tested_Attendees = sum(ECDS_bloods_any == "Yes" & HCV == "Yes", na.rm = TRUE),
    Proportion_Tested = Diagnosed_Attendees / Blood_Tested_Attendees
  ) %>%
  ungroup()

#Proportion diagnosed positive for HCV by IMD
Proportion_diagnosed_by_IMD_HCV <- attendees_HCV_Site1 %>%
  group_by(IMD) %>%
  summarise(
    Diagnosed_Attendees = sum(rna_positive_new == "New", na.rm = TRUE),
    Blood_Tested_Attendees = sum(ECDS_bloods_any == "Yes" & HCV == "Yes", na.rm = TRUE),
    Proportion_Tested = Diagnosed_Attendees / Blood_Tested_Attendees
  ) %>%
  ungroup()

# Combine the percentage diagnosed data frames for improved visualisation for all demographic groups
# Reshape and combine all demographic data frames
combined_Proportion_diagnosed_long_HCV <- bind_rows(
  Proportion_diagnosed_by_age_HCV %>% mutate(Demographic = "Age Group") %>% rename(Category_Value = age_group),
  Proportion_diagnosed_by_gender_HCV %>% mutate(Demographic = "Gender") %>% rename(Category_Value = Gender),
  Proportion_diagnosed_by_IMD_HCV %>% mutate(Demographic = "IMD") %>% rename(Category_Value = IMD),
  Proportion_diagnosed_by_Ethnic_Group_HCV %>% mutate(Demographic = "Ethnic Group") %>% rename(Category_Value = ethnic_group)
) %>% 
  select(Demographic, Category_Value, Proportion = Proportion_Tested)

# Filter out "Unknown IMD" from the dataset
combined_Proportion_diagnosed_long_HCV <- combined_Proportion_diagnosed_long_HCV %>% 
  filter(
    !(Demographic == "IMD" & Category_Value == "Unknown IMD"),
    !(Demographic == "Ethnic Group" & Category_Value %in% c("Unknown Ethnicity", "Other"))
  )

# Visualisation
# Improved Visualisation
ggplot(combined_Proportion_diagnosed_long_HCV, aes(x = Category_Value, y = Proportion)) +
  geom_bar(stat = "identity", fill = "darkblue", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text.x = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, color = "grey"),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Proportion diagnosed HCV positive by demographics (Site R1HNH)",
    x = "Demographic Category",
    y = "Proportion Diagnosed (%)"
  ) +
  facet_wrap(~ Demographic, scales = "free_y", ncol = 1) 

---
  #Proportion diagnosed positive for HBV by age group
  Proportion_diagnosed_by_age_HBV <- attendees_HBV_Site1 %>%
  group_by(age_group) %>%
  summarise(
    Diagnosed_Attendees = sum(HBsAg_positive_new == "New", na.rm = TRUE),
    Blood_Tested_Attendees = sum(ECDS_bloods_any == "Yes" & HBV == "Yes", na.rm = TRUE),
    Proportion_Tested = Diagnosed_Attendees / Blood_Tested_Attendees
  ) %>%
  ungroup()

#Proportion diagnosed positive for HBV by Gender
Proportion_diagnosed_by_gender_HBV <- attendees_HBV_Site1 %>%
  group_by(Gender) %>%
  summarise(
    Diagnosed_Attendees = sum(HBsAg_positive_new == "New", na.rm = TRUE),
    Blood_Tested_Attendees = sum(ECDS_bloods_any == "Yes" & HBV == "Yes", na.rm = TRUE),
    Proportion_Tested = Diagnosed_Attendees / Blood_Tested_Attendees
  ) %>%
  ungroup()

#Proportion diagnosed positive for HBV by Ethnic group
Proportion_diagnosed_by_Ethnic_Group_HBV <- attendees_HBV_Site1 %>%
  group_by(ethnic_group) %>%
  summarise(
    Diagnosed_Attendees = sum(HBsAg_positive_new == "New", na.rm = TRUE),
    Blood_Tested_Attendees = sum(ECDS_bloods_any == "Yes" & HBV == "Yes", na.rm = TRUE),
    Proportion_Tested = Diagnosed_Attendees / Blood_Tested_Attendees
  ) %>%
  ungroup()

#Proportion diagnosed positive for HBV by IMD
Proportion_diagnosed_by_IMD_HBV <- attendees_HBV_Site1 %>%
  group_by(IMD) %>%
  summarise(
    Diagnosed_Attendees = sum(HBsAg_positive_new == "New", na.rm = TRUE),
    Blood_Tested_Attendees = sum(ECDS_bloods_any == "Yes" & HBV == "Yes", na.rm = TRUE),
    Proportion_Tested = Diagnosed_Attendees / Blood_Tested_Attendees
  ) %>%
  ungroup()

# Combine the percentage diagnosed data frames for improved visualisation for all demographic groups
# Reshape and combine all demographic data frames
combined_Proportion_diagnosed_long_HBV <- bind_rows(
  Proportion_diagnosed_by_age_HBV %>% mutate(Demographic = "Age Group") %>% rename(Category_Value = age_group),
  Proportion_diagnosed_by_gender_HBV %>% mutate(Demographic = "Gender") %>% rename(Category_Value = Gender),
  Proportion_diagnosed_by_IMD_HBV %>% mutate(Demographic = "IMD") %>% rename(Category_Value = IMD),
  Proportion_diagnosed_by_Ethnic_Group_HBV %>% mutate(Demographic = "Ethnic Group") %>% rename(Category_Value = ethnic_group)
) %>% 
  select(Demographic, Category_Value, Proportion = Proportion_Tested)

# Filter out "Unknown IMD" from the dataset
combined_Proportion_diagnosed_long_HBV <- combined_Proportion_diagnosed_long_HBV %>% 
  filter(
    !(Demographic == "IMD" & Category_Value == "Unknown IMD"),
    !(Demographic == "Ethnic Group" & Category_Value %in% c("Unknown Ethnicity", "Other"))
  )

# Visualisation
# Improved Visualisation
ggplot(combined_Proportion_diagnosed_long_HBV, aes(x = Category_Value, y = Proportion)) +
  geom_bar(stat = "identity", fill = "darkblue", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text.x = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, color = "grey"),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Proportion diagnosed HBV positive by demographics (Site R1HNH)",
    x = "Demographic Category",
    y = "Proportion Diagnosed (%)"
  ) +
  facet_wrap(~ Demographic, scales = "free_y", ncol = 1) 

---

  # Combine and reshape the HBV data
  combined_Proportion_diagnosed_long_HBV <- bind_rows(
    Proportion_diagnosed_by_age_HBV %>% mutate(Demographic = "Age Group") %>% rename(Category_Value = age_group),
    Proportion_diagnosed_by_gender_HBV %>% mutate(Demographic = "Gender") %>% rename(Category_Value = Gender),
    Proportion_diagnosed_by_IMD_HBV %>% mutate(Demographic = "IMD") %>% rename(Category_Value = IMD),
    Proportion_diagnosed_by_Ethnic_Group_HBV %>% mutate(Demographic = "Ethnic Group") %>% rename(Category_Value = ethnic_group)
  ) %>% 
  mutate(Disease = "HBV") %>% 
  select(Demographic, Category_Value, Proportion = Proportion_Tested, Disease)

# Filter out unwanted categories from HBV dataset
combined_Proportion_diagnosed_long_HBV <- combined_Proportion_diagnosed_long_HBV %>% 
  filter(
    !(Demographic == "IMD" & Category_Value == "Unknown IMD"),
    !(Demographic == "Ethnic Group" & Category_Value %in% c("Unknown Ethnicity", "Other"))
  )

# Similar process for HCV dataset
# Ensure you have similar individual datasets for HCV like Proportion_diagnosed_by_age_HCV etc.
combined_Proportion_diagnosed_long_HCV <- bind_rows(
  Proportion_diagnosed_by_age_HCV %>% mutate(Demographic = "Age Group") %>% rename(Category_Value = age_group),
  Proportion_diagnosed_by_gender_HCV %>% mutate(Demographic = "Gender") %>% rename(Category_Value = Gender),
  Proportion_diagnosed_by_IMD_HCV %>% mutate(Demographic = "IMD") %>% rename(Category_Value = IMD),
  Proportion_diagnosed_by_Ethnic_Group_HCV %>% mutate(Demographic = "Ethnic Group") %>% rename(Category_Value = ethnic_group)
) %>% 
  mutate(Disease = "HCV") %>% 
  select(Demographic, Category_Value, Proportion = Proportion_Tested, Disease)

# Filter out unwanted categories from HCV dataset
combined_Proportion_diagnosed_long_HCV <- combined_Proportion_diagnosed_long_HCV %>% 
  filter(Category_Value != "Unknown Gender")

# Now combine the HBV and HCV data
combined_all_BBV <- bind_rows(combined_Proportion_diagnosed_long_HBV, 
                              combined_Proportion_diagnosed_long_HCV)

combined_all_BBV_filtered <- combined_all_BBV %>%
  filter(
    Category_Value != "Unknown Gender" & 
      Category_Value != "Other" & 
      Category_Value != "Unknown IMD" & 
      Category_Value != "Unknown Ethnicity"
  )

# Visualization
ggplot(combined_all_BBV_filtered, aes(x = Category_Value, y = Proportion, fill = Disease)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text.x = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, color = "grey"),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Proportion Diagnosed Positive by Demographics for HBV & HCV (Site R1HNH)",
    x = "Demographic Category",
    y = "Proportion"
  ) +
  facet_grid(Demographic ~ Disease, scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c("darkblue", "steelblue"))
  
     

#Number of Newly diagnosed with HCV
#Load HCV new diagnoses
attendees_HCV_new_diag <- attendees_HCV_Site1 %>% filter(rna_positive_new == "New")

#Data cleaning and manipulation
attendees_HCV_new_diag_counts <- attendees_HCV_new_diag %>% 
  mutate(IMD = if_else(
    is.na(IMD), 
    "Unknown IMD", 
    IMD, 
    IMD),
    Gender = if_else( # Replace Sex with Gender
      is.na(Sex), 
      "Unknown Gender", 
      Sex, 
      Sex),
    ethnic_group = if_else(
      (is.na(ethnic_group) |
         ethnic_group == "Unknown"), 
      "Unknown Ethnicity", 
      ethnic_group,
      ethnic_group)) %>% 
  mutate(All = case_when(age >= 0 ~ "All")) %>%
  mutate(Gender = replace(Gender, Gender=="FEMALE", "Women")) %>% # Replace Female
  mutate(Gender = replace(Gender, Gender=="MALE", "Men")) %>%    # Replace Male
  mutate(age_group = str_replace_all(age_group, "-", " to "),
         age_group = str_replace_all(age_group, "80\\+", "80 and over"),
         age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>%
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group)) %>%
  count(age_group, Gender, ethnic_group, IMD) %>%
  pivot_longer(cols = c(age_group, Gender, ethnic_group, IMD), names_to = "Demographic", values_to = "Category") %>%
  filter(Category != "Unknown Gender", Category != "Other", Category != "Unknown Ethnicity", Category != "Unknown IMD")


# Visualizing the count of new diagnoses by demographics
ggplot(attendees_HCV_new_diag_counts, aes(x = Category, y = n)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text.x = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, color = "grey"),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Count of Newly Diagnosed HCV Patients by Demographics (Site R1HNH)",
    x = "Demographic Category",
    y = "Number of New Diagnoses"
  ) +
  facet_wrap(~ Demographic, scales = "free_y", ncol = 1)


#Number of Newly diagnosed with HBV
#Load HBV Diagnoses data
attendees_HBV_new_diag <- attendees_HBV_Site1 %>% filter(HBsAg_positive_new == "New")

#Data cleaning and manipulation
attendees_HBV_new_diag_counts <- attendees_HBV_new_diag %>% 
  mutate(IMD = if_else(
    is.na(IMD), 
    "Unknown IMD", 
    IMD, 
    IMD),
    Gender = if_else( # Replace Sex with Gender
      is.na(Sex), 
      "Unknown Gender", 
      Sex, 
      Sex),
    ethnic_group = if_else(
      (is.na(ethnic_group) |
         ethnic_group == "Unknown"), 
      "Unknown Ethnicity", 
      ethnic_group,
      ethnic_group)) %>% 
  mutate(All = case_when(age >= 0 ~ "All")) %>%
  mutate(Gender = replace(Gender, Gender=="FEMALE", "Women")) %>% # Replace Female
  mutate(Gender = replace(Gender, Gender=="MALE", "Men")) %>%    # Replace Male
  mutate(age_group = str_replace_all(age_group, "-", " to "),
         age_group = str_replace_all(age_group, "80\\+", "80 and over"),
         age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")) %>%
  mutate(age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group)) %>%
  count(age_group, Gender, ethnic_group, IMD) %>%
  pivot_longer(cols = c(age_group, Gender, ethnic_group, IMD), names_to = "Demographic", values_to = "Category") %>%
  filter(Category != "Unknown Gender", Category != "Other", Category != "Unknown Ethnicity", Category != "Unknown IMD")


# Visualizing the count of new diagnoses by demographics
ggplot(attendees_HBV_new_diag_counts, aes(x = Category, y = n)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text.x = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, color = "grey"),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Count of Newly Diagnosed HBV Patients by Demographics (Site R1HNH)",
    x = "Demographic Category",
    y = "Number of New Diagnoses"
  ) +
  facet_wrap(~ Demographic, scales = "free_y", ncol = 1)

# Add a column to each dataset to identify the disease
attendees_HCV_new_diag_counts <- attendees_HCV_new_diag_counts %>% 
  mutate(Disease = "HCV")

attendees_HBV_new_diag_counts <- attendees_HBV_new_diag_counts %>% 
  mutate(Disease = "HBV")

# Combine the datasets
combined_BBV_counts <- bind_rows(attendees_HCV_new_diag_counts, attendees_HBV_new_diag_counts)

# Visualization
# Define a custom blue color palette
blue_palette <- c("HCV" = "darkblue", "HBV" = "lightblue")

# Visualization with custom blue shades
ggplot(combined_BBV_counts, aes(x = Category, y = n, fill = Disease)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = blue_palette) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text.x = element_blank(),
    strip.background = element_blank(),
    panel.grid.major.x = element_line(size = 0.5, color = "grey"),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Number of Newly Diagnosed HBV and HCV Patients by Demographics (Site R1HNH)",
    x = "Demographic Category",
    y = "Number of New Diagnoses"
  ) +
  facet_grid(Demographic ~ Disease, scales = "free_y", space = "free_y")


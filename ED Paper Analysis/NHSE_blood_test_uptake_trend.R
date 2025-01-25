#LOAD PACKAGES
pacman::p_load(
  pacman,
  tidyverse,
  lubridate,
  readxl,
  writexl,
  DBI,
  odbc,
  arrow,
  janitor,
  reshape2,
  ggplot2,
  scales,
  conflicted,
  plotly
)

# Handle package conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::recode)
conflicts_prefer(plotly::layout)



# Import site codes
site_codes <- read_xlsx("Data investigation_24month/sitecodelookup2024_final.xlsx", sheet = "site lookup codes") %>%
  clean_names() %>%
  mutate(site_hospital_name = toupper(hospital)) %>%
  filter(included == "Yes")  # Filter for included sites

# Clean site names according to NHSE document
site_codes <- site_codes %>%
  mutate(site_hospital_name = ifelse(site_hospital_name == "CHELSEA & WESTMINSTER HOSPITAL", "CHELSEA AND WESTMINSTER HOSPITAL",
                                     ifelse(site_hospital_name == "KING'S COLLEGE HOSPITAL", "KING'S COLLEGE HOSPITAL (DENMARK HILL)",
                                            site_hospital_name)))

# Define a list of included site codes and hospital names
included_sites <- c('RYJ02', 'RQXM1', 'R0A02', 'R1HNH', 'R0A66', 'RJ231', 'E0A3H', 'RJ122', 'R1H12', 'RJ224', 
                    'RQM91', 'R1HKH', 'R0A07', 'RYJ01')

included_hospitals <- c('CHARING CROSS HOSPITAL', 'HOMERTON UNIVERSITY HOSPITAL', 'MANCHESTER ROYAL INFIRMARY',
                        'NEWHAM GENERAL HOSPITAL', 'NORTH MANCHESTER GENERAL HOSPITAL', 'QUEEN ELIZABETH HOSPITAL',
                        'ROYAL SUSSEX COUNTY HOSPITAL', 'ST THOMAS\' HOSPITAL', 'THE ROYAL LONDON HOSPITAL',
                        'UNIVERSITY HOSPITAL LEWISHAM', 'WEST MIDDLESEX UNIVERSITY HOSPITAL',
                        'WHIPPS CROSS UNIVERSITY HOSPITAL', 'WYTHENSHAWE HOSPITAL', 'ST MARY\'S HOSPITAL')

# Filter site codes for included sites
site_codes_filtered <- site_codes %>%
  filter(site_code %in% included_sites & site_hospital_name %in% included_hospitals)

# Import NHSE file
nhse_numbers <- read_csv("Data investigation_24month/NHSE_dashboard_200824.csv") %>%
  clean_names() %>%
  mutate(site_hospital_name = toupper(trimws(site_hospital_name))) %>%
  select(metric_name, site_hospital_name, year_of_reporting_period_start, month_of_reporting_period_start, 
         metric_or_proportion_integer) 

# Define included hospital names, ensuring they are also standardized
included_hospitals <- c(
  'CHARING CROSS HOSPITAL', 'HOMERTON UNIVERSITY HOSPITAL', 'MANCHESTER ROYAL INFIRMARY',
  'NEWHAM GENERAL HOSPITAL', 'NORTH MANCHESTER GENERAL HOSPITAL', 'QUEEN ELIZABETH HOSPITAL',
  'ROYAL SUSSEX COUNTY HOSPITAL', 'ST THOMAS\' HOSPITAL', 'THE ROYAL LONDON HOSPITAL',
  'UNIVERSITY HOSPITAL LEWISHAM', 'WEST MIDDLESEX UNIVERSITY HOSPITAL',
  'WHIPPS CROSS UNIVERSITY HOSPITAL', 'WYTHENSHAWE HOSPITAL', 'ST MARY\'S HOSPITAL'
)

# Filter NHSE numbers for included hospitals
nhse_numbers_filtered <- nhse_numbers %>%
  filter(site_hospital_name %in% included_hospitals)


# concatenate the month and year into one field 
nhse_numbers_filtered$date <- with(
  nhse_numbers_filtered, 
  sprintf("%d-%02s", 
          year_of_reporting_period_start,
          month_of_reporting_period_start))

# Check dates
max_date <- max(nhse_numbers_filtered$date, na.rm = TRUE)
min_date <- min(nhse_numbers_filtered$date, na.rm = TRUE)

# Print results
print(paste("Max Date:", max_date))
print(paste("Min Date:", min_date))

# Remove dates after March 2024
nhse_numbers_by_site <- nhse_numbers_filtered %>%
  filter(( !date %in% c('2024-April'))  &
           metric_name %in% c("BBV attending ED (adults)", "BBV ED attendances with blood tests",
                              "Hep B antigen tests performed (BBV)", "Hep C antibody tests performed",
                              "HIV tests performed")) %>%
  mutate(metric_or_proportion_integer = ifelse(is.na(metric_or_proportion_integer), 0, metric_or_proportion_integer),
         site_hospital_name = toupper(site_hospital_name))

# Create tables for each BBV test, selecting relevant columns first
HIV_blood <- nhse_numbers_by_site %>%
  filter(metric_name == "BBV ED attendances with blood tests") %>%
  select(date, site_hospital_name, metric_or_proportion_integer) %>%  # Use hospital names
  group_by(date, site_hospital_name) %>%
  summarize(HIV_blood_tests = sum(metric_or_proportion_integer))

HIV_test <- nhse_numbers_by_site %>%
  filter(metric_name == "HIV tests performed") %>%
  select(date, site_hospital_name, metric_or_proportion_integer) %>%  # Use hospital names
  group_by(date, site_hospital_name) %>%
  summarize(HIV_tests = sum(metric_or_proportion_integer))

HBV_blood <- nhse_numbers_by_site %>%
  filter(metric_name == "BBV ED attendances with blood tests") %>%
  select(date, site_hospital_name, metric_or_proportion_integer) %>%  # Use hospital names
  group_by(date, site_hospital_name) %>%
  summarize(HBV_blood_tests = sum(metric_or_proportion_integer))

HBV_test <- nhse_numbers_by_site %>%
  filter(metric_name == "Hep B antigen tests performed (BBV)") %>%
  select(date, site_hospital_name, metric_or_proportion_integer) %>%  # Use hospital names
  group_by(date, site_hospital_name) %>%
  summarize(HBV_tests = sum(metric_or_proportion_integer))

HCV_blood <- nhse_numbers_by_site %>%
  filter(metric_name == "BBV ED attendances with blood tests") %>%
  select(date, site_hospital_name, metric_or_proportion_integer) %>%  # Use hospital names
  group_by(date, site_hospital_name) %>%
  summarize(HCV_blood_tests = sum(metric_or_proportion_integer))

HCV_test <- nhse_numbers_by_site %>%
  filter(metric_name == "Hep C antibody tests performed") %>%
  select(date, site_hospital_name, metric_or_proportion_integer) %>%  # Use hospital names
  group_by(date, site_hospital_name) %>%
  summarize(HCV_tests = sum(metric_or_proportion_integer))

# Calculate test uptake percentages by site
test_uptake <- HIV_blood %>%
  left_join(HIV_test, by = c("date", "site_hospital_name")) %>%
  left_join(HBV_blood, by = c("date", "site_hospital_name")) %>%
  left_join(HBV_test, by = c("date", "site_hospital_name")) %>%
  left_join(HCV_blood, by = c("date", "site_hospital_name")) %>%
  left_join(HCV_test, by = c("date", "site_hospital_name")) %>%
  mutate(
    percHIV = (HIV_tests / HIV_blood_tests) * 100,
    percHBV = (HBV_tests / HBV_blood_tests) * 100,
    percHCV = (HCV_tests / HCV_blood_tests) * 100
  )

# Pivot into long format for plotting
test_uptake_long <- test_uptake %>%
  pivot_longer(cols = starts_with("perc"), names_to = "Test", values_to = "Uptake") %>%
  mutate(Test = recode(Test, percHIV = "HIV", percHBV = "HBV", percHCV = "HCV"))

# Convert date format
test_uptake_long$date <- as.Date(paste0(test_uptake_long$date, "-01"), format = "%Y-%B-%d")



# Plot
# Adjust the ggplot2 code
p <- ggplot(test_uptake_long, aes(x = date, y = Uptake, color = Test, group = Test)) +
  geom_line(aes(linetype = Test), size = 0.8, alpha = 0.7) +  # Adjusted line size and opacity
  geom_point(size = 1.5, alpha = 0.8) +  # Adjusted point size and opacity
  facet_wrap(~ site_hospital_name, scales = "free_y", ncol = 4) +  # Limit to 4 plots per row
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  labs(
    title = "BBV Test Uptake Over Time by Site (2022-2024)",
    x = "Date",
    y = "Uptake Percentage (%)",
    color = "Test Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Convert to an interactive plot with plotly
ggplotly(p) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))  # Customize hover labels




--- blood tests ---
  
  # Summarize BBV ED attendances with blood tests by site and date
  bbv_ed_attendances <- nhse_numbers_by_site %>%
  filter(metric_name == "BBV ED attendances with blood tests") %>%
  group_by(date, site_hospital_name) %>%
  summarize(total_bbv_ed_attendances = sum(metric_or_proportion_integer, na.rm = TRUE)) %>%
  ungroup()

# Convert date format for bbv_ed_attendances
bbv_ed_attendances$date <- as.Date(paste0(bbv_ed_attendances$date, "-01"), format = "%Y-%B-%d")

# Plot BBV ED attendances across all sites
p_bbv_ed_attendances <- ggplot(bbv_ed_attendances, aes(x = date, y = total_bbv_ed_attendances, color = site_hospital_name, group = site_hospital_name)) +
  geom_line(size = 0.8, alpha = 0.7) +  # Adjusted line size and opacity for BBV ED attendances plot
  geom_point(size = 1.5, alpha = 0.8) +  # Adjusted point size and opacity
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma_format()) +  # Use comma format for y-axis labels
  labs(
    title = "BBV ED Attendances with Blood Tests Over Time by Site (2022-2024)",
    x = "Date",
    y = "Total BBV ED Attendances with Blood Tests",
    color = "Hospital Site"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Convert to an interactive plot with plotly
ggplotly(p_bbv_ed_attendances) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))  # Customize hover labels

--- test uptake ---
  
  # Summarize data by site and date for BBV attending ED (adults)
  bbv_attending_ed <- nhse_numbers_filtered %>%
  filter(metric_name == "BBV attending ED (adults)") %>%
  group_by(date, site_hospital_name) %>%
  summarize(total_bbv_attending_ed = sum(metric_or_proportion_integer, na.rm = TRUE)) %>%
  ungroup()

# Summarize data by site and date for BBV ED attendances with blood tests
bbv_ed_with_blood_tests <- nhse_numbers_filtered %>%
  filter(metric_name == "BBV ED attendances with blood tests") %>%
  group_by(date, site_hospital_name) %>%
  summarize(total_bbv_ed_with_blood_tests = sum(metric_or_proportion_integer, na.rm = TRUE)) %>%
  ungroup()

# Merge the two datasets to calculate the percentage uptake
bbv_uptake <- bbv_attending_ed %>%
  left_join(bbv_ed_with_blood_tests, by = c("date", "site_hospital_name")) %>%
  mutate(
    blood_test_uptake_percentage = (total_bbv_ed_with_blood_tests / total_bbv_attending_ed) * 100
  )

# Convert date format for bbv_uptake
bbv_uptake$date <- as.Date(paste0(bbv_uptake$date, "-01"), format = "%Y-%B-%d")

# Create a variable to identify missing data
bbv_uptake <- bbv_uptake %>%
  mutate(is_missing = is.na(blood_test_uptake_percentage))

# Create a dataset for observed data
observed_data <- bbv_uptake %>%
  filter(!is_missing)

# Create a dataset for missing data
missing_data <- bbv_uptake %>%
  filter(is_missing)

# Plot the blood test uptake percentage across all sites
p_blood_test_uptake <- ggplot() +
  # Plot the observed data (solid lines)
  geom_line(data = observed_data, aes(x = date, y = blood_test_uptake_percentage, color = site_hospital_name, group = site_hospital_name), 
            size = 0.8, alpha = 0.7) +
  geom_point(data = observed_data, aes(x = date, y = blood_test_uptake_percentage, color = site_hospital_name), 
             size = 1.5, alpha = 0.8) +
  # Plot the missing data (grey dashed lines)
  geom_line(data = missing_data, aes(x = date, y = blood_test_uptake_percentage, group = site_hospital_name),
            size = 0.8, linetype = "dashed", color = "grey50", na.rm = TRUE) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Use percentage format for y-axis labels
  
  labs(
    title = "BBV ED Attendances with Blood Tests Uptake Percentage Over Time by Site (2022-2024)",
    x = "Date",
    y = "Blood Test Uptake Percentage (%)",
    color = "Hospital Site"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Convert to an interactive plot with plotly
ggplotly(p_blood_test_uptake) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))  # Customize hover labels

# Save the interactive plot as an HTML file
htmlwidgets::saveWidget(ggplotly(p_blood_test_uptake), "blood_test_uptake_plot.html")


--- updated visualization with automated criteria ---
  
library(ggplot2)
library(plotly)
library(dplyr)

# Add a new column for automated testing status
bbv_uptake <- bbv_uptake %>%
  mutate(automated_testing = ifelse(site_hospital_name %in% c(
    'CHARING CROSS HOSPITAL',
    'MANCHESTER ROYAL INFIRMARY',
    'NORTH MANCHESTER GENERAL HOSPITAL',
    'QUEEN ELIZABETH HOSPITAL',
    'ST THOMAS\' HOSPITAL',
    'UNIVERSITY HOSPITAL LEWISHAM',
    'WEST MIDDLESEX UNIVERSITY HOSPITAL',
    'WYTHENSHAWE HOSPITAL',
    'ST MARY\'S HOSPITAL'
  ), "Yes", "No"))

# Identify missing data points and add dashed lines for gaps
bbv_uptake <- bbv_uptake %>%
  group_by(site_hospital_name) %>%
  arrange(date) %>%
  mutate(is_missing = is.na(blood_test_uptake_percentage))  # Flag missing data

# Create the ggplot object with separate layers for data and missing data (dashed grey lines)
p_blood_test_uptake_split <- ggplot(bbv_uptake, aes(x = date, y = blood_test_uptake_percentage, 
                                                    color = site_hospital_name, group = site_hospital_name)) +
  # Dashed line for missing data
  geom_line(data = bbv_uptake %>% filter(is_missing), aes(x = date, y = blood_test_uptake_percentage), 
            color = "grey", linetype = "dashed", size = 0.8, alpha = 0.6) +
  # Solid lines for observed data
  geom_line(data = bbv_uptake %>% filter(!is_missing), aes(x = date, y = blood_test_uptake_percentage),
            size = 0.8, alpha = 0.7) +
  geom_point(size = 1.5, alpha = 0.8) +  # Adjusted point size and opacity
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Use percentage format for y-axis labels
  labs(
    title = "Blood Tests Uptake over Time with Automation differences",
    x = "Date",
    y = "Blood Test Uptake Percentage (%)",
    color = "Hospital Site"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 10)),  # Add margin to move the labels down
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  facet_wrap(~ automated_testing, ncol = 1, scales = "free_y")  # Split by automated testing status

# Convert to an interactive plot with plotly
ggplotly(p_blood_test_uptake_split) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))  # Customize hover labels

# Save the interactive plot as an HTML file
htmlwidgets::saveWidget(ggplotly(p_blood_test_uptake_split), "blood_test_uptake_plot2.html")

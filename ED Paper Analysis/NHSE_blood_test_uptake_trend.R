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

# Import location codes
location_codes <- read_xlsx("location_lookup_2024.xlsx", sheet = "location_codes") %>%
  clean_names() %>%
  mutate(location_name = toupper(facility)) %>%
  filter(included == "Yes")  

# Clean location names
location_codes <- location_codes %>%
  mutate(location_name = ifelse(location_name == "FACILITY_A", "FACILITY_A_STANDARD",
                               ifelse(location_name == "FACILITY_B", "FACILITY_B_MAIN",
                                     location_name)))

# Define included locations
included_codes <- c('LOC001', 'LOC002', 'LOC003', 'LOC004', 'LOC005', 'LOC006', 
                   'LOC007', 'LOC008', 'LOC009', 'LOC010', 'LOC011', 'LOC012', 
                   'LOC013', 'LOC014')

included_facilities <- c(
  'FACILITY_1', 'FACILITY_2', 'FACILITY_3', 'FACILITY_4', 'FACILITY_5',
  'FACILITY_6', 'FACILITY_7', 'FACILITY_8', 'FACILITY_9', 'FACILITY_10',
  'FACILITY_11', 'FACILITY_12', 'FACILITY_13', 'FACILITY_14'
)

# Filter location codes
location_codes_filtered <- location_codes %>%
  filter(location_code %in% included_codes & location_name %in% included_facilities)

# Import metrics data
metrics_data <- read_csv("metrics_data_2024.csv") %>%
  clean_names() %>%
  mutate(location_name = toupper(trimws(location_name))) %>%
  select(metric_name, location_name, year_of_reporting_period_start, 
         month_of_reporting_period_start, metric_value) 

# Filter metrics for included facilities
metrics_data_filtered <- metrics_data %>%
  filter(location_name %in% included_facilities)

# Create date field
metrics_data_filtered$date <- with(
  metrics_data_filtered, 
  sprintf("%d-%02s", 
          year_of_reporting_period_start,
          month_of_reporting_period_start))

# Check dates
max_date <- max(metrics_data_filtered$date, na.rm = TRUE)
min_date <- min(metrics_data_filtered$date, na.rm = TRUE)

print(paste("Max Date:", max_date))
print(paste("Min Date:", min_date))

# Filter dates and metrics
metrics_by_location <- metrics_data_filtered %>%
  filter((!date %in% c('2024-April')) &
           metric_name %in% c("metric_1_attendance", "metric_2_tests",
                            "metric_3_tests", "metric_4_tests",
                            "metric_5_tests")) %>%
  mutate(metric_value = ifelse(is.na(metric_value), 0, metric_value),
         location_name = toupper(location_name))

# Create metric-specific tables
metric_1_base <- metrics_by_location %>%
  filter(metric_name == "metric_2_tests") %>%
  select(date, location_name, metric_value) %>%
  group_by(date, location_name) %>%
  summarize(metric_1_base_count = sum(metric_value))

metric_1_tests <- metrics_by_location %>%
  filter(metric_name == "metric_5_tests") %>%
  select(date, location_name, metric_value) %>%
  group_by(date, location_name) %>%
  summarize(metric_1_test_count = sum(metric_value))

# Repeat for other metrics...
# [Similar blocks for metrics 2-5]

# Calculate uptake percentages
uptake_metrics <- metric_1_base %>%
  left_join(metric_1_tests, by = c("date", "location_name")) %>%
  # Join other metrics...
  mutate(
    metric_1_perc = (metric_1_test_count / metric_1_base_count) * 100,
    metric_2_perc = (metric_2_test_count / metric_2_base_count) * 100,
    metric_3_perc = (metric_3_test_count / metric_3_base_count) * 100
  )

# Prepare data for visualization
uptake_long <- uptake_metrics %>%
  pivot_longer(cols = starts_with("metric") & ends_with("perc"), 
              names_to = "Metric", 
              values_to = "Uptake") %>%
  mutate(Metric = recode(Metric, 
                        metric_1_perc = "Metric_1", 
                        metric_2_perc = "Metric_2", 
                        metric_3_perc = "Metric_3"))

# Convert date format
uptake_long$date <- as.Date(paste0(uptake_long$date, "-01"), format = "%Y-%B-%d")

# Create visualization
p <- ggplot(uptake_long, aes(x = date, y = Uptake, color = Metric, group = Metric)) +
  geom_line(aes(linetype = Metric), size = 0.8, alpha = 0.7) +
  geom_point(size = 1.5, alpha = 0.8) +
  facet_wrap(~ location_name, scales = "free_y", ncol = 4) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  labs(
    title = "Metric Uptake Over Time by Location (2022-2024)",
    x = "Date",
    y = "Uptake Percentage (%)",
    color = "Metric Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Create interactive plot
ggplotly(p) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))

# Add automation status
uptake_metrics <- uptake_metrics %>%
  mutate(automated_status = ifelse(location_name %in% c(
    'FACILITY_1', 'FACILITY_3', 'FACILITY_5', 'FACILITY_6',
    'FACILITY_8', 'FACILITY_10', 'FACILITY_11', 'FACILITY_13',
    'FACILITY_14'
  ), "Automated", "Manual"))

# Create visualization with automation split
p_split <- ggplot(uptake_metrics, aes(x = date, 
                                     y = metric_1_perc, 
                                     color = location_name, 
                                     group = location_name)) +
  geom_line(size = 0.8, alpha = 0.7) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Metric Uptake by Automation Status",
    x = "Date",
    y = "Uptake Percentage (%)",
    color = "Location"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  facet_wrap(~ automated_status, ncol = 1, scales = "free_y")

# Create interactive split plot
ggplotly(p_split) %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))

# Save interactive plots
htmlwidgets::saveWidget(ggplotly(p), "metric_uptake_plot.html")
htmlwidgets::saveWidget(ggplotly(p_split), "metric_uptake_by_automation_plot.html")
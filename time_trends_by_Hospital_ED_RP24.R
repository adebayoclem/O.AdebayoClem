# Load required packages
if (!require("pacman")) {
  install.packages("pacman")
}

# Load necessary packages
pacman::p_load(
  here,
  janitor,
  lubridate,
  tsibble,
  scales,
  gtsummary,
  arrow,
  readxl,
  DBI,
  odbc,
  purrr,
  tidyverse
)

# Database Connection
analysis_db <- odbc::dbConnect(odbc::odbc(),
  .connection_string = "driver={SQL Server};server=SERVER_NAME;
                       database=ANALYSIS_DB;
                       Encrypt=true;trusted_connection=true",
  timeout = 60,
  timezone = Sys.timezone(),
  timezone_out = Sys.timezone()
)

# Load data from database
raw_data <- DBI::dbGetQuery(conn = analysis_db, statement = "
SELECT [SampleDate],
[Location],
[Metric1],
[Metric2_Primary], 
[Metric2_Secondary],
[Metric2_Tertiary],
[Metric3]
FROM [ANALYSIS_DB].[dbo].[DataTable]
WHERE (SampleDate <= '2024-03-31' and Phase like 'Phase 1')"
)

# Convert date format
raw_data$SampleDate <- as.Date(raw_data$SampleDate)

# Add month-year grouping
testing_data <- raw_data %>% 
  mutate(yearmonth = floor_date(SampleDate, unit = "month")) %>%   
  arrange(yearmonth)

# Calculate metrics by type
# Metric 1 counts
metric1_counts <- testing_data %>% 
  filter(!is.na(Metric1)) %>%
  group_by(yearmonth, Location) %>%
  tally() %>% 
  collect() %>%
  rename(count = n) %>%
  mutate(type = "Type1")

# Metric 2 counts
metric2_counts <- testing_data %>% 
  filter(!is.na(Metric2_Primary) | !is.na(Metric2_Secondary) | !is.na(Metric2_Tertiary)) %>%
  group_by(yearmonth, Location) %>%
  tally() %>% 
  collect() %>%
  rename(count = n) %>%
  mutate(type = "Type2")

# Metric 3 counts
metric3_counts <- testing_data %>% 
  filter(!is.na(Metric3)) %>%
  group_by(yearmonth, Location) %>%
  tally() %>% 
  collect() %>%
  rename(count = n) %>%
  mutate(type = "Type3")

# Define location groups
generate_location_plot <- function(data, locations, title) {
  filtered_data <- data %>% 
    filter(Location %in% locations)
  
  plot <- ggplot(filtered_data, aes(x = yearmonth,
                                   y = count, 
                                   color = Location)) +
    geom_line() +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%B-%Y") +
    labs(title = title,
         x = "Time Period", 
         y = "Count")
  
  return(plot)
}

# Location groups
location_groups <- list(
  group1 = c("LOCATION_A", "LOCATION_B", "LOCATION_C", "LOCATION_D"),
  group2 = c("LOCATION_E", "LOCATION_F", "LOCATION_G", "LOCATION_H", "LOCATION_I"),
  group3 = c("LOCATION_J", "LOCATION_K", "LOCATION_L", "LOCATION_M", "LOCATION_N"),
  group4 = c("LOCATION_O", "LOCATION_P", "LOCATION_Q", "LOCATION_R"),
  group5 = c("LOCATION_S", "LOCATION_T", "LOCATION_U")
)

# Generate plots for each metric and location group
for(metric in list(metric1_counts, metric2_counts, metric3_counts)) {
  metric_name <- unique(metric$type)
  
  for(i in seq_along(location_groups)) {
    plot_title <- sprintf("%s Analysis: Location Group %d", metric_name, i)
    plot <- generate_location_plot(metric, 
                                 location_groups[[i]], 
                                 plot_title)
    
    # Save plot
    filename <- sprintf("output/%s_group%d.png", 
                       tolower(metric_name), 
                       i)
    ggsave(filename, 
           plot = plot, 
           width = 12, 
           height = 6, 
           bg = "white")
  }
}
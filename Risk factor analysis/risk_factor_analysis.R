# Load required packages
if (!require("pacman")) {
  install.packages("pacman") 
}

pacman::p_load(
  rio,
  here,
  skimr,
  janitor,
  lubridate,
  epikit,
  tidyverse,
  flextable,
  scales,
  gtsummary,
  arrow,
  ggplot2,
  rlang,
  DBI,
  odbc,
  purrr,
  rmarkdown,
  gridExtra
)

# Define visualization palette
viz_palette <- c("#123456", "#234567", "#345678")

# Database connection
analysis_db <- odbc::dbConnect(odbc::odbc(),
                             .connection_string = "driver={SQL Server};server=SERVER_NAME;
                             database=ANALYSIS_DB;
                             Encrypt=true;trusted_connection=true",
                             timeout = 300,
                             timezone = Sys.timezone(),
                             timezone_out = Sys.timezone())

# Load source files
source(here("analysis", "plot_metrics.R"))
source(here("reports", "custom_theme.R"))

# Load main dataset
main_data <- read.csv(here("analysis", "main_data.csv"))
main_data_detailed <- read.csv(here("analysis", "detailed_data.csv"))

# View data structure
view(main_data)

# Extract summary row
summary_row <- main_data %>%
  filter(category == "Total")

##### RISK FACTOR ANALYSIS ##### 

# Data loading function
load_participant_data <- function(conn, table_name, end_date) {
  query <- paste0("
    SELECT *
    FROM [ANALYSIS_DB].[dbo].[", table_name, "]
    WHERE (entry_date <= '", end_date, "')"
  )
  data <- DBI::dbGetQuery(conn, statement = query)
  return(data)
}

# Data processing function
process_participant_data <- function(data) {
  data <- data %>% 
    mutate(
      social_index = if_else(is.na(social_index), "Unknown", social_index),
      demographic_1 = if_else(is.na(demographic_1), "Unknown", demographic_1),
      demographic_2 = if_else(
        is.na(demographic_2) | demographic_2 == "Unknown", 
        "Unknown Category", 
        demographic_2
      ),
      total_group = case_when(age >= 0 ~ "All"),
      demographic_1 = replace(demographic_1, demographic_1 == "CAT_A", "Category A"),
      demographic_1 = replace(demographic_1, demographic_1 == "CAT_B", "Category B"),
      age_group = str_replace_all(age_group, "-", " to "),
      age_group = str_replace_all(age_group, "80\\+", "80 and over"),
      age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3"),
      age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group)
    )
  return(data)
}

# Load and process data for each metric
participants_metric1 <- load_participant_data(conn = analysis_db, 
                                           table_name = "participants_metric1", 
                                           end_date = '2024-01-07') %>%
  process_participant_data()

participants_metric2 <- load_participant_data(conn = analysis_db, 
                                           table_name = "participants_metric2", 
                                           end_date = '2024-01-07') %>%
  process_participant_data()

participants_metric3 <- load_participant_data(conn = analysis_db, 
                                           table_name = "participants_metric3", 
                                           end_date = '2024-01-07') %>%
  process_participant_data()

# Filter positive cases
positive_metric2 <- participants_metric2 %>% 
  filter(result_1 == "Positive" | result_2 == "Positive")
positive_metric3 <- participants_metric3 %>% 
  filter(result_3 == "Positive")

# Calculate totals
total_positive_metric2 <- nrow(positive_metric2)
total_positive_metric3 <- nrow(positive_metric3)

# Risk Factor Analysis Functions
calc_risk_stats <- function(total_positive, risk_col) {
  risk_count <- summary_row %>%
    summarise(Total = sum(.data[[risk_col]], na.rm = TRUE))
  
  risk_percentage <- (risk_count$Total / total_positive) * 100
  
  list(
    count = risk_count$Total,
    percentage = risk_percentage
  )
}

# Calculate statistics for Factor 1
factor1_stats_metric2 <- calc_risk_stats(
  total_positive_metric2,
  "count_factor1_positive_metric2"
)

print(paste("Metric 2 - Total positive:", total_positive_metric2))
print(paste("Metric 2 - Factor 1 count:", factor1_stats_metric2$count))
print(paste("Metric 2 - Factor 1 percentage:", factor1_stats_metric2$percentage, "%"))

factor1_stats_metric3 <- calc_risk_stats(
  total_positive_metric3,
  "count_factor1_positive_metric3"
)

print(paste("Metric 3 - Total positive:", total_positive_metric3))
print(paste("Metric 3 - Factor 1 count:", factor1_stats_metric3$count))
print(paste("Metric 3 - Factor 1 percentage:", factor1_stats_metric3$percentage, "%"))

# Visualization Function
create_comparison_plots <- function(data, count_col, prop_col, title_count, title_prop) {
  count_plot <- ggplot(data, aes_string(x = "category", y = count_col, fill = "group")) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_manual(values = viz_palette) +
    labs(title = title_count,
         x = "Category",
         y = "Count",
         fill = "Group") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    )
  
  prop_plot <- ggplot(data, aes_string(x = "category", y = prop_col, fill = "group")) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_manual(values = viz_palette) +
    labs(title = title_prop,
         x = "Category",
         y = "Percentage",
         fill = "Group") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    )
  
  return(list(count_plot = count_plot, prop_plot = prop_plot))
}

# Create visualization data
viz_data <- data.frame(
  category = rep(c("Metric2", "Metric3"), each = 2),
  group = rep(c("Factor1", "Other"), 2),
  count = c(
    factor1_stats_metric2$count,
    total_positive_metric2 - factor1_stats_metric2$count,
    factor1_stats_metric3$count,
    total_positive_metric3 - factor1_stats_metric3$count
  ),
  percentage = c(
    factor1_stats_metric2$percentage,
    100 - factor1_stats_metric2$percentage,
    factor1_stats_metric3$percentage,
    100 - factor1_stats_metric3$percentage
  )
)

# Generate plots
plots <- create_comparison_plots(
  viz_data,
  "count",
  "percentage",
  "Count Comparison by Category",
  "Percentage Comparison by Category"
)

# Display plots
grid.arrange(plots$count_plot, plots$prop_plot, ncol = 2)

# New Analysis Section - Risk Factor Correlations
participants_metric2_new <- participants_metric2 %>% 
  filter(status == "New")
participants_metric3_new <- participants_metric3 %>% 
  filter(status == "New")

# Calculate new cases
total_new_metric2 <- nrow(participants_metric2_new)
total_new_metric3 <- nrow(participants_metric3_new)

# Advanced Correlation Analysis
calc_correlation_stats <- function(new_col, prev_total, metric_col) {
  new_count <- summary_row %>%
    summarise(Total = sum(.data[[new_col]], na.rm = TRUE))
  
  prev_count <- summary_row %>%
    summarise(Total = sum(.data[[metric_col]], na.rm = TRUE)) - new_count
  
  new_prop <- (new_count$Total / total_new_metric2) * 100
  prev_prop <- (prev_count$Total / prev_total) * 100
  
  list(
    new_count = new_count$Total,
    new_proportion = new_prop,
    prev_count = prev_count$Total,
    prev_proportion = prev_prop
  )
}

# Calculate correlations for Factor 1 - Metric 2
factor1_corr_metric2 <- calc_correlation_stats(
  "count_factor1_new_metric2",
  total_positive_metric2 - total_new_metric2,
  "count_factor1_metric2"
)

print(paste("Metric 2 - Factor 1 - New count:", factor1_corr_metric2$new_count))
print(paste("Metric 2 - Factor 1 - New proportion:", factor1_corr_metric2$new_proportion, "%"))
print(paste("Metric 2 - Factor 1 - Previous count:", factor1_corr_metric2$prev_count))
print(paste("Metric 2 - Factor 1 - Previous proportion:", factor1_corr_metric2$prev_proportion, "%"))

# Repeat for other factors and metrics...
[Additional correlation calculations would follow the same pattern]

# Clean data for demographic analysis
main_data_filtered <- main_data %>%
  filter(!(category == "Demographic 1" & value %in% c("Unknown", "Other")),
         !(category == "Demographic 2" & value %in% c("Unknown", "Other")),
         !(category == "Social Index" & value == "Unknown"),
         !(category == "Total" & value == "All"))

# Create demographic visualizations
plot_demographic_factors <- function(data, metric_col, title) {
  ggplot(data, aes(x = reorder(value, .data[[metric_col]]), 
                   y = .data[[metric_col]])) +
    geom_bar(stat = "identity", fill = viz_palette[1]) +
    facet_wrap(~category, scales = "free_y", ncol = 1) +
    coord_flip() +
    labs(
      title = title,
      x = "Demographic Category",
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      strip.text = element_text(size = 10, face = "bold")
    )
}

# Generate demographic plots
plot_demographic_factors(main_data_filtered, 
                        'count_factor1_metric2', 
                        'Factor 1 Distribution by Demographics')
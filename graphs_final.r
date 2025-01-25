############################## DATA VISUALIZATION SCRIPT ##############################

# Load required packages
required_packages <- c(
  "tidyverse",  # Data manipulation and visualization
  "lubridate",  # Date handling
  "readxl",     # Excel file reading
  "DBI",        # Database interface
  "odbc",       # Database connectivity
  "arrow",      # Large dataset handling
  "pacman",     # Package management
  "EpiFunc",    # Analysis functions
  "janitor",    # Data cleaning
  "gtsummary",  # Summary statistics
  "scales",     # Scale formatting
  "forcats",    # Factor manipulation
  "patchwork",  # Plot arrangement
  "ggrepel"     # Label positioning
)

# Install and load packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

############################## SOURCE FILES ##############################

# Load supporting scripts
source("data_processing/main_analysis.R")
source("data_processing/summary_table1.R")
source("data_processing/summary_table2.R")
source("visualization/custom_theme.R")
source("visualization/color_schemes.R")

############################## DATA PREPARATION ##############################

# Standardize column names in primary dataset
standardize_columns <- function(df) {
  names(df) <- names(df) %>%
    gsub(" ", "_", .) %>%
    gsub(":", "", .) %>%
    gsub("[(]", "", .) %>%
    gsub("[)]", "", .) %>%
    tolower()
  return(df)
}

# Primary dataset preparation
primary_dataset <- standardize_columns(raw_data_metric1) %>%
  mutate(
    category = case_when(
      category == "TYPE_A" ~ "Category_A",
      category == "TYPE_B" ~ "Category_B",
      category == "TYPE_C" ~ "Category_C",
      TRUE ~ category
    )
  ) %>%
  filter(
    !category %in% c(
      "Unknown_Category_A",
      "Unknown_Category_B",
      "Unknown_General"
    )
  ) %>%
  mutate(
    metric1_percentage = indicator1_proportion * 100,
    metric2_percentage = indicator2_proportion * 100,
    metric3_percentage = indicator3_proportion * 100,
    metric4_percentage = indicator4_proportion * 100
  )

############################## VISUALIZATION FUNCTIONS ##############################

# Color palettes
visualization_palettes <- list(
  primary_colors = c("#111111", "#222222", "#333333"),
  secondary_colors = c("#444444", "#555555", "#666666"),
  categorical_9 = c(paste0("#", sprintf("%06d", 1:9))),
  categorical_5 = c(paste0("#", sprintf("%06d", 1:5))),
  categorical_3 = c(paste0("#", sprintf("%06d", 1:3)))
)

# Base theme for all visualizations
theme_base <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    )
}

############################## VISUALIZATION GENERATORS ##############################

# 1. Demographic Distribution Visualization
create_demographic_plot <- function(data, group_var, metric_var, title_text) {
  ggplot(
    data %>% filter(breakdown == group_var),
    aes(
      x = location_type,
      y = !!sym(metric_var),
      group = forcats::fct_rev(reorder(category, category))
    )
  ) +
    geom_col(
      aes(fill = category),
      position = "dodge2"
    ) +
    scale_fill_manual(values = visualization_palettes$categorical_9) +
    labs(
      x = "Location Type",
      y = "Percentage",
      title = title_text,
      fill = "Category"
    ) +
    coord_flip() +
    theme_base()
}

# 2. Distribution Comparison Plot
create_comparison_plot <- function(data, split_var, metric_var) {
  data %>%
    filter(breakdown == split_var) %>%
    mutate(percentage = round(!!sym(metric_var), digits = 0)) %>%
    arrange(-percentage) %>%
    ggplot(aes(x = "", y = percentage, fill = category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_label_repel(
      aes(label = paste0(percentage, "%")),
      size = 4.5,
      nudge_x = 1,
      show.legend = FALSE
    ) +
    facet_wrap(~location_type, ncol = 2) +
    scale_fill_manual(values = visualization_palettes$categorical_9) +
    labs(
      title = paste("Distribution by", split_var),
      fill = "Category"
    ) +
    theme_base() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

# 3. Metric Analysis Visualization
create_metric_plot <- function(data, metric_type) {
  ggplot(
    data %>% filter(type == metric_type),
    aes(x = category, y = value, fill = group)
  ) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_manual(values = visualization_palettes$categorical_3) +
    labs(
      x = "Category",
      y = "Value (%)",
      title = paste("Analysis of", metric_type),
      fill = "Group"
    ) +
    coord_flip() +
    theme_base()
}

############################## GENERATE VISUALIZATIONS ##############################

# 1. Category Distribution Plots
category_plots <- list(
  demographic = create_demographic_plot(
    primary_dataset,
    "demographic_group",
    "metric1_percentage",
    "Demographic Distribution by Location"
  ),
  
  social = create_demographic_plot(
    primary_dataset,
    "social_group",
    "metric2_percentage",
    "Social Category Distribution by Location"
  ),
  
  age = create_demographic_plot(
    primary_dataset,
    "age_group",
    "metric3_percentage",
    "Age Distribution by Location"
  )
)

# 2. Comparison Plots
comparison_plots <- list(
  location_comparison = create_comparison_plot(
    primary_dataset,
    "location_type",
    "metric1_percentage"
  ),
  
  group_comparison = create_comparison_plot(
    primary_dataset,
    "group_type",
    "metric2_percentage"
  )
)

# 3. Metric Analysis Plots
metric_plots <- list(
  metric1 = create_metric_plot(
    primary_dataset,
    "metric_type_1"
  ),
  
  metric2 = create_metric_plot(
    primary_dataset,
    "metric_type_2"
  ),
  
  metric3 = create_metric_plot(
    primary_dataset,
    "metric_type_3"
  )
)

############################## SAVE VISUALIZATIONS ##############################

# Function to save visualizations
save_visualization <- function(plot, filename, width = 960, height = 640, dpi = 72) {
  ggsave(
    paste0("output/visualizations/", filename, ".svg"),
    plot = plot,
    width = width,
    height = height,
    units = "px",
    dpi = dpi
  )
}

# Save all plots
mapply(
  save_visualization,
  c(category_plots, comparison_plots, metric_plots),
  c(
    "demographic_distribution",
    "social_distribution",
    "age_distribution",
    "location_comparison",
    "group_comparison",
    "metric1_analysis",
    "metric2_analysis",
    "metric3_analysis"
  )
)

############################## OUTPUT COMBINED VISUALIZATIONS ##############################

# Combine related plots
combined_distributions <- wrap_plots(
  category_plots,
  ncol = 2,
  guides = "collect"
) +
  plot_annotation(
    title = "Distribution Analysis Overview",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

combined_metrics <- wrap_plots(
  metric_plots,
  ncol = 1,
  guides = "collect"
) +
  plot_annotation(
    title = "Metric Analysis Overview",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

# Save combined plots
save_visualization(combined_distributions, "combined_distributions", height = 960)
save_visualization(combined_metrics, "combined_metrics", height = 1280)

############################## SESSION INFO ##############################

# Save session info for reproducibility
writeLines(capture.output(sessionInfo()), "output/session_info.txt")
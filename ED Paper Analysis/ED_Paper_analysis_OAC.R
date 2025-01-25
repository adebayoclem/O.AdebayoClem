---
  title: "Logistic Regression Test Uptake"
author: "Olaide Adebayo-Clement"
date: "2024-05-20"
output: word_document
---
  
  ```{r setup, include=FALSE}

#rm(list = ls(all.names = TRUE))

# This chunk sets up the R environment for the entire document.
# It includes the necessary libraries for data manipulation, visualization, and analysis.
# Set global chunk options
knitr::opts_chunk$set(
  echo = FALSE,        # Hide code in output
  error = FALSE,       # Show errors but don't stop rendering
  warning = FALSE,     # Hide warnings in output
  message = FALSE,     # Hide messages in output
  fig.width = 16.00,   # Set figure width
  fig.height = 13.00,   # Set figure height (calculated based on aspect ratio)
  fig.align = 'center',# Center align figures
  fig.topcaption = TRUE, # Place figure captions at the top
  out.width = '100%',  # Ensure figures use full width
  out.height = 'auto'  # Ensure figures maintain aspect ratio
)

# Install and load packages using pacman
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse,  # A collection of R packages for data science.
  readxl,     # For reading Excel files.
  DBI,        # Database interface for R.
  dplyr,      # A grammar of data manipulation.
  odbc,       # Connect to ODBC databases.
  janitor,    # For cleaning data.
  gtsummary,  # Create presentation-ready summary tables.
  scales,     # Tools for improving base R graphics.
  gt,         # Create table outputs.
  purrr,      # Functional programming tools.
  broom,      # Convert statistical analysis objects into tidy tibbles.
  here,       # For file referencing.
  readr,      # Reading rectangular data (like csv).
  car,        # For variance inflation factor (VIF) calculation.
  MASS,       # Functions and datasets to support Venables and Ripley's MASS.
  mice,        # For multiple imputation of missing data.
  forestplot,    
  stringr,
  tidyr,
  ggplot2,
  grid,
  gridExtra,
  ggpubr,
  stats,
  parallel,
  data.table,
  forcats,
  conflicted
  
)

```


```{r database-connection-and-load-data}
# Connect to the database using ODBC.
# This section establishes a connection to the database using the provided connection string.
Y006 <- odbc::dbConnect(
  odbc::odbc(),
  .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;database=Y006_BBV_PID;Encrypt=true;trusted_connection=true",
  timeout = 60,
  timezone = Sys.timezone(),
  timezone_out = Sys.timezone()
)

```



```{r data-cleaning-and-preparation}


# Define a function to load attendees data from the database for a specific disease
load_attendees_data <- function(conn, disease) {
  query <- sprintf(
    "SELECT *, Site AS site_code
    FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsites%s_OAC]
    WHERE (arrdate <= '2024-01-07')",
    disease
  )
  dbGetQuery(conn, query)
}

# Clean and prepare each dataset separately
clean_data <- function(data, site_col) {
  data %>%
    mutate(
      IMD = if_else(is.na(IMD), "Unknown IMD", as.character(IMD)),
      Gender = if_else(is.na(Sex), "Unknown Gender", Sex),
      ethnic_group = if_else(is.na(ethnic_group) | ethnic_group == "Unknown", "Unknown Ethnicity", ethnic_group),
      age_group = case_when(
        age < 25 ~ "16-24",
        age >= 25 & age < 35 ~ "25-34",
        age >= 35 & age < 50 ~ "35-49",
        age >= 50 & age < 65 ~ "50-64",
        age >= 65 & age < 80 ~ "65-79",
        age >= 80 ~ "80+",
        TRUE ~ NA_character_
      ),
      Site = as.character(!!sym(site_col))
    ) %>%
    filter(
      IMD != "Unknown IMD",
      Gender != "Unknown Gender",
      ethnic_group != "Unknown Ethnicity",
      !is.na(age_group)
    ) %>%
    mutate(
      Gender = case_when(
        Gender == "MALE" ~ "Men",
        Gender == "FEMALE" ~ "Women",
        TRUE ~ Gender
      ),
      age_group = str_replace_all(age_group, "-", " to "),
      age_group = str_replace_all(age_group, "80\\+", "80 and over"),
      age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3")
    )
}

# Load and clean data for HIV, HCV, and HBV attendees
attendees_HIV <- clean_data(load_attendees_data(Y006, "HIV"), "Site")
attendees_HCV <- clean_data(load_attendees_data(Y006, "HCV"), "SITE")
attendees_HBV <- clean_data(load_attendees_data(Y006, "HBV"), "Site")

```



```{r line-list-equity-of-uptake }

# Create separate line lists for HIV, HCV, and HBV

# HIV line list
hiv_line_list <- attendees_HIV %>%
  mutate(
    HIV_test = case_when(
      ECDS_bloods_any == "Yes" & HIV == "Yes" ~ 1,
      ECDS_bloods_any == "Yes" & (is.na(HIV) | HIV != "Yes") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(HIV_test)) %>%
  dplyr::select(
    TOKEN_PERSON_ID, HIV_test, 
    age_group, Gender, ethnic_group, IMD, Site
  )

# HCV line list
hcv_line_list <- attendees_HCV %>%
  mutate(
    HCV_test = case_when(
      ECDS_bloods_any == "Yes" & HCV == "Yes" ~ 1,
      ECDS_bloods_any == "Yes" & (is.na(HCV) | HCV != "Yes") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(HCV_test)) %>%
  dplyr::select(
    TOKEN_PERSON_ID, HCV_test, 
    age_group, Gender, ethnic_group, IMD, Site
  )

# HBV line list
hbv_line_list <- attendees_HBV %>%
  mutate(
    HBV_test = case_when(
      ECDS_bloods_any == "Yes" & HBV == "Yes" ~ 1,
      ECDS_bloods_any == "Yes" & (is.na(HBV) | HBV != "Yes") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(HBV_test)) %>%
  dplyr::select(
    TOKEN_PERSON_ID, HBV_test, 
    age_group, Gender, ethnic_group, IMD, Site
  )

# Display the first few rows of each line list
print("HIV Line List:")
print(head(hiv_line_list))
print("HCV Line List:")
print(head(hcv_line_list))
print("HBV Line List:")
print(head(hbv_line_list))

# Save the line lists to CSV files
write.csv(hiv_line_list, "hiv_test_uptake_line_list.csv", row.names = FALSE)
write.csv(hcv_line_list, "hcv_test_uptake_line_list.csv", row.names = FALSE)
write.csv(hbv_line_list, "hbv_test_uptake_line_list.csv", row.names = FALSE)


```



```{r logistic-regression1}

# Handle package conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

# Load the data for each BBV (only the first 50000 rows)
hiv_data <- fread("hiv_test_uptake_line_list.csv")[1:50000, ]
hcv_data <- fread("hcv_test_uptake_line_list.csv")[1:50000, ]
hbv_data <- fread("hbv_test_uptake_line_list.csv")[1:50000, ]

# Ensure the largest group is the reference level for categorical variables
reorder_factors <- function(data) {
  data %>%
    mutate(
      age_group = fct_infreq(as.factor(age_group)),
      Gender = fct_infreq(as.factor(Gender)),
      ethnic_group = fct_infreq(as.factor(ethnic_group)),
      Site = fct_infreq(as.factor(Site)),
      IMD = fct_relevel(as.factor(IMD), "1")  # Convert IMD to factor and set level 1 as the reference
    )
}

hiv_data <- reorder_factors(hiv_data)
hcv_data <- reorder_factors(hcv_data)
hbv_data <- reorder_factors(hbv_data)

# Function to perform chi-squared tests (Univariable analysis)
chi_squared_test <- function(df, outcome) {
  predictor_vars <- c("age_group", "Gender", "ethnic_group", "IMD", "Site")
  results <- map_dfr(predictor_vars, function(var) {
    table <- table(df[[var]], df[[outcome]])
    test <- stats::chisq.test(table)
    tibble(
      Variable = var,
      Chi2 = test$statistic,
      p_value_chi = test$p.value
    )
  })
  results %>% arrange(p_value_chi)
}

# Perform chi-squared tests for each dataset
hiv_chi_squared <- chi_squared_test(hiv_data, "HIV_test")
hcv_chi_squared <- chi_squared_test(hcv_data, "HCV_test")
hbv_chi_squared <- chi_squared_test(hbv_data, "HBV_test")


# Define a function to perform logistic regression
logistic_regression <- function(data, outcome, predictors) {
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  model <- glm(formula, data = data, family = binomial)
  tidy_model <- tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tidy_model %>%
    rename(OR = estimate, `2.5%` = conf.low, `97.5%` = conf.high) %>%
    dplyr::select(term, OR, `2.5%`, `97.5%`, p.value)
}

# Define predictors
predictors <- c("age_group", "Gender", "ethnic_group", "IMD", "Site")

# Perform logistic regression in parallel
cl <- makeCluster(detectCores() - 1)

# Export necessary objects and functions to the cluster
clusterExport(cl, c("logistic_regression", "predictors", "hiv_data", "hcv_data", "hbv_data"))

# Load required packages on each cluster node
clusterEvalQ(cl, {
  library(dplyr)
  library(broom)
  library(stats)
})

# Perform logistic regression for each dataset
models <- parLapply(cl, list(
  list(data = hiv_data, outcome = "HIV_test"),
  list(data = hcv_data, outcome = "HCV_test"),
  list(data = hbv_data, outcome = "HBV_test")
), function(x) {
  logistic_regression(x$data, x$outcome, predictors)
})

stopCluster(cl)

# Assign the models to variables
hiv_model <- models[[1]]
hcv_model <- models[[2]]
hbv_model <- models[[3]]

# Summarize logistic regression results
summarize_logistic_results <- function(model) {
  coefficients <- model  # Model is already tidy
  coefficients
}

# Summarize logistic regression results
hiv_summary <- summarize_logistic_results(hiv_model)
hcv_summary <- summarize_logistic_results(hcv_model)
hbv_summary <- summarize_logistic_results(hbv_model)

# Combine logistic regression and chi-squared results
combine_results <- function(logistic_summary, chi_squared_results) {
  combined <- logistic_summary %>%
    left_join(chi_squared_results, by = c("term" = "Variable")) %>%
    mutate(Chi2 = ifelse(is.na(Chi2), "", sprintf("%.2f", Chi2)),
           p_value_chi = ifelse(is.na(p_value_chi), "", sprintf("%.3f", p_value_chi))) %>%
    dplyr::select(term, OR, `2.5%`, `97.5%`, p.value, Chi2, p_value_chi)
  combined
}

hiv_combined <- combine_results(hiv_summary, hiv_chi_squared)
hcv_combined <- combine_results(hcv_summary, hcv_chi_squared)
hbv_combined <- combine_results(hbv_summary, hbv_chi_squared)

# Function to create forest plot with improved visuals and chi-squared results
create_forest_plot <- function(summary_df, title) {
  summary_df <- summary_df %>%
    filter(!grepl("Site|Intercept", term)) %>%
    mutate(term = factor(term, levels = rev(unique(term))))
  
  # Create table text
  table_text <- cbind(
    as.character(summary_df$term),
    sprintf("%.2f", summary_df$OR),
    sprintf("[%.2f, %.2f]", summary_df$`2.5%`, summary_df$`97.5%`),
    sprintf("%.3f", summary_df$p.value),  # Adjust p-value to 3 decimal places
    summary_df$Chi2,
    summary_df$p_value_chi
  )
  
  # Add column headers
  col_headers <- c("Variable", "Odds Ratio", "95% CI", "P value", "Chi2", "P value (Chi2)")
  
  forestplot(
    labeltext = rbind(col_headers, table_text),
    mean = c(NA, summary_df$OR),
    lower = c(NA, summary_df$`2.5%`),
    upper = c(NA, summary_df$`97.5%`),
    title = title,
    xlab = "Odds Ratio (95% CI)",
    txt_gp = fpTxtGp(
      label = gpar(fontsize = 12, fontface = "bold"),  # Increase font size and bold labels
      ticks = gpar(fontsize = 12),
      xlab = gpar(fontsize = 14),
      title = gpar(fontsize = 16),
      cex = 1.2
    ),
    col = fpColors(box = "royalblue", line = "darkblue", summary = "royalblue"),
    xlog = TRUE,  # Change to logarithmic scale
    clip = c(0.25, 2.0),  # Adjust x-axis scale
    xticks = seq(0.25, 2.0, by = 0.25),  # Define x-axis ticks
    graph.pos = 2,
    graphwidth = unit(8, "cm"),
    line.margin = 0.1,
    boxsize = 0.15,
    colgap = unit(8, "mm"),
    zero = 1  # Show intercept as a point of reference
  )
}

# Create forest plots for each BBV with improved visuals and chi-squared results
create_forest_plot(hiv_combined, "HIV Test Uptake - Odds Ratios")
create_forest_plot(hcv_combined, "HCV Test Uptake - Odds Ratios")
create_forest_plot(hbv_combined, "HBV Test Uptake - Odds Ratios")

```



``` {r LR2}

library(tidyverse)
library(broom)

# Updated function to perform chi-squared tests with subgroup breakdown
chi_squared_test <- function(df, outcome) {
  predictor_vars <- c("age_group", "Gender", "ethnic_group", "IMD", "Site")
  
  results <- lapply(predictor_vars, function(var) {
    table <- table(df[[var]], df[[outcome]])
    test <- stats::chisq.test(table)
    
    # Calculate percentages for each subgroup
    percentages <- prop.table(table, margin = 1) * 100
    
    # Combine counts and percentages
    combined <- cbind(table, round(percentages, 1))
    colnames(combined) <- c("No", "Yes", "No_%", "Yes_%")
    
    # Create a data frame with detailed results
    detailed_results <- data.frame(
      Variable = var,
      Subgroup = rownames(combined),
      No = combined[,"No"],
      Yes = combined[,"Yes"],
      No_Percent = combined[,"No_%"],
      Yes_Percent = combined[,"Yes_%"],
      Chi2 = test$statistic,
      p_value_univariable = test$p.value
    )
    
    detailed_results
  })
  
  do.call(rbind, results)
}

logistic_regression <- function(df, outcome) {
  model <- glm(as.formula(paste(outcome, "~ age_group + Gender + ethnic_group + IMD + Site")),
               data = df, family = binomial)
  
  coef_summary <- summary(model)$coefficients
  ci <- confint.default(model)
  
  tidy_model <- data.frame(
    term = rownames(coef_summary),
    OR = exp(coef_summary[, "Estimate"]),
    CI_lower = exp(ci[, 1]),
    CI_upper = exp(ci[, 2]),
    p.value_multivariable = coef_summary[, "Pr(>|z|)"]
  )
  
  tidy_model
}


# Function to reorder factors and set IMD 1 as baseline
reorder_factors <- function(data) {
  data <- data %>%
    mutate(
      age_group = fct_infreq(as.factor(age_group)),
      Gender = fct_infreq(as.factor(Gender)),
      ethnic_group = fct_infreq(as.factor(ethnic_group)),
      Site = fct_infreq(as.factor(Site)),
      IMD = fct_relevel(as.factor(IMD), "1")  # Convert IMD to factor and set level 1 as the reference
    )
  return(data)
}

# Load the dataset

df_hiv <- read_csv("hiv_test_uptake_line_list.csv")

# Ensure the largest group is the reference level for categorical variables
df_hiv <- reorder_factors(df_hiv)

# Perform chi-squared tests with subgroup breakdown
chi_results_hiv <- chi_squared_test(df_hiv, "HIV_test")

chi_results_hiv <- chi_results_hiv %>%
  mutate(across(where(is.numeric), round, 3))

table_grob_hiv_univariable <- tableGrob(chi_results_hiv, rows = NULL)
ggsave("results_table_hiv_univariable.png", plot = table_grob_hiv_univariable, width = 10, height = 14)



# Perform logistic regression
#view(logit_results_hiv)
logit_results_hiv <- logistic_regression(df_hiv, "HIV_test")


# Add reference groups for each subgroup
reference_groups <- data.frame(
  term = c("age_group35 to 49 (reference)", "GenderWomen (reference)", "ethnic_groupWhite British (reference)", "IMD1 (reference)", "SiteRJ611 (reference)"),
  OR = NA, `2.5%` = NA, `97.5%` = NA, p.value_multivariable = NA, Subgroup = NA, No = NA, Yes = NA, No_Percent = NA, Yes_Percent = NA, Chi2 = NA, p_value_univariable = NA
)

combined_results_hiv <- bind_rows(reference_groups, logit_results_hiv)

# Ensure unique factor levels
combined_results_hiv <- combined_results_hiv %>%
  mutate(term = make.unique(as.character(term)))

# Add reference groups only if they are not already present
combined_results_hiv <- bind_rows(reference_groups, combined_results_hiv) %>%
  distinct(term, .keep_all = TRUE)

# Round all numeric columns to 3 decimal places
combined_results_hiv <- combined_results_hiv %>%
  mutate(across(where(is.numeric), round, 3))

# Print and plot results for HIV
print("Results for HIV")
print(combined_results_hiv)

results_table_hiv <- combined_results_hiv %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  dplyr::select(term, OR, CI_lower, CI_upper, p.value_multivariable)

results_table_hiv <- combined_results_hiv %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  dplyr::select(term, OR, CI_lower, CI_upper, p.value_multivariable) %>%
  rename(`2.5%` = CI_lower, `97.5%` = CI_upper)


# Save the results table as a separate figure
table_grob_hiv <- tableGrob(results_table_hiv, rows = NULL)
ggsave("results_table_hiv_multivariable.png", plot = table_grob_hiv, width = 10, height = 14)

# Filter out 'Site' terms for the forest plot
forest_plot_data <- combined_results_hiv %>%
  dplyr::filter(!str_detect(term, "^Site"))

forest_plot_hiv <- ggplot(forest_plot_data, aes(x = OR, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  xlab("Odds Ratio") +
  ylab("Predictor Variables") +
  ggtitle("Forest Plot of HIV Logistic Regression Results") +
  theme_pubr()

# Save the forest plot as a separate figure
ggsave("forest_plot_hiv.png", plot = forest_plot_hiv, width = 10, height = 8)

print("Results table and forest plot have been saved as separate figures.")
----
  
  # Load the dataset
  df_hcv <- read_csv("hcv_test_uptake_line_list.csv")

# Ensure the largest group is the reference level for categorical variables
df_hcv <- reorder_factors(df_hcv)

# Perform chi-squared tests with subgroup breakdown
chi_results_hcv <- chi_squared_test(df_hcv, "HCV_test")

chi_results_hcv <- chi_results_hcv %>%
  mutate(across(where(is.numeric), round, 3))

table_grob_hcv_univariable <- tableGrob(chi_results_hcv, rows = NULL)
ggsave("results_table_hcv_univariable.png", plot = table_grob_hcv_univariable, width = 10, height = 14)

# Perform logistic regression
logit_results_hcv <- logistic_regression(df_hcv, "HCV_test")


# Add reference groups for each subgroup
reference_groups <- data.frame(
  term = c("age_group35 to 49 (reference)", "GenderWomen (reference)", "ethnic_groupWhite British (reference)", "IMD1 (reference)", "SiteR0A07 (reference)"),
  OR = NA, `2.5%` = NA, `97.5%` = NA, p.value_multivariable = NA, Subgroup = NA, No = NA, Yes = NA, No_Percent = NA, Yes_Percent = NA, Chi2 = NA, p_value_univariable = NA
)

combined_results_hcv <- bind_rows(reference_groups, logit_results_hcv)

# Ensure unique factor levels
combined_results_hcv <- combined_results_hcv %>%
  mutate(term = make.unique(as.character(term)))

# Add reference groups only if they are not already present
combined_results_hcv <- bind_rows(reference_groups, combined_results_hcv) %>%
  distinct(term, .keep_all = TRUE)

# Round all numeric columns to 3 decimal places
combined_results_hcv <- combined_results_hcv %>%
  mutate(across(where(is.numeric), round, 3))

# Print and plot results for HBV
print("Results for HCV")
print(combined_results_hcv)

results_table_hcv <- combined_results_hcv %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  dplyr::select(term, OR, CI_lower, CI_upper, p.value_multivariable)

results_table_hcv <- combined_results_hcv %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  dplyr::select(term, OR, CI_lower, CI_upper, p.value_multivariable) %>%
  rename(`2.5%` = CI_lower, `97.5%` = CI_upper)


# Save the results table as a separate figure
table_grob_hcv <- tableGrob(results_table_hcv, rows = NULL)
ggsave("results_table_hcv_multivariable.png", plot = table_grob_hcv, width = 10, height = 14)

# Filter out 'Site' terms for the forest plot
forest_plot_data <- combined_results_hcv %>%
  dplyr::filter(!str_detect(term, "^Site"))

forest_plot_hcv <- ggplot(forest_plot_data, aes(x = OR, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  xlab("Odds Ratio") +
  ylab("Predictor Variables") +
  ggtitle("Forest Plot of HCV Logistic Regression Results") +
  theme_pubr()

# Save the forest plot as a separate figure
ggsave("forest_plot_hcv.png", plot = forest_plot_hcv, width = 10, height = 8)

print("Results table and forest plot have been saved as separate figures.")

---
  
  
  # Load the dataset
  df_hbv <- read_csv("hbv_test_uptake_line_list.csv")

# Ensure the largest group is the reference level for categorical variables
df_hbv <- reorder_factors(df_hbv)

# Perform chi-squared tests with subgroup breakdown
chi_results_hbv <- chi_squared_test(df_hbv, "HBV_test")

chi_results_hbv <- chi_results_hbv %>%
  mutate(across(where(is.numeric), round, 3))

table_grob_hbv_univariable <- tableGrob(chi_results_hbv, rows = NULL)
ggsave("results_table_hbv_univariable.png", plot = table_grob_hbv_univariable, width = 10, height = 14)

# Perform logistic regression
logit_results_hbv <- logistic_regression(df_hbv, "HBV_test")


# Add reference groups for each subgroup
reference_groups <- data.frame(
  term = c("age_group35 to 49 (reference)", "GenderWomen (reference)", "ethnic_groupWhite British (reference)", "IMD1 (reference)", "SiteR1HNH (reference)"),
  OR = NA, `2.5%` = NA, `97.5%` = NA, p.value_multivariable = NA, Subgroup = NA, No = NA, Yes = NA, No_Percent = NA, Yes_Percent = NA, Chi2 = NA, p_value_univariable = NA
)

combined_results_hbv <- bind_rows(reference_groups, logit_results_hbv)

# Ensure unique factor levels
combined_results_hbv <- combined_results_hbv %>%
  mutate(term = make.unique(as.character(term)))

# Add reference groups only if they are not already present
combined_results_hbv <- bind_rows(reference_groups, combined_results_hbv) %>%
  distinct(term, .keep_all = TRUE)

# Round all numeric columns to 3 decimal places
combined_results_hbv <- combined_results_hbv %>%
  mutate(across(where(is.numeric), round, 3))

# Print and plot results for HBV
print("Results for HBV")
print(combined_results_hbv)

results_table_hbv <- combined_results_hbv %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  dplyr::select(term, OR, CI_lower, CI_upper, p.value_multivariable)

results_table_hbv <- combined_results_hbv %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  dplyr::select(term, OR, CI_lower, CI_upper, p.value_multivariable) %>%
  rename(`2.5%` = CI_lower, `97.5%` = CI_upper)


# Save the results table as a separate figure
table_grob_hbv <- tableGrob(results_table_hbv, rows = NULL)
ggsave("results_table_hbv_multivariable.png", plot = table_grob_hbv, width = 10, height = 14)

# Filter out 'Site' terms for the forest plot
forest_plot_data <- combined_results_hbv %>%
  dplyr::filter(!str_detect(term, "^Site"))

forest_plot_hbv <- ggplot(forest_plot_data, aes(x = OR, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  xlab("Odds Ratio") +
  ylab("Predictor Variables") +
  ggtitle("Forest Plot of HBV Logistic Regression Results") +
  theme_pubr()

# Save the forest plot as a separate figure
ggsave("forest_plot_hbv.png", plot = forest_plot_hbv, width = 10, height = 8)

print("Results table and forest plot have been saved as separate figures.")


```




```{r logistic-regression3}

# Load the data for each BBV (only the first 50000 rows)
hiv_data2 <- fread("hiv_test_uptake_line_list.csv")[1:50000, ]
hcv_data2 <- fread("hcv_test_uptake_line_list.csv")[1:50000, ]
hbv_data2 <- fread("hbv_test_uptake_line_list.csv")[1:50000, ]

# Function to reorder factors
reorder_factors <- function(data) {
  data <- data %>%
    mutate(
      age_group = fct_infreq(as.factor(age_group)),
      Gender = fct_infreq(as.factor(Gender)),
      ethnic_group = fct_infreq(as.factor(ethnic_group)),
      Site = fct_infreq(as.factor(Site)),
      IMD = fct_relevel(as.factor(IMD), "1")  # Convert IMD to factor and set level 1 as the reference
    )
  return(data)
}

# Convert categorical variables to factors and reorder
hiv_data2 <- reorder_factors(hiv_data2)
hcv_data2 <- reorder_factors(hcv_data2)
hbv_data2 <- reorder_factors(hbv_data2)

# Function to perform chi-squared tests (Univariable analysis)
chi_squared_test <- function(df, outcome) {
  predictor_vars <- c("age_group", "Gender", "ethnic_group", "IMD", "Site")
  results <- map_dfr(predictor_vars, function(var) {
    table <- table(df[[var]], df[[outcome]])
    test <- stats::chisq.test(table)
    tibble(
      Variable = var,
      Chi2 = test$statistic,
      p_value = test$p.value,
      df = test$parameter
    )
  })
  results %>% arrange(p_value)
}

# Perform chi-squared tests for each outcome variable
chi_squared_results <- list(
  HIV = chi_squared_test(hiv_data2, "HIV_test"),
  HCV = chi_squared_test(hcv_data2, "HCV_test"),
  HBV = chi_squared_test(hbv_data2, "HBV_test")
)

# Print chi-squared results
walk(names(chi_squared_results), ~{
  cat("\nChi-squared test results for", .x, "test:\n")
  print(chi_squared_results[[.x]])
})

# Function to perform logistic regression and create forest plot
logistic_regression_plot <- function(df, outcome) {
  # Univariable analysis
  univariable_models <- c("age_group", "Gender", "ethnic_group", "IMD") %>%
    map(~ glm(as.formula(paste(outcome, "~", .x)), data = df, family = binomial())) %>%
    map(~ tidy(., conf.int = TRUE, exponentiate = TRUE)) %>%
    bind_rows(.id = "variable")
  
  # Multivariable analysis
  multivariable_model <- glm(as.formula(paste(outcome, "~ age_group + Gender + ethnic_group + IMD")),
                             data = df, family = binomial)
  
  multivariable_results <- tidy(multivariable_model, conf.int = TRUE, exponentiate = TRUE)
  
  # Combine results
  all_results <- bind_rows(
    univariable_models %>% mutate(model = "Univariable"),
    multivariable_results %>% mutate(model = "Multivariable")
  ) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      term = str_replace_all(term, c("age_group" = "", "Gender" = "", "ethnic_group" = "", "IMD" = "", "Site" = "")),
      group = case_when(
        str_detect(term, "^[0-9]") ~ "Age Group",
        str_detect(term, "^Women") ~ "Gender",
        str_detect(term, "^R") ~ "Site",
        str_detect(term, "^[2-5]$") ~ "IMD",
        TRUE ~ "Ethnic Group"
      )
    ) %>%
    filter(!str_detect(term, "^R"))  # Remove Site terms from the results
  
  # Create forest plot
  forest_plot <- ggplot(all_results, aes(x = estimate, y = term, color = model)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_log10() +
    facet_grid(group ~ variable, scales = "free_y", space = "free_y") +
    labs(
      x = "Odds Ratio (log scale)",
      y = "",
      title = paste("Forest Plot of", outcome, "Logistic Regression Results"),
      color = "Analysis Type"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.y = element_text(size = 8))
  
  # Create summary table
  summary_table <- all_results %>%
    select(term, estimate, conf.low, conf.high, p.value) %>%
    arrange(term)
  
  # Return both forest plot and summary table
  list(forest_plot = forest_plot, summary_table = summary_table)
}

# Perform logistic regression and create forest plots for each outcome variable
forest_plots <- list(
  HIV = logistic_regression_plot(hiv_data2, "HIV_test"),
  HCV = logistic_regression_plot(hcv_data2, "HCV_test"),
  HBV = logistic_regression_plot(hbv_data2, "HBV_test")
)

# Display forest plots and summary tables
walk(forest_plots, ~{
  grid.arrange(
    arrangeGrob(
      .x$forest_plot + theme(plot.margin = margin(10, 10, 10, 10)),
      nrow = 1
    ),
    arrangeGrob(
      tableGrob(.x$summary_table, theme = ttheme_minimal(), rows = NULL),
      nrow = 1
    ),
    ncol = 2,
    widths = c(2/3, 1/3)
  )
})

```

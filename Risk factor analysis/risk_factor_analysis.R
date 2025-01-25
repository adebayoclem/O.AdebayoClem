# Load required packages
# Ensures the package "pacman" is installed
if (!require("pacman")) {
  install.packages("pacman") }

# install (if necessary) from CRAN and load packages to be used
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways 
  skimr,      # review data
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse,  # data management and visualization
  flextable,  # converting tables to pretty images
  scales,     # define colour schemes for flextables 
  gtsummary,  # summary statistics, tests and regressions
  arrow,      # High-performance data interoperation
  ggplot2,    # Data visualization framework
  rlang,      # Language and environment tools
  DBI,        # Database connection
  odbc,       # connection to SQL database
  purrr,      # Functional programming toolkit
  gtsummary,  # Data summary tables
  rmarkdown,  # Dynamic document generation
  gridExtra,
  ukhsacharts # UKHSA data visualization format
)
# UKHSA colour pallete template
blue_palette <- c("#00A5DF", "#007C91", "#1D57A5")  


# Establish ODBC connection with the database
Y006 <- odbc::dbConnect(odbc::odbc(),
                        .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;
                             database=Y006_BBV_PID;
                             Encrypt=true;trusted_connection=true",
                        timeout = 300,
                        timezone = Sys.timezone(),
                        timezone_out = Sys.timezone())


#here("ed-monitoring", "MasterTables_Table1srf.csv")
source(here("Risk factor analysis", "plot_risk_factors.R"))
source(here("trust reports", "report_functions", "custom_ggplot_theme.R"))
# Read the dataset

master_table <- read.csv(here("Risk factor analysis", "MasterTables_Table1srf_OAC.csv"))
master_table_ED_tests <- read.csv(here("Risk factor analysis", "MasterTables_export_required.csv")) #

view(master_table)

# Extract the totals row
totals_row <- master_table %>%
  filter(breakdown == "Total")

##### SOCIAL RISK FACTORS ##### 

# Function to load data from Sentinel
load_attendees_data <- function(conn, table_name, end_date) {
  query <- paste0("
    SELECT *
    FROM [Y006_BBV_PID].[dbo].[", table_name, "]
    WHERE (arrdate <= '", end_date, "')"
  )
  data <- DBI::dbGetQuery(conn, statement = query)
  return(data)
}

# Function to process attendees data
process_attendees_data <- function(data) {
  data <- data %>% 
    mutate(
      IMD = if_else(is.na(IMD), "Unknown IMD", IMD),
      Gender = if_else(is.na(Sex), "Unknown Gender", Sex),
      ethnic_group = if_else(
        is.na(ethnic_group) | ethnic_group == "Unknown", 
        "Unknown Ethnicity", 
        ethnic_group
      ),
      All = case_when(age >= 0 ~ "All"),
      Gender = replace(Gender, Gender == "FEMALE", "Women"),
      Gender = replace(Gender, Gender == "MALE", "Men"),
      age_group = str_replace_all(age_group, "-", " to "),
      age_group = str_replace_all(age_group, "80\\+", "80 and over"),
      age_group = str_replace_all(age_group, "(\\d+)(to)(\\d+)", "\\1 to \\3"),
      age_group = ifelse(age_group == "16 to 24" & is.na(age_group), "0", age_group)
    )
  return(data)
}

# Load and process HIV attendees data
attendees_HIV_srf <- load_attendees_data(conn = Y006, table_name = "24mthECDSattendees_sentinelsitesHIV", end_date = '2024-01-07')
attendees_HIV_srf <- process_attendees_data(attendees_HIV_srf)

# Load and process HCV attendees data
attendees_HCV_srf <- load_attendees_data(conn = Y006, table_name = "24mthECDSattendees_sentinelsitesHCV", end_date = '2024-01-07')
attendees_HCV_srf <- process_attendees_data(attendees_HCV_srf)

# Load and process HBV attendees data
attendees_HBV_srf <- load_attendees_data(conn = Y006, table_name = "24mthECDSattendees_sentinelsitesHBV", end_date = '2024-01-07')
attendees_HBV_srf <- process_attendees_data(attendees_HBV_srf)

# Load the CSV file
master_table <- read.csv("MasterTables_Table1srf_OAC.csv")

# View the structure and first few rows of the dataset
str(master_table)
head(master_table)

# Define the attendees_characteristics function
attendees_characteristics_srf <- function(df) {
  df_processed <- df %>%
    mutate(
      IMD = if_else(is.na(IMD), "Unknown IMD", as.character(IMD)),
      Gender = if_else(is.na(Sex), "Unknown Gender", as.character(Sex)),
      ethnic_group = if_else(is.na(ethnic_group) | ethnic_group == "Unknown", 
                             "Unknown Ethnicity", as.character(ethnic_group)),
      Gender = case_when(
        Gender == "FEMALE" ~ "Women",
        Gender == "MALE" ~ "Men",
        TRUE ~ Gender
      ),
      age_group = case_when(
        age >= 0 & age <= 24 ~ "16 to 24",
        age >= 25 & age <= 34 ~ "25 to 34",
        age >= 35 & age <= 49 ~ "35 to 49",
        age >= 50 & age <= 64 ~ "50 to 64",
        age >= 65 & age <= 79 ~ "65 to 79",
        age >= 80 ~ "80 and over",
        TRUE ~ "Unknown Age Group"
      )
    ) %>%
    group_by(age_group, Gender, IMD, ethnic_group) %>%
    summarise(Total_Attendees = n(), .groups = 'drop')
  
  return(df_processed)
}



# 1. Homelessness
#Question 1: What is the percentage of ED attendees identified as homeless and how does it compare to the overall ED population?
#Question 2: #Question 2: Are people who are homeless more or less likely to have a BBV test than those who are not?

# Filter for positive cases
attendees_HCV_positive <- attendees_HCV_srf %>% filter(HCVAb == "Positive" | HCVPCR == "Positive")
attendees_HBV_positive <- attendees_HBV_srf %>% filter(HBVAg == "Positive")

# Calculate the total number of positive cases
total_positive_HCV <- nrow(attendees_HCV_positive)
total_positive_HBV <- nrow(attendees_HBV_positive)

# Extract the totals row for the master table
totals_row <- master_table %>% filter(breakdown == "Total")

# Define function to calculate BBV stats
calculate_bbv_stats <- function(total_positive, homeless_col) {
  # Calculate the number of homeless individuals with BBV
  homeless_count <- totals_row %>% summarise(Total = sum(.data[[homeless_col]], na.rm = TRUE))
  
  # Calculate the percentage of homeless individuals with BBV
  homeless_percentage <- (homeless_count$Total / total_positive) * 100
  
  # Calculate the number of non-homeless individuals with BBV
  non_homeless_count <- total_positive - homeless_count$Total
  
  # Calculate the percentage of non-homeless individuals with BBV
  non_homeless_percentage <- 100 - homeless_percentage
  
  list(
    total_positive = total_positive,
    homeless_count = homeless_count$Total,
    homeless_percentage = homeless_percentage,
    non_homeless_count = non_homeless_count,
    non_homeless_percentage = non_homeless_percentage
  )
}

# Calculate stats for HCV
hcv_stats <- calculate_bbv_stats(total_positive_HCV,
                                 "Number.of.patients.who.are.flagged.as.homeless.and.test.positive.for.HCV")

print(paste("HCV - Total Positive Cases:", hcv_stats$total_positive))
print(paste("HCV - Homeless count:", hcv_stats$homeless_count))
print(paste("HCV - Homeless percentage:", hcv_stats$homeless_percentage, "%"))
print(paste("HCV - Non-homeless count:", hcv_stats$non_homeless_count))
print(paste("HCV - Non-homeless percentage:", hcv_stats$non_homeless_percentage, "%"))

# Calculate stats for HBV
hbv_stats <- calculate_bbv_stats(total_positive_HBV,
                                 "Number.of.patients.who.are.flagged.as.homeless.and.test.positive.for.HBV")

print(paste("HBV - Total Positive Cases:", hbv_stats$total_positive))
print(paste("HBV - Homeless count:", hbv_stats$homeless_count))
print(paste("HBV - Homeless percentage:", hbv_stats$homeless_percentage, "%"))
print(paste("HBV - Non-homeless count:", hbv_stats$non_homeless_count))
print(paste("HBV - Non-homeless percentage:", hbv_stats$non_homeless_percentage, "%"))


# Combine the data into a single data frame for visualization
data <- data.frame(
  Condition = rep(c("HCV", "HBV"), each = 2),
  Group = rep(c("Homeless", "Non-homeless"), 2),
  Count = c(hcv_stats$homeless_count, hcv_stats$non_homeless_count, hbv_stats$homeless_count, hbv_stats$non_homeless_count),
  Percentage = c(hcv_stats$homeless_percentage, hcv_stats$non_homeless_percentage, hbv_stats$homeless_percentage, hbv_stats$non_homeless_percentage)
)

# UKHSA color palette
blue_palette <- c("#00A5DF", "#007C91", "#1D57A5")

# Plot counts
count_plot <- ggplot(data, aes(x = Condition, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = blue_palette, labels = c("Homeless", "Non-homeless")) +
  labs(title = "Counts of HCV and HBV Positive Cases",
       x = "Condition",
       y = "Count",
       fill = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

# Plot proportions
proportion_plot <- ggplot(data, aes(x = Condition, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = blue_palette, labels = c("Homeless", "Non-homeless")) +
  labs(title = "Proportions of HCV and HBV Positive Cases",
       x = "BBV",
       y = "Percentage",
       fill = "Risk Factor") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

# Arrange the plots side by side for better visualisation
grid.arrange(count_plot, proportion_plot, ncol = 2)

# Add summary below the graph
cat("Summary:\n")
cat("HCV - Homeless Percentage:", hcv_stats$homeless_percentage, "%\n")
cat("HCV - Non-homeless Percentage:", hcv_stats$non_homeless_percentage, "%\n")
cat("HBV - Homeless Percentage:", hbv_stats$homeless_percentage, "%\n")
cat("HBV - Non-homeless Percentage:", hbv_stats$non_homeless_percentage, "%\n")

#2. People Who Inject Drugs (PWID)
#Question 3: Among those tested for BBVs, what proportion are identified as PWID?

# Define function to calculate PWID statistics for each BBV
calculate_pwid_stats <- function(total_positive, pwid_col) {
  # Calculate the number of PWID ED attendees for the BBV
  pwid_count <- totals_row %>%
    summarise(Total = sum(.data[[pwid_col]], na.rm = TRUE))
  
  # Calculate the proportion of PWID ED attendees for the BBV
  pwid_proportion <- (pwid_count$Total / total_positive) * 100
  
  list(
    pwid_count = pwid_count$Total,
    pwid_proportion = pwid_proportion
  )
}

# Calculate stats for HCV
hcv_pwid_stats <- calculate_pwid_stats(total_positive_HCV, 
                                       "Number.of.patients.who.are.flagged.as.PWID.who.test.positive.for.HCV")

print(paste("HCV - Total positive:", total_positive_HCV))
print(paste("HCV - PWID count:", hcv_pwid_stats$pwid_count))
print(paste("HCV - PWID proportion:", hcv_pwid_stats$pwid_proportion, "%"))

# Calculate stats for HBV
hbv_pwid_stats <- calculate_pwid_stats(total_positive_HBV, 
                                       "Number.of.patients.who.are.flagged.as.PWID.who.test.positive.for.HBV")

print(paste("HBV - Total positive:", total_positive_HBV))
print(paste("HBV - PWID count:", hbv_pwid_stats$pwid_count))
print(paste("HBV - PWID proportion:", hbv_pwid_stats$pwid_proportion, "%"))

# Define a reusable function to create plots
create_plots <- function(data, count_col, proportion_col, title_count, title_proportion, x_label, y_label_count, y_label_proportion) {
  count_plot <- ggplot(data, aes_string(x = "BBV", y = count_col, fill = "Risk Factor")) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_manual(values = blue_palette) +
    labs(title = title_count,
         x = x_label,
         y = y_label_count,
         fill = "Risk Factor") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8))
  
  proportion_plot <- ggplot(data, aes_string(x = "BBV", y = proportion_col, fill = "Risk Factor")) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_manual(values = blue_palette) +
    labs(title = title_proportion,
         x = x_label,
         y = y_label_proportion,
         fill = "Risk Factor") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          axis.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8))
  
  return(list(count_plot = count_plot, proportion_plot = proportion_plot))
}

# Create data frame for visualization
data <- data.frame(
  BBV = c("HCV", "HBV"),
  `Risk Factor` = c("PWID", "PWID"),
  Count = c(hcv_pwid_stats$pwid_count, hbv_pwid_stats$pwid_count),
  Percentage = c(hcv_pwid_stats$pwid_proportion, hbv_pwid_stats$pwid_proportion)
)

# Create plots
plots <- create_plots(data, "Count", "Percentage",
                      "Counts of HCV and HBV Positive Cases Among PWID",
                      "Proportions of HCV and HBV Positive Cases Among PWID",
                      "BBV", "Count", "Percentage")

# Display plots
grid.arrange(plots$count_plot, plots$proportion_plot, ncol = 2)

# Summary of the results
summary_text <- paste(
  "Summary:\n",
  "HCV - Total positive cases: ", total_positive_HCV, "\n",
  "HCV - PWID count: ", hcv_pwid_stats$pwid_count, "\n",
  "HCV - PWID proportion: ", hcv_pwid_stats$pwid_proportion, "%\n\n",
  "HBV - Total positive cases: ", total_positive_HBV, "\n",
  "HBV - PWID count: ", hbv_pwid_stats$pwid_count, "\n",
  "HBV - PWID proportion: ", hbv_pwid_stats$pwid_proportion, "%"
)

cat(summary_text)


#3. People Tested in Prison
#Question 4: What number or percentage of individuals tested in ED have a prison record/history of testing in prison?

# Define function to calculate prison testing statistics
calculate_prison_stats <- function(total_positive, prison_col) {
  # Calculate the number of individuals tested in prison for the BBV
  prison_count <- totals_row %>%
    summarise(Total = sum(.data[[prison_col]], na.rm = TRUE))
  
  # Calculate the proportion of individuals tested in prison for the BBV
  prison_proportion <- (prison_count$Total / total_positive) * 100
  
  list(
    prison_count = prison_count$Total,
    prison_proportion = prison_proportion
  )
}

# Calculate stats for HCV
hcv_prison_stats <- calculate_prison_stats(total_positive_HCV, "Number.of.patients.who.test.positive.for.HCV.who.have.also.tested.in.prison")

print(paste("HCV - Total positive:", total_positive_HCV))
print(paste("HCV - Prison history count:", hcv_prison_stats$prison_count))
print(paste("HCV - Prison history proportion:", hcv_prison_stats$prison_proportion, "%"))

# Calculate stats for HBV
hbv_prison_stats <- calculate_prison_stats(total_positive_HBV, "Number.of.patients.who.test.positive.for.HBV.who.have.also.tested.in.prison")

print(paste("HBV - Total positive:", total_positive_HBV))
print(paste("HBV - Prison history count:", hbv_prison_stats$prison_count))
print(paste("HBV - Prison history proportion:", hbv_prison_stats$prison_proportion, "%"))

#4. People Tested in Drug Service
#Question 5: What proportion of individuals tested for BBVs have a history of drug service testing?

# Define function to calculate drug service testing statistics
calculate_drug_service_stats <- function(total_positive, drug_service_col) {
  # Calculate the number of individuals tested in a drug service for the BBV
  drug_service_count <- totals_row %>%
    summarise(Total = sum(.data[[drug_service_col]], na.rm = TRUE))
  
  # Calculate the proportion of individuals tested in a drug service for the BBV
  drug_service_proportion <- (drug_service_count$Total / total_positive) * 100
  
  list(
    drug_service_count = drug_service_count$Total,
    drug_service_proportion = drug_service_proportion
  )
}

# Calculate stats for HCV
hcv_drug_service_stats <- calculate_drug_service_stats(total_positive_HCV, "Number.of.patients.who.test.positive.for.HCV.who.have.also.tested.in.a.drug.service")

print(paste("HCV - Total positive:", total_positive_HCV))
print(paste("HCV - Drug service history count:", hcv_drug_service_stats$drug_service_count))
print(paste("HCV - Drug service history proportion:", hcv_drug_service_stats$drug_service_proportion, "%"))

# Calculate stats for HBV
hbv_drug_service_stats <- calculate_drug_service_stats(total_positive_HBV, "Number.of.patients.who.test.positive.for.HBV.who.have.also.tested.in.a.drug.service")

print(paste("HBV - Total positive:", total_positive_HBV))
print(paste("HBV - Drug service history count:", hbv_drug_service_stats$drug_service_count))
print(paste("HBV - Drug service history proportion:", hbv_drug_service_stats$drug_service_proportion, "%"))



#5. People with overseas charging status

#Question 6: What proportion of ED attendees have overseas charging status recorded?
#Are people with overseas charging status any more or less likely to have a BBV test when they attend ED?

# Define function to calculate overseas charging status statistics
calculate_overseas_stats <- function(total_positive, overseas_col) {
  # Calculate the number of individuals with overseas charging status for the BBV
  overseas_count <- totals_row %>%
    summarise(Total = sum(.data[[overseas_col]], na.rm = TRUE))
  
  # Calculate the proportion of individuals with overseas charging status for the BBV
  overseas_proportion <- (overseas_count$Total / total_positive) * 100
  
  list(
    overseas_count = overseas_count$Total,
    overseas_proportion = overseas_proportion
  )
}

# Calculate stats for HCV
hcv_overseas_stats <- calculate_overseas_stats(total_positive_HCV, "Number.of.patients.who.test.positive.for.HCV.who.also.have.overseas.charging.status.recorded")

print(paste("HCV - Total positive:", total_positive_HCV))
print(paste("HCV - Overseas charging status count:", hcv_overseas_stats$overseas_count))
print(paste("HCV - Overseas charging status proportion:", hcv_overseas_stats$overseas_proportion, "%"))

# Calculate stats for HBV
hbv_overseas_stats <- calculate_overseas_stats(total_positive_HBV, "Number.of.patients.who.test.positive.for.HBV.who.also.have.overseas.charging.status.recorded")

print(paste("HBV - Total positive:", total_positive_HBV))
print(paste("HBV - Overseas charging status count:", hbv_overseas_stats$overseas_count))
print(paste("HBV - Overseas charging status proportion:", hbv_overseas_stats$overseas_proportion, "%"))


#Question 7: Are PWID more or less likely to have been tested in the ED compared to other locations like drug services or prisons?

# Calculate the number of PWID tested in the ED for HCV
pwid_tested_HCV_ED <- totals_row$Number.of.patients.who.are.flagged.as.PWID.who.test.positive.for.HCV

# Calculate the number of PWID tested in the ED for HBV
pwid_tested_HBV_ED <- totals_row$Number.of.patients.who.are.flagged.as.PWID.who.test.positive.for.HBV

# Calculate the number of PWID tested in drug services for HCV
pwid_tested_HCV_drug_service <- totals_row$Number.of.patients.who.test.positive.for.HCV.who.have.also.tested.in.a.drug.service

# Calculate the number of PWID tested in drug services for HBV
pwid_tested_HBV_drug_service <- totals_row$Number.of.patients.who.test.positive.for.HBV.who.have.also.tested.in.a.drug.service

# Calculate the number of PWID tested in prisons for HCV
pwid_tested_HCV_prison <- totals_row$Number.of.patients.who.test.positive.for.HCV.who.have.also.tested.in.prison

# Calculate the number of PWID tested in prisons for HBV
pwid_tested_HBV_prison <- totals_row$Number.of.patients.who.test.positive.for.HBV.who.have.also.tested.in.prison

#Compare the Proportions of PWID Tested in Each Location
# Calculate the total number of positive cases
total_positive_HCV <- nrow(attendees_HCV_positive)
total_positive_HBV <- nrow(attendees_HBV_positive)

# Function to calculate proportions
calculate_proportion <- function(total_positive, location_count) {
  # 'total_positive' is the total number of positive cases for a specific BBV (HCV or HBV).
  # 'location_count' is the number of individuals who tested positive in a specific location (e.g., ED, drug services, prisons).
  
  # Calculate the proportion of positive cases in the specified location.
  # The proportion is calculated as (location_count / total_positive) * 100 to convert it into a percentage.
  proportion <- (location_count / total_positive) * 100
  
  # Return the calculated proportion.
  return(proportion)
}

# Calculate proportions for HCV
proportion_HCV_ED <- calculate_proportion(total_positive_HCV, pwid_tested_HCV_ED)
proportion_HCV_drug_service <- calculate_proportion(total_positive_HCV, pwid_tested_HCV_drug_service)
proportion_HCV_prison <- calculate_proportion(total_positive_HCV, pwid_tested_HCV_prison)

# Print the results for HCV
print(paste("HCV - Proportion of PWID tested in ED:", proportion_HCV_ED, "%"))
print(paste("HCV - Proportion of PWID tested in drug services:", proportion_HCV_drug_service, "%"))
print(paste("HCV - Proportion of PWID tested in prisons:", proportion_HCV_prison, "%"))

# Calculate proportions for HBV
proportion_HBV_ED <- calculate_proportion(total_positive_HBV, pwid_tested_HBV_ED)
proportion_HBV_drug_service <- calculate_proportion(total_positive_HBV, pwid_tested_HBV_drug_service)
proportion_HBV_prison <- calculate_proportion(total_positive_HBV, pwid_tested_HBV_prison)

# Print the results for HBV
print(paste("HBV - Proportion of PWID tested in ED:", proportion_HBV_ED, "%"))
print(paste("HBV - Proportion of PWID tested in drug services:", proportion_HBV_drug_service, "%"))
print(paste("HBV - Proportion of PWID tested in prisons:", proportion_HBV_prison, "%"))

#Question 8: Do previously tested homeless individuals have a higher or lower rate of repeat BBV testing in the ED?

# Calculate the total number of positive cases
attendees_HCV_positive <- attendees_HCV_srf %>% filter(HCVAb == "Positive" | HCVPCR == "Positive")
attendees_HBV_positive <- attendees_HBV_srf %>% filter(HBVAg == "Positive")

# Total positive cases
total_positive_HCV <- nrow(attendees_HCV_positive)
total_positive_HBV <- nrow(attendees_HBV_positive)

# Filter the datasets for new diagnoses
attendees_HBV_srf_new <- attendees_HBV_srf %>% filter(HBsAg_positive_new == "New")
attendees_HCV_srf_new <- attendees_HCV_srf %>% filter(rna_positive_new == "New")

# Calculate the total new diagnoses
total_new_diagnoses_HCV <- nrow(attendees_HCV_srf_new)
total_new_diagnoses_HBV <- nrow(attendees_HBV_srf_new)

# Extract the totals row for the master table
totals_row <- master_table %>% filter(breakdown == "Total")

# Number of homeless individuals tested positive for HCV and HBV
total_homeless_tested_HCV <- totals_row$Number.of.patients.who.are.flagged.as.homeless.and.test.positive.for.HCV
total_homeless_tested_HBV <- totals_row$Number.of.patients.who.are.flagged.as.homeless.and.test.positive.for.HBV

# Number of homeless individuals newly diagnosed for HCV and HBV
new_homeless_tested_HCV <- totals_row$Number.of.patients.who.are.flagged.as.homeless.and.newly.diagnosed.with.HCV
new_homeless_tested_HBV <- totals_row$Number.of.patients.who.are.flagged.as.homeless.and.newly.diagnosed.with.HBV

# Calculate previously tested homeless individuals
previously_tested_homeless_HCV <- total_homeless_tested_HCV - new_homeless_tested_HCV
previously_tested_homeless_HBV <- total_homeless_tested_HBV - new_homeless_tested_HBV

# Calculate repeat testing rates among homeless individuals for HCV and HBV
repeat_testing_rate_homeless_HCV <- (previously_tested_homeless_HCV / total_homeless_tested_HCV) * 100
repeat_testing_rate_homeless_HBV <- (previously_tested_homeless_HBV / total_homeless_tested_HBV) * 100

# Calculate overall previously tested individuals for HCV and HBV
overall_previously_tested_HCV <- total_positive_HCV - total_new_diagnoses_HCV
overall_previously_tested_HBV <- total_positive_HBV - total_new_diagnoses_HBV

# Calculate repeat testing rates among the overall population for HCV and HBV
repeat_testing_rate_overall_HCV <- (overall_previously_tested_HCV / total_positive_HCV) * 100
repeat_testing_rate_overall_HBV <- (overall_previously_tested_HBV / total_positive_HBV) * 100

# Print results
print(paste("HCV - Repeat testing rate among homeless individuals:", repeat_testing_rate_homeless_HCV, "%"))
print(paste("HCV - Repeat testing rate among overall population:", repeat_testing_rate_overall_HCV, "%"))

print(paste("HBV - Repeat testing rate among homeless individuals:", repeat_testing_rate_homeless_HBV, "%"))
print(paste("HBV - Repeat testing rate among overall population:", repeat_testing_rate_overall_HBV, "%"))

#Question 8: What is the difference in the comparison of proportion of risk factors among newly diagnose vs previously diagnosed

# Calculate the total number of positive cases for HCV and HBV
attendees_HCV_positive <- attendees_HCV_srf %>% filter(HCVAb == "Positive" | HCVPCR == "Positive")
attendees_HBV_positive <- attendees_HBV_srf %>% filter(HBVAg == "Positive")

# Total positive cases
total_positive_HCV <- nrow(attendees_HCV_positive)
total_positive_HBV <- nrow(attendees_HBV_positive)

# Filter the datasets for new diagnoses
attendees_HBV_srf_new <- attendees_HBV_srf %>% filter(HBsAg_positive_new == "New")
attendees_HCV_srf_new <- attendees_HCV_srf %>% filter(rna_positive_new == "New")

# Calculate the total new diagnoses
total_new_diagnoses_HCV <- nrow(attendees_HCV_srf_new)
total_new_diagnoses_HBV <- nrow(attendees_HBV_srf_new)

# Calculate the total previously diagnosed cases
total_previously_diagnosed_HCV <- total_positive_HCV - total_new_diagnoses_HCV
total_previously_diagnosed_HBV <- total_positive_HBV - total_new_diagnoses_HBV

# Extract the totals row for the master table
totals_row <- master_table %>% filter(breakdown == "Total")

# Define function to calculate risk factor statistics
calculate_risk_factor_stats <- function(new_diag_col, prev_diag_total, risk_factor_col) {
  # Calculate the number of individuals with the risk factor among the newly diagnosed
  new_risk_factor_count <- totals_row %>%
    summarise(Total = sum(.data[[new_diag_col]], na.rm = TRUE))
  
  # Calculate the number of individuals with the risk factor among the previously diagnosed
  prev_risk_factor_count <- totals_row %>%
    summarise(Total = sum(.data[[risk_factor_col]], na.rm = TRUE)) - new_risk_factor_count
  
  # Calculate the proportion of individuals with the risk factor among the newly diagnosed
  new_risk_factor_proportion <- (new_risk_factor_count$Total / total_new_diagnoses_HCV) * 100
  
  # Calculate the proportion of individuals with the risk factor among the previously diagnosed
  prev_risk_factor_proportion <- (prev_risk_factor_count$Total / prev_diag_total) * 100
  
  list(
    new_risk_factor_count = new_risk_factor_count$Total,
    new_risk_factor_proportion = new_risk_factor_proportion,
    prev_risk_factor_count = prev_risk_factor_count$Total,
    prev_risk_factor_proportion = prev_risk_factor_proportion
  )
}

# Calculate risk factor stats for HCV (PWID)
hcv_pwid_stats <- calculate_risk_factor_stats(
  "Number.of.patients.who.are.flagged.as.PWID.and.newly.diagnosed.with.HCV",
  total_previously_diagnosed_HCV,
  "Number.of.patients.who.are.flagged.as.PWID.who.test.positive.for.HCV"
)

print(paste("HCV - PWID - Newly diagnosed count:", hcv_pwid_stats$new_risk_factor_count))
print(paste("HCV - PWID - Newly diagnosed proportion:", hcv_pwid_stats$new_risk_factor_proportion, "%"))
print(paste("HCV - PWID - Previously diagnosed count:", hcv_pwid_stats$prev_risk_factor_count))
print(paste("HCV - PWID - Previously diagnosed proportion:", hcv_pwid_stats$prev_risk_factor_proportion, "%"))

# Calculate risk factor stats for HCV (prison testing history)
hcv_prison_stats <- calculate_risk_factor_stats(
  "Number.of.patients.newly.diagnosed.with.HCV.who.have.also.tested.in.a.prison",
  total_previously_diagnosed_HCV,
  "Number.of.patients.who.test.positive.for.HCV.who.have.also.tested.in.prison"
)

print(paste("HCV - Prison - Newly diagnosed count:", hcv_prison_stats$new_risk_factor_count))
print(paste("HCV - Prison - Newly diagnosed proportion:", hcv_prison_stats$new_risk_factor_proportion, "%"))
print(paste("HCV - Prison - Previously diagnosed count:", hcv_prison_stats$prev_risk_factor_count))
print(paste("HCV - Prison - Previously diagnosed proportion:", hcv_prison_stats$prev_risk_factor_proportion, "%"))

# Calculate risk factor stats for HCV (drug service testing history)
hcv_drug_service_stats <- calculate_risk_factor_stats(
  "Number.of.patients.newly.diagnosed.with.HCV.who.have.also.tested.in.a.drug.service",
  total_previously_diagnosed_HCV,
  "Number.of.patients.who.test.positive.for.HCV.who.have.also.tested.in.a.drug.service"
)

print(paste("HCV - Drug Service - Newly diagnosed count:", hcv_drug_service_stats$new_risk_factor_count))
print(paste("HCV - Drug Service - Newly diagnosed proportion:", hcv_drug_service_stats$new_risk_factor_proportion, "%"))
print(paste("HCV - Drug Service - Previously diagnosed count:", hcv_drug_service_stats$prev_risk_factor_count))
print(paste("HCV - Drug Service - Previously diagnosed proportion:", hcv_drug_service_stats$prev_risk_factor_proportion, "%"))

# Calculate risk factor stats for HCV (overseas charging status)
hcv_overseas_stats <- calculate_risk_factor_stats(
  "Number.of.patients.newly.diagnosed.with.HCV.who.also.have.overseas.charging.status.recorded",
  total_previously_diagnosed_HCV,
  "Number.of.patients.who.test.positive.for.HCV.who.also.have.overseas.charging.status.recorded"
)

print(paste("HCV - Overseas - Newly diagnosed count:", hcv_overseas_stats$new_risk_factor_count))
print(paste("HCV - Overseas - Newly diagnosed proportion:", hcv_overseas_stats$new_risk_factor_proportion, "%"))
print(paste("HCV - Overseas - Previously diagnosed count:", hcv_overseas_stats$prev_risk_factor_count))
print(paste("HCV - Overseas - Previously diagnosed proportion:", hcv_overseas_stats$prev_risk_factor_proportion, "%"))


# Repeat similar calculations for HBV

# Calculate risk factor stats for HBV (PWID)
hbv_pwid_stats <- calculate_risk_factor_stats(
  "Number.of.patients.who.are.flagged.as.PWID.and.newly.diagnosed.with.HBV",
  total_previously_diagnosed_HBV,
  "Number.of.patients.who.are.flagged.as.PWID.who.test.positive.for.HBV"
)

print(paste("HBV - PWID - Newly diagnosed count:", hbv_pwid_stats$new_risk_factor_count))
print(paste("HBV - PWID - Newly diagnosed proportion:", hbv_pwid_stats$new_risk_factor_proportion, "%"))
print(paste("HBV - PWID - Previously diagnosed count:", hbv_pwid_stats$prev_risk_factor_count))
print(paste("HBV - PWID - Previously diagnosed proportion:", hbv_pwid_stats$prev_risk_factor_proportion, "%"))

# Calculate risk factor stats for HBV (prison testing history)
hbv_prison_stats <- calculate_risk_factor_stats(
  "Number.of.patients.newly.diagnosed.with.HBV.who.have.also.tested.in.a.prison",
  total_previously_diagnosed_HBV,
  "Number.of.patients.who.test.positive.for.HBV.who.have.also.tested.in.prison"
)

print(paste("HBV - Prison - Newly diagnosed count:", hbv_prison_stats$new_risk_factor_count))
print(paste("HBV - Prison - Newly diagnosed proportion:", hbv_prison_stats$new_risk_factor_proportion, "%"))
print(paste("HBV - Prison - Previously diagnosed count:", hbv_prison_stats$prev_risk_factor_count))
print(paste("HBV - Prison - Previously diagnosed proportion:", hbv_prison_stats$prev_risk_factor_proportion, "%"))

# Calculate risk factor stats for HBV (drug service testing history)
hbv_drug_service_stats <- calculate_risk_factor_stats(
  "Number.of.patients.newly.diagnosed.with.HBV.who.have.also.tested.in.a.drug.service",
  total_previously_diagnosed_HBV,
  "Number.of.patients.who.test.positive.for.HBV.who.have.also.tested.in.a.drug.service"
)

print(paste("HBV - Drug Service - Newly diagnosed count:", hbv_drug_service_stats$new_risk_factor_count))
print(paste("HBV - Drug Service - Newly diagnosed proportion:", hbv_drug_service_stats$new_risk_factor_proportion, "%"))
print(paste("HBV - Drug Service - Previously diagnosed count:", hbv_drug_service_stats$prev_risk_factor_count))
print(paste("HBV - Drug Service - Previously diagnosed proportion:", hbv_drug_service_stats$prev_risk_factor_proportion, "%"))

# Calculate risk factor stats for HBV (overseas charging status)
hbv_overseas_stats <- calculate_risk_factor_stats(
  "Number.of.patients.newly.diagnosed.with.HBV.who.also.have.overseas.charging.status.recorded",
  total_previously_diagnosed_HCV,
  "Number.of.patients.who.test.positive.for.HBV.who.also.have.overseas.charging.status.recorded"
)

print(paste("HBV - Overseas - Newly diagnosed count:", hcv_overseas_stats$new_risk_factor_count))
print(paste("HBV - Overseas - Newly diagnosed proportion:", hcv_overseas_stats$new_risk_factor_proportion, "%"))
print(paste("HBV - Overseas - Previously diagnosed count:", hcv_overseas_stats$prev_risk_factor_count))
print(paste("HBV - Overseas - Previously diagnosed proportion:", hcv_overseas_stats$prev_risk_factor_proportion, "%"))



----------------------
  
#Additional analysis with new diagnoses

# Filter the datasets for new diagnoses - drug service testing
attendees_HBV_srf_new <- attendees_HBV_srf %>% filter(HBsAg_positive_new == "New")
attendees_HCV_srf_new <- attendees_HCV_srf %>% filter(rna_positive_new == "New")

# Calculate the total new diagnoses
total_new_diagnoses_HCV <- nrow(attendees_HCV_srf_new)
total_new_diagnoses_HBV <- nrow(attendees_HBV_srf_new)

# Calculate the number of individuals with a history of drug service testing
drug_service_testing_HCV <- attendees_HCV_srf_new %>% filter(testedindrugservice == "Yes")
drug_service_testing_HBV <- attendees_HBV_srf_new %>% filter(testedindrugservice == "Yes")

# Calculate the total number of individuals with a history of drug service testing
total_drug_service_testing_HCV <- nrow(drug_service_testing_HCV)
total_drug_service_testing_HBV <- nrow(drug_service_testing_HBV)

# Calculate the proportion of individuals with a history of drug service testing
proportion_drug_service_testing_HCV <- (total_drug_service_testing_HCV / total_new_diagnoses_HCV) * 100
proportion_drug_service_testing_HBV <- (total_drug_service_testing_HBV / total_new_diagnoses_HBV) * 100

# Print the results
print(paste("HCV - Total new diagnoses:", total_new_diagnoses_HCV))
print(paste("HCV - Total with drug service testing history:", total_drug_service_testing_HCV))
print(paste("HCV - Proportion with drug service testing history:", proportion_drug_service_testing_HCV, "%"))

print(paste("HBV - Total new diagnoses:", total_new_diagnoses_HBV))
print(paste("HBV - Total with drug service testing history:", total_drug_service_testing_HBV))
print(paste("HBV - Proportion with drug service testing history:", proportion_drug_service_testing_HBV, "%"))


# Filter the datasets for new diagnoses - homeless flag
attendees_HBV_srf_new <- attendees_HBV_srf %>% filter(HBsAg_positive_new == "New")
attendees_HCV_srf_new <- attendees_HCV_srf %>% filter(rna_positive_new == "New")

# Calculate the total new diagnoses
total_new_diagnoses_HCV <- nrow(attendees_HCV_srf_new)
total_new_diagnoses_HBV <- nrow(attendees_HBV_srf_new)

# Calculate the number of individuals with a homeless flag
homeless_new_diag_HCV <- attendees_HCV_srf_new %>% filter(homelessflag == "Yes")
homeless_new_diag_HBV <- attendees_HBV_srf_new %>% filter(homelessflag == "Yes")

# Calculate the total number of individuals with a homeless flag
total_homeless_new_diag_HCV <- nrow(homeless_new_diag_HCV)
total_homeless_new_diag_HBV <- nrow(homeless_new_diag_HBV)

# Calculate the proportion of individuals with a homeless flag
proportion_homeless_new_diag_HCV <- (total_homeless_new_diag_HCV / total_new_diagnoses_HCV) * 100
proportion_homeless_new_diag_HBV <- (total_homeless_new_diag_HBV / total_new_diagnoses_HBV) * 100

# Print the results
print(paste("HCV - Total new diagnoses:", total_new_diagnoses_HCV))
print(paste("HCV - Total with homeless flag:", total_homeless_new_diag_HCV))
print(paste("HCV - Proportion with homeless flag:", proportion_homeless_new_diag_HCV, "%"))

print(paste("HBV - Total new diagnoses:", total_new_diagnoses_HBV))
print(paste("HBV - Total with homeless flag:", total_homeless_new_diag_HBV))
print(paste("HBV - Proportion with homeless flag:", proportion_homeless_new_diag_HBV, "%"))


# Filter the datasets for new diagnoses - Prison history
attendees_HBV_srf_new <- attendees_HBV_srf %>% filter(HBsAg_positive_new == "New")
attendees_HCV_srf_new <- attendees_HCV_srf %>% filter(rna_positive_new == "New")

# Calculate the total new diagnoses
total_new_diagnoses_HCV <- nrow(attendees_HCV_srf_new)
total_new_diagnoses_HBV <- nrow(attendees_HBV_srf_new)

# Calculate the number of individuals with a Prison history
prison_tested_new_diag_HCV <- attendees_HCV_srf_new %>% filter(testedinaprison == "Yes")
prison_tested_new_diag_HBV <- attendees_HBV_srf_new %>% filter(testedinaprison == "Yes")

# Calculate the total number of individuals with a Prison history
total_prison_tested_new_diag_HCV <- nrow(prison_tested_new_diag_HCV)
total_prison_tested_new_diag_HBV <- nrow(prison_tested_new_diag_HBV)

# Calculate the proportion of individuals with a Prison history
proportion_prison_tested_new_diag_HCV <- (total_prison_tested_new_diag_HCV / total_new_diagnoses_HCV) * 100
proportion_prison_tested_new_diag_HBV <- (total_prison_tested_new_diag_HBV / total_new_diagnoses_HBV) * 100

# Print the results
print(paste("HCV - Total new diagnoses:", total_new_diagnoses_HCV))
print(paste("HCV - Total tested in prison:", total_prison_tested_new_diag_HCV))
print(paste("HCV - Proportion tested in prison:", proportion_prison_tested_new_diag_HCV, "%"))

print(paste("HBV - Total new diagnoses:", total_new_diagnoses_HBV))
print(paste("HBV - Total tested in prison:", total_prison_tested_new_diag_HBV))
print(paste("HBV - Proportion tested in prison:", proportion_prison_tested_new_diag_HBV, "%"))













-----------------------------------------------------------

  # Clean the data: remove any unnecessary columns or rows if needed
  # Filter out entries with Unknown Gender, Unknown Ethnicity, Unknown IMD, and Other
  # These categories are in columns named `Gender`, `Ethnicity`, and `IMD`
  
  master_table_filt <- master_table %>%
  filter(!(breakdown == "Gender" & var %in% c("Unknown Gender", "Other")),
         !(breakdown == "Ethnic group" & var %in% c("Unknown Ethnicity", "Other")),
         !(breakdown == "IMD" & var == "Unknown IMD"),
         !(breakdown == "Total" & var == "All"))

view(master_table_filt)

# Plotting Number of patients who are flagged as PWID and test positive for HCV by Demographics

plot_risk_factors(master_table_filt, 'Number.of.patients.who.are.flagged.as.PWID.who.test.positive.for.HCV', 'PWID Positive for HCV by Demographic Category')

# Plotting Number of patients who are flagged as Homeless and test positive for HCV by Demographics

plot_risk_factors(master_table_filt, 'Number.of.patients.who.are.flagged.as.homeless.and.test.positive.for.HCV', 'Homeless Positive for HCV by Demographic Category')

# Plotting Number of patients who are tested in prison and test positive for HCV by Demographics

plot_risk_factors(master_table_filt, 'Number.of.patients.who.test.positive.for.HCV.who.have.also.tested.in.prison', 'Prison tested Positive for HCV by Demographic Category')


# Plotting Number of patients who are tested in a drug service and test positive for HCV by Demographics

plot_risk_factors(master_table_filt, 'Number.of.patients.who.test.positive.for.HCV.who.have.also.tested.in.a.drug.service', 'Drug Service tested Positive for HCV by Demographic Category')

# Plotting Number of patients who are tested in prison and test positive for HBV by Demographics

plot_risk_factors(master_table_filt, 'Number.of.patients.who.test.positive.for.HBV.who.have.also.tested.in.prison', 'Prison tested Positive for HBV by Demographic Category')


# Identify relevant columns for our analysis
relevant_columns <- master_table %>%
  select(breakdown, var, Number.of.patients.who.are.flagged.as.homeless.and.test.positive.for.HCV,
         Number.of.patients.who.are.flagged.as.PWID.who.test.positive.for.HCV,
         Number.of.patients.who.test.positive.for.HCV.who.have.also.tested.in.prison,
         Number.of.patients.who.test.positive.for.HCV.who.have.also.tested.in.a.drug.service,
         Number.of.patients.who.test.positive.for.HBV.who.have.also.tested.in.prison,
         Number.of.patients.who.test.positive.for.HCV.who.also.have.overseas.charging.status.recorded)

# View the relevant columns
head(relevant_columns)

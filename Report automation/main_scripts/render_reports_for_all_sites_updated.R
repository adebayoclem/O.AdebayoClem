library(rmarkdown)
library(dplyr)
library(readxl)
library(fs)
library(here)

# Load the Excel file with site codes and names
site_codes_path <- here("reports", "data", "site_lookup.xlsx")
site_code_to_name <- readxl::read_excel(site_codes_path, col_names = TRUE)

# Clean column names if necessary
site_code_to_name <- site_code_to_name %>%
  rename(site_code = `Site_Code`, site_name = `Site_Name`)  # Adjust column names as they appear in the Excel file

# Load the CSV file with inclusion flags
included_sites_df <- read.csv(here("reports", "data", "included_sites.csv"))

# Join to match site names with site codes
included_sites_df <- included_sites_df %>%
  left_join(site_code_to_name, by = "site_code")

# Function to render reports for all eligible sites
render_reports_for_all_sites <- function(included_sites_df, rmd_file, output_folder_base) {
  for (i in seq_along(included_sites_df$site_name)) {
    site_name <- included_sites_df$site_name[i]
    site_code <- included_sites_df$site_code[i]
    
    # Only proceed if any inclusion flag is "YES" (columns 3 onward)
    if (any(included_sites_df[i, 3:ncol(included_sites_df)] == "YES")) {
      params_list <- list(
        site_name = site_name,
        data_date = data_date,  # Use current date or a specific date as needed
        include_Attendance = included_sites_df$include_Attendance[i] == "YES",
        include_Blood_test = included_sites_df$include_Blood_test[i] == "YES",
        include_Virus1_test = included_sites_df$include_Virus1_test[i] == "YES",
        include_Virus3_test = included_sites_df$include_Virus3_test[i] == "YES",
        include_Virus2_test = included_sites_df$include_Virus2_test[i] == "YES",
        include_Virus3_diagnosis = included_sites_df$include_Virus3_diagnosis[i] == "YES",
        include_Virus2_diagnosis = included_sites_df$include_Virus2_diagnosis[i] == "YES",
        include_Virus1_diagnosis = included_sites_df$include_Virus1_diagnosis[i] == "YES",
        site_code = site_code
      )
      
      # Define the output folder and create it if it doesn't exist
      site_output_folder <- file.path(output_folder_base, gsub(" ", "_", site_name))
      if (!dir.exists(site_output_folder)) {
        dir.create(site_output_folder, recursive = TRUE)
      }
      file_name <- paste0(gsub(" ", "_", site_name), "_report.html")
      
      # Render the report
      rmarkdown::render(
        input = rmd_file,
        output_format = "html_document",
        output_file = file.path(site_output_folder, file_name),
        params = params_list
      )
    }
  }
}

# Function to test rendering for one site
test_render_for_one_site <- function(site_name, included_sites_df, rmd_file, output_folder_base) {
  # Trim whitespace and convert to uppercase for comparison
  site_name_clean <- stringr::str_trim(stringr::str_to_upper(site_name))
  
  specific_site_df <- included_sites_df %>%
    filter(stringr::str_trim(stringr::str_to_upper(site_name)) == site_name_clean)
  
  # Debugging output
  print(paste("Looking for site name:", site_name))
  print(paste("Filtered DataFrame has", nrow(specific_site_df), "rows"))
  print(specific_site_df)
  
  if (nrow(specific_site_df) > 0) {
    i <- 1
    params_list <- list(
      site_name = specific_site_df$site_name[i],
      data_date = data_date,
      include_Attendance = specific_site_df$include_Attendance[i] == "YES",
      include_Blood_test = specific_site_df$include_Blood_test[i] == "YES",
      include_Virus1_test = specific_site_df$include_Virus1_test[i] == "YES",
      include_Virus3_test = specific_site_df$include_Virus3_test[i] == "YES",
      include_Virus2_test = specific_site_df$include_Virus2_test[i] == "YES",
      include_Virus3_diagnosis = specific_site_df$include_Virus3_diagnosis[i] == "YES",
      include_Virus2_diagnosis = specific_site_df$include_Virus2_diagnosis[i] == "YES",
      include_Virus1_diagnosis = specific_site_df$include_Virus1_diagnosis[i] == "YES",
      site_code = specific_site_df$site_code[i]
    )
    
    # Debugging output
    print(paste("Rendering report for site:", params_list$site_name))
    
    site_output_folder <- file.path(output_folder_base, gsub(" ", "_", site_name))
    if (!dir.exists(site_output_folder)) {
      dir.create(site_output_folder, recursive = TRUE)
    }
    file_name <- paste0(gsub(" ", "_", site_name), "_report.html")
    
    rmarkdown::render(
      input = rmd_file,
      output_format = "html_document",
      output_file = file.path(site_output_folder, file_name),
      params = params_list
    )
  } else {
    message("No entries found for the site name: ", site_name)
  }
}

# Define paths
rmd_file <- here("reports", "scripts", "Site_Level_Report_Generic.Rmd")
output_folder_base <- here("reports", "Results")
data_date <- "2024-03-31"  # Example; adjust as needed

# Call to render reports for all sites
render_reports_for_all_sites(included_sites_df, rmd_file, output_folder_base)

# Test render for a single site
test_site_name <- "Site A"  # Replace with the site name you want to test
# test_render_for_one_site(test_site_name, included_sites_df, rmd_file, output_folder_base)

# Optionally clear the workspace
# rm(list = ls())

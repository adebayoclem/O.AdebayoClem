library(rmarkdown)
library(dplyr)
library(readxl)
library(fs)
library(here)

# Load the Excel file with site codes and names
site_codes_path <- here("trust reports", "report_data", "sitecodelookup.xlsx")
site_code_to_name <- readxl::read_excel(site_codes_path, col_names = TRUE)

# Clean column names if necessary
site_code_to_name <- site_code_to_name %>%
  rename(site_code = `Site ODS code`, site_name = `Hospital`)  # Adjust column names as they appear in the Excel file

# Load the CSV file with inclusion flags
included_sites_df <- read.csv(here("trust reports", "report_data", "included_sites.csv"))


# Join to match site names with site codes
included_sites_df <- included_sites_df %>%
  dplyr::left_join(site_code_to_name, by = "site_code")


# Function to render reports for all eligible sites
render_reports_for_all_sites <- function(included_sites_df, rmd_file, output_folder_base) {
  for (i in seq_along(included_sites_df$site_name)) {
    site_name <- included_sites_df$site_name[i]
    site_code <- included_sites_df$site_code[i]
    
    # Only proceed if any criteria are met
    if (any(included_sites_df[i, 3:ncol(included_sites_df)] == "YES")) {
      params_list <- list(
        site_name = site_name,
        data_date = data_date,  # Use current date or a specific date as needed
        include_Attendance = included_sites_df$include_Attendance[i] == "YES",
        include_Blood_test = included_sites_df$include_Blood_test[i] == "YES",
        include_HIV_test = included_sites_df$include_HIV_test[i] == "YES",
        include_HCV_test = included_sites_df$include_HCV_test[i] == "YES",
        include_HBV_test = included_sites_df$include_HBV_test[i] == "YES",
        include_HCV_diagnosis = included_sites_df$include_HCV_diagnosis[i] == "YES",
        include_HBV_diagnosis = included_sites_df$include_HBV_diagnosis[i] == "YES",
        include_HIV_diagnosis = included_sites_df$include_HIV_diagnosis[i] == "YES",
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
      include_HIV_test = specific_site_df$include_HIV_test[i] == "YES",
      include_HCV_test = specific_site_df$include_HCV_test[i] == "YES",
      include_HBV_test = specific_site_df$include_HBV_test[i] == "YES",
      include_HCV_diagnosis = specific_site_df$include_HCV_diagnosis[i] == "YES",
      include_HBV_diagnosis = specific_site_df$include_HBV_diagnosis[i] == "YES",
      include_HIV_diagnosis = specific_site_df$include_HIV_diagnosis[i] == "YES",
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
rmd_file <- here("trust reports", "main_scripts", "Site_Level_Report_April.Rmd")
output_folder_base <- here("trust reports", "Results")
data_date <- "2024-03-31"  # Example, you can dynamically set this as needed

# call to render reports for all sites
render_reports_for_all_sites(included_sites_df, rmd_file, output_folder_base)

# Test render for a single site
test_site_name <- "Chelsea & Westminster Hospital"  # Replace with the site name you want to test
#test_render_for_one_site(test_site_name, included_sites_df, rmd_file, output_folder_base) #remove comment to run


#rm(list = ls())

# Helper function to return go-live dates for each disease
go_live_dates <- function(disease) {
  if (disease == "HCV") {
    return(list(
      RAL01 = "2022-04-22", RAL26 = "2022-04-22", RAPNM = "2022-04-01", 
      RRV03 = "2023-04-03", RKEQ4 = "2022-09-05", R1HNH = "2022-04-01", 
      R1H12 = "2022-04-01", R1HKH = "2022-04-04", RF4QH = "2022-11-28", 
      RF4DG = "2022-11-28", RQXM1 = "2022-09-12", RQM01 = "2022-04-01", 
      RQM91 = "2022-04-01", RYJ01 = "2022-08-15", RYJ02 = "2022-07-01", 
      R1K01 = "2022-05-19", R1K04 = "2022-05-19", RAS01 = "2022-07-22", 
      RJZ01 = "2022-11-16", RJ224 = "2023-05-09", RJ231 = "2023-05-09", 
      RJ122 = "2022-11-01", RJ611 = "2023-03-20", RJ701 = "2022-11-17", 
      RVR05 = "2023-03-07", RVR50 = "2023-03-07", RAX01 = "2023-04-24", 
      E0A3H = "2023-03-06", R0A66 = "2022-09-08", R0A02 = "2022-04-01", 
      R0A07 = "2022-04-01", RXL01 = "2023-08-02", RMC10 = "2024-01-08", 
      RJZ30 = "2024-02-01"
    ))
  } else if (disease == "HBV") {
    return(list(
      RAL01 = "2022-04-22", RAL26 = "2022-04-22", RAPNM = "2022-04-01", 
      RRV03 = "2023-04-03", RKEQ4 = "2022-09-05", R1HNH = "2022-04-25", 
      R1H12 = "2022-04-25", R1HKH = "2022-04-25", RQXM1 = "2022-09-12", 
      RQM01 = "2022-04-01", RQM91 = "2022-04-01", RYJ01 = "2022-08-15", 
      RYJ02 = "2022-07-01", R1K01 = "2022-05-19", R1K04 = "2022-05-19", 
      RAS01 = "2022-07-22", RJZ01 = "2022-11-16", RJ224 = "2023-05-09", 
      RJ231 = "2023-05-09", RJ122 = "2022-11-01", RJ611 = "2023-03-20", 
      RJ701 = "2022-11-17", RVR05 = "2023-03-07", RVR50 = "2023-03-07", 
      RAX01 = "2023-04-24", RF4QH = "2023-10-02", RF4DG = "2023-10-02", 
      RJZ30 = "2024-02-01", RXL01 = "2023-08-04"
    ))
  } else if (disease == "HIV") {
    return(list(
      RAL01 = "2022-04-22", RAL26 = "2022-04-22", RAPNM = "2022-04-01", 
      RRV03 = "2023-04-01", RKEQ4 = "2022-09-05", R1HNH = "2022-04-01", 
      R1H12 = "2022-04-01", R1HKH = "2022-04-04", RF4QH = "2022-08-01", 
      RF4DG = "2022-07-28", RQXM1 = "2022-04-01", RQM01 = "2022-04-01", 
      RQM91 = "2022-04-01", RYJ01 = "2022-04-01", RYJ02 = "2022-04-01", 
      R1K01 = "2022-05-19", R1K04 = "2022-05-19", RAS01 = "2022-07-22", 
      RJZ01 = "2022-04-01", RJZ30 = "2022-04-21", RJ224 = "2022-04-01", 
      RJ231 = "2022-04-01", RJ122 = "2022-04-01", RJ611 = "2022-04-01", 
      RJ701 = "2022-04-01", RVR05 = "2022-04-01", RVR50 = "2022-05-16", 
      RAX01 = "2022-04-01", E0A3H = "2022-04-06", R0A66 = "2022-09-08", 
      R0A02 = "2022-04-01", R0A07 = "2022-04-01", RXL01 = "2022-04-01", 
      RMC10 = "2024-01-08"
    ))
  }
}

load_attendees_data <- function(conn, site_code, disease) {
  # Define the live dates for each site and disease
  live_dates <- list(
    HCV = go_live_dates("HCV"),
    HBV = go_live_dates("HBV"),
    HIV = go_live_dates("HIV")
  )
  
  # Get the go-live date for the specific site and disease
  live_date <- live_dates[[disease]][[site_code]]
  
  # If there's no go-live date for the site and disease, return an empty dataframe
  if (is.null(live_date)) {
    warning(sprintf("Site %s is not live for %s.", site_code, disease))
    return(data.frame())  # Return empty dataframe if site is not live for the disease
  }
  
  # First query: attempt to load data between the go-live date and 2024-03-31 from the first table
  specific_query <- sprintf(
    "SELECT * 
    FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsites%s] 
    WHERE Site = '%s' 
    AND arrdate BETWEEN '%s' AND '2024-03-31'",
    disease, site_code, live_date
  )
  
  # Execute the specific query
  specific_data <- DBI::dbGetQuery(conn, specific_query)
  
  # If data is found in the first table, return it
  if (nrow(specific_data) > 0) {
    return(specific_data)
  } else {
    warning(sprintf("No specific data found for site %s for %s between go-live date and 2024-03-31.", site_code, disease))
    
    # Try loading data from the second table
    second_query <- sprintf(
      "SELECT * 
      FROM [Y006_BBV_PID].[dbo].[24monthECDSattendances_includedsites] 
      WHERE Site = '%s' 
      AND ARRIVAL_DATE BETWEEN '%s' AND '2024-03-31'",
      site_code, live_date
    )
    
    second_data <- DBI::dbGetQuery(conn, second_query)
    
    # If data is found in the second table, return it
    if (nrow(second_data) > 0) {
      return(second_data)
    } else {
      warning(sprintf("No data found for site %s for %s in second table.", site_code, disease))
      
      # Try loading data from the third table
      third_query <- sprintf(
        "SELECT * 
        FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_includedsites] 
        WHERE Site = '%s' 
        AND arrdate BETWEEN '%s' AND '2024-03-31'",
        site_code, live_date
      )
      
      third_data <- DBI::dbGetQuery(conn, third_query)
      
      # If data is found in the third table, return it
      if (nrow(third_data) > 0) {
        return(third_data)
      } else {
        warning(sprintf("No data found for site %s for %s in third table. Attempting less restrictive query.", site_code, disease))
        
        # If no data from all three tables, and go-live date present, try the less restrictive query
        less_restrictive_query <- sprintf(
          "SELECT * 
          FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsites%s] 
          WHERE Site = '%s' 
          AND arrdate <= '2024-03-31'",
          disease, site_code
        )
        
        # Execute the less restrictive query
        less_restrictive_data <- DBI::dbGetQuery(conn, less_restrictive_query)
        
        # Check if the less restrictive query returned any data
        if (nrow(less_restrictive_data) > 0) {
          return(less_restrictive_data)  # Return the less restrictive data if found
        } else {
          # If no data from the less restrictive query, attempt to load general attendance data
          warning(sprintf("No data found for site %s for %s in less restrictive query. Attempting to load general data.", site_code, disease))
          
          general_query <- sprintf(
            "SELECT * 
        FROM [Y006_BBV_PID].[dbo].[24mthECDSattendees_includedsites] 
        WHERE Site = '%s' 
        AND arrdate <= '2024-03-31'",
            site_code
          )
          
          general_data <- DBI::dbGetQuery(conn, general_query)
          
          # Check if general data was returned and if it contains necessary columns
          if (nrow(general_data) > 0 && all(c("HIV", "HBV", "HCV") %in% colnames(general_data))) {
            return(general_data)
          } else {
            warning(sprintf("General data not found or incomplete for site %s for %s. Attempting fallback query.", site_code, disease))
            
            # Fallback to alternative general query
            fallback_query <- sprintf(
              "SELECT * 
          FROM [Y006_BBV_PID].[dbo].[24monthECDSattendances_includedsites] 
          WHERE Site = '%s' 
          AND ARRIVAL_DATE <= '2024-03-31'",
              site_code
            )
            
            fallback_data <- DBI::dbGetQuery(conn, fallback_query)
            
            if (nrow(fallback_data) > 0) {
              return(fallback_data)  # Return fallback data if available
            } else {
              warning(sprintf("No data found for site %s for %s in all queries. Returning empty dataframe.", site_code, disease))
              return(data.frame())  # Return empty dataframe if no data is found
            }
          }
        }
      }
    }
  }
}
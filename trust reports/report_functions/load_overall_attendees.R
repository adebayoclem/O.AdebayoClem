
# Load all ED data for each disease
load_all_ed_data <- function(conn, disease) {
  # Define the tables for each disease
  tables <- list(
    HIV = "[Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHIV]",
    HBV = "[Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHBV]",
    HCV = "[Y006_BBV_PID].[dbo].[24mthECDSattendees_sentinelsitesHCV]"
  )
  
  # Check if the table for the disease exists
  if (!disease %in% names(tables)) {
    stop(sprintf("Invalid disease type: %s", disease))
  }
  
  # Query the data for the specified disease
  query <- sprintf(
    "SELECT *
    FROM %s
    WHERE arrdate <= '2024-03-31'",
    tables[[disease]]
  )
  
  # Execute the query
  data <- DBI::dbGetQuery(conn, query)
  
  # Validate that the data is not empty
  if (nrow(data) == 0) {
    warning(sprintf("No data found for %s. Returning an empty dataframe.", disease))
    return(data.frame())
  }
  
  return(data)
}
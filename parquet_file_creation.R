# Load necessary libraries
if (!require(odbc)) install.packages("odbc")
if (!require(DBI)) install.packages("DBI")
if (!require(arrow)) install.packages("arrow")
if (!require(dplyr)) install.packages("dplyr")

# Establish a connection to the SQL Server database
Y006 <- DBI::dbConnect(
  odbc::odbc(),
  .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;database=Y006_BBV_PID;Encrypt=true;trusted_connection=true",
  timeout = 60,
  timezone = Sys.timezone(),
  timezone_out = Sys.timezone()
)

# Define chunk size (number of rows per chunk)
chunk_size <- 100000  # Adjust based on your system capacity

# Initialize connection to SQL Server
con <- dbConnect(odbc::odbc(),
                 .connection_string = "driver={SQL Server};server=SQLClusColLK19\\Lake19;database=Y006_BBV_PID;trusted_connection=true;timeout=300")
# Define the output file path for Parquet file
output_file <- "//filepor10/datalake$/Y006_BBV_PID/ED evaluation/parquets/Y1_24monthECDS.parquet"

# Define the SQL query
sql_query <- "SELECT * FROM [24monthECDS]"

# Initialize variables for iteration
offset <- 0

# Fetch data in chunks and write to Parquet
repeat {
  # Construct the SQL query with OFFSET and FETCH
  query_chunk <- sprintf("%s ORDER BY [TOKEN_PERSON_ID] OFFSET %d ROWS FETCH NEXT %d ROWS ONLY", sql_query, as.integer(offset), chunk_size)
  

  # Fetch the chunk from the database with a specified timeout
  chunk <- dbGetQuery(con, query_chunk, timeout = 300)  # Set query timeout to 300 seconds
  
  # Break if no more data is returned
  if (nrow(chunk) == 0) break
  
  # Convert the chunk to an Arrow Table
  arrow_table <- arrow::arrow_table(chunk)
  
  # Write or append to the Parquet file
  if (offset == 0) {
    # Write the first chunk directly to create the file
    arrow::write_parquet(arrow_table, output_file)
  } else {
    # Append subsequent chunks to the existing Parquet file
    existing_data <- arrow::read_parquet(output_file)
    combined_data <- bind_rows(as.data.frame(existing_data), as.data.frame(arrow_table))
    arrow::write_parquet(arrow::arrow_table(combined_data), output_file)
  }
  
  # Update the offset for the next chunk
  offset <- offset + chunk_size
}

# Close the connection
dbDisconnect(con)
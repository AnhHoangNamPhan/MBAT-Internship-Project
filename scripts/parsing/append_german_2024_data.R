#!/usr/bin/env Rscript

# ==================================================
# Append German 2024 Data to Existing Database
# ==================================================
# Appends German fuel price data from 2024 (August-December)
# to the existing German database
# ==================================================  

library(DBI)
library(duckdb)
library(readr)
library(dplyr)
library(fs)

# Configuration
db_path <- '/Users/alexphan/Desktop/MBAT-Internship-Project/databases/german_fuel_data.duckdb'
data_dir <- '/Users/alexphan/Desktop/MBAT-Internship-Project/scraped_data/tankerkoenig-data'
prices_table <- "german_prices"

cat("Appending German 2024 Data to Existing Database\n")
cat("==============================================\n\n")

# Connect to database
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

# Check current database state
current_stats <- DBI::dbGetQuery(con, "
  SELECT 
    MIN(date) as min_date,
    MAX(date) as max_date,
    COUNT(*) as total_records
  FROM german_prices
")

cat("Current database state:\n")
cat("  Date range:", current_stats$min_date, "to", current_stats$max_date, "\n")
cat("  Total records:", format(current_stats$total_records, big.mark = ","), "\n\n")

# ---------- Load 2024 Prices ----------
cat("Loading 2024 price data (August-December)...\n")

# Find all price CSV files in prices-2024 folder
price_files <- fs::dir_ls(file.path(data_dir, "prices-2024"), 
                        recurse = TRUE, 
                        glob = "*.csv")

cat("Found", length(price_files), "2024 price files to process\n\n")

total_processed <- 0
total_records <- 0

for (price_file in price_files) {
  file_name <- basename(price_file)
  
  tryCatch({
    # Read the price file
    price_data <- readr::read_csv(price_file,
                                  col_types = cols(
                                    date = col_character(),
                                    station_uuid = col_character(),
                                    diesel = col_double(),
                                    e5 = col_double(),
                                    e10 = col_double(),
                                    dieselchange = col_integer(),
                                    e5change = col_integer(),
                                    e10change = col_integer()
                                  ),
                                  show_col_types = FALSE)
    
    # Convert date format from "2024-07-31 22:01:17 UTC" to "2024-07-31 22:01:17"
    # This matches the format in the existing database (e.g., "2024-12-31 23:00:29")
    # Remove the " UTC" timezone suffix
    price_data$date <- gsub(" UTC$", "", price_data$date)
    
    # Remove duplicates within the file (same station_uuid and date)
    price_data <- price_data[!duplicated(price_data[c('station_uuid', 'date')]), ]
    
    # Add file source column
    price_data$file_source <- file_name
    
    # Check if data already exists for this date to avoid duplicates
    unique_date <- unique(price_data$date)[1]  # Get the date for this file
    date_check <- DBI::dbGetQuery(con, paste0("
      SELECT COUNT(*) as count 
      FROM ", prices_table, " 
      WHERE date = '", unique_date, "' 
        AND file_source = '", file_name, "'
    "))
    
    if (date_check$count > 0) {
      cat("  Skipping", file_name, "- already exists in database\n")
      next
    }
    
    # Batch insert using DBI (much faster than row-by-row)
    if (!DBI::dbIsValid(con)) {
      cat("Reconnecting to DuckDB...\n")
      con <<- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
    }
    
    # Create a temporary table and use INSERT OR REPLACE
    temp_table <- paste0("temp_prices_", gsub("[^0-9]", "", file_name))
    
    # Write to temp table first
    DBI::dbWriteTable(con, temp_table, price_data, temporary = TRUE, overwrite = TRUE)
    
    # Then INSERT OR REPLACE from temp table
    DBI::dbExecute(con, paste0("
      INSERT OR REPLACE INTO ", prices_table, " 
      SELECT * FROM ", temp_table, "
    "))
    
    # Drop temp table
    DBI::dbExecute(con, paste0("DROP TABLE ", temp_table))
    
    total_records <- total_records + nrow(price_data)
    total_processed <- total_processed + 1
    
    cat("[", total_processed, "/", length(price_files), "] ", 
        file_name, ": ", format(nrow(price_data), big.mark = ","), " records (Total: ",
        format(total_records, big.mark = ","), ")\n", sep = "")
    
  }, error = function(e) {
    cat("Error processing", file_name, ":", e$message, "\n")
  })
}

cat("\n")
cat("==================================================\n")
cat("Processing Summary\n")
cat("==================================================\n")
cat("2024 price files processed:", total_processed, "\n")
cat("New records added:", format(total_records, big.mark = ","), "\n\n")

# Show updated database stats
updated_stats <- DBI::dbGetQuery(con, "
  SELECT 
    MIN(date) as min_date,
    MAX(date) as max_date,
    COUNT(*) as total_records
  FROM german_prices
")

cat("Updated database state:\n")
cat("  Date range:", updated_stats$min_date, "to", updated_stats$max_date, "\n")
cat("  Total records:", format(updated_stats$total_records, big.mark = ","), "\n")

# Check 2024 data specifically
data_2024 <- DBI::dbGetQuery(con, "
  SELECT 
    COUNT(*) as records_2024,
    COUNT(DISTINCT DATE(date)) as unique_days_2024
  FROM german_prices
  WHERE date >= '2024-08-01' AND date < '2025-01-01'
")

cat("  2024 records (Aug-Dec):", format(data_2024$records_2024, big.mark = ","), "\n")
cat("  2024 unique days:", data_2024$unique_days_2024, "\n\n")

# Sample query - show some 2024 data
cat("Sample 2024 data (first few days):\n")
sample_data <- DBI::dbGetQuery(con, "
  SELECT 
    DATE(date) as date,
    COUNT(*) as records_per_day
  FROM german_prices
  WHERE date >= '2024-08-01' AND date < '2025-01-01'
  GROUP BY DATE(date)
  ORDER BY DATE(date)
  LIMIT 10
")
print(sample_data)

cat("\n")

# Disconnect
DBI::dbDisconnect(con, shutdown = TRUE)
cat("2024 data append completed successfully!\n")

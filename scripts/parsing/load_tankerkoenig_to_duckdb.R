#!/usr/bin/env Rscript

# ==================================================
# Tankerkönig German Fuel Price Data Parser
# ==================================================
# Parses German fuel price data from Tankerkönig
# into DuckDB with two tables:
# 1. stations - Station metadata (including opening hours)
# 2. prices - Daily price data
# 
# Data source: https://dev.azure.com/tankerkoenig/tankerkoenig-data
# ==================================================

library(DBI)
library(duckdb)
library(readr)
library(dplyr)
library(fs)

# ---------- Configuration ----------
db_path <- '/Users/alexphan/Desktop/MBAT-Internship-Project/databases/german_fuel_data.duckdb'
data_dir <- '/Users/alexphan/Desktop/MBAT-Internship-Project/scraped_data/tankerkoenig-data'
stations_table <- "german_stations"
prices_table <- "german_prices"

cat("==================================================\n")
cat("🇩🇪 Tankerkönig German Fuel Data Parser\n")
cat("==================================================\n\n")

# ---------- Helper Functions ----------

connect_duckdb <- function() {
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
}

create_stations_table <- function(table_name) {
  if (!DBI::dbIsValid(con)) {
    cat("🔄 Reconnecting to DuckDB...\n")
    con <<- connect_duckdb()
  }
  
  if (!DBI::dbExistsTable(con, table_name)) {
    cat("📋 Creating stations table:", table_name, "\n")
    DBI::dbExecute(con, paste0("
      CREATE TABLE ", table_name, " (
        uuid VARCHAR PRIMARY KEY,
        name VARCHAR,
        brand VARCHAR,
        street VARCHAR,
        house_number VARCHAR,
        post_code VARCHAR,
        city VARCHAR,
        latitude DOUBLE,
        longitude DOUBLE,
        first_active TIMESTAMP,
        openingtimes_json VARCHAR
      )
    "))
    cat("✅ Stations table created\n\n")
  } else {
    cat("✅ Stations table already exists\n\n")
  }
}

create_prices_table <- function(table_name) {
  if (!DBI::dbIsValid(con)) {
    cat("🔄 Reconnecting to DuckDB...\n")
    con <<- connect_duckdb()
  }
  
  if (!DBI::dbExistsTable(con, table_name)) {
    cat("📋 Creating prices table:", table_name, "\n")
    DBI::dbExecute(con, paste0("
      CREATE TABLE ", table_name, " (
        date TIMESTAMP,
        station_uuid VARCHAR,
        diesel DOUBLE,
        e5 DOUBLE,
        e10 DOUBLE,
        dieselchange INTEGER,
        e5change INTEGER,
        e10change INTEGER,
        file_source VARCHAR,
        PRIMARY KEY (date, station_uuid)
      )
    "))
    cat("✅ Prices table created\n\n")
  } else {
    cat("✅ Prices table already exists\n\n")
  }
}

# ---------- Main Processing ----------

# Connect to DuckDB
con <- connect_duckdb()
cat("📊 Connected to DuckDB:", db_path, "\n\n")

# Create tables
create_stations_table(stations_table)
create_prices_table(prices_table)

# ---------- Option 1: Load from master stations.csv ----------
cat("🏪 Loading station data...\n")
cat("Choose source: [1] Master stations.csv OR [2] Daily 2025 stations (with opening hours)\n")

# For now, let's use the most recent daily station file (has opening hours)
latest_station_file <- fs::dir_ls(file.path(data_dir, "stations-2025"), 
                                  recurse = TRUE, 
                                  glob = "*.csv") %>% 
                       sort() %>% 
                       tail(1)

cat("📍 Using:", basename(latest_station_file), "(includes opening hours)\n")

if (file.exists(latest_station_file)) {
  stations_data <- readr::read_csv(latest_station_file, 
                                   col_types = cols(
                                     uuid = col_character(),
                                     name = col_character(),
                                     brand = col_character(),
                                     street = col_character(),
                                     house_number = col_character(),
                                     post_code = col_character(),
                                     city = col_character(),
                                     latitude = col_double(),
                                     longitude = col_double(),
                                     first_active = col_character(),
                                     openingtimes_json = col_character()
                                   ),
                                   show_col_types = FALSE)
  
  cat("📍 Found", nrow(stations_data), "stations\n")
  
  # Bulk insert stations (much faster)
  if (!DBI::dbIsValid(con)) {
    cat("🔄 Reconnecting to DuckDB...\n")
    con <<- connect_duckdb()
  }
  
  DBI::dbWriteTable(con, stations_table, stations_data, append = TRUE, overwrite = FALSE)
  
  cat("✅ All stations loaded!\n\n")
} else {
  cat("⚠️  Station file not found\n\n")
}

# ---------- Load 2025 Prices ----------
cat("💰 Loading 2025 price data...\n")

# Find all price CSV files in prices-2025 folder
price_files <- fs::dir_ls(file.path(data_dir, "prices-2025"), 
                          recurse = TRUE, 
                          glob = "*.csv")

cat("📁 Found", length(price_files), "price files to process\n")
cat("⏱️  This will take 15-30 minutes for ~105 million records...\n\n")

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
    
    # Add file source column
    price_data$file_source <- file_name
    
    # Batch insert using DBI (much faster than row-by-row)
    if (!DBI::dbIsValid(con)) {
      cat("🔄 Reconnecting to DuckDB...\n")
      con <<- connect_duckdb()
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
    
    cat("✅ [", total_processed, "/", length(price_files), "] ", 
        file_name, ": ", format(nrow(price_data), big.mark = ","), " records (Total: ",
        format(total_records, big.mark = ","), ")\n", sep = "")
    
  }, error = function(e) {
    cat("⚠️  Error processing", file_name, ":", e$message, "\n")
  })
}

cat("\n")
cat("==================================================\n")
cat("📊 Processing Summary\n")
cat("==================================================\n")
cat("Price files processed:", total_processed, "\n")
cat("Total records inserted:", format(total_records, big.mark = ","), "\n")
cat("\n")

# Show database stats
total_stations <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", stations_table))$count
cat("📍 Total stations in database:", format(total_stations, big.mark = ","), "\n")

total_prices <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", prices_table))$count
cat("💰 Total price records:", format(total_prices, big.mark = ","), "\n")

unique_dates <- DBI::dbGetQuery(con, paste0("SELECT COUNT(DISTINCT DATE(date)) as count FROM ", prices_table))$count
cat("📅 Unique dates:", unique_dates, "\n")

cat("\n")

# Sample query - cheapest diesel on latest date
cat("🔍 Sample: Top 5 cheapest Diesel stations (latest date):\n")
sample_data <- DBI::dbGetQuery(con, paste0("
  SELECT 
    p.diesel as price,
    s.name as station,
    s.brand,
    s.city,
    DATE(p.date) as date
  FROM ", prices_table, " p
  JOIN ", stations_table, " s ON p.station_uuid = s.uuid
  WHERE p.diesel > 0 AND DATE(p.date) = (SELECT MAX(DATE(date)) FROM ", prices_table, ")
  ORDER BY p.diesel ASC
  LIMIT 5
"))
print(sample_data)

cat("\n")

# Disconnect
DBI::dbDisconnect(con, shutdown = TRUE)
cat("✅ Parser completed successfully!\n")

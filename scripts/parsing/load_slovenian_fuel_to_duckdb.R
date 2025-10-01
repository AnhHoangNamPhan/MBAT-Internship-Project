# Load necessary libraries
library(jsonlite)
library(dplyr)
library(tibble)
library(DBI)
library(duckdb)
library(fs)

# ---------- Configuration ----------
data_dir <- "/Users/alexphan/Desktop/MBAT-Internship-Project/data_slo"
db_dir <- "/Users/alexphan/Desktop/MBAT-Internship-Project/db"
db_path <- file.path(db_dir, "slovenian_fuel_data.duckdb")
table_name <- "slovenian_fuel"

# Ensure database directory exists
if (!dir.exists(db_dir)) {
  dir.create(db_dir, recursive = TRUE)
}

# Function to connect to DuckDB
connect_duckdb <- function() {
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
}

# Initialize connection
con <- connect_duckdb()

# Custom operator for null handling
`%or%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.atomic(a) && all(is.na(a)))) {
    b
  } else {
    a
  }
}

# Helper function to safely convert to numeric
safe_as_numeric <- function(x) {
  if (is.null(x) || length(x) == 0) {
    NA_real_
  } else {
    as.numeric(x)
  }
}

# Helper function to extract page number from filename
extract_page_number <- function(filename) {
  # Expected format: fuel_page-X_YYYY-MM-DD_HH-MM.json
  # Extract X
  page_match <- regmatches(filename, regexec("fuel_page-(\\d+)_", filename))
  if (length(page_match[[1]]) > 1) {
    return(as.numeric(page_match[[1]][2]))
  }
  NA_real_
}

# ---------- Create Table with Unique Constraint ----------
create_table_if_not_exists <- function(table_name) {
  # Check connection validity first
  if (!DBI::dbIsValid(con)) {
    cat("🔄 Reconnecting to DuckDB...\n")
    con <<- connect_duckdb()
  }
  
  # Check if table exists
  if (!DBI::dbExistsTable(con, table_name)) {
    cat("📋 Creating table:", table_name, "\n")
    
    # Create table with proper schema
    create_sql <- paste0("
      CREATE TABLE ", table_name, " (
        station_id REAL,
        franchise_id REAL,
        station_name VARCHAR,
        station_address VARCHAR,
        station_lat REAL,
        station_lng REAL,
        fuel_type VARCHAR,
        fuel_price REAL,
        zip_code VARCHAR,
        direction VARCHAR,
        open_hours VARCHAR,
        page_number REAL,
        file_source VARCHAR,
        PRIMARY KEY (station_id, fuel_type, file_source)
      )
    ")
    
    DBI::dbExecute(con, create_sql)
    cat("✅ Table created successfully\n")
  } else {
    cat("☑️ Table", table_name, "already exists\n")
  }
}

# Call function to create table if it doesn't exist
create_table_if_not_exists(table_name)

# Get list of all JSON files
json_files <- list.files(data_dir, pattern = "\\.json$", full.names = TRUE)

if (length(json_files) == 0) {
  stop("No JSON files found in the specified directory: ", data_dir)
}

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("🚀 Starting Slovenian Fuel Data Parsing and Loading\n")
cat(paste(rep("=", 50), collapse=""), "\n\n")

total_rows <- 0
processed_files <- 0
failed_files <- 0

# Process each JSON file
for (file in json_files) {
  cat("Processing:", basename(file), "\n")
  
  # Capture file name early to prevent scope issues
  current_file <- basename(file)
  page_number <- extract_page_number(current_file)

  # Read and validate JSON
  raw <- try(jsonlite::fromJSON(file, simplifyVector = FALSE), silent = TRUE)

  if (inherits(raw, "try-error") || is.null(raw$results)) {
    cat("❌ Error reading or parsing JSON from", basename(file), "\n")
    failed_files <- failed_files + 1
    next
  }

  # Extract data from results using a simple loop approach
  all_stations <- list()
  station_counter <- 0
  
  for (station in raw$results) {
    station_counter <- station_counter + 1
    
    # Extract station information
    station_id <- station$pk %or% NA_real_
    franchise_id <- station$franchise %or% NA_real_
    station_name <- station$name %or% NA_character_
    station_address <- station$address %or% NA_character_
    station_lat <- safe_as_numeric(station$lat)
    station_lng <- safe_as_numeric(station$lng)
    zip_code <- station$zip_code %or% NA_character_
    direction <- station$direction %or% NA_character_
    open_hours <- station$open_hours %or% NA_character_

    # Extract prices
    prices <- station$prices %or% list()
    
    # Process each fuel type
    if (length(prices) > 0) {
      for (fuel_type in names(prices)) {
        price <- safe_as_numeric(prices[[fuel_type]])
        
        # Only include rows with valid prices
        if (!is.na(price)) {
          station_row <- list(
            station_id = station_id,
            franchise_id = franchise_id,
            station_name = station_name,
            station_address = station_address,
            station_lat = station_lat,
            station_lng = station_lng,
            fuel_type = fuel_type,
            fuel_price = price,
            zip_code = zip_code,
            direction = direction,
            open_hours = open_hours,
            page_number = page_number,
            file_source = current_file
          )
          all_stations[[length(all_stations) + 1]] <- station_row
        }
      }
    }
  }

  # Convert to data frame
  if (length(all_stations) > 0) {
    df <- do.call(rbind, lapply(all_stations, function(x) {
      data.frame(x, stringsAsFactors = FALSE)
    }))
    
    # Insert or replace data using INSERT OR REPLACE
    tryCatch({
      for (i in 1:nrow(df)) {
        row <- df[i, ]
        insert_sql <- paste0("
          INSERT OR REPLACE INTO ", table_name, " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ")
        DBI::dbExecute(con, insert_sql, params = list(
          row$station_id, row$station_name, row$station_address, row$franchise_id,
          row$station_lat, row$station_lng, row$zip_code, row$direction, row$open_hours,
          NA_real_, row$fuel_type, row$fuel_price, row$page_number, row$file_source
        ))
      }
      cat("✅ Written", nrow(df), "rows to", table_name, "\n")
      total_rows <- total_rows + nrow(df)
      processed_files <- processed_files + 1
    }, error = function(e) {
      cat("❌ Error writing to database for", basename(file), ":", e$message, "\n")
      failed_files <- failed_files + 1
    })
  } else {
    cat("ℹ️ No valid data to write from", basename(file), "\n")
  }
}

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("📊 PROCESSING SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n")
cat("✅ Files processed successfully:", processed_files, "\n")
cat("❌ Files failed:", failed_files, "\n")
cat("📁 Total files:", length(json_files), "\n")
cat("📊 Total rows written:", total_rows, "\n")

# Optional: Query some statistics
cat("\n📈 Price statistics for Slovenian fuel:\n")
price_query <- paste0("
  SELECT
    fuel_type,
    COUNT(DISTINCT station_id) AS num_stations,
    AVG(fuel_price) AS avg_price,
    MIN(fuel_price) AS min_price,
    MAX(fuel_price) AS max_price
  FROM ", table_name, "
  WHERE fuel_price IS NOT NULL
  GROUP BY fuel_type
  ORDER BY fuel_type
")
price_stats <- DBI::dbGetQuery(con, price_query)
if (nrow(price_stats) > 0) {
  print(price_stats)
} else {
  cat("No price data available.\n")
}

# Disconnect from DuckDB
DBI::dbDisconnect(con, shutdown = TRUE)
cat("\nDisconnected from DuckDB. Database saved to:", db_path, "\n")
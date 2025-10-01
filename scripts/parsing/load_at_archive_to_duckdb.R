#!/usr/bin/env Rscript

# ==================================================
# Austrian Archive Fuel Price Data Parser
# ==================================================
# Parses historical Austrian fuel price data from
# spritvergleich.at archive into DuckDB
# ==================================================

library(DBI)
library(duckdb)
library(stringr)
library(fs)

# ---------- Configuration ----------
db_path <- '/Users/alexphan/Desktop/MBAT-Internship-Project/db/austrian_archive_fuel_data.duckdb'
table_name <- "austrian_archive_fuel"
data_dir <- '/Users/alexphan/Desktop/MBAT-Internship-Project/data_at_archive'

cat("==================================================\n")
cat("🇦🇹 Austrian Archive Fuel Data Parser\n")
cat("==================================================\n\n")

# ---------- Helper Functions ----------

connect_duckdb <- function() {
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
}

create_table_if_not_exists <- function(table_name) {
  # Check connection validity first
  if (!DBI::dbIsValid(con)) {
    cat("🔄 Reconnecting to DuckDB...\n")
    con <<- connect_duckdb()
  }
  
  if (!DBI::dbExistsTable(con, table_name)) {
    cat("📋 Creating table:", table_name, "\n")
    DBI::dbExecute(con, paste0("
      CREATE TABLE ", table_name, " (
        price DOUBLE,
        station_name VARCHAR,
        postal_code VARCHAR,
        city VARCHAR,
        state VARCHAR,
        fuel_type VARCHAR,
        date DATE,
        file_source VARCHAR,
        PRIMARY KEY (station_name, postal_code, state, fuel_type, date)
      )
    "))
    cat("✅ Table created successfully\n\n")
  } else {
    cat("✅ Table already exists:", table_name, "\n\n")
  }
}

parse_txt_file <- function(txt_file, state, fuel_type, date_str, file_source) {
  # Read the file - try different encodings for German characters
  lines <- tryCatch({
    readLines(txt_file, warn = FALSE, encoding = "latin1")
  }, error = function(e) {
    readLines(txt_file, warn = FALSE, encoding = "UTF-8")
  })
  
  # Remove empty lines
  lines <- lines[nchar(trimws(lines)) > 0]
  
  # Parse each line
  results <- list()
  
  for (line in lines) {
    # Format: Price EUR | Station Name, , Postal Code - City
    # Example: 1.488 EUR | Rekord-Tankstelle,  , 7423 - Pinkafeld
    
    # Split by |
    parts <- str_split(line, "\\|", simplify = TRUE)
    if (ncol(parts) < 2) next
    
    # Extract price
    price_part <- trimws(parts[1])
    price <- as.numeric(str_extract(price_part, "[0-9]+\\.[0-9]+"))
    
    # Extract station info
    station_part <- trimws(parts[2])
    
    # Split station part by commas
    station_parts <- str_split(station_part, ",", simplify = TRUE)
    if (ncol(station_parts) < 3) next
    
    station_name <- trimws(station_parts[1])
    
    # Extract postal code and city from last part
    location_part <- trimws(station_parts[3])
    location_match <- str_match(location_part, "^([0-9]+)\\s*-\\s*(.+)$")
    
    if (!is.na(location_match[1])) {
      postal_code <- location_match[2]
      city <- location_match[3]
      
      results[[length(results) + 1]] <- list(
        price = price,
        station_name = station_name,
        postal_code = postal_code,
        city = city,
        state = state,
        fuel_type = fuel_type,
        date = date_str,
        file_source = file_source
      )
    }
  }
  
  # Convert to data frame
  if (length(results) > 0) {
    do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
  } else {
    NULL
  }
}

# ---------- Main Processing ----------

# Connect to DuckDB
con <- connect_duckdb()
cat("📊 Connected to DuckDB:", db_path, "\n\n")

# Create table if not exists
create_table_if_not_exists(table_name)

# Find all extracted folders
extracted_dirs <- fs::dir_ls(data_dir, recurse = TRUE, type = "directory")

cat("📁 Found", length(extracted_dirs), "extracted folders\n")
cat("🔄 Processing files...\n\n")

total_processed <- 0
total_records <- 0
total_skipped <- 0

for (extract_dir in extracted_dirs) {
  folder_name <- basename(extract_dir)
  
  # Parse folder name: Benzin_2025-1-03 or Diesel_2025-1-03
  folder_parts <- str_match(folder_name, "^(Benzin|Diesel)_([0-9]{4}-[0-9]{1,2}-[0-9]{1,2})$")
  
  if (is.na(folder_parts[1])) next
  
  fuel_type <- folder_parts[2]
  date_str <- folder_parts[3]
  
  # Convert date to proper format
  date_obj <- as.Date(date_str, format = "%Y-%m-%d")
  
  # Find all TXT files in this folder
  txt_files <- fs::dir_ls(extract_dir, glob = "*.txt")
  
  for (txt_file in txt_files) {
    state <- tools::file_path_sans_ext(basename(txt_file))
    
    # Parse the file
    tryCatch({
      df <- parse_txt_file(txt_file, state, fuel_type, date_obj, folder_name)
      
      if (!is.null(df) && nrow(df) > 0) {
        # Insert rows one by one with INSERT OR REPLACE
        for (i in 1:nrow(df)) {
          # Check connection validity before each query
          if (!DBI::dbIsValid(con)) {
            cat("🔄 Reconnecting to DuckDB...\n")
            con <<- connect_duckdb()
          }
          
          row <- df[i, ]
          
          insert_sql <- paste0("
            INSERT OR REPLACE INTO ", table_name, " VALUES (?, ?, ?, ?, ?, ?, ?, ?)
          ")
          
          DBI::dbExecute(con, insert_sql, params = list(
            row$price, row$station_name, row$postal_code, row$city,
            row$state, row$fuel_type, row$date, row$file_source
          ))
        }
        
        total_records <- total_records + nrow(df)
        cat("✅ [", total_processed + 1, "/", length(extracted_dirs), "] ", 
            folder_name, " - ", state, ": ", nrow(df), " records\n", sep = "")
      }
    }, error = function(e) {
      cat("⚠️  Error processing", txt_file, ":", e$message, "\n")
      total_skipped <<- total_skipped + 1
    })
  }
  
  total_processed <- total_processed + 1
}

cat("\n")
cat("==================================================\n")
cat("📊 Processing Summary\n")
cat("==================================================\n")
cat("Folders processed:", total_processed, "\n")
cat("Total records inserted:", total_records, "\n")
cat("Files skipped (errors):", total_skipped, "\n")
cat("\n")

# Show database stats
total_db_records <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table_name))$count
cat("📈 Total records in database:", total_db_records, "\n")

unique_dates <- DBI::dbGetQuery(con, paste0("SELECT COUNT(DISTINCT date) as count FROM ", table_name))$count
cat("📅 Unique dates:", unique_dates, "\n")

unique_stations <- DBI::dbGetQuery(con, paste0("SELECT COUNT(DISTINCT station_name || postal_code) as count FROM ", table_name))$count
cat("⛽ Unique stations:", unique_stations, "\n")

cat("\n")

# Disconnect
DBI::dbDisconnect(con, shutdown = TRUE)
cat("✅ Parser completed successfully!\n")


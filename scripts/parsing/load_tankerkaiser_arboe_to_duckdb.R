# Parse Tankerkaiser ARBÖ Data to DuckDB (Fixed Version)
# Based on the working old parser but adapted for ZIP files

library(DBI)
library(duckdb)
library(dplyr)
library(jsonlite)
library(rlang)

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("./")
}

# Database configuration
db_path <- "~/Desktop/MBAT-Internship-Project/databases/arboe_fuel_prices.duckdb"
table_name <- "arboe_prices"
data_dir <- "scraped_data/tankerkaiser-data/data_arboe_extracted/data"

cat("Loading Tankerkaiser ARBÖ data into DuckDB (Fixed Version)\n")
cat("========================================================\n\n")

# Connect to database
con <- dbConnect(duckdb(), db_path)

# Create table schema (matching old parser)
dbExecute(con, "
  CREATE OR REPLACE TABLE arboe_prices (
    id VARCHAR,
    name VARCHAR,
    fuel_s98 REAL,
    fuel_s95 REAL,
    fuel_n REAL,
    fuel_d REAL,
    fuel_g REAL,
    zip VARCHAR,
    land VARCHAR,
    bundesland VARCHAR,
    city VARCHAR,
    city2 VARCHAR,
    strasse VARCHAR,
    date VARCHAR,
    time VARCHAR,
    file_source VARCHAR,
    lon REAL,
    lat REAL,
    PRIMARY KEY (id, file_source, date, time)
  )
")

# Get all GeoJSON files
geojson_files <- list.files(data_dir, pattern = "\\.geojson$", full.names = TRUE)
cat("Found", length(geojson_files), "GeoJSON files to process\n")

# Helper function to extract coordinates (from old parser)
extract_coordinates <- function(geometry) {
  if (!is.list(geometry) || is.null(geometry$coordinates)) {
    return(list(lon = NA_real_, lat = NA_real_))
  }
  
  coords <- geometry$coordinates
  if (!is.numeric(coords[[1]]) || !is.numeric(coords[[2]])) {
    return(list(lon = NA_real_, lat = NA_real_))
  }
  
  list(
    lon = as.numeric(coords[[1]]),
    lat = as.numeric(coords[[2]])
  )
}

# Process each GeoJSON file
total_records <- 0
processed_files <- 0

for (i in 1:length(geojson_files)) {
  file_path <- geojson_files[i]
  file_name <- basename(file_path)
  
  cat("Processing file", i, "of", length(geojson_files), ":", file_name, "\n")
  
  tryCatch({
    # Read and parse GeoJSON directly
    data <- fromJSON(file_path, simplifyVector = FALSE)
    
    if (is.null(data$features) || length(data$features) == 0) {
      cat("  ⚠️ No features found\n")
      next
    }
    
    features <- data$features
    cat("  Found", length(features), "features\n")
    
    # Process each feature (using old parser's approach)
    records <- list()
    record_count <- 0
    
    for (j in 1:length(features)) {
      feature <- features[[j]]
      props <- feature$properties %||% list()
      geom <- feature$geometry %||% list()
      
      coords <- extract_coordinates(geom)
      
      # Extract date (no filtering - parse all data)
      date_str <- props$date %||% NA_character_
      if (is.na(date_str)) {
        next  # Skip records without date
      }
      
      record_count <- record_count + 1
      records[[record_count]] <- list(
        id = props$id %||% NA_character_,
        name = props$name %||% NA_character_,
        fuel_s98 = as.numeric(props$S98 %||% NA),
        fuel_s95 = as.numeric(props$S95 %||% NA),
        fuel_n = as.numeric(props$N %||% NA),
        fuel_d = as.numeric(props$D %||% NA),
        fuel_g = as.numeric(props$G %||% NA),
        zip = props$zip %||% NA_character_,
        land = props$land %||% NA_character_,
        bundesland = props$bundesland %||% NA_character_,
        city = props$city %||% NA_character_,
        city2 = props$city2 %||% NA_character_,
        strasse = props$strasse %||% NA_character_,
        date = date_str,
        time = props$time %||% NA_character_,
        file_source = file_name,
        lon = coords$lon,
        lat = coords$lat
      )
    }
    
    if (record_count > 0) {
      # Convert to dataframe
      df <- do.call(rbind, lapply(records, function(x) {
        data.frame(x, stringsAsFactors = FALSE)
      }))
      
      # Write to database
      dbWriteTable(con, table_name, df, append = TRUE)
      total_records <- total_records + record_count
      cat("  ✅ Added", record_count, "records\n")
    } else {
      cat("  ⚠️ No valid records found\n")
    }
    
    processed_files <- processed_files + 1
    
  }, error = function(e) {
    cat("  ❌ Error:", e$message, "\n")
  })
}

# Show final summary
final_count <- dbGetQuery(con, "SELECT COUNT(*) FROM arboe_prices")[[1]]
cat("\n✅ ARBÖ parsing completed!\n")
cat("Files processed:", processed_files, "\n")
cat("Total records in database:", final_count, "\n")

# Show date range
date_range <- dbGetQuery(con, "
  SELECT 
    MIN(date) as earliest_date,
    MAX(date) as latest_date,
    COUNT(DISTINCT date) as unique_dates
  FROM arboe_prices
")
cat("\n📅 Date Range:\n")
print(date_range)

# Show fuel type summary
fuel_summary <- dbGetQuery(con, "
  SELECT 
    SUM(CASE WHEN fuel_d > 0 THEN 1 ELSE 0 END) as diesel_records,
    SUM(CASE WHEN fuel_s95 > 0 THEN 1 ELSE 0 END) as gasoline_95_records,
    SUM(CASE WHEN fuel_s98 > 0 THEN 1 ELSE 0 END) as gasoline_98_records
  FROM arboe_prices
")
cat("\n⛽ Fuel Type Summary:\n")
print(fuel_summary)

dbDisconnect(con, shutdown = TRUE)
cat("\nDatabase saved to:", db_path, "\n")


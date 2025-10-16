# Parse Tankerkaiser ARBÖ Data to DuckDB
# This script parses the comprehensive ARBÖ GeoJSON files from tankerkaiser-data

library(DBI)
library(duckdb)
library(dplyr)
library(jsonlite)

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("./")
}

# Database configuration
db_path <- "~/Desktop/MBAT-Internship-Project/databases/arboe_fuel_prices.duckdb"
table_name <- "arboe_prices"
data_dir <- "scraped_data/tankerkaiser-data/data_arboe"

cat("Loading Tankerkaiser ARBÖ data into DuckDB\n")
cat("==========================================\n\n")

# Connect to database
con <- dbConnect(duckdb(), db_path)

# Create table schema
dbExecute(con, "
  CREATE OR REPLACE TABLE arboe_prices (
    id VARCHAR,
    name VARCHAR,
    fuel_s98 DOUBLE,
    fuel_s95 DOUBLE,
    fuel_n DOUBLE,
    fuel_d DOUBLE,
    fuel_g DOUBLE,
    zip VARCHAR,
    land VARCHAR,
    bundesland VARCHAR,
    city VARCHAR,
    city2 VARCHAR,
    strasse VARCHAR,
    date DATE,
    time TIME,
    file_source VARCHAR,
    lon DOUBLE,
    lat DOUBLE,
    scraped_at TIMESTAMP
  )
")

# Get list of files
files <- list.files(data_dir, pattern = "\\.zip$", full.names = TRUE)
cat("Found", length(files), "ARBÖ files to process\n\n")

# Initialize counters
processed_files <- 0
total_records <- 0
errors <- 0

# Process files
for (i in 1:length(files)) {
  file_path <- files[i]
  file_name <- basename(file_path)
  
  # Extract date from filename
  date_match <- regmatches(file_name, regexpr("\\d{4}-\\d{2}-\\d{2}", file_name))
  if (length(date_match) > 0) {
    file_date <- as.Date(date_match[1])
  } else {
    file_date <- as.Date("2024-01-01")  # Default date
  }
  
  # Extract time from filename
  time_match <- regmatches(file_name, regexpr("\\d{2}:\\d{2}", file_name))
  if (length(time_match) > 0) {
    file_time <- paste0(time_match[1], ":00")
  } else {
    file_time <- "12:00:00"  # Default time
  }
  
  cat("Processing file", i, "of", length(files), ":", file_name, "\n")
  
  tryCatch({
    # Extract zip file
    temp_dir <- tempdir()
    unzip(file_path, exdir = temp_dir, overwrite = TRUE)
    extracted_files <- list.files(temp_dir, pattern = "\\.geojson$", full.names = TRUE, recursive = TRUE)
    
    if (length(extracted_files) > 0) {
      # Read and parse GeoJSON with error handling for malformed JSON
      geojson_text <- readLines(extracted_files[1], warn = FALSE)
      geojson_text <- iconv(geojson_text, from = "UTF-8", to = "UTF-8", sub = "")  # Remove invalid UTF-8
      geojson_data <- fromJSON(paste(geojson_text, collapse = "\n"), simplifyVector = FALSE)
      
      if ("features" %in% names(geojson_data) && length(geojson_data$features) > 0) {
        # Extract features data
        features <- geojson_data$features
        
        # Create data frame
        records <- data.frame(
          id = rep(NA, nrow(features)),
          name = rep(NA, nrow(features)),
          fuel_s98 = rep(NA, nrow(features)),
          fuel_s95 = rep(NA, nrow(features)),
          fuel_n = rep(NA, nrow(features)),
          fuel_d = rep(NA, nrow(features)),
          fuel_g = rep(NA, nrow(features)),
          zip = rep(NA, nrow(features)),
          land = rep(NA, nrow(features)),
          bundesland = rep(NA, nrow(features)),
          city = rep(NA, nrow(features)),
          city2 = rep(NA, nrow(features)),
          strasse = rep(NA, nrow(features)),
          date = rep(file_date, nrow(features)),
          time = rep(file_time, nrow(features)),
          file_source = rep(file_name, nrow(features)),
          lon = rep(NA, nrow(features)),
          lat = rep(NA, nrow(features)),
          scraped_at = rep(Sys.time(), nrow(features))
        )
        
        # Extract properties from each feature
        for (j in 1:nrow(features)) {
          props <- features$properties[j, ]
          
          # Map properties to our schema
          if ("id" %in% names(props)) records$id[j] <- props$id
          if ("name" %in% names(props)) records$name[j] <- props$name
          if ("fuel_s98" %in% names(props)) records$fuel_s98[j] <- as.numeric(props$fuel_s98)
          if ("fuel_s95" %in% names(props)) records$fuel_s95[j] <- as.numeric(props$fuel_s95)
          if ("fuel_n" %in% names(props)) records$fuel_n[j] <- as.numeric(props$fuel_n)
          if ("fuel_d" %in% names(props)) records$fuel_d[j] <- as.numeric(props$fuel_d)
          if ("fuel_g" %in% names(props)) records$fuel_g[j] <- as.numeric(props$fuel_g)
          if ("zip" %in% names(props)) records$zip[j] <- props$zip
          if ("land" %in% names(props)) records$land[j] <- props$land
          if ("bundesland" %in% names(props)) records$bundesland[j] <- props$bundesland
          if ("city" %in% names(props)) records$city[j] <- props$city
          if ("city2" %in% names(props)) records$city2[j] <- props$city2
          if ("strasse" %in% names(props)) records$strasse[j] <- props$strasse
          
          # Extract coordinates from geometry (using old parser's approach)
          if ("geometry" %in% names(features)) {
            geom <- features$geometry[[j]]
            if (!is.null(geom) && "coordinates" %in% names(geom)) {
              coords <- geom$coordinates
              if (is.numeric(coords[[1]]) && is.numeric(coords[[2]])) {
                records$lon[j] <- as.numeric(coords[[1]])
                records$lat[j] <- as.numeric(coords[[2]])
              }
            }
          }
        }
        
        # Extract actual date from data properties instead of filename
        # Update records with actual dates from the data
        for (j in 1:nrow(records)) {
          props <- features$properties[j, ]
          if ("date" %in% names(props) && !is.null(props$date)) {
            # Parse date format like "16.9.2025" to proper date
            tryCatch({
              actual_date <- as.Date(props$date, format = "%d.%m.%Y")
              if (!is.na(actual_date)) {
                records$date[j] <- actual_date
              }
            }, error = function(e) {
              # Keep filename date if parsing fails
            })
          }
        }
        
        # Filter out records with no valid data and only keep data from 2025-01-01 onwards
        valid_records <- records[(!is.na(records$id) | !is.na(records$name)) & 
                                records$date >= as.Date("2025-01-01"), ]
        
        if (nrow(valid_records) > 0) {
          # Write to database
          dbWriteTable(con, table_name, valid_records, append = TRUE)
          total_records <- total_records + nrow(valid_records)
          cat("  ✅ Added", nrow(valid_records), "records\n")
        } else {
          cat("  ⚠️  No valid records found\n")
        }
        
        # Clean up extracted files
        unlink(extracted_files)
      } else {
        cat("  ⚠️  No features found in GeoJSON\n")
      }
    } else {
      cat("  ⚠️  No GeoJSON file found in zip\n")
    }
    
    processed_files <- processed_files + 1
    
  }, error = function(e) {
    cat("  ❌ Error processing file:", e$message, "\n")
    errors <- errors + 1
  })
  
  # Progress update every 100 files
  if (i %% 100 == 0) {
    cat("Progress:", i, "/", length(files), "files processed\n")
    cat("Total records so far:", format(total_records, big.mark = ","), "\n\n")
  }
}

# Final summary
cat("\n📊 Processing Summary\n")
cat("====================\n")
cat("Files processed:", processed_files, "/", length(files), "\n")
cat("Total records added:", format(total_records, big.mark = ","), "\n")
cat("Errors encountered:", errors, "\n")

# Show sample data
cat("\n🔍 Sample Data\n")
cat("=============\n")
sample_data <- dbGetQuery(con, paste("SELECT * FROM", table_name, "LIMIT 5"))
print(sample_data)

# Show data summary
cat("\n📈 Data Summary\n")
cat("==============\n")
summary_stats <- dbGetQuery(con, paste("
  SELECT 
    COUNT(*) as total_records,
    COUNT(DISTINCT id) as unique_stations,
    MIN(date) as earliest_date,
    MAX(date) as latest_date,
    COUNT(DISTINCT bundesland) as states_covered
  FROM", table_name
))
print(summary_stats)

dbDisconnect(con, shutdown = TRUE)

cat("\n✅ ARBÖ data parsing completed!\n")
cat("Database saved to:", db_path, "\n")

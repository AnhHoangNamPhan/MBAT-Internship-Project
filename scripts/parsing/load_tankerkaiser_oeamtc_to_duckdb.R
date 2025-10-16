# Parse Tankerkaiser ÖAMTC Data to DuckDB
# This script parses the comprehensive ÖAMTC JSON files from tankerkaiser-data

library(DBI)
library(duckdb)
library(dplyr)
library(jsonlite)

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("./")
}

# Database configuration
db_path <- "~/Desktop/MBAT-Internship-Project/databases/oeamtc_fuel_data.duckdb"
table_name <- "oeamtc_fuel"
data_dir <- "scraped_data/tankerkaiser-data/data_oeamtc"

cat("Loading Tankerkaiser ÖAMTC data into DuckDB\n")
cat("==========================================\n\n")

# Connect to database
con <- dbConnect(duckdb(), db_path)

# Create table schema
dbExecute(con, "
  CREATE OR REPLACE TABLE oeamtc_fuel (
    station_id VARCHAR,
    station_name VARCHAR,
    fuel_type VARCHAR,
    price DOUBLE,
    latitude DOUBLE,
    longitude DOUBLE,
    city VARCHAR,
    zip VARCHAR,
    address VARCHAR,
    phone VARCHAR,
    website VARCHAR,
    operator VARCHAR,
    operation_mode VARCHAR,
    date TIMESTAMP,
    file_source VARCHAR,
    scraped_at TIMESTAMP
  )
")

# Get list of files
files <- list.files(data_dir, pattern = "\\.json\\.gz$", full.names = TRUE)
cat("Found", length(files), "ÖAMTC files to process\n\n")

# Initialize counters
processed_files <- 0
total_records <- 0
errors <- 0

# Process files
for (i in 1:length(files)) {
  file_path <- files[i]
  file_name <- basename(file_path)
  
  # Extract date and time from filename
  date_match <- regmatches(file_name, regexpr("\\d{4}-\\d{2}-\\d{2}", file_name))
  time_match <- regmatches(file_name, regexpr("\\d{2}-\\d{2}", file_name))
  
  if (length(date_match) > 0 && length(time_match) > 0) {
    file_datetime <- as.POSIXct(paste(date_match[1], time_match[1]), format = "%Y-%m-%d %H-%M")
  } else {
    file_datetime <- Sys.time()  # Default timestamp
  }
  
  cat("Processing file", i, "of", length(files), ":", file_name, "\n")
  
  tryCatch({
    # Read gzipped JSON file
    con_file <- gzfile(file_path, "r")
    json_content <- readLines(con_file, n = 1)
    close(con_file)
    
    # Parse JSON
    json_data <- fromJSON(json_content)
    
    if ("results" %in% names(json_data) && length(json_data$results) > 0) {
      results <- json_data$results
      
      # Initialize records list
      all_records <- list()
      record_count <- 0
      
      # Process each result
      for (j in 1:length(results)) {
        result <- results[[j]]
        
        if ("data" %in% names(result)) {
          data <- result$data
          
          # Extract basic station info
          station_id <- if ("header" %in% names(data) && length(data$header) > 0) data$header[[1]] else NA
          
          # Extract coordinates
          lat <- NA
          lon <- NA
          if ("geoData" %in% names(data) && length(data$geoData) > 0) {
            geo_data <- data$geoData[[1]]
            if ("latitude" %in% names(geo_data)) lat <- as.numeric(geo_data$latitude)
            if ("longitude" %in% names(geo_data)) lon <- as.numeric(geo_data$longitude)
          }
          
          # Extract operator info
          operator <- NA
          phone <- NA
          website <- NA
          if ("operator" %in% names(data) && length(data$operator) > 0) {
            op_data <- data$operator[[1]]
            if (length(op_data) > 0) operator <- op_data[1]  # First element is usually the name
            if (length(op_data) > 2) phone <- op_data[3]    # Third element is often phone
            if (length(op_data) > 3) website <- op_data[4]  # Fourth element is often website
          }
          
          # Extract operation mode
          operation_mode <- NA
          if ("operationMode" %in% names(data)) {
            operation_mode <- data$operationMode
          }
          
          # Extract prices
          if ("prices" %in% names(data) && "prices" %in% names(data$prices)) {
            prices_list <- data$prices$prices[[1]]
            
            # prices_list is a list of price objects, each with fuel, price, etc.
            for (k in 1:length(prices_list)) {
              price_obj <- prices_list[[k]]
              
              if ("fuel" %in% names(price_obj) && "price" %in% names(price_obj)) {
                fuel_type_raw <- price_obj$fuel
                price_value <- price_obj$price
                
                # Only process if we have both fuel type and price
                if (!is.na(fuel_type_raw) && !is.na(price_value) && price_value > 0) {
                  record_count <- record_count + 1
                  
                  # Map fuel types to our standard names
                  fuel_type <- switch(
                    fuel_type_raw,
                    "DIESEL" = "diesel",
                    "GASOLINE_SUPER" = "gasoline_95",
                    "GASOLINE_SUPER_PLUS" = "gasoline_98",
                    fuel_type_raw  # Keep original if no mapping
                  )
                  
                  all_records[[record_count]] <- data.frame(
                    station_id = station_id,
                    station_name = operator,
                    fuel_type = fuel_type,
                    price = as.numeric(price_value),
                    latitude = lat,
                    longitude = lon,
                    city = NA,  # Not available in this data structure
                    zip = NA,   # Not available in this data structure
                    address = NA,  # Not available in this data structure
                    phone = phone,
                    website = website,
                    operator = operator,
                    operation_mode = operation_mode,
                    date = file_datetime,
                    file_source = file_name,
                    scraped_at = Sys.time(),
                    stringsAsFactors = FALSE
                  )
                }
              }
            }
          }
        }
      }
      
      # Combine all records and write to database
      if (record_count > 0) {
        records_df <- do.call(rbind, all_records)
        
        # Filter out records with no coordinates and only keep data from 2025-01-01 onwards
        valid_records <- records_df[!is.na(records_df$latitude) & !is.na(records_df$longitude) & 
                                   records_df$last_updated >= as.POSIXct("2025-01-01 00:00:00", tz = "UTC"), ]
        
        if (nrow(valid_records) > 0) {
          dbWriteTable(con, table_name, valid_records, append = TRUE)
          total_records <- total_records + nrow(valid_records)
          cat("  Added", nrow(valid_records), "records\n")
        } else {
          cat("  No valid records with coordinates found\n")
        }
      } else {
        cat("  No price data found\n")
      }
      
    } else {
      cat("  No results found in JSON\n")
    }
    
    processed_files <- processed_files + 1
    
  }, error = function(e) {
    cat("  ❌ Error processing file:", e$message, "\n")
    errors <- errors + 1
  })
  
  # Progress update every 50 files
  if (i %% 50 == 0) {
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
    COUNT(DISTINCT station_id) as unique_stations,
    COUNT(DISTINCT fuel_type) as fuel_types,
    MIN(date) as earliest_date,
    MAX(date) as latest_date
  FROM", table_name
))
print(summary_stats)

# Show fuel type distribution
cat("\n⛽ Fuel Type Distribution\n")
cat("========================\n")
fuel_dist <- dbGetQuery(con, paste("
  SELECT fuel_type, COUNT(*) as count
  FROM", table_name, "
  GROUP BY fuel_type
  ORDER BY count DESC
"))
print(fuel_dist)

dbDisconnect(con, shutdown = TRUE)

cat("\n✅ ÖAMTC data parsing completed!\n")
cat("Database saved to:", db_path, "\n")

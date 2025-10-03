#!/usr/bin/env Rscript

# =============================================================================
# Italian Fuel Data Parser - FAST BULK LOADING VERSION
# Optimized for speed using batch inserts instead of row-by-row
# =============================================================================

library(jsonlite)
library(dplyr)
library(duckdb)
library(DBI)
library(fs)

# Define %||% operator for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

cat("==================================================\n")
cat("🇮🇹 Italian Fuel Data Parser (Fast Version)\n")
cat("==================================================\n\n")

# ---------- Configuration ----------
json_dir   <- "~/Desktop/MBAT-Internship-Project/scraped_data/data_ita"
db_path    <- "~/Desktop/MBAT-Internship-Project/databases/italian_fuel_data.duckdb"
table_name <- "italian_fuel"

# ---------- Database Connection ----------
con <- DBI::dbConnect(duckdb::duckdb(dbdir = path.expand(db_path)))

# Create table with PRIMARY KEY
if (!DBI::dbExistsTable(con, table_name)) {
  cat("📋 Creating table:", table_name, "\n")
  DBI::dbExecute(con, paste0("
    CREATE TABLE ", table_name, " (
      station_id INTEGER,
      station_name VARCHAR,
      station_address VARCHAR,
      station_brand VARCHAR,
      fuel_id INTEGER,
      fuel_name VARCHAR,
      fuel_price DOUBLE,
      fuel_is_self BOOLEAN,
      insert_date VARCHAR,
      distance DOUBLE,
      region_number INTEGER,
      center_lat DOUBLE,
      center_lng DOUBLE,
      station_lat DOUBLE,
      station_lng DOUBLE,
      file_source VARCHAR,
      PRIMARY KEY (station_id, fuel_id, insert_date, file_source)
    )
  "))
  cat("✅ Table created\n\n")
} else {
  cat("✅ Table already exists\n\n")
}

# ---------- Find Files ----------
fuel_files <- fs::dir_ls(path.expand(json_dir), regexp = "fuel_region-.*\\.json(\\.gz)?$")

cat("📁 Found", length(fuel_files), "files to process\n")
cat("🚀 Using FAST bulk loading method\n\n")

total_files <- 0
total_records <- 0

# Process files in batches
for (file in fuel_files) {
  tryCatch({
    # Read JSON (handles both .json and .json.gz)
    raw <- jsonlite::fromJSON(file, simplifyVector = FALSE)
    
    if (is.null(raw$results) || length(raw$results) == 0) {
      next
    }
    
    current_file <- basename(file)
    
    # Extract all data into a list
    all_rows <- list()
    
    for (station in raw$results) {
      if (is.null(station$fuel) || length(station$fuel) == 0) next
      
      for (fuel in station$fuel) {
        all_rows[[length(all_rows) + 1]] <- data.frame(
          station_id = as.integer(station$id %||% NA),
          station_name = station$name %||% "",
          station_address = station$address %||% "",
          station_brand = station$brand %||% "",
          fuel_id = as.integer(fuel$id %||% NA),
          fuel_name = fuel$name %||% "",
          fuel_price = as.numeric(fuel$price %||% NA),
          fuel_is_self = as.logical(fuel$isSelf %||% FALSE),
          insert_date = station$insertDate %||% "",
          distance = as.numeric(station$distance %||% NA),
          region_number = as.integer(gsub(".*region-(\\d+).*", "\\1", current_file)),
          center_lat = as.numeric(raw$center$lat %||% NA),
          center_lng = as.numeric(raw$center$lng %||% NA),
          station_lat = as.numeric(station$lat %||% NA),
          station_lng = as.numeric(station$lng %||% NA),
          file_source = current_file,
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (length(all_rows) > 0) {
      # Combine all rows into one dataframe
      df <- bind_rows(all_rows)
      
      # Bulk insert using temporary table
      temp_table <- paste0("temp_italian_", gsub("[^0-9]", "", current_file))
      
      DBI::dbWriteTable(con, temp_table, df, temporary = TRUE, overwrite = TRUE)
      
      # INSERT OR REPLACE from temp table
      DBI::dbExecute(con, paste0("
        INSERT OR REPLACE INTO ", table_name, "
        SELECT * FROM ", temp_table, "
      "))
      
      # Drop temp table
      DBI::dbExecute(con, paste0("DROP TABLE ", temp_table))
      
      total_records <- total_records + nrow(df)
      total_files <- total_files + 1
      
      cat("✅ [", total_files, "/", length(fuel_files), "] ", current_file, 
          ": ", format(nrow(df), big.mark = ","), " records (Total: ",
          format(total_records, big.mark = ","), ")\n", sep = "")
    }
    
  }, error = function(e) {
    cat("⚠️  Error processing", basename(file), ":", e$message, "\n")
  })
}

cat("\n")
cat("==================================================\n")
cat("📊 Processing Summary\n")
cat("==================================================\n")
cat("Files processed:", total_files, "\n")
cat("Total records inserted:", format(total_records, big.mark = ","), "\n\n")

# Final database stats
final_count <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table_name))$count
cat("📈 Total records in database:", format(final_count, big.mark = ","), "\n")

unique_stations <- DBI::dbGetQuery(con, paste0("SELECT COUNT(DISTINCT station_id) as count FROM ", table_name))$count
cat("🏪 Unique stations:", format(unique_stations, big.mark = ","), "\n")

unique_dates <- DBI::dbGetQuery(con, paste0("SELECT COUNT(DISTINCT insert_date) as count FROM ", table_name))$count
cat("📅 Unique dates:", unique_dates, "\n")

cat("\n")

# Disconnect
DBI::dbDisconnect(con, shutdown = TRUE)
cat("✅ Parser completed successfully!\n")


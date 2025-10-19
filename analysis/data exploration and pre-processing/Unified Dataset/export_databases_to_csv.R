#!/usr/bin/env Rscript

# Export All Databases to CSV and Delete to Free Space
# This script exports all individual country databases to CSV files
# then deletes the databases to free up disk space

library(DBI)
library(duckdb)

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("../../")
}

cat("Exporting databases to CSV and freeing disk space\n")
cat("================================================\n\n")

# Database paths and their CSV export paths
databases <- list(
  list(
    name = "German",
    db_path = "databases/german_fuel_data.duckdb",
    csv_path = "data/german_fuel_prices.csv",
    tables = c("german_prices", "german_stations")
  ),
  list(
    name = "ARBÖ",
    db_path = "databases/arboe_fuel_prices.duckdb", 
    csv_path = "data/arboe_fuel_prices.csv",
    tables = c("arboe_prices")
  ),
  list(
    name = "ÖAMTC",
    db_path = "databases/oeamtc_fuel_data.duckdb",
    csv_path = "data/oeamtc_fuel_prices.csv", 
    tables = c("oeamtc_fuel")
  ),
  list(
    name = "JET Austria",
    db_path = "databases/jet_austria_fuel_data.duckdb",
    csv_path = "data/jet_austria_fuel_prices.csv",
    tables = c("jet_austria_fuel")
  ),
  list(
    name = "OMV Austria", 
    db_path = "databases/omv_austria_fuel_data.duckdb",
    csv_path = "data/omv_austria_fuel_prices.csv",
    tables = c("omv_austria_fuel")
  ),
  list(
    name = "Italian",
    db_path = "databases/italian_fuel_data.duckdb",
    csv_path = "data/italian_fuel_prices.csv",
    tables = c("italian_fuel")
  ),
  list(
    name = "Slovenian",
    db_path = "databases/slovenian_fuel_data.duckdb", 
    csv_path = "data/slovenian_fuel_prices.csv",
    tables = c("slovenian_fuel")
  )
)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

total_space_freed <- 0

for (db_info in databases) {
  cat("Processing", db_info$name, "database...\n")
  
  # Check if database exists
  if (!file.exists(db_info$db_path)) {
    cat("  ⚠️ Database not found:", db_info$db_path, "\n")
    next
  }
  
  # Get database size before export
  db_size <- file.size(db_info$db_path)
  cat("  Database size:", round(db_size / (1024^3), 2), "GB\n")
  
  tryCatch({
    # Connect to database
    con <- dbConnect(duckdb(), db_info$db_path, read_only = TRUE)
    
    # Export each table
    for (table in db_info$tables) {
      # Check if table exists
      tables <- dbListTables(con)
      if (!table %in% tables) {
        cat("  ⚠️ Table", table, "not found\n")
        next
      }
      
      # Get record count
      count <- dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", table))[[1]]
      cat("  Table", table, ":", format(count, big.mark = ","), "records\n")
      
      # Export to CSV
      csv_file <- if (length(db_info$tables) == 1) {
        db_info$csv_path
      } else {
        gsub("\\.csv$", paste0("_", table, ".csv"), db_info$csv_path)
      }
      
      dbExecute(con, paste0("
        COPY ", table, " 
        TO '", csv_file, "' 
        (HEADER, DELIMITER ',')
      "))
      
      # Get CSV size
      csv_size <- file.size(csv_file)
      cat("  Exported to:", csv_file, "(", round(csv_size / (1024^2), 1), "MB)\n")
    }
    
    dbDisconnect(con, shutdown = TRUE)
    
    # Delete database to free space
    file.remove(db_info$db_path)
    total_space_freed <- total_space_freed + db_size
    cat("  ✅ Database deleted, freed", round(db_size / (1024^3), 2), "GB\n")
    
  }, error = function(e) {
    cat("  ❌ Error:", e$message, "\n")
  })
  
  cat("\n")
}

cat("================================================\n")
cat("✅ Export and cleanup complete!\n")
cat("Total space freed:", round(total_space_freed / (1024^3), 2), "GB\n")
cat("All databases exported to CSV files in data/ folder\n")
cat("Original databases deleted to free disk space\n")


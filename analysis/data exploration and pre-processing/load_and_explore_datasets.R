# Load and Explore Fuel Price Datasets
# This script loads and explores datasets summary statistics from different countries
# Focus: German data for model development, Austrian data combination, other countries overview

library(DBI)
library(duckdb)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# === Configuration ===
db_path <- "/Users/alexphan/Desktop/MBAT-Internship-Project/databases/german_fuel_data.duckdb"

# Connect to database
con <- dbConnect(duckdb(), db_path, read_only = TRUE)

# === 1. GERMAN DATASET EXPLORATION ===

# Check German tables
german_tables <- dbListTables(con)
german_tables <- german_tables[grepl("german", german_tables)]
cat("German tables:", paste(german_tables, collapse = ", "), "\n")

# German stations overview
german_stations_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM german_stations")$count
cat("German stations:", format(german_stations_count, big.mark = ","), "\n")

# German prices overview
german_prices_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM german_prices")$count
cat("German prices:", format(german_prices_count, big.mark = ","), "\n")

# German date range
german_date_range <- dbGetQuery(con, "
  SELECT 
    MIN(date) as min_date,
    MAX(date) as max_date,
    COUNT(DISTINCT DATE(date)) as unique_days
  FROM german_prices
")
cat("German date range:", german_date_range$min_date, "to", german_date_range$max_date, "\n")
cat("Unique days:", german_date_range$unique_days, "\n")

# German fuel types
german_fuel_types <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as total_records,
    COUNT(CASE WHEN diesel > 0 THEN 1 END) as diesel_records,
    COUNT(CASE WHEN e5 > 0 THEN 1 END) as e5_records,
    COUNT(CASE WHEN e10 > 0 THEN 1 END) as e10_records
  FROM german_prices
")
cat("German fuel type coverage:\n")
cat("  - Diesel:", format(german_fuel_types$diesel_records, big.mark = ","), "records\n")
cat("  - E5:", format(german_fuel_types$e5_records, big.mark = ","), "records\n")
cat("  - E10:", format(german_fuel_types$e10_records, big.mark = ","), "records\n")

# German brand analysis
german_brands <- dbGetQuery(con, "
  SELECT 
    brand,
    COUNT(*) as station_count
  FROM german_stations
  WHERE brand IS NOT NULL AND brand != ''
  GROUP BY brand
  ORDER BY station_count DESC
  LIMIT 10
")
cat("Top 10 German brands:\n")
for(i in 1:nrow(german_brands)) {
  cat("  ", i, ".", german_brands$brand[i], ":", german_brands$station_count[i], "stations\n")
}

# === 2. AUSTRIAN DATASET EXPLORATION ===

# Check Austrian tables
austrian_tables <- dbListTables(con)
austrian_tables <- austrian_tables[grepl("austria|at_", austrian_tables, ignore.case = TRUE)]
if(length(austrian_tables) > 0) {
  cat("Austrian tables:", paste(austrian_tables, collapse = ", "), "\n")
  
  # Explore each Austrian table
  for(table in austrian_tables) {
    cat("\nTable:", table, "\n")
    tryCatch({
      count <- dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table))$count
      cat("  Records:", format(count, big.mark = ","), "\n")
      
      # Check date range if date column exists
      columns <- dbListFields(con, table)
      if("date" %in% columns) {
        date_range <- dbGetQuery(con, paste0("
          SELECT MIN(date) as min_date, MAX(date) as max_date
          FROM ", table, "
          WHERE date IS NOT NULL
        "))
        if(!is.na(date_range$min_date)) {
          cat("  Date range:", date_range$min_date, "to", date_range$max_date, "\n")
        }
      }
    }, error = function(e) {
      cat("  Error accessing table:", e$message, "\n")
    })
  }
}

# === 3. ITALIAN DATASET EXPLORATION ===

italian_tables <- dbListTables(con)
italian_tables <- italian_tables[grepl("italy|ita|italian", italian_tables, ignore.case = TRUE)]
if(length(italian_tables) > 0) {
  cat("Italian tables:", paste(italian_tables, collapse = ", "), "\n")
  
  for(table in italian_tables) {
    cat("\nTable:", table, "\n")
    tryCatch({
      count <- dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table))$count
      cat("  Records:", format(count, big.mark = ","), "\n")
    }, error = function(e) {
      cat("  Error accessing table:", e$message, "\n")
    })
  }
} else {
  cat("No Italian tables found in database\n")
}

cat("\n")

# === 4. SLOVENIAN DATASET EXPLORATION ===

slovenian_tables <- dbListTables(con)
slovenian_tables <- slovenian_tables[grepl("slovenia|slo|slovenian", slovenian_tables, ignore.case = TRUE)]
if(length(slovenian_tables) > 0) {
  cat("Slovenian tables:", paste(slovenian_tables, collapse = ", "), "\n")
  
  for(table in slovenian_tables) {
    cat("\nTable:", table, "\n")
    tryCatch({
      count <- dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table))$count
      cat("  Records:", format(count, big.mark = ","), "\n")
    }, error = function(e) {
      cat("  Error accessing table:", e$message, "\n")
    })
  }
} else {
  cat("No Slovenian tables found in database\n")
}

cat("\n")

# === 5. DATA QUALITY SUMMARY ===

# Overall database statistics
all_tables <- dbListTables(con)
cat("Total tables in database:", length(all_tables), "\n")
cat("Tables:", paste(all_tables, collapse = ", "), "\n")

# Check for data completeness
cat("\nData completeness check:\n")
for(table in all_tables) {
  tryCatch({
    count <- dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table))$count
    cat("  ", table, ":", format(count, big.mark = ","), "records\n")
  }, error = function(e) {
    cat("  ", table, ": Error -", e$message, "\n")
  })
}

cat("\n")

# === 6. Dataset Analysis Summary ===
# German dataset: Primary choice for model development
#   - Largest dataset with comprehensive coverage (96M+ price records, 17K+ stations)
#   - Good temporal coverage and data quality (Dec 2024 - Sep 2025)
#   - Multiple fuel types available (Diesel, E5, E10)
#   - Major brands: ARAL, Shell, Esso, TotalEnergies, BP, OMV
#
# Austrian dataset: Secondary for model validation
#   - Multiple sources available (ARBÖ, OMV, Shell, BP)
#   - Need to combine and check for duplicates
#   - Limited temporal coverage (2-3 months)
#   - Good for transfer learning experiments
#
# Italian and Slovenian datasets: Limited use
#   - Use for transfer learning experiments only
#   - Insufficient data for standalone models
#   - Good for testing model generalization
#
# Next steps:
#   - Run german_dataset_deeper_exploration.R for detailed analysis
#   - Combine Austrian datasets and check quality
#   - Develop German model first, then transfer learning

# Close database connection
dbDisconnect(con)
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

# === 2.1 AUSTRIAN STATION OVERLAP ANALYSIS ===
cat("\n=== AUSTRIAN STATION OVERLAP ANALYSIS ===\n")

# Connect to Austrian databases
arboe_db <- "/Users/alexphan/Desktop/MBAT-Internship-Project/databases/arboe_fuel_prices.duckdb"
oeamtc_db <- "/Users/alexphan/Desktop/MBAT-Internship-Project/databases/oeamtc_fuel_data.duckdb"
jet_db <- "/Users/alexphan/Desktop/MBAT-Internship-Project/databases/jet_austria_fuel_data.duckdb"
omv_db <- "/Users/alexphan/Desktop/MBAT-Internship-Project/databases/omv_austria_fuel_data.duckdb"

# Check if databases exist and analyze overlaps
if(file.exists(arboe_db) && file.exists(oeamtc_db) && file.exists(jet_db) && file.exists(omv_db)) {
  
  cat("Analyzing station overlaps between Austrian sources...\n")
  
  # Connect to all Austrian databases
  con_arboe <- dbConnect(duckdb(), arboe_db, read_only = TRUE)
  con_oeamtc <- dbConnect(duckdb(), oeamtc_db, read_only = TRUE)
  con_jet <- dbConnect(duckdb(), jet_db, read_only = TRUE)
  con_omv <- dbConnect(duckdb(), omv_db, read_only = TRUE)
  
  # Get station data with coordinates (rounded to 4 decimal places for precision)
  cat("Loading station coordinates (rounded to 4 decimal places)...\n")
  
  arboe_stations <- dbGetQuery(con_arboe, "
    SELECT DISTINCT 
      id as station_id,
      name as station_name,
      ROUND(lat, 4) as lat_rounded,
      ROUND(lon, 4) as lon_rounded,
      lat as lat_original,
      lon as lon_original,
      city,
      strasse as street
    FROM arboe_prices 
    WHERE lat IS NOT NULL AND lon IS NOT NULL
    ORDER BY lat_rounded, lon_rounded
  ")
  
  oeamtc_stations <- dbGetQuery(con_oeamtc, "
    SELECT DISTINCT 
      station_id,
      station_name,
      ROUND(station_lat, 4) as lat_rounded,
      ROUND(station_lng, 4) as lon_rounded,
      station_lat as lat_original,
      station_lng as lon_original,
      station_address as street
    FROM oeamtc_fuel 
    WHERE station_lat IS NOT NULL AND station_lng IS NOT NULL
    ORDER BY lat_rounded, lon_rounded
  ")
  
  jet_stations <- dbGetQuery(con_jet, "
    SELECT DISTINCT 
      station_id,
      station_name,
      ROUND(lat, 4) as lat_rounded,
      ROUND(lng, 4) as lon_rounded,
      lat as lat_original,
      lng as lon_original,
      city,
      street
    FROM jet_austria_fuel 
    WHERE lat IS NOT NULL AND lng IS NOT NULL
    ORDER BY lat_rounded, lon_rounded
  ")
  
  omv_stations <- dbGetQuery(con_omv, "
    SELECT DISTINCT 
      station_id,
      town as station_name,
      ROUND(lat, 4) as lat_rounded,
      ROUND(lon, 4) as lon_rounded,
      lat as lat_original,
      lon as lon_original,
      postcode
    FROM omv_austria_fuel 
    WHERE lat IS NOT NULL AND lon IS NOT NULL
    ORDER BY lat_rounded, lon_rounded
  ")
  
  cat("Station counts:\n")
  cat("  ARBÖ:", nrow(arboe_stations), "unique stations\n")
  cat("  ÖAMTC:", nrow(oeamtc_stations), "unique stations\n")
  cat("  JET:", nrow(jet_stations), "unique stations\n")
  cat("  OMV:", nrow(omv_stations), "unique stations\n")
  
  # Create coordinate keys for overlap detection
  arboe_coords <- paste(arboe_stations$lat_rounded, arboe_stations$lon_rounded, sep=",")
  oeamtc_coords <- paste(oeamtc_stations$lat_rounded, oeamtc_stations$lon_rounded, sep=",")
  jet_coords <- paste(jet_stations$lat_rounded, jet_stations$lon_rounded, sep=",")
  omv_coords <- paste(omv_stations$lat_rounded, omv_stations$lon_rounded, sep=",")
  
  # Find exact coordinate overlaps (4 decimal precision)
  cat("\nExact coordinate overlaps (4 decimal precision):\n")
  
  arboe_oeamtc_exact <- intersect(arboe_coords, oeamtc_coords)
  arboe_jet_exact <- intersect(arboe_coords, jet_coords)
  arboe_omv_exact <- intersect(arboe_coords, omv_coords)
  oeamtc_jet_exact <- intersect(oeamtc_coords, jet_coords)
  oeamtc_omv_exact <- intersect(oeamtc_coords, omv_coords)
  jet_omv_exact <- intersect(jet_coords, omv_coords)
  
  cat("  ARBÖ ∩ ÖAMTC:", length(arboe_oeamtc_exact), "overlapping coordinates\n")
  cat("  ARBÖ ∩ JET:", length(arboe_jet_exact), "overlapping coordinates\n")
  cat("  ARBÖ ∩ OMV:", length(arboe_omv_exact), "overlapping coordinates\n")
  cat("  ÖAMTC ∩ JET:", length(oeamtc_jet_exact), "overlapping coordinates\n")
  cat("  ÖAMTC ∩ OMV:", length(oeamtc_omv_exact), "overlapping coordinates\n")
  cat("  JET ∩ OMV:", length(jet_omv_exact), "overlapping coordinates\n")
  
  # Create station mapping table
  cat("\nCreating station mapping table...\n")
  
  # Combine all unique coordinates
  all_unique_coords <- unique(c(arboe_coords, oeamtc_coords, jet_coords, omv_coords))
  
  # Create mapping table
  station_mapping <- data.frame()
  
  for(coord in all_unique_coords) {
    coord_parts <- strsplit(coord, ",")[[1]]
    lat <- as.numeric(coord_parts[1])
    lon <- as.numeric(coord_parts[2])
    
    # Find all sources reporting this coordinate
    sources <- c()
    station_ids <- c()
    station_names <- c()
    
    # Check ARBÖ
    arboe_idx <- which(arboe_coords == coord)
    if(length(arboe_idx) > 0) {
      sources <- c(sources, "ARBÖ")
      station_ids <- c(station_ids, arboe_stations$station_id[arboe_idx[1]])
      station_names <- c(station_names, arboe_stations$station_name[arboe_idx[1]])
    }
    
    # Check ÖAMTC
    oeamtc_idx <- which(oeamtc_coords == coord)
    if(length(oeamtc_idx) > 0) {
      sources <- c(sources, "ÖAMTC")
      station_ids <- c(station_ids, oeamtc_stations$station_id[oeamtc_idx[1]])
      station_names <- c(station_names, oeamtc_stations$station_name[oeamtc_idx[1]])
    }
    
    # Check JET
    jet_idx <- which(jet_coords == coord)
    if(length(jet_idx) > 0) {
      sources <- c(sources, "JET")
      station_ids <- c(station_ids, jet_stations$station_id[jet_idx[1]])
      station_names <- c(station_names, jet_stations$station_name[jet_idx[1]])
    }
    
    # Check OMV
    omv_idx <- which(omv_coords == coord)
    if(length(omv_idx) > 0) {
      sources <- c(sources, "OMV")
      station_ids <- c(station_ids, omv_stations$station_id[omv_idx[1]])
      station_names <- c(station_names, omv_stations$station_name[omv_idx[1]])
    }
    
    # Add to mapping table
    station_mapping <- rbind(station_mapping, data.frame(
      unique_coord_key = coord,
      lat_rounded = lat,
      lon_rounded = lon,
      source_count = length(sources),
      sources = paste(sources, collapse = ";"),
      station_ids = paste(station_ids, collapse = ";"),
      station_names = paste(station_names, collapse = ";")
    ))
  }
  
  # Summary statistics
  cat("\nStation mapping summary:\n")
  cat("  Total unique physical locations:", nrow(station_mapping), "\n")
  cat("  Single-source stations:", sum(station_mapping$source_count == 1), "\n")
  cat("  Multi-source stations:", sum(station_mapping$source_count > 1), "\n")
  
  # Show examples of multi-source stations
  multi_source <- station_mapping[station_mapping$source_count > 1, ]
  if(nrow(multi_source) > 0) {
    cat("\nExample multi-source stations (first 5):\n")
    for(i in 1:min(5, nrow(multi_source))) {
      cat("  Location (", multi_source$lat_rounded[i], ",", multi_source$lon_rounded[i], "):\n")
      cat("    Sources:", multi_source$sources[i], "\n")
      cat("    Station IDs:", multi_source$station_ids[i], "\n")
      cat("    Names:", multi_source$station_names[i], "\n")
    }
  }
  
  # Save station mapping for visualization
  cat("\nSaving station mapping to file...\n")
  write.csv(station_mapping, "/Users/alexphan/Desktop/MBAT-Internship-Project/results/austrian_station_mapping.csv", row.names = FALSE)
  cat("Station mapping saved to: results/austrian_station_mapping.csv\n")
  
  # Disconnect from Austrian databases
  dbDisconnect(con_arboe, shutdown = TRUE)
  dbDisconnect(con_oeamtc, shutdown = TRUE)
  dbDisconnect(con_jet, shutdown = TRUE)
  dbDisconnect(con_omv, shutdown = TRUE)
  
} else {
  cat("Austrian databases not found - skipping overlap analysis\n")
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
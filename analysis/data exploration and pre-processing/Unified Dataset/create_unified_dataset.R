#!/usr/bin/env Rscript

# Create Unified Multi-Country Fuel Price Dataset - Direct CSV Export
# Combines fuel price data from Germany, Austria, Italy, and Slovenia
# Exports directly to CSV without creating heavy unified database

library(DBI)
library(duckdb)

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("../../")
}

# Database paths
german_db <- "databases/german_fuel_data.duckdb"
arboe_db <- "databases/arboe_fuel_prices.duckdb"
oeamtc_fuel_db <- "databases/oeamtc_fuel_data.duckdb"
jet_db <- "databases/jet_austria_fuel_data.duckdb"
omv_db <- "databases/omv_austria_fuel_data.duckdb"
italian_db <- "databases/italian_fuel_data.duckdb"
slovenian_db <- "databases/slovenian_fuel_data.duckdb"

# Output CSV path
csv_path <- "data/unified_fuel_prices.csv"

cat("Creating unified multi-country fuel price dataset\n")
cat("Direct CSV export (no database creation)\n")
cat("===============================================\n\n")

# Create temporary database for processing
temp_db <- ":memory:"
con <- dbConnect(duckdb(), temp_db)

# Set DuckDB memory limits to handle large datasets
dbExecute(con, "SET memory_limit='20GB'")
dbExecute(con, "SET max_temp_directory_size='40GB'")
dbExecute(con, "SET threads=6")
dbExecute(con, "SET preserve_insertion_order=false")

cat("DuckDB settings configured:\n")
cat("  Memory limit: 20GB\n")
cat("  Max temp directory: 40GB\n")
cat("  Threads: 6\n\n")

# Create temporary table
dbExecute(con, "
  CREATE TABLE unified_fuel_data (
    country VARCHAR,
    station_id VARCHAR,
    station_name VARCHAR,
    latitude DOUBLE,
    longitude DOUBLE,
    city VARCHAR,
    state_region VARCHAR,
    brand VARCHAR,
    fuel_type VARCHAR,
    price DOUBLE,
    date_raw VARCHAR,
    data_source VARCHAR,
    file_source VARCHAR
  )
")

cat("Loading data from all sources...\n\n")

# 1. German Data
cat("1. Loading German data...\n")
dbExecute(con, sprintf("ATTACH '%s' AS german_db (READ_ONLY)", german_db))

# Load German data in monthly chunks
month_ranges <- list(
  c("2024-08-01", "2024-09-01"),
  c("2024-09-01", "2024-10-01"),
  c("2024-10-01", "2024-11-01"),
  c("2024-11-01", "2024-12-01"),
  c("2024-12-01", "2025-01-01"),
  c("2025-01-01", "2025-02-01"),
  c("2025-02-01", "2025-03-01"),
  c("2025-03-01", "2025-04-01"),
  c("2025-04-01", "2025-05-01"),
  c("2025-05-01", "2025-06-01"),
  c("2025-06-01", "2025-07-01"),
  c("2025-07-01", "2025-08-01")
)

for (i in seq_along(month_ranges)) {
  start_date <- month_ranges[[i]][1]
  end_date <- month_ranges[[i]][2]
  month_label <- substr(start_date, 1, 7)
  
  cat("  Loading", month_label, "...\n")
  
  dbExecute(con, paste0("
    INSERT INTO unified_fuel_data
    SELECT
      'Germany' as country,
      p.station_uuid as station_id,
      s.name as station_name,
      s.latitude,
      s.longitude,
      s.city,
      NULL as state_region,
      s.brand,
      'diesel' as fuel_type,
      p.diesel as price,
      CAST(p.date AS VARCHAR) as date_raw,
      'Tankerkönig' as data_source,
      p.file_source
    FROM german_db.german_prices p
    JOIN german_db.german_stations s ON p.station_uuid = s.uuid
    WHERE p.diesel > 0 AND p.diesel < 10
      AND s.latitude IS NOT NULL AND s.longitude IS NOT NULL
      AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
      AND p.date >= '", start_date, "' AND p.date < '", end_date, "'
    
    UNION ALL
    
    SELECT
      'Germany' as country,
      p.station_uuid as station_id,
      s.name as station_name,
      s.latitude,
      s.longitude,
      s.city,
      NULL as state_region,
      s.brand,
      'gasoline_95' as fuel_type,
      p.e5 as price,
      CAST(p.date AS VARCHAR) as date_raw,
      'Tankerkönig' as data_source,
      p.file_source
    FROM german_db.german_prices p
    JOIN german_db.german_stations s ON p.station_uuid = s.uuid
    WHERE p.e5 > 0 AND p.e5 < 10
      AND s.latitude IS NOT NULL AND s.longitude IS NOT NULL
      AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
      AND p.date >= '", start_date, "' AND p.date < '", end_date, "'
    
    UNION ALL
    
    SELECT
      'Germany' as country,
      p.station_uuid as station_id,
      s.name as station_name,
      s.latitude,
      s.longitude,
      s.city,
      NULL as state_region,
      s.brand,
      'gasoline_98' as fuel_type,
      p.e10 as price,
      CAST(p.date AS VARCHAR) as date_raw,
      'Tankerkönig' as data_source,
      p.file_source
    FROM german_db.german_prices p
    JOIN german_db.german_stations s ON p.station_uuid = s.uuid
    WHERE p.e10 > 0 AND p.e10 < 10
      AND s.latitude IS NOT NULL AND s.longitude IS NOT NULL
      AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
      AND p.date >= '", start_date, "' AND p.date < '", end_date, "'
  "))
  
  # Check progress
  count <- dbGetQuery(con, paste0("
    SELECT COUNT(*) as count 
    FROM unified_fuel_data 
    WHERE country = 'Germany' 
      AND date_raw >= '", start_date, "' 
      AND date_raw < '", end_date, "'
  "))$count
  cat("    Records:", format(count, big.mark = ","), "\n")
}

dbExecute(con, "DETACH german_db")
cat("✓ German data loaded\n\n")

# 2. Austrian ARBÖ Data - SKIPPED (database deleted)
cat("2. Austrian ARBÖ data: SKIPPED (database deleted)\n")

# 3. Austrian ÖAMTC Data
cat("3. Loading Austrian ÖAMTC data...\n")
dbExecute(con, sprintf("ATTACH '%s' AS oeamtc_db (READ_ONLY)", oeamtc_fuel_db))

dbExecute(con, "
  INSERT INTO unified_fuel_data
  SELECT
    'Austria' as country,
    station_id,
    station_name,
    station_lat as latitude,
    station_lng as longitude,
    LEFT(station_address, POSITION(',' IN station_address) - 1) as city,
    NULL as state_region,
    station_brand as brand,
    fuel_type,
    fuel_price as price,
    fuel_last_updated as date_raw,
    'ÖAMTC' as data_source,
    file_source
  FROM oeamtc_db.oeamtc_fuel
  WHERE fuel_price > 0 AND fuel_price < 10
    AND station_lat IS NOT NULL AND station_lng IS NOT NULL
    AND station_lat BETWEEN 45 AND 50 AND station_lng BETWEEN 9 AND 18
")

oeamtc_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM unified_fuel_data WHERE country = 'Austria' AND data_source = 'ÖAMTC'")$count
cat("  ÖAMTC records:", format(oeamtc_count, big.mark = ","), "\n")
dbExecute(con, "DETACH oeamtc_db")

# 4. Austrian JET Data
cat("4. Loading Austrian JET data...\n")
dbExecute(con, sprintf("ATTACH '%s' AS jet_db (READ_ONLY)", jet_db))

dbExecute(con, "
  INSERT INTO unified_fuel_data
  SELECT
    'Austria' as country,
    station_id,
    station_name,
    lat as latitude,
    lng as longitude,
    city,
    NULL as state_region,
    'JET' as brand,
    fuel_type,
    price,
    CAST(date AS VARCHAR) as date_raw,
    'Jet Austria' as data_source,
    file_source
  FROM jet_db.jet_austria_fuel
  WHERE price > 0 AND price < 10
    AND lat IS NOT NULL AND lng IS NOT NULL
    AND lat BETWEEN 45 AND 50 AND lng BETWEEN 9 AND 18
")

jet_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM unified_fuel_data WHERE country = 'Austria' AND data_source = 'Jet Austria'")$count
cat("  JET records:", format(jet_count, big.mark = ","), "\n")
dbExecute(con, "DETACH jet_db")

# 5. Austrian OMV Data
cat("5. Loading Austrian OMV data...\n")
dbExecute(con, sprintf("ATTACH '%s' AS omv_db (READ_ONLY)", omv_db))

dbExecute(con, "
  INSERT INTO unified_fuel_data
  SELECT
    'Austria' as country,
    station_id,
    town as station_name,
    lat as latitude,
    lon as longitude,
    town as city,
    NULL as state_region,
    'OMV' as brand,
    fuel_type,
    price,
    CAST(date AS VARCHAR) as date_raw,
    'OMV Austria' as data_source,
    file_source
  FROM omv_db.omv_austria_fuel
  WHERE price > 0 AND price < 10
    AND lat IS NOT NULL AND lon IS NOT NULL
    AND lat BETWEEN 45 AND 50 AND lon BETWEEN 9 AND 18
")

omv_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM unified_fuel_data WHERE country = 'Austria' AND data_source = 'OMV Austria'")$count
cat("  OMV records:", format(omv_count, big.mark = ","), "\n")
dbExecute(con, "DETACH omv_db")

# 6. Italian Data (skip - no coordinates)
cat("6. Italian data: SKIPPED (no station coordinates)\n")

# 7. Slovenian Data
cat("7. Loading Slovenian data...\n")
dbExecute(con, sprintf("ATTACH '%s' AS slovenian_db (READ_ONLY)", slovenian_db))

dbExecute(con, "
  INSERT INTO unified_fuel_data
  SELECT
    'Slovenia' as country,
    CAST(station_id AS VARCHAR) as station_id,
    station_name,
    station_lat as latitude,
    station_lng as longitude,
    NULL as city,
    NULL as state_region,
    CASE 
      WHEN franchise_id = 1 THEN 'Petrol'
      WHEN franchise_id = 4 THEN 'MOL'
      WHEN franchise_id = 5 THEN 'Shell'
      ELSE 'Unknown'
    END as brand,
    fuel_type,
    fuel_price as price,
    CAST(date AS VARCHAR) as date_raw,
    'Slovenian National' as data_source,
    file_source
  FROM slovenian_db.slovenian_fuel
  WHERE fuel_price > 0 AND fuel_price < 10
    AND station_lat IS NOT NULL AND station_lng IS NOT NULL
    AND station_lat BETWEEN 45 AND 47 AND station_lng BETWEEN 13 AND 17
")

slovenian_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM unified_fuel_data WHERE country = 'Slovenia'")$count
cat("  Slovenian records:", format(slovenian_count, big.mark = ","), "\n")
dbExecute(con, "DETACH slovenian_db")

# Export to CSV
cat("\n📊 Exporting to CSV...\n")
cat("Export path:", csv_path, "\n")

dbExecute(con, sprintf("
  COPY unified_fuel_data 
  TO '%s' 
  (HEADER, DELIMITER ',')
", csv_path))

# Get final statistics
total_records <- dbGetQuery(con, "SELECT COUNT(*) as total FROM unified_fuel_data")$total
csv_size <- file.size(csv_path) / (1024^3)

cat("\n✓ Export complete!\n")
cat("Total records:", format(total_records, big.mark = ","), "\n")
cat("CSV size:", round(csv_size, 2), "GB\n")
cat("CSV path:", csv_path, "\n")

# Summary by country
summary_stats <- dbGetQuery(con, "
  SELECT 
    country,
    COUNT(*) as records,
    COUNT(DISTINCT station_id) as unique_stations,
    MIN(price) as min_price,
    MAX(price) as max_price,
    ROUND(AVG(price), 3) as avg_price
  FROM unified_fuel_data
  GROUP BY country
  ORDER BY records DESC
")

cat("\nSummary by country:\n")
print(summary_stats)

# Disconnect
dbDisconnect(con, shutdown = TRUE)

cat("\n✓ Unified dataset exported to CSV successfully!\n")
cat("No heavy database created - direct CSV export complete!\n")

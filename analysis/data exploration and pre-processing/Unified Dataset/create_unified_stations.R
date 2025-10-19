#!/usr/bin/env Rscript

# Create Unified Stations Table - Direct CSV Export
# Creates table with all unique stations from all countries
# Exports directly to CSV without creating heavy unified database

library(DBI)
library(duckdb)

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("../../")
}

# Database paths
arboe_db <- "databases/arboe_fuel_prices.duckdb"
oeamtc_fuel_db <- "databases/oeamtc_fuel_data.duckdb"
jet_db <- "databases/jet_austria_fuel_data.duckdb"
omv_db <- "databases/omv_austria_fuel_data.duckdb"
italian_db <- "databases/italian_fuel_data.duckdb"
slovenian_db <- "databases/slovenian_fuel_data.duckdb"
german_db <- "databases/german_fuel_data.duckdb"

# Output CSV path
csv_path <- "data/unified_stations.csv"

cat("Creating Unified Stations Table\n")
cat("Direct CSV export (no database creation)\n")
cat("========================================\n\n")

# Create temporary database for processing
temp_db <- ":memory:"
con_out <- dbConnect(duckdb(), temp_db)

# Create unified stations table
dbExecute(con_out, "
  CREATE TABLE unified_stations (
    country VARCHAR,
    station_id VARCHAR,
    station_name VARCHAR,
    latitude DOUBLE,
    longitude DOUBLE,
    city VARCHAR,
    state_region VARCHAR,
    brand VARCHAR,
    data_source VARCHAR,
    nearby_stations_1km INTEGER DEFAULT 0
  )
")

cat("Loading stations from all countries...\n")

# 1. Austrian ARBÖ Stations
cat("  Loading ARBÖ stations...\n")
con_arboe <- dbConnect(duckdb(), arboe_db, read_only = TRUE)

arboe_stations <- dbGetQuery(con_arboe, "
  SELECT DISTINCT
    'Austria' as country,
    id as station_id,
    station_name,
    ROUND(lat, 4) as latitude,
    ROUND(lng, 4) as longitude,
    city,
    NULL as state_region,
    brand,
    'ARBÖ' as data_source
  FROM arboe_prices
  WHERE lat IS NOT NULL AND lng IS NOT NULL
    AND lat BETWEEN 45 AND 50 AND lng BETWEEN 9 AND 18
")

dbWriteTable(con_out, "unified_stations", arboe_stations, append = TRUE)
cat("    ARBÖ stations:", nrow(arboe_stations), "\n")
dbDisconnect(con_arboe)

# 2. Austrian ÖAMTC Stations
cat("  Loading ÖAMTC stations...\n")
con_oeamtc <- dbConnect(duckdb(), oeamtc_fuel_db, read_only = TRUE)

oeamtc_stations <- dbGetQuery(con_oeamtc, "
  SELECT DISTINCT
    'Austria' as country,
    station_id,
    station_name,
    ROUND(lat, 4) as latitude,
    ROUND(lng, 4) as longitude,
    city,
    NULL as state_region,
    brand,
    'ÖAMTC' as data_source
  FROM oeamtc_prices
  WHERE lat IS NOT NULL AND lng IS NOT NULL
    AND lat BETWEEN 45 AND 50 AND lng BETWEEN 9 AND 18
")

dbWriteTable(con_out, "unified_stations", oeamtc_stations, append = TRUE)
cat("    ÖAMTC stations:", nrow(oeamtc_stations), "\n")
dbDisconnect(con_oeamtc)

# 3. Austrian JET Stations
cat("  Loading JET stations...\n")
con_jet <- dbConnect(duckdb(), jet_db, read_only = TRUE)

jet_stations <- dbGetQuery(con_jet, "
  SELECT DISTINCT
    'Austria' as country,
    station_id,
    station_name,
    ROUND(lat, 4) as latitude,
    ROUND(lng, 4) as longitude,
    city,
    NULL as state_region,
    'JET' as brand,
    'Jet Austria' as data_source
  FROM jet_austria_fuel
  WHERE lat IS NOT NULL AND lng IS NOT NULL
    AND lat BETWEEN 45 AND 50 AND lng BETWEEN 9 AND 18
")

dbWriteTable(con_out, "unified_stations", jet_stations, append = TRUE)
cat("    JET stations:", nrow(jet_stations), "\n")
dbDisconnect(con_jet)

# 4. Austrian OMV Stations
cat("  Loading OMV stations...\n")
con_omv <- dbConnect(duckdb(), omv_db, read_only = TRUE)

omv_stations <- dbGetQuery(con_omv, "
  SELECT DISTINCT
    'Austria' as country,
    station_id,
    town as station_name,
    ROUND(lat, 4) as latitude,
    ROUND(lon, 4) as longitude,
    town as city,
    NULL as state_region,
    'OMV' as brand,
    'OMV Austria' as data_source
  FROM omv_austria_fuel
  WHERE lat IS NOT NULL AND lon IS NOT NULL
    AND lat BETWEEN 45 AND 50 AND lon BETWEEN 9 AND 18
")

dbWriteTable(con_out, "unified_stations", omv_stations, append = TRUE)
cat("    OMV stations:", nrow(omv_stations), "\n")
dbDisconnect(con_omv)

# 5. Italian Stations (skip - no coordinates)
cat("  Loading Italian stations...\n")
cat("    Italian stations: 0 (no coordinates)\n")

# 6. Slovenian Stations
cat("  Loading Slovenian stations...\n")
con_slovenian <- dbConnect(duckdb(), slovenian_db, read_only = TRUE)

slovenian_stations <- dbGetQuery(con_slovenian, "
  SELECT DISTINCT
    'Slovenia' as country,
    CAST(station_id AS VARCHAR) as station_id,
    station_name,
    ROUND(station_lat, 4) as latitude,
    ROUND(station_lng, 4) as longitude,
    NULL as city,
    NULL as state_region,
    CASE 
      WHEN franchise_id = 1 THEN 'Petrol'
      WHEN franchise_id = 4 THEN 'MOL'
      WHEN franchise_id = 5 THEN 'Shell'
      ELSE 'Unknown'
    END as brand,
    'Slovenian National' as data_source
  FROM slovenian_fuel
  WHERE station_lat IS NOT NULL AND station_lng IS NOT NULL
    AND station_lat BETWEEN 45 AND 47 AND station_lng BETWEEN 13 AND 17
")

dbWriteTable(con_out, "unified_stations", slovenian_stations, append = TRUE)
cat("    Slovenian stations:", nrow(slovenian_stations), "\n")
dbDisconnect(con_slovenian)

# 7. German Stations
cat("  Loading German stations...\n")
con_german <- dbConnect(duckdb(), german_db, read_only = TRUE)

german_stations <- dbGetQuery(con_german, "
  SELECT DISTINCT
    'Germany' as country,
    uuid as station_id,
    name as station_name,
    ROUND(latitude, 4) as latitude,
    ROUND(longitude, 4) as longitude,
    city,
    NULL as state_region,
    brand,
    'Tankerkönig' as data_source
  FROM german_stations
  WHERE latitude IS NOT NULL AND longitude IS NOT NULL
    AND latitude BETWEEN 47 AND 55 AND longitude BETWEEN 5 AND 15
")

dbWriteTable(con_out, "unified_stations", german_stations, append = TRUE)
cat("    German stations:", nrow(german_stations), "\n")
dbDisconnect(con_german)

# Remove duplicates and create unique physical locations
cat("\nRemoving duplicate stations...\n")

# Remove Austrian duplicates: prioritize ÖAMTC > ARBÖ > OMV > JET
# Other countries: keep all stations
dbExecute(con_out, "
  CREATE OR REPLACE TABLE unified_stations_clean AS
  WITH ranked_stations AS (
    SELECT *,
      CASE 
        WHEN country = 'Austria' AND data_source = 'ÖAMTC' THEN 1
        WHEN country = 'Austria' AND data_source = 'ARBÖ' THEN 2
        WHEN country = 'Austria' AND data_source = 'OMV Austria' THEN 3
        WHEN country = 'Austria' AND data_source = 'Jet Austria' THEN 4
        ELSE 1
      END as priority
    FROM unified_stations
  )
  SELECT 
    country,
    station_id,
    station_name,
    latitude,
    longitude,
    city,
    state_region,
    brand,
    data_source
  FROM (
    SELECT *,
      ROW_NUMBER() OVER (
        PARTITION BY country, latitude, longitude 
        ORDER BY priority, station_id
      ) as rn
    FROM ranked_stations
  ) ranked
  WHERE rn = 1
")

# Replace original table with deduplicated version
dbExecute(con_out, "DROP TABLE unified_stations")
dbExecute(con_out, "ALTER TABLE unified_stations_clean RENAME TO unified_stations")

# Calculate nearby stations
cat("\nCalculating nearby stations...\n")
dbExecute(con_out, "
  UPDATE unified_stations 
  SET nearby_stations_1km = (
    SELECT COUNT(*)
    FROM unified_stations other_stations
    WHERE other_stations.station_id != unified_stations.station_id
      AND other_stations.latitude IS NOT NULL 
      AND other_stations.longitude IS NOT NULL
      AND unified_stations.latitude IS NOT NULL 
      AND unified_stations.longitude IS NOT NULL
      AND (
        (other_stations.latitude - unified_stations.latitude) * (other_stations.latitude - unified_stations.latitude) +
        (other_stations.longitude - unified_stations.longitude) * (other_stations.longitude - unified_stations.longitude)
      ) <= 0.000008
  )
  WHERE latitude IS NOT NULL AND longitude IS NOT NULL
")

# Get statistics
overall_stats <- dbGetQuery(con_out, "
  SELECT 
    COUNT(*) as total_stations,
    COUNT(DISTINCT CONCAT(latitude, ',', longitude)) as unique_locations,
    AVG(nearby_stations_1km) as avg_nearby_stations,
    MIN(nearby_stations_1km) as min_nearby_stations,
    MAX(nearby_stations_1km) as max_nearby_stations
  FROM unified_stations
  WHERE latitude IS NOT NULL AND longitude IS NOT NULL
")

cat("\nOverall statistics:\n")
cat("Total stations:", overall_stats$total_stations, "\n")
cat("Unique physical locations:", overall_stats$unique_locations, "\n")
cat("Average nearby stations:", round(overall_stats$avg_nearby_stations, 2), "\n")
cat("Min nearby stations:", overall_stats$min_nearby_stations, "\n")
cat("Max nearby stations:", overall_stats$max_nearby_stations, "\n")

# Export to CSV
cat("\n📊 Exporting to CSV...\n")
cat("Export path:", csv_path, "\n")

dbExecute(con_out, sprintf("
  COPY unified_stations 
  TO '%s' 
  (HEADER, DELIMITER ',')
", csv_path))

# Get CSV file size
csv_size <- file.size(csv_path) / (1024^2)  # Convert to MB
cat("✓ CSV export complete! Size:", round(csv_size, 2), "MB\n")

# Disconnect
dbDisconnect(con_out, shutdown = TRUE)

cat("\n✓ Unified stations table exported to CSV successfully!\n")
cat("No heavy database created - direct CSV export complete!\n")

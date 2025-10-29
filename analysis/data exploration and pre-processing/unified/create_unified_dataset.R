#!/usr/bin/env Rscript

# Create Unified Multi-Country Fuel Price Dataset
# Combines fuel price data from Germany, Austria, Italy, and Slovenia
# 
# Uses DuckDB's ATTACH and INSERT INTO ... SELECT for efficient data loading
# (no data transfer through R memory, much faster!)
#
# FUEL TYPE CATEGORIZATION (4 categories):
# - diesel: Regular diesel
# - diesel_premium: Premium diesel (HVO, Blue Diesel, diesel_plus, maxxmotion_diesel, dizel-premium)
# - gasoline_95: Regular gasoline (95 octane, E5, E10)
# - gasoline_98: Premium gasoline (98+ octane, Super Plus, 100 octane)
#
# See info/brand_categorization_reference.md for detailed categorization logic

library(DBI)
library(duckdb)

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("../../")
}

# Database paths (relative paths for ATTACH)
german_db <- "databases/german_fuel_data.duckdb"
arboe_db <- "databases/arboe_fuel_prices.duckdb"
oeamtc_fuel_db <- "databases/oeamtc_fuel_data.duckdb"
jet_db <- "databases/jet_austria_fuel_data.duckdb"
omv_db <- "databases/omv_austria_fuel_data.duckdb"
italian_db <- "databases/italian_fuel_data.duckdb"
slovenian_db <- "databases/slovenian_fuel_data.duckdb"

# Output database
unified_db <- "databases/unified_fuel_data.duckdb"

cat("Creating unified multi-country fuel price dataset\n")
cat("Using DuckDB ATTACH for efficient data loading\n")
cat("===============================================\n\n")

# Connect to output database
con <- dbConnect(duckdb(), unified_db)

# Create main unified table with raw data only (no feature engineering)
dbExecute(con, "
  CREATE OR REPLACE TABLE unified_fuel_data (
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

cat("Loading data from all sources using direct SQL...\n\n")

# 1. German Data (Tankerkönig) - August 2024 to July 2025
cat("1. Loading German data (August 2024 - July 2025)...\n")
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
  
  dbExecute(con, sprintf("
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
      AND p.date >= '%s' AND p.date < '%s'
    
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
      AND p.date >= '%s' AND p.date < '%s'
    
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
      AND p.date >= '%s' AND p.date < '%s'
  ", start_date, end_date, start_date, end_date, start_date, end_date))
  
  # Get count for this month
  count <- dbGetQuery(con, sprintf("
    SELECT COUNT(*) as count 
    FROM unified_fuel_data 
    WHERE country = 'Germany' AND date_raw >= '%s' AND date_raw < '%s'
  ", start_date, end_date))$count
  
  cat("    Records:", format(count, big.mark = ","), "\n")
}

dbExecute(con, "DETACH german_db")
cat("✓ German data loaded\n\n")

# 2. Austrian ARBÖ Data
cat("2. Loading Austrian ARBÖ data...\n")
dbExecute(con, sprintf("ATTACH '%s' AS arboe_db (READ_ONLY)", arboe_db))

dbExecute(con, "
  INSERT INTO unified_fuel_data
  SELECT 
    'Austria' as country,
    id as station_id,
    name as station_name,
    lat as latitude,
    lon as longitude,
    city,
    bundesland as state_region,
    CASE 
      WHEN name LIKE '%OMV%' THEN 'OMV'
      WHEN name LIKE '%Shell%' THEN 'Shell'
      WHEN name LIKE '%BP%' THEN 'BP'
      WHEN name LIKE '%JET%' THEN 'JET'
      WHEN name LIKE '%AVIA%' THEN 'AVIA'
      WHEN name LIKE '%ENI%' THEN 'ENI'
      ELSE 'Unknown'
    END as brand,
    'diesel' as fuel_type,
    fuel_d as price,
    CAST(date || ' ' || time AS VARCHAR) as date_raw,
    'ARBÖ' as data_source,
    NULL as file_source
  FROM arboe_db.arboe_prices
  WHERE fuel_d > 0 AND fuel_d < 10
    AND lat IS NOT NULL AND lon IS NOT NULL
    AND lat BETWEEN 46 AND 49 AND lon BETWEEN 9 AND 17
    AND date >= '2024-01-01'
  
  UNION ALL
  
  SELECT 
    'Austria' as country,
    id as station_id,
    name as station_name,
    lat as latitude,
    lon as longitude,
    city,
    bundesland as state_region,
    CASE 
      WHEN name LIKE '%OMV%' THEN 'OMV'
      WHEN name LIKE '%Shell%' THEN 'Shell'
      WHEN name LIKE '%BP%' THEN 'BP'
      WHEN name LIKE '%JET%' THEN 'JET'
      WHEN name LIKE '%AVIA%' THEN 'AVIA'
      WHEN name LIKE '%ENI%' THEN 'ENI'
      ELSE 'Unknown'
    END as brand,
    'gasoline_95' as fuel_type,
    fuel_s95 as price,
    CAST(date || ' ' || time AS VARCHAR) as date_raw,
    'ARBÖ' as data_source,
    NULL as file_source
  FROM arboe_db.arboe_prices
  WHERE fuel_s95 > 0 AND fuel_s95 < 10
    AND lat IS NOT NULL AND lon IS NOT NULL
    AND lat BETWEEN 46 AND 49 AND lon BETWEEN 9 AND 17
    AND date >= '2024-01-01'
  
  UNION ALL
  
  SELECT 
    'Austria' as country,
    id as station_id,
    name as station_name,
    lat as latitude,
    lon as longitude,
    city,
    bundesland as state_region,
    CASE 
      WHEN name LIKE '%OMV%' THEN 'OMV'
      WHEN name LIKE '%Shell%' THEN 'Shell'
      WHEN name LIKE '%BP%' THEN 'BP'
      WHEN name LIKE '%JET%' THEN 'JET'
      WHEN name LIKE '%AVIA%' THEN 'AVIA'
      WHEN name LIKE '%ENI%' THEN 'ENI'
      ELSE 'Unknown'
    END as brand,
    'gasoline_98' as fuel_type,
    fuel_s98 as price,
    CAST(date || ' ' || time AS VARCHAR) as date_raw,
    'ARBÖ' as data_source,
    NULL as file_source
  FROM arboe_db.arboe_prices
  WHERE fuel_s98 > 0 AND fuel_s98 < 10
    AND lat IS NOT NULL AND lon IS NOT NULL
    AND lat BETWEEN 46 AND 49 AND lon BETWEEN 9 AND 17
    AND date >= '2024-01-01'
")

arboe_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM unified_fuel_data WHERE data_source = 'ARBÖ'")$count
cat("  Records:", format(arboe_count, big.mark = ","), "\n")
dbExecute(con, "DETACH arboe_db")
cat("✓ ARBÖ data loaded\n\n")

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
    NULL as city,
    NULL as state_region,
    station_brand as brand,
    CASE 
      WHEN fuel_type = 'DIESEL' THEN 'diesel'
      WHEN fuel_type = 'GASOLINE_SUPER' THEN 'gasoline_95'
      WHEN fuel_type = 'GASOLINE_PREMIUM' THEN 'gasoline_98'
      ELSE fuel_type
    END as fuel_type,
    fuel_price as price,
    CAST(last_imported AS VARCHAR) as date_raw,
    'ÖAMTC' as data_source,
    NULL as file_source
  FROM oeamtc_db.oeamtc_fuel
  WHERE fuel_price > 0 AND fuel_price < 10
    AND station_lat IS NOT NULL AND station_lng IS NOT NULL
    AND station_lat BETWEEN 46 AND 49 AND station_lng BETWEEN 9 AND 17
    AND last_imported >= '2024-01-01'
")

oeamtc_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM unified_fuel_data WHERE data_source = 'ÖAMTC'")$count
cat("  Records:", format(oeamtc_count, big.mark = ","), "\n")
dbExecute(con, "DETACH oeamtc_db")
cat("✓ ÖAMTC data loaded\n\n")

# 4. Austrian Jet Data
cat("4. Loading Austrian Jet data...\n")
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
    CASE 
      WHEN fuel_type = 'Diesel' THEN 'diesel'
      WHEN fuel_type = 'Super' THEN 'gasoline_95'
      WHEN fuel_type = 'Super Plus' THEN 'gasoline_98'
      ELSE fuel_type
    END as fuel_type,
    fuel_price as price,
    CAST(file_date AS VARCHAR) as date_raw,
    'Jet Austria' as data_source,
    NULL as file_source
  FROM jet_db.jet_austria_fuel
  WHERE fuel_price > 0 AND fuel_price < 10
    AND lat IS NOT NULL AND lng IS NOT NULL
    AND lat BETWEEN 46 AND 49 AND lng BETWEEN 9 AND 17
    AND file_date >= '2024-01-01'
")

jet_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM unified_fuel_data WHERE data_source = 'Jet Austria'")$count
cat("  Records:", format(jet_count, big.mark = ","), "\n")
dbExecute(con, "DETACH jet_db")
cat("✓ Jet data loaded\n\n")

# 5. Austrian OMV Data
cat("5. Loading OMV Austria data...\n")
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
    CASE 
      WHEN fuel = 'diesel' THEN 'diesel'
      WHEN fuel = 'gasoline_95' THEN 'gasoline_95'
      WHEN fuel = 'maxxmotion_super_100plus' THEN 'gasoline_98'
      WHEN fuel IN ('maxxmotion_diesel', 'diesel_plus', 'premium_power_diesel', 'hvo100_diesel') THEN 'diesel_premium'
      ELSE fuel
    END as fuel_type,
    price_eur as price,
    CAST(scraped_at AS VARCHAR) as date_raw,
    'OMV Austria' as data_source,
    NULL as file_source
  FROM omv_db.omv_austria_fuel
  WHERE price_eur > 0 AND price_eur < 10
    AND lat IS NOT NULL AND lon IS NOT NULL
    AND lat BETWEEN 46 AND 49 AND lon BETWEEN 9 AND 17
    AND scraped_at >= '2024-01-01'
")

omv_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM unified_fuel_data WHERE data_source = 'OMV Austria'")$count
cat("  Records:", format(omv_count, big.mark = ","), "\n")
dbExecute(con, "DETACH omv_db")
cat("✓ OMV data loaded\n\n")

# 6. Italian Data (skipping - no station coordinates)
cat("6. Skipping Italian data (no station-level coordinates)\n\n")

# 7. Slovenian Data
cat("7. Loading Slovenian data...\n")
dbExecute(con, sprintf("ATTACH '%s' AS slovenian_db (READ_ONLY)", slovenian_db))

dbExecute(con, "
  INSERT INTO unified_fuel_data
  SELECT 
    'Slovenia' as country,
    station_id,
    station_name,
    station_lat as latitude,
    station_lng as longitude,
    NULL as city,
    NULL as state_region,
    CAST(franchise_id AS VARCHAR) as brand,
    CASE 
      WHEN fuel_type = 'dizel' THEN 'diesel'
      WHEN fuel_type = 'dizel-premium' THEN 'diesel_premium'
      WHEN fuel_type = '95' THEN 'gasoline_95'
      WHEN fuel_type IN ('98', '100') THEN 'gasoline_98'
      ELSE fuel_type
    END as fuel_type,
    fuel_price as price,
    CAST(REGEXP_EXTRACT(file_source, '([0-9]{4}-[0-9]{2}-[0-9]{2})_([0-9]{2}-[0-9]{2})', 1) || ' ' || 
         REPLACE(REGEXP_EXTRACT(file_source, '([0-9]{4}-[0-9]{2}-[0-9]{2})_([0-9]{2}-[0-9]{2})', 2), '-', ':') || ':00' AS VARCHAR) as date_raw,
    'Slovenian National' as data_source,
    file_source
  FROM slovenian_db.slovenian_fuel
  WHERE fuel_price > 0 AND fuel_price < 10
    AND station_lat IS NOT NULL AND station_lng IS NOT NULL
    AND station_lat BETWEEN 45 AND 47 AND station_lng BETWEEN 13 AND 17
    AND REGEXP_EXTRACT(file_source, '([0-9]{4}-[0-9]{2}-[0-9]{2})_([0-9]{2}-[0-9]{2})', 1) >= '2024-01-01'
")

slovenian_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM unified_fuel_data WHERE data_source = 'Slovenian National'")$count
cat("  Records:", format(slovenian_count, big.mark = ","), "\n")
dbExecute(con, "DETACH slovenian_db")
cat("✓ Slovenian data loaded\n\n")

# Create indexes for better performance
cat("Creating indexes...\n")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_country ON unified_fuel_data(country)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_fuel_type ON unified_fuel_data(fuel_type)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_date_raw ON unified_fuel_data(date_raw)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_country_fuel ON unified_fuel_data(country, fuel_type)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_station_id ON unified_fuel_data(station_id)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_coordinates ON unified_fuel_data(latitude, longitude)")
cat("✓ Indexes created\n\n")

# Summary statistics
cat("===============================================\n")
cat("UNIFIED DATASET SUMMARY\n")
cat("===============================================\n\n")

summary_stats <- dbGetQuery(con, "
  SELECT 
    country,
    fuel_type,
    COUNT(*) as records,
    ROUND(MIN(price), 3) as min_price,
    ROUND(MAX(price), 3) as max_price,
    ROUND(AVG(price), 3) as avg_price,
    MIN(date_raw) as earliest_date,
    MAX(date_raw) as latest_date,
    COUNT(DISTINCT station_id) as unique_stations,
    COUNT(DISTINCT brand) as unique_brands
  FROM unified_fuel_data
  GROUP BY country, fuel_type
  ORDER BY country, fuel_type
")

print(summary_stats)

cat("\n")
total_records <- dbGetQuery(con, "SELECT COUNT(*) as total FROM unified_fuel_data")$total
total_stations <- dbGetQuery(con, "SELECT COUNT(DISTINCT station_id) as total FROM unified_fuel_data")$total
total_countries <- dbGetQuery(con, "SELECT COUNT(DISTINCT country) as total FROM unified_fuel_data")$total
cat("Total records:", format(total_records, big.mark = ","), "\n")
cat("Total unique stations:", format(total_stations, big.mark = ","), "\n")
cat("Total countries:", total_countries, "\n")

# Disconnect
dbDisconnect(con, shutdown = TRUE)

cat("\n✓ Unified dataset created successfully!\n")
cat("Database saved to:", unified_db, "\n")

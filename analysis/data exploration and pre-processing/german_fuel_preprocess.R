# German Fuel Data Preprocessing Script
# This script performs preprocessing steps for the German fuel data:
# 1. Creates database indexes for better query performance
# 2. Pre-calculates nearby stations feature for spatial analysis

library(DBI)
library(duckdb)
library(sf)
library(dplyr)

# Configuration 
db_path <- "../../databases/german_fuel_data.duckdb"

# Connect to Database
con <- dbConnect(duckdb(), db_path, read_only = FALSE)

# Set optimal DuckDB memory settings for preprocessing
dbExecute(con, "SET memory_limit='16GB'")
dbExecute(con, "SET max_temp_directory_size='20GB'") 
dbExecute(con, "SET threads=8")

# Creating Database Index

# Check database size
tryCatch({
  price_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM german_prices")$count
  station_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM german_stations")$count
  cat("   Database size: ", format(price_count, big.mark = ","), " price records, ", 
      format(station_count, big.mark = ","), " stations\n")
  cat("   JOIN operations: ", format(price_count * station_count, scientific = TRUE), " potential comparisons\n")
  cat("   Index benefit: Massive reduction in comparison operations\n\n")
}, error = function(e) {
  cat("   Could not retrieve database statistics:", e$message, "\n\n")
})

# Index 1: german_stations.uuid (for JOIN performance)
tryCatch({
  cat("   Creating idx_stations_uuid on german_stations(uuid)...\n")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stations_uuid ON german_stations(uuid)")
  cat("   ✓ SUCCESS: Index created on german_stations.uuid\n")
}, error = function(e) {
  cat("   ✗ FAILED:", e$message, "\n")
})

# Index 2: german_prices.station_uuid (for JOIN performance)
tryCatch({
  cat("   Creating idx_prices_station on german_prices(station_uuid)...\n")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_prices_station ON german_prices(station_uuid)")
  cat("   ✓ SUCCESS: Index created on german_prices.station_uuid\n")
}, error = function(e) {
  cat("   ✗ FAILED:", e$message, "\n")
})

#  Data Quality - Outlier Detection and Removal

# Step 1a: Check for extremely large coordinate values (likely data entry errors)
extreme_coord_check <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as total_stations,
    COUNT(CASE WHEN latitude > 100000 OR longitude > 100000 THEN 1 END) as extreme_outliers
  FROM german_stations
")

if(extreme_coord_check$extreme_outliers > 0) {
  cat("Found", extreme_coord_check$extreme_outliers, "stations with EXTREME coordinates (>100000)\n")
  extreme_outliers <- dbGetQuery(con, "
    SELECT uuid, name, latitude, longitude, brand
    FROM german_stations
    WHERE latitude > 100000 OR longitude > 100000
    ORDER BY latitude + longitude DESC
  ")
  cat("   Extreme outliers to remove:\n")
  print(extreme_outliers)
  
  removed <- dbExecute(con, "
    DELETE FROM german_stations 
    WHERE latitude > 100000 OR longitude > 100000
  ")
  cat("   ✓ Removed", removed, "extreme coordinate outliers\n\n")
} else {
  cat("   ✓ No extreme coordinate outliers (>100000) found\n\n")
}

# Step 1b: Check for coordinates outside valid German bounds
# Germany's approximate bounds: 47-55°N latitude, 5-15°E longitude
cat("   Checking coordinates outside valid German bounds (47-55°N, 5-15°E)...\n")
out_of_bounds_check <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as total_stations,
    COUNT(CASE WHEN longitude > 20 OR longitude < 0 THEN 1 END) as longitude_outliers,
    COUNT(CASE WHEN latitude > 55 OR latitude < 47 THEN 1 END) as latitude_outliers,
    MIN(longitude) as min_lon, MAX(longitude) as max_lon,
    MIN(latitude) as min_lat, MAX(latitude) as max_lat
  FROM german_stations
  WHERE latitude != 0 AND longitude != 0
")

cat("   Current coordinate ranges:\n")
cat("   - Latitude: ", out_of_bounds_check$min_lat, "°N to ", out_of_bounds_check$max_lat, "°N\n")
cat("   - Longitude: ", out_of_bounds_check$min_lon, "°E to ", out_of_bounds_check$max_lon, "°E\n")
cat("   - Longitude outliers (>20°E or <0°E):", out_of_bounds_check$longitude_outliers, "\n")
cat("   - Latitude outliers (>55°N or <47°N):", out_of_bounds_check$latitude_outliers, "\n")

# Remove longitude outliers (most critical for model performance)
if(out_of_bounds_check$longitude_outliers > 0) {
  cat("\n CRITICAL: Removing stations with invalid longitude (outside 0-20°E)...\n")
  
  # Show which stations will be removed
  lon_outliers <- dbGetQuery(con, "
    SELECT uuid, name, latitude, longitude, brand
    FROM german_stations
    WHERE longitude > 20 OR longitude < 0
    ORDER BY ABS(longitude - 10) DESC
  ")
  cat("   Stations with invalid longitude:\n")
  print(lon_outliers)
  
  removed_lon <- dbExecute(con, "
    DELETE FROM german_stations 
    WHERE longitude > 20 OR longitude < 0
  ")
  cat("   Removed", removed_lon, "stations with invalid longitude\n")
  cat("   (These outliers would corrupt normalization statistics)\n\n")
} else {
  cat("   No longitude outliers found\n\n")
}

# Remove latitude outliers (outside Germany's borders)
if(out_of_bounds_check$latitude_outliers > 0) {
  cat("\n CRITICAL: Removing stations with invalid latitude (outside 47-55°N)...\n")
  
  # Show which stations will be removed
  lat_outliers <- dbGetQuery(con, "
    SELECT uuid, name, latitude, longitude, brand
    FROM german_stations
    WHERE latitude > 55 OR latitude < 47
    ORDER BY ABS(latitude - 51) DESC
  ")
  cat("   Stations with invalid latitude:\n")
  print(lat_outliers)
  
  removed_lat <- dbExecute(con, "
    DELETE FROM german_stations 
    WHERE latitude > 55 OR latitude < 47
  ")
  cat("   Removed", removed_lat, "stations with invalid latitude\n")
  cat("   (These outliers are outside Germany's borders)\n\n")
} else {
  cat("   No latitude outliers found\n\n")
}

# Step 1c: Remove zero coordinates and test entries
zero_coord_check <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as zero_coords,
    COUNT(CASE WHEN LOWER(name) LIKE '%test%' OR LOWER(name) LIKE '%delete%' OR LOWER(name) LIKE '%please%' THEN 1 END) as test_entries
  FROM german_stations
  WHERE latitude = 0 AND longitude = 0
")

if(zero_coord_check$zero_coords > 0) {
  cat("Found", zero_coord_check$zero_coords, "stations with zero coordinates (0,0)\n")
  if(zero_coord_check$test_entries > 0) {
    cat("Including", zero_coord_check$test_entries, "test/placeholder entries\n")
  }
  
  # Show sample zero coordinate entries
  zero_entries <- dbGetQuery(con, "
    SELECT uuid, name, latitude, longitude, brand
    FROM german_stations
    WHERE latitude = 0 AND longitude = 0
    ORDER BY name
    LIMIT 10
  ")
  cat("   Sample zero coordinate entries:\n")
  print(zero_entries)
  
  removed_zero <- dbExecute(con, "
    DELETE FROM german_stations 
    WHERE latitude = 0 AND longitude = 0
  ")
  cat("   ✓ Removed", removed_zero, "stations with zero coordinates\n\n")
} else {
  cat("   ✓ No stations with zero coordinates found\n\n")
}

# Final coordinate quality check
final_coord_check <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as total_stations,
    COUNT(CASE WHEN latitude BETWEEN 47 AND 55 AND longitude BETWEEN 5 AND 15 THEN 1 END) as valid_german_coords,
    ROUND(AVG(latitude), 2) as avg_lat,
    ROUND(AVG(longitude), 2) as avg_lon,
    ROUND(STDDEV(latitude), 2) as std_lat,
    ROUND(STDDEV(longitude), 2) as std_lon
  FROM german_stations
  WHERE latitude != 0 AND longitude != 0
")

cat("   Final coordinate statistics (for normalization):\n")
cat("   - Total stations:", final_coord_check$total_stations, "\n")
cat("   - Within German bounds (47-55°N, 5-15°E):", final_coord_check$valid_german_coords, 
    "(", round(final_coord_check$valid_german_coords/final_coord_check$total_stations*100, 1), "%)\n")
cat("   - Mean latitude:", final_coord_check$avg_lat, "°N (σ =", final_coord_check$std_lat, ")\n")
cat("   - Mean longitude:", final_coord_check$avg_lon, "°E (σ =", final_coord_check$std_lon, ")\n")
cat("   → These statistics will be used for feature normalization in the model\n")

# PRICE OUTLIERS
cat("\n2. Checking price outliers...\n")
price_outlier_check <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as total_prices,
    COUNT(CASE WHEN diesel <= 0 OR diesel >= 10 THEN 1 END) as diesel_outliers,
    COUNT(CASE WHEN e5 <= 0 OR e5 >= 10 THEN 1 END) as e5_outliers,
    COUNT(CASE WHEN e10 <= 0 OR e10 >= 10 THEN 1 END) as e10_outliers,
    COUNT(CASE WHEN diesel BETWEEN 0.5 AND 5.0 THEN 1 END) as valid_diesel,
    COUNT(CASE WHEN e5 BETWEEN 0.5 AND 5.0 THEN 1 END) as valid_e5,
    COUNT(CASE WHEN e10 BETWEEN 0.5 AND 5.0 THEN 1 END) as valid_e10
  FROM german_prices
  WHERE date >= '2024-12-01' AND date <= '2025-09-30'
")

cat("   Price quality check:\n")
cat("   - Total price records:", format(price_outlier_check$total_prices, big.mark = ","), "\n")
cat("   - Diesel outliers (≤0 or ≥10):", format(price_outlier_check$diesel_outliers, big.mark = ","), 
    "(", round(price_outlier_check$diesel_outliers/price_outlier_check$total_prices*100, 2), "%)\n")
cat("   - E5 outliers (≤0 or ≥10):", format(price_outlier_check$e5_outliers, big.mark = ","), 
    "(", round(price_outlier_check$e5_outliers/price_outlier_check$total_prices*100, 2), "%)\n")
cat("   - E10 outliers (≤0 or ≥10):", format(price_outlier_check$e10_outliers, big.mark = ","), 
    "(", round(price_outlier_check$e10_outliers/price_outlier_check$total_prices*100, 2), "%)\n")
cat("   - Valid prices (0.5-5.0): Diesel", round(price_outlier_check$valid_diesel/price_outlier_check$total_prices*100, 1), 
    "%, E5", round(price_outlier_check$valid_e5/price_outlier_check$total_prices*100, 1), 
    "%, E10", round(price_outlier_check$valid_e10/price_outlier_check$total_prices*100, 1), "%\n")

# Show sample price outliers
if(price_outlier_check$diesel_outliers > 0 || price_outlier_check$e5_outliers > 0 || price_outlier_check$e10_outliers > 0) {
  cat("   Sample price outliers:\n")
  price_outliers <- dbGetQuery(con, "
    SELECT date, station_uuid, diesel, e5, e10
    FROM german_prices
    WHERE (diesel <= 0 OR diesel >= 10 OR e5 <= 0 OR e5 >= 10 OR e10 <= 0 OR e10 >= 10)
    AND date >= '2024-12-01' AND date <= '2025-09-30'
    ORDER BY RANDOM()
    LIMIT 10
  ")
  print(price_outliers)
}

# FINAL DATA QUALITY SUMMARY
cat("\n4. Final data quality summary after cleanup:\n")
final_stats <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as total_stations,
    COUNT(CASE WHEN latitude BETWEEN 47 AND 55 AND longitude BETWEEN 5 AND 15 THEN 1 END) as valid_coords,
    COUNT(CASE WHEN brand IS NULL OR brand = '' THEN 1 END) as unknown_brands,
    ROUND(AVG(latitude), 2) as avg_lat,
    ROUND(AVG(longitude), 2) as avg_lon
  FROM german_stations
")

cat("   - Total stations:", final_stats$total_stations, "\n")
cat("   - Valid coordinates:", final_stats$valid_coords, 
    "(", round(final_stats$valid_coords/final_stats$total_stations*100, 1), "%)\n")
cat("   - Unknown brands:", final_stats$unknown_brands, 
    "(", round(final_stats$unknown_brands/final_stats$total_stations*100, 1), "%)\n")
cat("   - Average location: ", final_stats$avg_lat, "°N, ", final_stats$avg_lon, "°E\n")

cat("\n   ✓ Data quality assessment complete\n\n")

# Check if nearby_stations_1km already exists
nearby_check <- dbGetQuery(con, "
  SELECT COUNT(*) as has_nearby_column
  FROM information_schema.columns 
  WHERE table_name = 'german_stations' 
    AND column_name = 'nearby_stations_1km'
")

if (nearby_check$has_nearby_column == 0) {
  cat("   nearby_stations_1km column doesn't exist - calculating...\n")
  
  # Load station metadata with valid coordinates
  station_metadata <- dbGetQuery(con, "
    SELECT uuid, latitude, longitude, brand
    FROM german_stations
    WHERE latitude IS NOT NULL AND longitude IS NOT NULL
  ")

  cat("   Loaded ", nrow(station_metadata), " stations with valid coordinates\n")

  # Convert to spatial format
  stations_sf <- st_as_sf(station_metadata, 
                         coords = c("longitude", "latitude"),
                         crs = 4326)  # WGS84 coordinate system

  # Calculate nearby stations using spatial indexing
  nearby_stations_1km <- sapply(1:nrow(stations_sf), function(i) {
    if(i %% 1000 == 0) {
      cat("   Processing station ", i, " of ", nrow(stations_sf), "\n")
    }
    
    # Get stations within 1km (including self)
    nearby_indices <- st_is_within_distance(stations_sf[i,], 
                                          stations_sf, 
                                          dist = 1000)
    # Count nearby stations (subtract 1 to exclude self)
    return(length(nearby_indices[[1]]) - 1)
  })

  # Add to station metadata
  station_metadata$nearby_stations_1km <- nearby_stations_1km

  print(summary(station_metadata$nearby_stations_1km))

  # Update database with nearby stations feature

  # Add column if it doesn't exist
  tryCatch({
    dbExecute(con, "
      ALTER TABLE german_stations 
      ADD COLUMN nearby_stations_1km INTEGER
    ")
    cat(" Added nearby_stations_1km column\n")
  }, error = function(e) {
    cat(" Column already exist:", e$message, "\n")
  })

  # Create temporary table for update
  dbWriteTable(con, "temp_nearby_stations", 
               station_metadata[, c("uuid", "nearby_stations_1km")], 
               overwrite = TRUE)

  # Update the main stations table
  dbExecute(con, "
    UPDATE german_stations 
    SET nearby_stations_1km = t.nearby_stations_1km
    FROM temp_nearby_stations t
    WHERE german_stations.uuid = t.uuid
  ")

  # Clean up temporary table
  dbExecute(con, "DROP TABLE temp_nearby_stations")

  # Verify the update
  verification <- dbGetQuery(con, "
    SELECT COUNT(*) as total_stations,
           COUNT(nearby_stations_1km) as stations_with_nearby_data,
           AVG(nearby_stations_1km) as avg_nearby_stations,
           MIN(nearby_stations_1km) as min_nearby_stations,
           MAX(nearby_stations_1km) as max_nearby_stations
    FROM german_stations
  ")

  cat("   Verification results:\n")
  print(verification)
} else {
  cat("   ✓ nearby_stations_1km column already exists, skipping calculation\n")
}

# =============================================================================
# TRAIN/TEST SPLIT AND FEATURE ENGINEERING
# =============================================================================

cat("\n5. Creating proper train/test splits and feature engineering...\n")
cat("   ✓ CORRECT APPROACH: Split first, then normalize using training-only statistics\n")
cat("   ✓ NO DATA LEAKAGE: Test set is completely independent\n")

# Step 1: Create raw train/test splits FIRST (before any feature engineering)
cat("   Step 1: Creating raw train/test splits...\n")

# Drop existing tables if they exist
dbExecute(con, "DROP TABLE IF EXISTS diesel_train_test")
dbExecute(con, "DROP TABLE IF EXISTS e5_train_test")
dbExecute(con, "DROP TABLE IF EXISTS e10_train_test")
dbExecute(con, "DROP TABLE IF EXISTS diesel_train_test_features")
dbExecute(con, "DROP TABLE IF EXISTS e5_train_test_features")
dbExecute(con, "DROP TABLE IF EXISTS e10_train_test_features")

# Create diesel raw split
cat("   Creating diesel raw train/test split...\n")
dbExecute(con, "
  CREATE TABLE diesel_train_test AS
  WITH diesel_data AS (
    SELECT 
      p.date, p.station_uuid, p.diesel as price,
      s.latitude, s.longitude, s.brand, s.nearby_stations_1km,
      ROW_NUMBER() OVER (ORDER BY RANDOM()) as rn,
      COUNT(*) OVER () as total_count
    FROM german_prices p
    JOIN german_stations s ON p.station_uuid = s.uuid
    WHERE p.date >= '2024-12-01' AND p.date <= '2025-09-30'
      AND p.diesel > 0 AND p.diesel < 10
      AND s.latitude IS NOT NULL AND s.longitude IS NOT NULL
      AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  )
  SELECT 
    date, station_uuid, price, latitude, longitude, brand, nearby_stations_1km,
    CASE WHEN rn <= (total_count * 0.8) THEN 'train' ELSE 'test' END as split_type
  FROM diesel_data
")

# Create e5 raw split
cat("   Creating e5 raw train/test split...\n")
dbExecute(con, "
  CREATE TABLE e5_train_test AS
  WITH e5_data AS (
    SELECT 
      p.date, p.station_uuid, p.e5 as price,
      s.latitude, s.longitude, s.brand, s.nearby_stations_1km,
      ROW_NUMBER() OVER (ORDER BY RANDOM()) as rn,
      COUNT(*) OVER () as total_count
    FROM german_prices p
    JOIN german_stations s ON p.station_uuid = s.uuid
    WHERE p.date >= '2024-12-01' AND p.date <= '2025-09-30'
      AND p.e5 > 0 AND p.e5 < 10
      AND s.latitude IS NOT NULL AND s.longitude IS NOT NULL
      AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  )
  SELECT 
    date, station_uuid, price, latitude, longitude, brand, nearby_stations_1km,
    CASE WHEN rn <= (total_count * 0.8) THEN 'train' ELSE 'test' END as split_type
  FROM e5_data
")

# Create e10 raw split
cat("   Creating e10 raw train/test split...\n")
dbExecute(con, "
  CREATE TABLE e10_train_test AS
  WITH e10_data AS (
    SELECT 
      p.date, p.station_uuid, p.e10 as price,
      s.latitude, s.longitude, s.brand, s.nearby_stations_1km,
      ROW_NUMBER() OVER (ORDER BY RANDOM()) as rn,
      COUNT(*) OVER () as total_count
    FROM german_prices p
    JOIN german_stations s ON p.station_uuid = s.uuid
    WHERE p.date >= '2024-12-01' AND p.date <= '2025-09-30'
      AND p.e10 > 0 AND p.e10 < 10
      AND s.latitude IS NOT NULL AND s.longitude IS NOT NULL
      AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  )
  SELECT 
    date, station_uuid, price, latitude, longitude, brand, nearby_stations_1km,
    CASE WHEN rn <= (total_count * 0.8) THEN 'train' ELSE 'test' END as split_type
  FROM e10_data
")

# Step 2: Calculate normalization statistics ONLY from training data
cat("   Step 2: Calculating normalization statistics from training data only...\n")

# Get diesel training statistics
diesel_train_stats <- dbGetQuery(con, "
  SELECT 
    AVG(latitude) as lat_mean,
    AVG(longitude) as lon_mean,
    AVG(nearby_stations_1km) as nearby_mean,
    STDDEV(latitude) as lat_std,
    STDDEV(longitude) as lon_std,
    STDDEV(nearby_stations_1km) as nearby_std
  FROM diesel_train_test 
  WHERE split_type = 'train'
")

# Get e5 training statistics
e5_train_stats <- dbGetQuery(con, "
  SELECT 
    AVG(latitude) as lat_mean,
    AVG(longitude) as lon_mean,
    AVG(nearby_stations_1km) as nearby_mean,
    STDDEV(latitude) as lat_std,
    STDDEV(longitude) as lon_std,
    STDDEV(nearby_stations_1km) as nearby_std
  FROM e5_train_test 
  WHERE split_type = 'train'
")

# Get e10 training statistics
e10_train_stats <- dbGetQuery(con, "
  SELECT 
    AVG(latitude) as lat_mean,
    AVG(longitude) as lon_mean,
    AVG(nearby_stations_1km) as nearby_mean,
    STDDEV(latitude) as lat_std,
    STDDEV(longitude) as lon_std,
    STDDEV(nearby_stations_1km) as nearby_std
  FROM e10_train_test 
  WHERE split_type = 'train'
")

cat("   Training-only normalization statistics:\n")
cat("   - Diesel: lat_mean =", round(diesel_train_stats$lat_mean, 4), ", lon_mean =", round(diesel_train_stats$lon_mean, 4), "\n")
cat("   - E5: lat_mean =", round(e5_train_stats$lat_mean, 4), ", lon_mean =", round(e5_train_stats$lon_mean, 4), "\n")
cat("   - E10: lat_mean =", round(e10_train_stats$lat_mean, 4), ", lon_mean =", round(e10_train_stats$lon_mean, 4), "\n")

# Step 3: Create feature-engineered tables with proper normalization
cat("   Step 3: Creating feature-engineered tables with proper normalization...\n")

# Create diesel features with proper normalization
cat("   Creating diesel features...\n")
dbExecute(con, "
  CREATE OR REPLACE TABLE diesel_train_test_features AS
  SELECT 
    date, station_uuid, price, split_type,
    -- Temporal features
    EXTRACT(hour FROM date) as hour,
    EXTRACT(dow FROM date) as day_of_week,
    EXTRACT(month FROM date) as month,
    SIN(2 * PI() * EXTRACT(hour FROM date) / 24) as hour_sin,
    COS(2 * PI() * EXTRACT(hour FROM date) / 24) as hour_cos,
    SIN(2 * PI() * EXTRACT(dow FROM date) / 7) as dow_sin,
    COS(2 * PI() * EXTRACT(dow FROM date) / 7) as dow_cos,
    SIN(2 * PI() * EXTRACT(month FROM date) / 12) as month_sin,
    COS(2 * PI() * EXTRACT(month FROM date) / 12) as month_cos,
    -- Brand features
    CASE
      WHEN LOWER(brand) IN ('shell', 'esso', 'bp', 'aral', 'total', 'jet') THEN 'major_brand'
      WHEN LOWER(brand) IN ('omv', 'eni', 'q8', 'avia', 'star', 'esso express') THEN 'regional_brand'
      WHEN LOWER(brand) IN ('freie tankstelle', 'freie', 'independent', 'unbranded') THEN 'independent'
      ELSE 'other'
    END as brand_category,
    -- Normalized spatial features (using training-only statistics)
    latitude - ? as latitude_centered,
    longitude - ? as longitude_centered,
    (nearby_stations_1km - ?) / ? as nearby_stations_1km_norm
  FROM diesel_train_test
", params = list(
  diesel_train_stats$lat_mean, diesel_train_stats$lon_mean,
  diesel_train_stats$nearby_mean, diesel_train_stats$nearby_std
))

# Create e5 features with proper normalization
cat("   Creating e5 features...\n")
dbExecute(con, "
  CREATE OR REPLACE TABLE e5_train_test_features AS
  SELECT 
    date, station_uuid, price, split_type,
    -- Temporal features
    EXTRACT(hour FROM date) as hour,
    EXTRACT(dow FROM date) as day_of_week,
    EXTRACT(month FROM date) as month,
    SIN(2 * PI() * EXTRACT(hour FROM date) / 24) as hour_sin,
    COS(2 * PI() * EXTRACT(hour FROM date) / 24) as hour_cos,
    SIN(2 * PI() * EXTRACT(dow FROM date) / 7) as dow_sin,
    COS(2 * PI() * EXTRACT(dow FROM date) / 7) as dow_cos,
    SIN(2 * PI() * EXTRACT(month FROM date) / 12) as month_sin,
    COS(2 * PI() * EXTRACT(month FROM date) / 12) as month_cos,
    -- Brand features
    CASE
      WHEN LOWER(brand) IN ('shell', 'esso', 'bp', 'aral', 'total', 'jet') THEN 'major_brand'
      WHEN LOWER(brand) IN ('omv', 'eni', 'q8', 'avia', 'star', 'esso express') THEN 'regional_brand'
      WHEN LOWER(brand) IN ('freie tankstelle', 'freie', 'independent', 'unbranded') THEN 'independent'
      ELSE 'other'
    END as brand_category,
    -- Normalized spatial features (using training-only statistics)
    latitude - ? as latitude_centered,
    longitude - ? as longitude_centered,
    (nearby_stations_1km - ?) / ? as nearby_stations_1km_norm
  FROM e5_train_test
", params = list(
  e5_train_stats$lat_mean, e5_train_stats$lon_mean,
  e5_train_stats$nearby_mean, e5_train_stats$nearby_std
))

# Create e10 features with proper normalization
cat("   Creating e10 features...\n")
dbExecute(con, "
  CREATE OR REPLACE TABLE e10_train_test_features AS
  SELECT 
    date, station_uuid, price, split_type,
    -- Temporal features
    EXTRACT(hour FROM date) as hour,
    EXTRACT(dow FROM date) as day_of_week,
    EXTRACT(month FROM date) as month,
    SIN(2 * PI() * EXTRACT(hour FROM date) / 24) as hour_sin,
    COS(2 * PI() * EXTRACT(hour FROM date) / 24) as hour_cos,
    SIN(2 * PI() * EXTRACT(dow FROM date) / 7) as dow_sin,
    COS(2 * PI() * EXTRACT(dow FROM date) / 7) as dow_cos,
    SIN(2 * PI() * EXTRACT(month FROM date) / 12) as month_sin,
    COS(2 * PI() * EXTRACT(month FROM date) / 12) as month_cos,
    -- Brand features
    CASE
      WHEN LOWER(brand) IN ('shell', 'esso', 'bp', 'aral', 'total', 'jet') THEN 'major_brand'
      WHEN LOWER(brand) IN ('omv', 'eni', 'q8', 'avia', 'star', 'esso express') THEN 'regional_brand'
      WHEN LOWER(brand) IN ('freie tankstelle', 'freie', 'independent', 'unbranded') THEN 'independent'
      ELSE 'other'
    END as brand_category,
    -- Normalized spatial features (using training-only statistics)
    latitude - ? as latitude_centered,
    longitude - ? as longitude_centered,
    (nearby_stations_1km - ?) / ? as nearby_stations_1km_norm
  FROM e10_train_test
", params = list(
  e10_train_stats$lat_mean, e10_train_stats$lon_mean,
  e10_train_stats$nearby_mean, e10_train_stats$nearby_std
))

# Add indexes for better performance
cat("   Adding indexes for better performance...\n")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_diesel_split_type ON diesel_train_test_features(split_type)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_e5_split_type ON e5_train_test_features(split_type)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_e10_split_type ON e10_train_test_features(split_type)")

# Final verification
cat("   Final verification of train/test splits...\n")

# Check diesel split
diesel_train_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM diesel_train_test_features WHERE split_type = 'train'")$count
diesel_test_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM diesel_train_test_features WHERE split_type = 'test'")$count
diesel_total <- diesel_train_count + diesel_test_count

# Check e5 split
e5_train_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM e5_train_test_features WHERE split_type = 'train'")$count
e5_test_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM e5_train_test_features WHERE split_type = 'test'")$count
e5_total <- e5_train_count + e5_test_count

# Check e10 split
e10_train_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM e10_train_test_features WHERE split_type = 'train'")$count
e10_test_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM e10_train_test_features WHERE split_type = 'test'")$count
e10_total <- e10_train_count + e10_test_count

cat("   Final record counts:\n")
cat("   - Diesel: Total", format(diesel_total, big.mark = ","), "- Train:", format(diesel_train_count, big.mark = ","), 
    "(", round(diesel_train_count/diesel_total*100, 1), "%), Test:", format(diesel_test_count, big.mark = ","), 
    "(", round(diesel_test_count/diesel_total*100, 1), "%)\n")
cat("   - E5: Total", format(e5_total, big.mark = ","), "- Train:", format(e5_train_count, big.mark = ","), 
    "(", round(e5_train_count/e5_total*100, 1), "%), Test:", format(e5_test_count, big.mark = ","), 
    "(", round(e5_test_count/e5_total*100, 1), "%)\n")
cat("   - E10: Total", format(e10_total, big.mark = ","), "- Train:", format(e10_train_count, big.mark = ","), 
    "(", round(e10_train_count/e10_total*100, 1), "%), Test:", format(e10_test_count, big.mark = ","), 
    "(", round(e10_test_count/e10_total*100, 1), "%)\n")

# Close Database Connection 
dbDisconnect(con)

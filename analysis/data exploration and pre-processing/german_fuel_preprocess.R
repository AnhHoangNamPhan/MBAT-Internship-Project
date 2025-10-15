# German Fuel Data Preprocessing Script
# This script performs preprocessing steps for the German fuel data:
# 1. Creates database indexes for better query performance
# 2. Pre-calculates nearby stations feature for spatial analysis

library(DBI)
library(duckdb)
library(sf)
library(dplyr)

# Configuration 
db_path <- "../databases/german_fuel_data.duckdb"

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
    COUNT(CASE WHEN latitude > 60 OR latitude < 40 THEN 1 END) as latitude_outliers,
    MIN(longitude) as min_lon, MAX(longitude) as max_lon,
    MIN(latitude) as min_lat, MAX(latitude) as max_lat
  FROM german_stations
  WHERE latitude != 0 AND longitude != 0
")

cat("   Current coordinate ranges:\n")
cat("   - Latitude: ", out_of_bounds_check$min_lat, "°N to ", out_of_bounds_check$max_lat, "°N\n")
cat("   - Longitude: ", out_of_bounds_check$min_lon, "°E to ", out_of_bounds_check$max_lon, "°E\n")
cat("   - Longitude outliers (>20°E or <0°E):", out_of_bounds_check$longitude_outliers, "\n")
cat("   - Latitude outliers (>60°N or <40°N):", out_of_bounds_check$latitude_outliers, "\n")

# Remove longitude outliers (most critical for model performance)
if(out_of_bounds_check$longitude_outliers > 0) {
  cat("\n   ⚠️  CRITICAL: Removing stations with invalid longitude (outside 0-20°E)...\n")
  
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
  cat("   ✓ Removed", removed_lon, "stations with invalid longitude\n")
  cat("   → These outliers would have corrupted normalization statistics!\n\n")
} else {
  cat("   ✓ No longitude outliers found\n\n")
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

# Nearby Stations Feature Engineering

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

# Close Database Connection 
dbDisconnect(con)

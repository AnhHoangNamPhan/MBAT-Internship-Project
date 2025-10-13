# German Fuel Data Preprocessing Script
# This script performs preprocessing steps for the German fuel data:
# 1. Creates database indexes for better query performance
# 2. Pre-calculates nearby stations feature for spatial analysis

library(DBI)
library(duckdb)
library(sf)
library(dplyr)

# Configuration 
db_path <- "/Users/alexphan/Desktop/MBAT-Internship-Project/databases/german_fuel_data.duckdb"

# Connect to Database
con <- dbConnect(duckdb(), db_path, read_only = FALSE)

# Set optimal DuckDB memory settings for preprocessing
dbExecute(con, "SET memory_limit='16GB'")
dbExecute(con, "SET max_temp_directory_size='20GB'") 
dbExecute(con, "SET threads=8")

# Part 1: Creating Database Index

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

# Part 2: Nearby Stations Feature Engineering

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

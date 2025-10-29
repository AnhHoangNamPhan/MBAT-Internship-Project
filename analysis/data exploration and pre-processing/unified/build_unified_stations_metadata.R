# Rebuild Unified Stations Database with Regional Mapping and Brand Categorization
# This script rebuilds the stations.duckdb database with proper deduplication and brand categorization

library(DBI)
library(duckdb)
library(dplyr)

# Load regional mapping functions
source("analysis/data exploration and pre-processing/unified/regional_mapping_functions.R")

cat("=== BUILDING UNIFIED STATIONS DATABASE ===\n")
cat("Features: Regional mapping, Austrian deduplication, Brand categorization\n\n")

# Paths
aus_db <- "databases/austrian_fuel_database.duckdb"
slo_db <- "databases/slovenian_fuel_database.duckdb"
ger_csv <- "databases/german_stations.csv"

# Create/overwrite target metadata DB
target_path <- "databases/stations.duckdb"
if (file.exists(target_path)) file.remove(target_path)
con <- dbConnect(duckdb(), target_path, read_only = FALSE)

# Performance settings
dbExecute(con, "SET memory_limit='32GB'")
dbExecute(con, "SET max_temp_directory_size='50GB'")
dbExecute(con, "SET threads=16")
dbExecute(con, "SET preserve_insertion_order=false")

# Create destination table with region and brand_category columns
dbExecute(con, "
CREATE TABLE stations (
  station_uuid VARCHAR,
  station_name VARCHAR,
  station_brand VARCHAR,
  latitude DOUBLE,
  longitude DOUBLE,
  address VARCHAR,
  city VARCHAR,
  state VARCHAR,
  zip_code VARCHAR,
  country VARCHAR,
  nearby_station_1km INTEGER DEFAULT 0,
  region VARCHAR,
  brand_category VARCHAR,
  PRIMARY KEY (station_uuid, country)
)")

cat("✓ Created stations table with region and brand_category columns\n")

# Attach source DBs if available
try(dbExecute(con, sprintf("ATTACH '%s' AS aus (READ_ONLY)", aus_db)), silent = TRUE)
try(dbExecute(con, sprintf("ATTACH '%s' AS slo (READ_ONLY)", slo_db)), silent = TRUE)

# Create source views
dbExecute(con, "CREATE OR REPLACE VIEW v_aus AS
  SELECT 
    station_uuid,
    station_name,
    COALESCE(NULLIF(station_brand, ''), station_operator) AS station_brand,
    ROUND(latitude, 4) AS latitude,
    ROUND(longitude, 4) AS longitude,
    address,
    city,
    state,
    zip_code,
    'Austria'::VARCHAR AS country
  FROM aus.austrian_stations")

dbExecute(con, "CREATE OR REPLACE VIEW v_slo AS
  SELECT 
    CAST(station_uuid AS VARCHAR) AS station_uuid,
    station_name,
    COALESCE(NULLIF(station_brand, ''), station_operator) AS station_brand,
    ROUND(latitude, 4) AS latitude,
    ROUND(longitude, 4) AS longitude,
    address,
    city,
    state,
    CAST(zip_code AS VARCHAR) AS zip_code,
    'Slovenia'::VARCHAR AS country
  FROM slo.slovenian_stations")

# German stations from CSV
dbExecute(con, sprintf("CREATE OR REPLACE VIEW v_ger AS
    SELECT 
      uuid AS station_uuid,
      name AS station_name,
      brand AS station_brand,
      ROUND(CAST(latitude AS DOUBLE), 4) AS latitude,
      ROUND(CAST(longitude AS DOUBLE), 4) AS longitude,
      CONCAT_WS(' ', street, CAST(house_number AS VARCHAR)) AS address,
      city,
      NULL::VARCHAR AS state,
      post_code AS zip_code,
      'Germany'::VARCHAR AS country
    FROM read_csv_auto('%s')", ger_csv))

cat("✓ Created source views\n")

# Union all sources into a staging table
dbExecute(con, "CREATE TEMP TABLE staging AS
  SELECT * FROM v_aus
  UNION ALL
  SELECT * FROM v_slo
  UNION ALL
  SELECT * FROM v_ger")

cat("✓ Created staging table\n")

# Insert all stations (keep all IDs for price matching)
cat(" Inserting all stations...\n")
dbExecute(con, "INSERT INTO stations (station_uuid, station_name, station_brand, 
                                    latitude, longitude, address, city, state, zip_code, country)
  SELECT station_uuid, station_name, station_brand,
         latitude, longitude, address, city, state, zip_code, country
  FROM staging")

cat("✓ All stations inserted (keeping all IDs for price matching)\n")

# Apply regional mapping
cat("\n️ Applying regional mapping...\n")

# Get all stations for mapping
all_stations <- dbGetQuery(con, "SELECT * FROM stations")

# Apply regional mapping
cat("  Mapping German stations...\n")
german_stations <- all_stations %>% filter(country == "Germany")
german_stations$region <- map_german_region_vec(german_stations$zip_code)

cat("  Mapping Austrian stations...\n")
austrian_stations <- all_stations %>% filter(country == "Austria")
austrian_stations$region <- map_austrian_region_vec(
  austrian_stations$state,
  austrian_stations$city,
  austrian_stations$longitude
)

cat("  Mapping Slovenian stations...\n")
slovenian_stations <- all_stations %>% filter(country == "Slovenia")
slovenian_stations$region <- map_slovenian_region_vec(slovenian_stations$longitude)

# Combine all mapped stations
mapped_stations <- rbind(german_stations, austrian_stations, slovenian_stations)

# Update stations table with regions
for (i in 1:nrow(mapped_stations)) {
  station <- mapped_stations[i, ]
  update_query <- sprintf(
    "UPDATE stations SET region = '%s' WHERE station_uuid = '%s' AND country = '%s'",
    station$region, station$station_uuid, station$country
  )
  dbExecute(con, update_query)
}

cat("✓ Regional mapping applied\n")

# Country-specific brand categorization function
categorize_brand <- function(brand, country) {
  if (is.na(brand) || brand == "" || brand == "Unknown") {
    return("Independent")
  }
  
  brand_upper <- toupper(brand)
  
  # Major International brands - same across all countries
  international_brands <- c("SHELL", "JET", "OMV", "BP", "ENI", "TOTAL", "ESSO", "ARAL", "STAR", "GULF", "Q8", "AGIP")
  if (any(sapply(international_brands, function(x) grepl(x, brand_upper, fixed = TRUE)))) {
    return("International")
  }
  
  # Country-specific regional brands
  if (country == "Germany") {
    regional_brands <- c("AVIA", "AVANTI", "HEM", "OIL!", "CLASSIC", "ED", "STAR", "BFT", "FREIE TANKSTELLE", 
                        "WESTFALEN", "BAYWA", "DISKONT", "PINK", "SPRINT")
  } else if (country == "Austria") {
    regional_brands <- c("GENOL", "TURMÖL", "TURMÖL QUICK", "LAGERHAUS", "SOCAR", "DISKONT", "PINK", 
                        "SPRINT", "FREIE TANKSTELLE", "AVANTI", "AVIA", "OIL!", "HEM", "CLASSIC", "ED")
  } else if (country == "Slovenia") {
    regional_brands <- c("PETROL", "AVIA", "AVANTI", "SOCAR", "DISKONT", "PINK", "SPRINT", 
                        "FREIE TANKSTELLE", "OIL!", "HEM", "CLASSIC", "ED")
  } else if (any(sapply(regional_brands, function(x) grepl(x, brand_upper, fixed = TRUE)))) {
    return("Regional")
  }
  
  # Everything else is Independent
  return("Independent")
}

# Apply brand categorization
cat("\n️ Applying brand categorization...\n")

# Get all stations with brands and countries
stations_with_brands <- dbGetQuery(con, "SELECT station_uuid, country, station_brand FROM stations")

# Categorize brands with country-specific logic
stations_with_brands$brand_category <- mapply(categorize_brand, 
                                             stations_with_brands$station_brand, 
                                             stations_with_brands$country)

# Update stations table with brand categories
for (i in 1:nrow(stations_with_brands)) {
  station <- stations_with_brands[i, ]
  update_query <- sprintf(
    "UPDATE stations SET brand_category = '%s' WHERE station_uuid = '%s' AND country = '%s'",
    station$brand_category, station$station_uuid, station$country
  )
  dbExecute(con, update_query)
}

cat("✓ Brand categorization applied\n")

# Compute nearby_station_1km using Haversine
cat("\n Computing nearby station counts (deduplicating by location and brand)...\n")
dbExecute(con, "CREATE OR REPLACE VIEW v_pairs AS
  SELECT 
    a.station_uuid AS a_uuid, a.country AS a_country,
    a.latitude AS a_lat, a.longitude AS a_lon, a.station_brand AS a_brand,
    b.station_uuid AS b_uuid, b.country AS b_country,
    b.latitude AS b_lat, b.longitude AS b_lon, b.station_brand AS b_brand
  FROM stations a
  JOIN stations b
    ON a.country = b.country
   AND a.station_uuid <> b.station_uuid
   AND abs(a.latitude - b.latitude) <= 0.02
   AND abs(a.longitude - b.longitude) <= 0.02")

dbExecute(con, "CREATE OR REPLACE VIEW v_neighbors AS
  SELECT a_uuid, a_country,
         COUNT(DISTINCT CONCAT(ROUND(b_lat, 4), ',', ROUND(b_lon, 4), ',', COALESCE(b_brand, ''))) AS cnt
  FROM (
    SELECT a_uuid, a_country, b_lat, b_lon, b_brand,
           2 * 6371 * ASIN(SQRT(
               SIN(RADIANS((b_lat - a_lat) / 2)) * SIN(RADIANS((b_lat - a_lat) / 2)) +
               COS(RADIANS(a_lat)) * COS(RADIANS(b_lat)) *
               SIN(RADIANS((b_lon - a_lon) / 2)) * SIN(RADIANS((b_lon - a_lon) / 2))
           )) AS dist_km,
           (a_lat = b_lat AND a_lon = b_lon) AS same_coord
    FROM v_pairs
  )
  WHERE dist_km <= 1.0 AND same_coord = FALSE
  GROUP BY a_uuid, a_country")

dbExecute(con, "UPDATE stations s
  SET nearby_station_1km = COALESCE(n.cnt, 0)
  FROM v_neighbors n
  WHERE s.station_uuid = n.a_uuid AND s.country = n.a_country")

cat("✓ Nearby station counts computed\n")

# Final summary
cat("\n Final Summary\n")
cat("================\n")

# Overall summary
summary <- dbGetQuery(con, "
  SELECT 
    country, 
    COUNT(*) AS stations, 
    COUNT(CASE WHEN region IS NOT NULL THEN 1 END) AS mapped_stations,
    ROUND(COUNT(CASE WHEN region IS NOT NULL THEN 1 END) * 100.0 / COUNT(*), 2) AS coverage_percent,
    SUM(nearby_station_1km) AS total_neighbors 
  FROM stations 
  GROUP BY country 
  ORDER BY stations DESC
")
print(summary)

# Brand category summary
cat("\n Brand Category Distribution\n")
cat("===============================\n")
brand_summary <- dbGetQuery(con, "
  SELECT 
    country,
    brand_category,
    COUNT(*) AS stations
  FROM stations 
  WHERE brand_category IS NOT NULL
  GROUP BY country, brand_category
  ORDER BY country, stations DESC
")
print(brand_summary)

# Regional distribution
cat("\n️ Regional Distribution\n")
cat("========================\n")
regional_summary <- dbGetQuery(con, "
  SELECT 
    country,
    region,
    COUNT(*) AS stations
  FROM stations 
  WHERE region IS NOT NULL
  GROUP BY country, region
  ORDER BY country, stations DESC
")
print(regional_summary)

dbDisconnect(con)

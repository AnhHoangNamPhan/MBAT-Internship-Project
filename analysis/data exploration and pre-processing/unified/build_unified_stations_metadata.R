library(DBI)
library(duckdb)
library(dplyr)

cat("=== BUILDING UNIFIED STATIONS METADATA (stations.duckdb) ===\n")

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

# Create destination table (aligning to German schema superset)
dbExecute(con, "
CREATE TABLE stations (
  station_uuid VARCHAR,
  station_name VARCHAR,
  station_brand VARCHAR,
  station_operator VARCHAR,
  latitude DOUBLE,
  longitude DOUBLE,
  address VARCHAR,
  city VARCHAR,
  state VARCHAR,
  zip_code VARCHAR,
  country VARCHAR,
  nearby_station_1km INTEGER DEFAULT 0,
  PRIMARY KEY (station_uuid, country)
)")

# Attach source DBs if available
try(dbExecute(con, sprintf("ATTACH '%s' AS aus (READ_ONLY)", aus_db)), silent = TRUE)
try(dbExecute(con, sprintf("ATTACH '%s' AS slo (READ_ONLY)", slo_db)), silent = TRUE)

# Create source views
dbExecute(con, "CREATE OR REPLACE VIEW v_aus AS
  SELECT 
    station_uuid,
    station_name,
    station_brand,
    station_operator,
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
    station_brand,
    station_operator,
    ROUND(latitude, 4) AS latitude,
    ROUND(longitude, 4) AS longitude,
    address,
    city,
    state,
    CAST(zip_code AS VARCHAR) AS zip_code,
    'Slovenia'::VARCHAR AS country
  FROM slo.slovenian_stations")

  # German stations from CSV (authoritative list)
dbExecute(con, sprintf("CREATE OR REPLACE VIEW v_ger AS
    SELECT 
      uuid AS station_uuid,
      name AS station_name,
      brand AS station_brand,
      NULL::VARCHAR AS station_operator,
      ROUND(CAST(latitude AS DOUBLE), 4) AS latitude,
      ROUND(CAST(longitude AS DOUBLE), 4) AS longitude,
      CONCAT_WS(' ', street, CAST(house_number AS VARCHAR)) AS address,
      city,
      NULL::VARCHAR AS state,
      post_code AS zip_code,
      'Germany'::VARCHAR AS country
    FROM read_csv_auto('%s')", ger_csv))

# Union all sources into a staging table with de-duplication by (station_uuid,country)
dbExecute(con, "CREATE TEMP TABLE staging AS
  SELECT * FROM v_aus
  UNION ALL
  SELECT * FROM v_slo
  UNION ALL
  SELECT * FROM v_ger")

# Deduplicate: keep first occurrence per (station_uuid,country)
dbExecute(con, "INSERT OR REPLACE INTO stations
  SELECT * FROM (
    SELECT *,
      row_number() OVER (PARTITION BY station_uuid, country ORDER BY station_uuid) AS rn
    FROM staging
  )
  WHERE rn = 1")

cat("✓ Stations inserted\n")

# Compute nearby_station_1km using Haversine; exclude identical lat/lon pairs
# Limit candidate pairs via simple lat/lon window ~0.02 degrees (~2.2km) for speed
dbExecute(con, "CREATE OR REPLACE VIEW v_pairs AS
  SELECT 
    a.station_uuid AS a_uuid, a.country AS a_country,
    a.latitude AS a_lat, a.longitude AS a_lon,
    b.station_uuid AS b_uuid, b.country AS b_country,
    b.latitude AS b_lat, b.longitude AS b_lon
  FROM stations a
  JOIN stations b
    ON a.country = b.country
   AND a.station_uuid <> b.station_uuid
   AND abs(a.latitude - b.latitude) <= 0.02
   AND abs(a.longitude - b.longitude) <= 0.02")

dbExecute(con, "CREATE OR REPLACE VIEW v_neighbors AS
  SELECT a_uuid, a_country,
         COUNT(*) AS cnt
  FROM (
    SELECT a_uuid, a_country,
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

cat("✓ nearby_station_1km computed\n")

# Simple summary
summary <- dbGetQuery(con, "SELECT country, COUNT(*) AS stations, SUM(nearby_station_1km) AS total_neighbors FROM stations GROUP BY country ORDER BY stations DESC")
print(summary)

dbDisconnect(con)
cat("\n✓ stations.duckdb created at databases/stations.duckdb\n")



# German Fuel Dataset Deep Dive Analysis
# Focus on temporal patterns, data collection frequency, and LSTM suitability

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)

# Database path
db_path <- "/Users/alexphan/Desktop/MBAT-Internship-Project/databases/german_fuel_data.duckdb"

# Connect to German database in read-only mode
con <- dbConnect(duckdb(), db_path, read_only = TRUE)

# German Fuel Dataset Deep Dive Analysis

# 1. Overall dataset statistics

overview <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as total_records,
    COUNT(DISTINCT station_uuid) as unique_stations,
    MIN(date) as earliest_date,
    MAX(date) as latest_date,
    COUNT(DISTINCT DATE(date)) as unique_days
  FROM german_prices
")
print(overview)

# 2. Temporal patterns analysis

# Hourly distribution of data collection
hourly_patterns <- dbGetQuery(con, "
  SELECT 
    EXTRACT(hour FROM date) as hour,
    COUNT(*) as record_count,
    COUNT(DISTINCT station_uuid) as unique_stations
  FROM german_prices 
  GROUP BY EXTRACT(hour FROM date)
  ORDER BY hour
")
print(hourly_patterns)

# Daily patterns
daily_patterns <- dbGetQuery(con, "
  SELECT 
    EXTRACT(dow FROM date) as day_of_week,
    COUNT(*) as record_count,
    COUNT(DISTINCT station_uuid) as unique_stations,
    COUNT(DISTINCT DATE(date)) as unique_days
  FROM german_prices 
  GROUP BY EXTRACT(dow FROM date)
  ORDER BY day_of_week
")
print(daily_patterns)

# 3. Station-level analysis

# Records per station statistics
station_stats <- dbGetQuery(con, "
  SELECT 
    station_uuid,
    COUNT(*) as total_records,
    MIN(date) as first_record,
    MAX(date) as last_record,
    COUNT(DISTINCT DATE(date)) as active_days,
    COUNT(DISTINCT EXTRACT(hour FROM date)) as unique_hours
  FROM german_prices 
  GROUP BY station_uuid
  ORDER BY total_records DESC
  LIMIT 10
")
print(station_stats)

# Station record count distribution
record_distribution <- dbGetQuery(con, "
  WITH station_counts AS (
    SELECT station_uuid, COUNT(*) as total_records
    FROM german_prices 
    GROUP BY station_uuid
  ),
  categorized_stations AS (
    SELECT 
      CASE 
        WHEN total_records < 1000 THEN '< 1K'
        WHEN total_records < 5000 THEN '1K-5K'
        WHEN total_records < 10000 THEN '5K-10K'
        WHEN total_records < 20000 THEN '10K-20K'
        WHEN total_records < 50000 THEN '20K-50K'
        ELSE '> 50K'
      END as record_range,
      total_records
    FROM station_counts
  )
  SELECT 
    record_range,
    COUNT(*) as station_count,
    AVG(total_records) as avg_records
  FROM categorized_stations
  GROUP BY record_range
  ORDER BY MIN(CASE 
    WHEN record_range = '< 1K' THEN 1
    WHEN record_range = '1K-5K' THEN 2
    WHEN record_range = '5K-10K' THEN 3
    WHEN record_range = '10K-20K' THEN 4
    WHEN record_range = '20K-50K' THEN 5
    ELSE 6
  END)
")
print(record_distribution)

# 4. Temporal spacing analysis (for LSTM)

# Sample a few stations to check temporal spacing
sample_stations <- dbGetQuery(con, "
  SELECT DISTINCT station_uuid 
  FROM german_prices 
  ORDER BY RANDOM() 
  LIMIT 10
")$station_uuid

# Analyze temporal spacing for 5 random stations
for(i in 1:length(sample_stations)) {
  station_id <- sample_stations[i]
  
  # Get time differences for this station
  time_diffs <- dbGetQuery(con, paste0("
    SELECT 
      date,
      LAG(date) OVER (ORDER BY date) as prev_date,
      EXTRACT(EPOCH FROM (date - LAG(date) OVER (ORDER BY date)))/60 as minutes_diff
    FROM german_prices 
    WHERE station_uuid = '", station_id, "'
    ORDER BY date
    LIMIT 100
  "))
  
  # Calculate spacing statistics
  valid_diffs <- time_diffs$minutes_diff[!is.na(time_diffs$minutes_diff)]
  
  # Print spacing statistics for this station
  cat(paste0("Station ", i, " (", substr(station_id, 1, 8), "):\n"))
  cat(paste0("  Records: ", length(valid_diffs), 
             " | Min: ", round(min(valid_diffs), 1), "min",
             " | Max: ", round(max(valid_diffs), 1), "min",
             " | Mean: ", round(mean(valid_diffs), 1), "min",
             " | Median: ", round(median(valid_diffs), 1), "min\n"))
  
  # Check for common intervals
  interval_counts <- table(round(valid_diffs))
  common_intervals <- head(sort(interval_counts, decreasing = TRUE), 3)
  cat("  Common intervals: ")
  for(j in 1:length(common_intervals)) {
    if(j > 1) cat(", ")
    cat(paste0(names(common_intervals)[j], "min (", common_intervals[j], "x)"))
  }
  cat("\n\n")
}

# 5. Data quality analysis

# NA values check in joined data
sample_query <- "
  SELECT 
    p.date,
    p.station_uuid,
    p.diesel,
    p.e5, 
    p.e10,
    s.latitude,
    s.longitude,
    s.brand
  FROM german_prices p
  JOIN german_stations s ON p.station_uuid = s.uuid
  WHERE p.date >= '2024-12-01' 
    AND p.date <= '2025-09-30'
  LIMIT 10000
"

sample_data <- dbGetQuery(con, sample_query)

# Check for NA values in each column
na_counts <- sapply(sample_data, function(x) sum(is.na(x)))
for(i in 1:length(na_counts)) {
  col_name <- names(na_counts)[i]
  na_count <- na_counts[i]
  na_pct <- round(na_count / nrow(sample_data) * 100, 2)
  cat("- ", col_name, ": ", na_count, " (", na_pct, "%)\n")
}

# Check complete cases
complete_cases <- sum(complete.cases(sample_data))
complete_pct <- round(complete_cases / nrow(sample_data) * 100, 2)
cat("\nComplete cases: ", complete_cases, " out of ", nrow(sample_data), " (", complete_pct, "%)\n")

# Brand analysis
brand_counts <- table(sample_data$brand, useNA = "ifany")
print(brand_counts)

# Price quality by fuel type
price_quality <- dbGetQuery(con, "
  SELECT 
    'diesel' as fuel_type,
    COUNT(*) as total_records,
    COUNT(CASE WHEN diesel > 0 AND diesel < 10 THEN 1 END) as valid_records,
    COUNT(CASE WHEN diesel <= 0 OR diesel >= 10 THEN 1 END) as invalid_records,
    AVG(CASE WHEN diesel > 0 AND diesel < 10 THEN diesel END) as avg_price,
    MIN(CASE WHEN diesel > 0 AND diesel < 10 THEN diesel END) as min_price,
    MAX(CASE WHEN diesel > 0 AND diesel < 10 THEN diesel END) as max_price
  FROM german_prices 
  
  UNION ALL
  
  SELECT 
    'e5' as fuel_type,
    COUNT(*) as total_records,
    COUNT(CASE WHEN e5 > 0 AND e5 < 10 THEN 1 END) as valid_records,
    COUNT(CASE WHEN e5 <= 0 OR e5 >= 10 THEN 1 END) as invalid_records,
    AVG(CASE WHEN e5 > 0 AND e5 < 10 THEN e5 END) as avg_price,
    MIN(CASE WHEN e5 > 0 AND e5 < 10 THEN e5 END) as min_price,
    MAX(CASE WHEN e5 > 0 AND e5 < 10 THEN e5 END) as max_price
  FROM german_prices 
  
  UNION ALL
  
  SELECT 
    'e10' as fuel_type,
    COUNT(*) as total_records,
    COUNT(CASE WHEN e10 > 0 AND e10 < 10 THEN 1 END) as valid_records,
    COUNT(CASE WHEN e10 > 0 AND e10 < 10 THEN 1 END) as invalid_records,
    AVG(CASE WHEN e10 > 0 AND e10 < 10 THEN e10 END) as avg_price,
    MIN(CASE WHEN e10 > 0 AND e10 < 10 THEN e10 END) as min_price,
    MAX(CASE WHEN e10 > 0 AND e10 < 10 THEN e10 END) as max_price
  FROM german_prices 
")

print(price_quality)

# 6. Visualizations

# Hourly pattern visualization
hourly_data <- dbGetQuery(con, "
  SELECT 
    EXTRACT(hour FROM date) as hour,
    COUNT(*) as record_count
  FROM german_prices 
  GROUP BY EXTRACT(hour FROM date)
  ORDER BY hour
")

p1 <- ggplot(hourly_data, aes(x = hour, y = record_count)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "German Fuel Data Collection by Hour",
       x = "Hour of Day", y = "Number of Records") +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal()

print(p1)

# Daily pattern visualization
daily_data <- dbGetQuery(con, "
  SELECT 
    EXTRACT(dow FROM date) as day_of_week,
    COUNT(*) as record_count
  FROM german_prices 
  GROUP BY EXTRACT(dow FROM date)
  ORDER BY day_of_week
")

# Add day names
daily_data$day_name <- c("Sunday", "Monday", "Tuesday", "Wednesday", 
                        "Thursday", "Friday", "Saturday")[daily_data$day_of_week + 1]

p2 <- ggplot(daily_data, aes(x = reorder(day_name, day_of_week), y = record_count)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "German Fuel Data Collection by Day of Week",
       x = "Day of Week", y = "Number of Records") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# Check all available columns in German prices table
columns_info <- dbGetQuery(con, "DESCRIBE german_prices")
print(columns_info)

# Check German stations table structure
stations_info <- dbGetQuery(con, "DESCRIBE german_stations")
print(stations_info)

# Price distribution visualization (sample)
price_sample <- dbGetQuery(con, "
  SELECT diesel, e5, e10
  FROM german_prices 
  WHERE diesel > 0 AND diesel < 10 
    AND e5 > 0 AND e5 < 10 
    AND e10 > 0 AND e10 < 10
  ORDER BY RANDOM() 
  LIMIT 10000
")

# Reshape for plotting
price_long <- price_sample %>%
  pivot_longer(cols = c(diesel, e5, e10), 
               names_to = "fuel_type", 
               values_to = "price")

p3 <- ggplot(price_long, aes(x = fuel_type, y = price, fill = fuel_type)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "German Fuel Price Distribution by Type (Sample)",
       x = "Fuel Type", y = "Price (EUR)") +
  theme_minimal()

print(p3)

# 7.1 Coordinate outliers check

# Check coordinate ranges and outliers
coord_analysis <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as total_stations,
    COUNT(CASE WHEN latitude IS NULL OR longitude IS NULL THEN 1 END) as missing_coords,
    COUNT(CASE WHEN latitude = 0 AND longitude = 0 THEN 1 END) as zero_coords,
    COUNT(CASE WHEN latitude > 100000 OR longitude > 100000 THEN 1 END) as extreme_outliers,
    COUNT(CASE WHEN latitude > 60 OR latitude < 40 OR longitude > 20 OR longitude < 0 THEN 1 END) as outside_germany,
    COUNT(CASE WHEN latitude BETWEEN 47 AND 55 AND longitude BETWEEN 5 AND 15 THEN 1 END) as valid_german_coords,
    MIN(latitude) as min_lat,
    MAX(latitude) as max_lat,
    MIN(longitude) as min_lon,
    MAX(longitude) as max_lon,
    AVG(latitude) as avg_lat,
    AVG(longitude) as avg_lon,
    STDDEV(latitude) as std_lat,
    STDDEV(longitude) as std_lon
  FROM german_stations
  WHERE latitude IS NOT NULL AND longitude IS NOT NULL
")

print(coord_analysis)

# Find coordinate outliers
if(coord_analysis$extreme_outliers > 0 || coord_analysis$outside_germany > 0) {
  cat("\n=== COORDINATE OUTLIERS FOUND ===\n")
  
  # Show extreme outliers (coordinates > 100000)
  if(coord_analysis$extreme_outliers > 0) {
    cat("EXTREME OUTLIERS (coordinates > 100000):\n")
    extreme_outliers <- dbGetQuery(con, "
      SELECT uuid, name, latitude, longitude, brand
      FROM german_stations
      WHERE latitude > 100000 OR longitude > 100000
      ORDER BY ABS(latitude - 50) + ABS(longitude - 10) DESC
    ")
    print(extreme_outliers)
  }
  
  # Show outliers outside German bounds
  if(coord_analysis$outside_germany > 0) {
    cat("\nOUTLIERS OUTSIDE GERMAN BOUNDS (47-55°N, 5-15°E):\n")
    outside_outliers <- dbGetQuery(con, "
      SELECT uuid, name, latitude, longitude, brand
      FROM german_stations
      WHERE (latitude > 60 OR latitude < 40 OR longitude > 20 OR longitude < 0)
        AND NOT (latitude > 100000 OR longitude > 100000)
      ORDER BY ABS(latitude - 50) + ABS(longitude - 10) DESC
      LIMIT 20
    ")
    print(outside_outliers)
  }
  
  cat("\nThese outliers will be removed in preprocessing before modeling.\n")
}

# 7.2 Brand check

brand_analysis <- dbGetQuery(con, "
  SELECT 
    CASE 
      WHEN brand IS NULL OR brand = '' THEN 'Unknown/Missing'
      ELSE brand
    END as brand_category,
    COUNT(*) as station_count,
    ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM german_stations), 2) as percentage
  FROM german_stations
  GROUP BY brand_category
  ORDER BY station_count DESC
  LIMIT 20
")

print(brand_analysis)

# 7.3 Station density analysis (basic spatial info only)

station_density <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as total_stations,
    COUNT(CASE WHEN latitude IS NOT NULL AND longitude IS NOT NULL THEN 1 END) as stations_with_coords,
    MIN(latitude) as min_lat,
    MAX(latitude) as max_lat,
    MIN(longitude) as min_lon,
    MAX(longitude) as max_lon
  FROM german_stations
")

print(station_density)

# 8. DATA FILTERING FOR MODELING

# 8.1 Check price records without station metadata
metadata_coverage <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as total_price_records,
    COUNT(DISTINCT station_uuid) as unique_stations_in_prices,
    COUNT(CASE WHEN s.uuid IS NOT NULL THEN 1 END) as records_with_metadata,
    COUNT(CASE WHEN s.uuid IS NULL THEN 1 END) as records_without_metadata,
    ROUND(COUNT(CASE WHEN s.uuid IS NULL THEN 1 END) * 100.0 / COUNT(*), 2) as pct_without_metadata
  FROM german_prices p
  LEFT JOIN german_stations s ON p.station_uuid = s.uuid
")

print(metadata_coverage)

# 8.2 Check which stations in prices table are missing from stations table
missing_stations <- dbGetQuery(con, "
  SELECT 
    p.station_uuid,
    COUNT(*) as price_record_count,
    MIN(p.date) as earliest_price,
    MAX(p.date) as latest_price
  FROM german_prices p
  LEFT JOIN german_stations s ON p.station_uuid = s.uuid
  WHERE s.uuid IS NULL
  GROUP BY p.station_uuid
  ORDER BY price_record_count DESC
  LIMIT 20
")

print(missing_stations)

# 8.3 Sample of filtered data for modeling (INNER JOIN approach)
filtered_sample <- dbGetQuery(con, "
  SELECT 
    p.date,
    p.station_uuid,
    p.diesel,
    p.e5,
    p.e10,
    s.latitude,
    s.longitude,
    s.brand
  FROM german_prices p
  INNER JOIN german_stations s ON p.station_uuid = s.uuid
  WHERE p.date >= '2024-12-01' AND p.date <= '2025-09-30'
    AND p.diesel > 0 AND p.diesel < 10
    AND s.latitude IS NOT NULL 
    AND s.longitude IS NOT NULL
    AND s.latitude BETWEEN 47 AND 55 
    AND s.longitude BETWEEN 5 AND 15
  ORDER BY p.date
  LIMIT 1000
")

# Check data quality in filtered sample
filtered_na_counts <- sapply(filtered_sample, function(x) sum(is.na(x)))
for(i in 1:length(filtered_na_counts)) {
  col_name <- names(filtered_na_counts)[i]
  na_count <- filtered_na_counts[i]
  na_pct <- round(na_count / nrow(filtered_sample) * 100, 2)
  cat("Filtered data - ", col_name, ": ", na_count, " (", na_pct, "%)\n")
}

# 8.4 Overall dataset statistics after filtering
overall_stats <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as total_filtered_records,
    COUNT(DISTINCT p.station_uuid) as unique_stations,
    MIN(p.date) as earliest_date,
    MAX(p.date) as latest_date,
    AVG(p.diesel) as avg_diesel_price,
    AVG(p.e5) as avg_e5_price,
    AVG(p.e10) as avg_e10_price,
    COUNT(CASE WHEN p.diesel IS NULL THEN 1 END) as null_diesel,
    COUNT(CASE WHEN s.latitude IS NULL THEN 1 END) as null_latitude,
    COUNT(CASE WHEN s.longitude IS NULL THEN 1 END) as null_longitude,
    COUNT(CASE WHEN s.brand IS NULL OR s.brand = '' THEN 1 END) as null_brand
  FROM german_prices p
  INNER JOIN german_stations s ON p.station_uuid = s.uuid
  WHERE p.date >= '2024-12-01' AND p.date <= '2025-09-30'
    AND p.diesel > 0 AND p.diesel < 10
    AND s.latitude IS NOT NULL 
    AND s.longitude IS NOT NULL
    AND s.latitude BETWEEN 47 AND 55 
    AND s.longitude BETWEEN 5 AND 15
")

print(overall_stats)

# 9. LSTM suitability assessment

# Station consistency check
station_consistency <- dbGetQuery(con, "
  SELECT 
    AVG(records_per_station) as avg_records,
    MIN(records_per_station) as min_records,
    MAX(records_per_station) as max_records,
    STDDEV(records_per_station) as std_records
  FROM (
    SELECT station_uuid, COUNT(*) as records_per_station
    FROM german_prices 
    GROUP BY station_uuid
  )
")

print(station_consistency)

dbDisconnect(con)


# Key findings:
# 1. German data has irregular temporal spacing
# 2. Station observation counts vary significantly  
# 3. Data collection peaks during business hours
# 4. Price quality issues need cleaning
# 5. LSTM will require data preprocessing for regular sequences

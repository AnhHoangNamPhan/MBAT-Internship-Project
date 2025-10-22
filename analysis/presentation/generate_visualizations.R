#!/usr/bin/env Rscript

# Generate Visualization Plots for Fuel Price Prediction Presentation
# This script extracts statistics from the three databases and creates plots

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(gridExtra)
})

# Configuration
german_db <- "databases/german_fuel.duckdb"
austrian_db <- "databases/austrian_fuel_database.duckdb"
slovenian_db <- "databases/slovenian_fuel_database.duckdb"
stations_db <- "databases/stations.duckdb"

# Create visualizations directory
dir.create("analysis/presentation/visualizations", recursive = TRUE, showWarnings = FALSE)

# Connect to databases
con <- dbConnect(duckdb())
dbExecute(con, "SET threads=8")
dbExecute(con, "SET memory_limit='16GB'")

# Attach all databases
german_path <- normalizePath(german_db)
austrian_path <- normalizePath(austrian_db)
slovenian_path <- normalizePath(slovenian_db)
stations_path <- normalizePath(stations_db)

dbExecute(con, paste0("ATTACH '", german_path, "' AS ger"))
dbExecute(con, paste0("ATTACH '", austrian_path, "' AS aut"))
dbExecute(con, paste0("ATTACH '", slovenian_path, "' AS slo"))
dbExecute(con, paste0("ATTACH '", stations_path, "' AS stations_db"))

cat("=== GENERATING FUEL PRICE VISUALIZATIONS ===\n")

# 1. Price by Day of Week Analysis
cat("\n1. Analyzing price patterns by day of week...\n")

day_of_week_query <- "
WITH all_fuel_data AS (
  -- German data
  SELECT 
    p.date,
    p.diesel as price,
    'diesel' as fuel_type,
    'Germany' as country,
    EXTRACT(DOW FROM p.date) as day_of_week
  FROM ger.german_prices_wide p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.diesel > 0 AND p.diesel < 10
    AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  
  UNION ALL
  
  SELECT 
    p.date,
    p.gasoline_95 as price,
    'gasoline_95' as fuel_type,
    'Germany' as country,
    EXTRACT(DOW FROM p.date) as day_of_week
  FROM ger.german_prices_wide p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.gasoline_95 > 0 AND p.gasoline_95 < 10
    AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  
  UNION ALL
  
  SELECT 
    p.date,
    p.gasoline_98 as price,
    'gasoline_98' as fuel_type,
    'Germany' as country,
    EXTRACT(DOW FROM p.date) as day_of_week
  FROM ger.german_prices_wide p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.gasoline_98 > 0 AND p.gasoline_98 < 10
    AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  
  UNION ALL
  
  -- Austrian data
  SELECT 
    p.date,
    p.price,
    CASE 
      WHEN p.fuel_type IN ('DIESEL', 'diesel', 'fuel_d') THEN 'diesel'
      WHEN p.fuel_type IN ('GASOLINE_SUPER', 'fuel_s95', 'gasoline_95') THEN 'gasoline_95'
      WHEN p.fuel_type IN ('GASOLINE_SUPER_PLUS', 'fuel_s98', 'gasoline_98') THEN 'gasoline_98'
      ELSE p.fuel_type
    END as fuel_type,
    'Austria' as country,
    EXTRACT(DOW FROM p.date) as day_of_week
  FROM aut.austrian_prices p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Austria'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.price > 0 AND p.price < 10
    AND s.latitude BETWEEN 46 AND 49 AND s.longitude BETWEEN 9 AND 17
    AND p.fuel_type IN ('DIESEL', 'diesel', 'fuel_d', 'GASOLINE_SUPER', 'fuel_s95', 'gasoline_95', 
                       'GASOLINE_SUPER_PLUS', 'fuel_s98', 'gasoline_98')
  
  UNION ALL
  
  -- Slovenian data
  SELECT 
    p.date,
    p.price,
    CASE 
      WHEN p.fuel_type IN ('dizel', 'dizel-premium') THEN 'diesel'
      WHEN p.fuel_type = '95' THEN 'gasoline_95'
      WHEN p.fuel_type = '98' THEN 'gasoline_98'
      ELSE p.fuel_type
    END as fuel_type,
    'Slovenia' as country,
    EXTRACT(DOW FROM p.date) as day_of_week
  FROM slo.slovenian_prices p
  JOIN stations_db.stations s ON CAST(p.station_uuid AS DOUBLE) = CAST(s.station_uuid AS DOUBLE) AND s.country = 'Slovenia'
  WHERE p.price > 0 AND p.price < 10
    AND s.latitude BETWEEN 45 AND 47 AND s.longitude BETWEEN 13 AND 16
    AND p.fuel_type IN ('dizel', 'dizel-premium', '95', '98')
)
SELECT 
  country,
  fuel_type,
  day_of_week,
  AVG(price) as avg_price,
  COUNT(*) as record_count,
  STDDEV(price) as price_std
FROM all_fuel_data
WHERE fuel_type IN ('diesel', 'gasoline_95', 'gasoline_98')
GROUP BY country, fuel_type, day_of_week
ORDER BY country, fuel_type, day_of_week
"

day_of_week_data <- dbGetQuery(con, day_of_week_query)

# Create day of week plot
day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
day_of_week_data$day_name <- day_names[day_of_week_data$day_of_week + 1]
day_of_week_data$day_name <- factor(day_of_week_data$day_name, levels = day_names)

p1 <- ggplot(day_of_week_data, aes(x = day_name, y = avg_price, color = fuel_type)) +
  geom_line(aes(group = fuel_type), size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ country, scales = "free_y") +
  labs(
    title = "Average Fuel Prices by Day of Week",
    subtitle = "Across countries and fuel types (Aug 2024 - Sep 2025)",
    x = "Day of Week",
    y = "Average Price (€)",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("diesel" = "#2E8B57", "gasoline_95" = "#4169E1", "gasoline_98" = "#DC143C"))

ggsave("analysis/presentation/visualizations/price_by_day_of_week.png", p1, width = 12, height = 8, dpi = 300)
cat("Saved: price_by_day_of_week.png\n")

# 2. Price by Hour of Day Analysis
cat("\n2. Analyzing price patterns by hour of day...\n")

hour_of_day_query <- "
WITH all_fuel_data AS (
  -- German data
  SELECT 
    p.date,
    p.diesel as price,
    'diesel' as fuel_type,
    'Germany' as country,
    EXTRACT(HOUR FROM p.date) as hour_of_day
  FROM ger.german_prices_wide p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.diesel > 0 AND p.diesel < 10
    AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  
  UNION ALL
  
  SELECT 
    p.date,
    p.gasoline_95 as price,
    'gasoline_95' as fuel_type,
    'Germany' as country,
    EXTRACT(HOUR FROM p.date) as hour_of_day
  FROM ger.german_prices_wide p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.gasoline_95 > 0 AND p.gasoline_95 < 10
    AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  
  UNION ALL
  
  -- Austrian data (diesel only for efficiency)
  SELECT 
    p.date,
    p.price,
    'diesel' as fuel_type,
    'Austria' as country,
    EXTRACT(HOUR FROM p.date) as hour_of_day
  FROM aut.austrian_prices p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Austria'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.price > 0 AND p.price < 10
    AND s.latitude BETWEEN 46 AND 49 AND s.longitude BETWEEN 9 AND 17
    AND p.fuel_type IN ('DIESEL', 'diesel', 'fuel_d')
)
SELECT 
  country,
  fuel_type,
  hour_of_day,
  AVG(price) as avg_price,
  COUNT(*) as record_count
FROM all_fuel_data
GROUP BY country, fuel_type, hour_of_day
ORDER BY country, fuel_type, hour_of_day
"

hour_of_day_data <- dbGetQuery(con, hour_of_day_query)

# Create hour of day plot
p2 <- ggplot(hour_of_day_data, aes(x = hour_of_day, y = avg_price, color = fuel_type)) +
  geom_line(aes(group = fuel_type), size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ country, scales = "free_y") +
  labs(
    title = "24-Hour Price Patterns",
    subtitle = "Peak and off-peak pricing across countries",
    x = "Hour of Day",
    y = "Average Price (€)",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0, 23, 4)) +
  scale_color_manual(values = c("diesel" = "#2E8B57", "gasoline_95" = "#4169E1", "gasoline_98" = "#DC143C"))

ggsave("analysis/presentation/visualizations/price_by_hour.png", p2, width = 12, height = 8, dpi = 300)
cat("Saved: price_by_hour.png\n")

# 3. Seasonal Price Patterns (Monthly)
cat("\n3. Analyzing seasonal price patterns...\n")

monthly_query <- "
WITH all_fuel_data AS (
  -- German data
  SELECT 
    p.date,
    p.diesel as price,
    'diesel' as fuel_type,
    'Germany' as country,
    EXTRACT(MONTH FROM p.date) as month
  FROM ger.german_prices_wide p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.diesel > 0 AND p.diesel < 10
    AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  
  UNION ALL
  
  SELECT 
    p.date,
    p.gasoline_95 as price,
    'gasoline_95' as fuel_type,
    'Germany' as country,
    EXTRACT(MONTH FROM p.date) as month
  FROM ger.german_prices_wide p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.gasoline_95 > 0 AND p.gasoline_95 < 10
    AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  
  UNION ALL
  
  -- Austrian data (diesel only)
  SELECT 
    p.date,
    p.price,
    'diesel' as fuel_type,
    'Austria' as country,
    EXTRACT(MONTH FROM p.date) as month
  FROM aut.austrian_prices p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Austria'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.price > 0 AND p.price < 10
    AND s.latitude BETWEEN 46 AND 49 AND s.longitude BETWEEN 9 AND 17
    AND p.fuel_type IN ('DIESEL', 'diesel', 'fuel_d')
)
SELECT 
  country,
  fuel_type,
  month,
  AVG(price) as avg_price,
  COUNT(*) as record_count
FROM all_fuel_data
GROUP BY country, fuel_type, month
ORDER BY country, fuel_type, month
"

monthly_data <- dbGetQuery(con, monthly_query)

# Create monthly plot
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthly_data$month_name <- month_names[monthly_data$month]

p3 <- ggplot(monthly_data, aes(x = month, y = avg_price, color = fuel_type)) +
  geom_line(aes(group = fuel_type), size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ country, scales = "free_y") +
  labs(
    title = "Monthly Price Variations",
    subtitle = "Seasonal trends across countries (Aug 2024 - Sep 2025)",
    x = "Month",
    y = "Average Price (€)",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = 1:12, labels = month_names) +
  scale_color_manual(values = c("diesel" = "#2E8B57", "gasoline_95" = "#4169E1", "gasoline_98" = "#DC143C"))

ggsave("analysis/presentation/visualizations/price_by_month.png", p3, width = 12, height = 8, dpi = 300)
cat("Saved: price_by_month.png\n")

# 4. Summary Statistics Table
cat("\n4. Creating summary statistics table...\n")

summary_query <- "
WITH all_fuel_data AS (
  -- German data
  SELECT 
    p.diesel as price,
    'diesel' as fuel_type,
    'Germany' as country
  FROM ger.german_prices_wide p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.diesel > 0 AND p.diesel < 10
    AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  
  UNION ALL
  
  SELECT 
    p.gasoline_95 as price,
    'gasoline_95' as fuel_type,
    'Germany' as country
  FROM ger.german_prices_wide p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.gasoline_95 > 0 AND p.gasoline_95 < 10
    AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  
  UNION ALL
  
  -- Austrian data
  SELECT 
    p.price,
    CASE 
      WHEN p.fuel_type IN ('DIESEL', 'diesel', 'fuel_d') THEN 'diesel'
      WHEN p.fuel_type IN ('GASOLINE_SUPER', 'fuel_s95', 'gasoline_95') THEN 'gasoline_95'
      ELSE p.fuel_type
    END as fuel_type,
    'Austria' as country
  FROM aut.austrian_prices p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Austria'
  WHERE p.date >= '2024-08-01' AND p.date <= '2025-09-30'
    AND p.price > 0 AND p.price < 10
    AND s.latitude BETWEEN 46 AND 49 AND s.longitude BETWEEN 9 AND 17
    AND p.fuel_type IN ('DIESEL', 'diesel', 'fuel_d', 'GASOLINE_SUPER', 'fuel_s95', 'gasoline_95')
)
SELECT 
  country,
  fuel_type,
  COUNT(*) as total_records,
  AVG(price) as avg_price,
  MIN(price) as min_price,
  MAX(price) as max_price,
  STDDEV(price) as price_std
FROM all_fuel_data
WHERE fuel_type IN ('diesel', 'gasoline_95')
GROUP BY country, fuel_type
ORDER BY country, fuel_type
"

summary_data <- dbGetQuery(con, summary_query)

# Save summary table
write.csv(summary_data, "analysis/presentation/visualizations/fuel_price_summary.csv", row.names = FALSE)
cat("Saved: fuel_price_summary.csv\n")

# Print summary
cat("\n=== SUMMARY STATISTICS ===\n")
print(summary_data)

# Disconnect
dbDisconnect(con)

cat("\n=== VISUALIZATION GENERATION COMPLETE ===\n")
cat("Generated files:\n")
cat("- analysis/presentation/visualizations/price_by_day_of_week.png\n")
cat("- analysis/presentation/visualizations/price_by_hour.png\n")
cat("- analysis/presentation/visualizations/price_by_month.png\n")
cat("- analysis/presentation/visualizations/fuel_price_summary.csv\n")
cat("\nThese files can now be included in your Quarto presentation!\n")


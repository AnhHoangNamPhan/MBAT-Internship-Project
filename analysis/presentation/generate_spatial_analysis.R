#!/usr/bin/env Rscript

# Generate Improved Spatial Analysis Plots for Fuel Price Prediction Presentation

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(viridis)
  library(gridExtra)
  library(scales)
})

# Configuration
german_db <- "databases/german_fuel.duckdb"
austrian_db <- "databases/austrian_fuel_database.duckdb"
slovenian_db <- "databases/slovenian_fuel_database.duckdb"
stations_db <- "databases/stations.duckdb"

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

cat("=== GENERATING IMPROVED SPATIAL ANALYSIS PLOTS ===\n")

# Define date range for August 2025
date_start <- "2025-08-01"
date_end <- "2025-08-31"

# --- Get Station Data with Average Prices for August 2025 ---

# German data (wide format)
german_stations_query <- sprintf("
SELECT 
  s.station_uuid,
  s.latitude,
  s.longitude,
  s.country,
  AVG(p.diesel) as avg_diesel,
  AVG(p.gasoline_95) as avg_gasoline_95,
  AVG(p.gasoline_98) as avg_gasoline_98,
  COUNT(*) as price_count
FROM stations_db.stations s
JOIN ger.german_prices_wide p ON s.station_uuid = p.station_uuid
WHERE s.country = 'Germany'
  AND p.date >= '%s' AND p.date <= '%s'
  AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  AND p.diesel > 0 AND p.diesel < 10
  AND p.gasoline_95 > 0 AND p.gasoline_95 < 10
  AND p.gasoline_98 > 0 AND p.gasoline_98 < 10
GROUP BY s.station_uuid, s.latitude, s.longitude, s.country
HAVING price_count >= 5
", date_start, date_end)

# Austrian data (long format)
austrian_stations_query <- sprintf("
SELECT 
  s.station_uuid,
  s.latitude,
  s.longitude,
  s.country,
  AVG(CASE WHEN p.fuel_type IN ('DIESEL', 'fuel_d', 'diesel') THEN p.price END) as avg_diesel,
  AVG(CASE WHEN p.fuel_type IN ('GASOLINE_SUPER', 'fuel_s95', 'gasoline_95') THEN p.price END) as avg_gasoline_95,
  AVG(CASE WHEN p.fuel_type IN ('GASOLINE_SUPER_PLUS', 'Super Plus', 'fuel_s98', 'gasoline_98', 'maxxmotion_super_100plus') THEN p.price END) as avg_gasoline_98,
  COUNT(*) as price_count
FROM stations_db.stations s
JOIN aut.austrian_prices p ON s.station_uuid = p.station_uuid
WHERE s.country = 'Austria'
  AND p.date >= '%s' AND p.date <= '%s'
  AND s.latitude BETWEEN 46 AND 49 AND s.longitude BETWEEN 9 AND 17
  AND p.price > 0 AND p.price < 10
GROUP BY s.station_uuid, s.latitude, s.longitude, s.country
HAVING price_count >= 3
", date_start, date_end)

# Slovenian data (long format)
slovenian_stations_query <- "
SELECT 
  s.station_uuid,
  s.latitude,
  s.longitude,
  s.country,
  AVG(CASE WHEN p.fuel_type IN ('dizel', 'dizel-premium') THEN p.price END) as avg_diesel,
  AVG(CASE WHEN p.fuel_type = '95' THEN p.price END) as avg_gasoline_95,
  AVG(CASE WHEN p.fuel_type = '98' THEN p.price END) as avg_gasoline_98,
  AVG(CASE WHEN p.fuel_type = '100' THEN p.price END) as avg_gasoline_100,
  COUNT(*) as price_count
FROM stations_db.stations s
JOIN slo.slovenian_prices p ON CAST(s.station_uuid AS DOUBLE) = CAST(p.station_uuid AS DOUBLE)
WHERE s.country = 'Slovenia'
  AND s.latitude BETWEEN 45 AND 47 AND s.longitude BETWEEN 13 AND 16
  AND p.price > 0 AND p.price < 10
GROUP BY s.station_uuid, s.latitude, s.longitude, s.country
HAVING price_count >= 3
"

# Execute queries
german_stations <- dbGetQuery(con, german_stations_query)
austrian_stations <- dbGetQuery(con, austrian_stations_query)
slovenian_stations <- dbGetQuery(con, slovenian_stations_query)

# Combine all station data
all_stations <- bind_rows(
  german_stations %>% select(station_uuid, latitude, longitude, country, avg_diesel, avg_gasoline_95, avg_gasoline_98),
  austrian_stations %>% select(station_uuid, latitude, longitude, country, avg_diesel, avg_gasoline_95, avg_gasoline_98),
  slovenian_stations %>% select(station_uuid, latitude, longitude, country, avg_diesel, avg_gasoline_95, avg_gasoline_98)
) %>%
  filter(!is.na(avg_diesel) | !is.na(avg_gasoline_95) | !is.na(avg_gasoline_98))

# Disconnect from database
dbDisconnect(con)

cat("Station data extracted. Processing improved spatial analysis...\n")

# --- Get Country Borders ---
cat("Loading country borders...\n")
countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name %in% c("Germany", "Austria", "Slovenia"))

# --- Create Improved Spatial Price Map ---
cat("Creating improved spatial price map...\n")

# Convert stations to SF object
stations_sf <- st_as_sf(all_stations, coords = c("longitude", "latitude"), crs = 4326)

# Create the improved spatial map for diesel prices
improved_spatial_map <- ggplot() +
  # Country borders with distinct colors and labels
  geom_sf(data = countries, fill = "lightgray", color = "black", alpha = 0.2, size = 1.2) +
  # Add country labels
  geom_sf_text(data = countries, aes(label = name), size = 4, fontface = "bold", color = "darkblue") +
  # Station points colored by diesel price
  geom_sf(data = stations_sf %>% filter(!is.na(avg_diesel)), 
          aes(color = avg_diesel), size = 0.6, alpha = 0.8) +
  # Color scale
  scale_color_viridis_c(name = "Diesel Price (€)", 
                       option = "plasma", 
                       trans = "identity",
                       breaks = pretty_breaks(n = 6)) +
  # Theme and labels
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  labs(
    title = "Spatial Distribution of Diesel Prices by Country",
    subtitle = "Average prices by station location (August 2025) - Germany, Austria, Slovenia",
    x = "", y = ""
  ) +
  # Set coordinate limits to focus on the region
  coord_sf(xlim = c(5, 17), ylim = c(45, 55), expand = FALSE)

# Save improved spatial map
ggsave("analysis/presentation/visualizations/improved_spatial_price_map.png", 
       improved_spatial_map, width = 12, height = 8, dpi = 300, bg = "white")

# --- Create Boxplot for Price Comparison ---
cat("Creating boxplot for price comparison...\n")

# Prepare data for boxplot
boxplot_data <- all_stations %>%
  select(country, avg_diesel, avg_gasoline_95, avg_gasoline_98) %>%
  pivot_longer(cols = starts_with("avg_"), 
               names_to = "fuel_type", 
               values_to = "price") %>%
  filter(!is.na(price)) %>%
  mutate(
    fuel_type = case_when(
      fuel_type == "avg_diesel" ~ "Diesel",
      fuel_type == "avg_gasoline_95" ~ "Gasoline 95",
      fuel_type == "avg_gasoline_98" ~ "Gasoline 98",
      TRUE ~ fuel_type
    )
  )

# Create boxplot
price_boxplot <- ggplot(boxplot_data, aes(x = fuel_type, y = price, fill = country)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  scale_fill_manual(values = c("Germany" = "#E31A1C", 
                              "Austria" = "#1F78B4", 
                              "Slovenia" = "#33A02C"),
                   name = "Country") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Price Distribution Comparison by Country and Fuel Type",
    subtitle = "Boxplot showing median, quartiles, and outliers (August 2025)",
    x = "Fuel Type",
    y = "Price (€ per liter)"
  ) +
  # Add facet for better separation
  facet_wrap(~ fuel_type, scales = "free_y", ncol = 3)

# Save boxplot
ggsave("analysis/presentation/visualizations/price_comparison_boxplot.png", 
       price_boxplot, width = 12, height = 8, dpi = 300, bg = "white")

# --- Create Alternative Boxplot (Single Panel) ---
cat("Creating alternative single-panel boxplot...\n")

price_boxplot_single <- ggplot(boxplot_data, aes(x = fuel_type, y = price, fill = country)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5, position = position_dodge(0.8)) +
  scale_fill_manual(values = c("Germany" = "#E31A1C", 
                              "Austria" = "#1F78B4", 
                              "Slovenia" = "#33A02C"),
                   name = "Country") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  labs(
    title = "Price Distribution Comparison by Country and Fuel Type",
    subtitle = "Boxplot showing median, quartiles, and outliers (August 2025)",
    x = "Fuel Type",
    y = "Price (€ per liter)"
  )

# Save single panel boxplot
ggsave("analysis/presentation/visualizations/price_comparison_boxplot_single.png", 
       price_boxplot_single, width = 10, height = 6, dpi = 300, bg = "white")

# --- Print Summary Statistics ---
cat("\n=== IMPROVED SPATIAL ANALYSIS SUMMARY ===\n")
cat("Stations analyzed:\n")
print(table(all_stations$country))

cat("\nPrice statistics by country and fuel type:\n")
price_stats <- boxplot_data %>%
  group_by(country, fuel_type) %>%
  summarise(
    count = n(),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE),
    q25 = quantile(price, 0.25, na.rm = TRUE),
    q75 = quantile(price, 0.75, na.rm = TRUE),
    .groups = 'drop'
  )
print(price_stats)

cat("\n=== IMPROVED SPATIAL ANALYSIS PLOTS GENERATED ===\n")
cat("Files saved:\n")
cat("- analysis/presentation/visualizations/improved_spatial_price_map.png\n")
cat("- analysis/presentation/visualizations/price_comparison_boxplot.png\n")
cat("- analysis/presentation/visualizations/price_comparison_boxplot_single.png\n")


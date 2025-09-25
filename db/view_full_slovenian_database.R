# View FULL Slovenian Fuel Database
# This script loads the ENTIRE dataset into R for viewing

library(DBI)
library(duckdb)
library(dplyr)

# Connect to the database
db_path <- "/Users/alexphan/Desktop/MBAT-Internship-Project/db/slovenian_fuel_data.duckdb"
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

cat("🔍 Loading FULL Slovenian Fuel Database\n")
cat("=====================================\n\n")

# Load the ENTIRE dataset into R
cat("📊 Loading all 49,224 records into R...\n")
full_dataset <- DBI::dbGetQuery(con, "SELECT * FROM slovenian_fuel ORDER BY station_id, fuel_type, file_source")

cat("✅ Dataset loaded successfully!\n")
cat("📋 Dataset dimensions:", nrow(full_dataset), "rows ×", ncol(full_dataset), "columns\n\n")

# Show the complete dataset
cat("🗂️ Opening FULL dataset viewer...\n")
View(full_dataset)

# Also create some useful subsets for analysis
cat("\n📈 Creating summary tables...\n")

# 1. Complete fuel summary
fuel_summary_full <- full_dataset %>%
  filter(!is.na(fuel_price)) %>%
  group_by(fuel_type) %>%
  summarise(
    total_records = n(),
    unique_stations = n_distinct(station_id),
    avg_price = round(mean(fuel_price), 3),
    min_price = round(min(fuel_price), 3),
    max_price = round(max(fuel_price), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_records))

View(fuel_summary_full)

# 2. All unique stations
unique_stations <- full_dataset %>%
  select(station_id, station_name, station_address, franchise_id, lat, lng, zip_code) %>%
  distinct() %>%
  arrange(station_id)

View(unique_stations)

# 3. All unique files processed
files_processed <- full_dataset %>%
  select(file_source) %>%
  distinct() %>%
  arrange(file_source)

View(files_processed)

# 4. Price trends over time (by file)
price_trends <- full_dataset %>%
  filter(!is.na(fuel_price)) %>%
  group_by(file_source, fuel_type) %>%
  summarise(
    avg_price = round(mean(fuel_price), 3),
    stations = n_distinct(station_id),
    .groups = 'drop'
  ) %>%
  arrange(file_source, fuel_type)

View(price_trends)

# 5. Station fuel availability matrix
station_fuels <- full_dataset %>%
  filter(!is.na(fuel_price)) %>%
  select(station_id, station_name, fuel_type, fuel_price) %>%
  arrange(station_id, fuel_type)

View(station_fuels)

cat("\n✅ All data loaded and views created!\n")
cat("📊 Available objects in R environment:\n")
cat("   - full_dataset: Complete dataset (49,224 rows)\n")
cat("   - fuel_summary_full: Summary by fuel type\n")
cat("   - unique_stations: All unique stations\n")
cat("   - files_processed: All processed files\n")
cat("   - price_trends: Price trends over time\n")
cat("   - station_fuels: Station-fuel combinations\n\n")

cat("💡 You can now:\n")
cat("   - Browse 'full_dataset' to see ALL records\n")
cat("   - Filter data: filter(full_dataset, fuel_type == '95')\n")
cat("   - Analyze trends: group_by(full_dataset, file_source)\n")
cat("   - Export data: write.csv(full_dataset, 'slovenian_fuel_full.csv')\n\n")

# Keep connection open
cat("🔗 Database connection is still open for custom queries.\n")
cat("🔚 Use 'DBI::dbDisconnect(con, shutdown = TRUE)' when done.\n")

# Don't disconnect automatically
# DBI::dbDisconnect(con, shutdown = TRUE)

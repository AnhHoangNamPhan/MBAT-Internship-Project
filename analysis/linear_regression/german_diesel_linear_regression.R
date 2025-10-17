library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Database path
db_path <- "databases/german_fuel_data.duckdb"
con <- dbConnect(duckdb(), db_path, read_only = FALSE)

# Set DuckDB memory settings
dbExecute(con, "SET memory_limit='16GB'")
dbExecute(con, "SET max_temp_directory_size='20GB'") 
dbExecute(con, "SET threads=8")
dbExecute(con, "SET preserve_insertion_order=false")

# Helper functions
create_features <- function(data) {
  data %>%
    mutate(
      hour = hour(date),
      day_of_week = wday(date),
      month = month(date),
      hour_sin = sin(2 * pi * hour / 24),
      hour_cos = cos(2 * pi * hour / 24),
      dow_sin = sin(2 * pi * day_of_week / 7),
      dow_cos = cos(2 * pi * day_of_week / 7),
      month_sin = sin(2 * pi * month / 12),
      month_cos = cos(2 * pi * month / 12)
    ) %>%
    select(-hour, -day_of_week, -month, -date)
}

normalize_features <- function(data, stats) {
  data %>%
    mutate(
      latitude_centered = latitude - stats$latitude_mean,
      longitude_centered = longitude - stats$longitude_mean,
      nearby_stations_1km_norm = ifelse(stats$nearby_stations_1km_sd == 0, 0,
                                        (nearby_stations_1km - stats$nearby_stations_1km_mean) / stats$nearby_stations_1km_sd)
    ) %>%
    select(-latitude, -longitude, -nearby_stations_1km)
}

# Load station metadata
station_metadata <- dbGetQuery(con, "
  SELECT uuid, latitude, longitude, brand, nearby_stations_1km
  FROM german_stations
  WHERE latitude IS NOT NULL AND longitude IS NOT NULL
    AND latitude BETWEEN 47 AND 55 AND longitude BETWEEN 5 AND 15
")

brand_counts <- station_metadata %>%
  filter(!is.na(brand) & brand != "") %>%
  group_by(brand) %>%
  summarise(stations = n(), .groups = "drop") %>%
  mutate(
    brand_category = case_when(
      stations >= 700 ~ "Major",
      stations >= 100 ~ "Moderate", 
      TRUE ~ "Small"
    )
  )

station_metadata <- station_metadata %>%
  left_join(brand_counts %>% select(brand, brand_category), by = "brand") %>%
  mutate(brand_category = ifelse(is.na(brand_category), "Unknown", brand_category))

category_summary <- station_metadata %>%
  count(brand_category) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

cat("Station metadata loaded:", nrow(station_metadata), "stations\n")
cat("\nBrand category distribution:\n")
print(category_summary)

# Random train/test split (80/20)
set.seed(42)
train_fraction <- 0.8

# Training parameters
learning_rate <- 0.0005
chunk_size <- 250000
num_epochs <- 5

# Load all data for random split
all_data_query <- "
  SELECT 
    p.date,
    p.station_uuid,
    p.diesel,
    s.latitude,
    s.longitude,
    s.brand,
    s.nearby_stations_1km,
    ROW_NUMBER() OVER (ORDER BY RANDOM()) as row_num,
    COUNT(*) OVER () as total_count
  FROM german_prices p
  JOIN german_stations s ON p.station_uuid = s.uuid
  WHERE p.date >= '2024-12-01' AND p.date <= '2025-09-30'
    AND p.diesel > 0 AND p.diesel < 10
    AND s.latitude IS NOT NULL AND s.longitude IS NOT NULL
    AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
"

all_data <- dbGetQuery(con, all_data_query)
total_records <- nrow(all_data)
train_size <- floor(total_records * train_fraction)
test_size <- total_records - train_size

cat("Total valid records:", total_records, "\n")
cat("Training records:", train_size, "\n")
cat("Test records:", test_size, "\n")

# Create train/test split
train_indices <- 1:train_size
test_indices <- (train_size + 1):total_records

train_data <- all_data[train_indices, ]
test_data <- all_data[test_indices, ]

# Calculate global normalization statistics from training data
global_stats <- train_data %>%
  summarise(
    latitude_mean = mean(latitude, na.rm = TRUE),
    latitude_sd = sd(latitude, na.rm = TRUE),
    longitude_mean = mean(longitude, na.rm = TRUE),
    longitude_sd = sd(longitude, na.rm = TRUE),
    nearby_stations_1km_mean = mean(nearby_stations_1km, na.rm = TRUE),
    nearby_stations_1km_sd = sd(nearby_stations_1km, na.rm = TRUE)
  )

cat("\nGlobal normalization statistics for diesel:\n")
print(global_stats)

feature_stats <- list(
  latitude_mean = global_stats$latitude_mean,
  latitude_sd = global_stats$latitude_sd,
  longitude_mean = global_stats$longitude_mean,
  longitude_sd = global_stats$longitude_sd,
  nearby_stations_1km_mean = global_stats$nearby_stations_1km_mean,
  nearby_stations_1km_sd = global_stats$nearby_stations_1km_sd
)

# Calculate chunks for training data
max_chunks <- ceiling(train_size / chunk_size)

cat("\nTotal diesel training records:", train_size, "\n")
cat("Processing in", max_chunks, "chunks of", chunk_size, "records each\n")

# Training metrics
total_samples <- 0
mse_history <- c()
chunk_count <- 0
epoch_mse_history <- c()

# Multi-epoch training
for(epoch in 1:num_epochs) {
  cat("\n=== EPOCH", epoch, "/", num_epochs, "===\n")
  epoch_start_time <- Sys.time()
  epoch_total_mse <- 0
  epoch_chunks <- 0
  
  for(chunk_idx in 1:max_chunks) {
    start_idx <- (chunk_idx - 1) * chunk_size + 1
    end_idx <- min(chunk_idx * chunk_size, train_size)
    
    chunk <- train_data[start_idx:end_idx, ]
    chunk$price <- chunk$diesel
    
    if (nrow(chunk) == 0) {
      cat("Chunk", chunk_idx, ": No data found\n")
      next
    }
    
    chunk_count <- chunk_count + 1
    total_samples <- total_samples + nrow(chunk)
    
    chunk_data <- chunk %>%
      mutate(brand = ifelse(is.na(brand) | brand == "", "Unknown", brand))
    
    chunk_features <- create_features(chunk_data)
    
    chunk_features <- chunk_features %>%
      left_join(station_metadata %>% select(uuid, brand_category), 
                by = c("station_uuid" = "uuid"))
    
    chunk_features <- normalize_features(chunk_features, feature_stats)
    
    chunk_features <- chunk_features %>%
      mutate(brand_category = as.factor(brand_category))
    
    chunk_features$brand_category <- relevel(chunk_features$brand_category, ref = "Unknown")
    
    valid_rows <- complete.cases(chunk_features)
    chunk_features <- chunk_features[valid_rows, ]
    
    X <- model.matrix(~ hour_sin + hour_cos + dow_sin + dow_cos + month_sin + month_cos +
                        latitude_centered + longitude_centered + nearby_stations_1km_norm + brand_category,
                      data = chunk_features, drop.unused.levels = TRUE)
    
    y <- chunk_features$price
    
    if(nrow(X) == 0) {
      cat("Skipping chunk", chunk_idx, "- no valid data\n")
      next
    }
    
    if(epoch == 1 && chunk_idx == 1) {
      weights <- runif(ncol(X), -0.01, 0.01)
      cat("Initialized weights vector with", ncol(X), "features\n")
      cat("Feature names:", paste(colnames(X), collapse = ", "), "\n")
    }
    
    predictions <- as.vector(X %*% weights)
    errors <- y - predictions
    mse <- mean(errors^2)
    mse_history[chunk_count] <- mse
    epoch_total_mse <- epoch_total_mse + mse
    epoch_chunks <- epoch_chunks + 1
    
    gradient_weights <- -2 * colMeans(X * as.vector(errors))
    
    max_grad <- max(abs(gradient_weights))
    if (max_grad > 1000) {
      gradient_weights <- gradient_weights * (1000 / max_grad)
    }
    
    weights <- weights - learning_rate * gradient_weights
    
    if (chunk_count %% 25 == 0 || chunk_count == 1) {
      cat("Epoch", epoch, "- Chunk", chunk_idx, "/", max_chunks, 
          "- Records:", nrow(chunk), 
          "- MSE:", round(mse, 6),
          "- Total samples:", total_samples, "\n")
    }
  }
  
  epoch_end_time <- Sys.time()
  epoch_duration <- difftime(epoch_end_time, epoch_start_time, units = "mins")
  epoch_avg_mse <- epoch_total_mse / epoch_chunks
  epoch_mse_history[epoch] <- epoch_avg_mse
  
  cat("\n--- Epoch", epoch, "Summary ---\n")
  cat("Average MSE:", round(epoch_avg_mse, 6), "\n")
  cat("Chunks processed:", epoch_chunks, "\n")
}

cat("\n=== TRAINING COMPLETED ===\n")
cat("Total epochs:", num_epochs, "\n")
cat("Total chunks processed:", chunk_count, "\n")
cat("\nEpoch-by-Epoch MSE:\n")
for(i in 1:length(epoch_mse_history)) {
  cat("  Epoch", i, ":", round(epoch_mse_history[i], 6), "\n")
}

# Test the model
if (nrow(test_data) > 0) {
  cat("Test records:", nrow(test_data), "\n")
  
  # Process test data in chunks
  test_chunk_size <- 50000  
  test_max_chunks <- ceiling(nrow(test_data) / test_chunk_size)
  
  all_test_predictions <- c()
  all_test_actual <- c()
  
  for(test_chunk_idx in 1:test_max_chunks) {
    start_idx <- (test_chunk_idx - 1) * test_chunk_size + 1
    end_idx <- min(test_chunk_idx * test_chunk_size, nrow(test_data))
    
    test_chunk <- test_data[start_idx:end_idx, ]
    
    test_with_brand <- test_chunk %>%
      left_join(station_metadata %>% select(uuid, brand_category), 
                by = c("station_uuid" = "uuid"))
    
    test_features <- create_features(test_with_brand)
    test_features <- test_features %>%
      select(-station_uuid, -brand)
    test_features <- normalize_features(test_features, feature_stats)
    
    X_test <- model.matrix(~ hour_sin + hour_cos + dow_sin + dow_cos + month_sin + month_cos +
                            latitude_centered + longitude_centered + nearby_stations_1km_norm + brand_category,
                          data = test_features, drop.unused.levels = TRUE)
    y_test <- test_features$diesel
    
    predictions_test <- as.vector(X_test %*% weights)
    
    all_test_predictions <- c(all_test_predictions, predictions_test)
    all_test_actual <- c(all_test_actual, y_test)
    
    if (test_chunk_idx %% 50 == 0) {
      cat("Processed test chunk", test_chunk_idx, "/", test_max_chunks, "\n")
    }
  }
  
  # Calculate final test metrics
  mse_test <- mean((all_test_actual - all_test_predictions)^2)
  rmse_test <- sqrt(mse_test)
  mae_test <- mean(abs(all_test_actual - all_test_predictions))
  r2_test <- 1 - (sum((all_test_actual - all_test_predictions)^2) / sum((all_test_actual - mean(all_test_actual))^2))
  
  cat("\n=== TEST RESULTS ===\n")
  cat("Test MSE:", round(mse_test, 6), "\n")
  cat("Test RMSE:", round(rmse_test, 6), "€\n")
  cat("Test MAE:", round(mae_test, 6), "€\n")
  cat("Test R²:", round(r2_test, 6), "\n")
  
  # Save model
  model_result <- list(
    fuel_type = "diesel",
    weights = weights,
    feature_stats = feature_stats,
    num_epochs = num_epochs,
    epoch_mse_history = epoch_mse_history,
    mse_history = mse_history,
    test_mse = mse_test,
    test_r2 = r2_test,
    total_training_samples = total_samples,
    chunks_processed = chunk_count,
    learning_rate = learning_rate
  )
  
  saveRDS(model_result, "analysis/linear_regression/results/diesel_linear_regression_model.rds")
  
}

dbDisconnect(con)

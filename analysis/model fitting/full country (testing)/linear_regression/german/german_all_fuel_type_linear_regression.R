# German Fuel Price Prediction Model
# Training using incremental learning approach

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Database path
db_path <- "databases/german_fuel_data.duckdb"

# Connect to database
con <- dbConnect(duckdb(), db_path, read_only = FALSE)

# Set DuckDB memory settings
dbExecute(con, "SET memory_limit='16GB'")
dbExecute(con, "SET max_temp_directory_size='20GB'") 
dbExecute(con, "SET threads=8")
dbExecute(con, "SET preserve_insertion_order=false")

# Feature engineering function with CYCLICAL encoding
create_features <- function(data) {
  data %>%
    mutate(
      # Temporal features - CYCLICAL ENCODING
      hour = hour(date),
      day_of_week = wday(date),
      month = month(date),
      
      # Cyclical encoding for hour (24-hour cycle)
      hour_sin = sin(2 * pi * hour / 24),
      hour_cos = cos(2 * pi * hour / 24),
      
      # Cyclical encoding for day of week (7-day cycle)
      dow_sin = sin(2 * pi * day_of_week / 7),
      dow_cos = cos(2 * pi * day_of_week / 7),
      
      # Cyclical encoding for month (12-month cycle)
      month_sin = sin(2 * pi * month / 12),
      month_cos = cos(2 * pi * month / 12),
      
      # Keep fuel_type as factor (no dummy encoding needed)
      # model.matrix will handle the encoding automatically
    ) %>%
    select(-hour, -day_of_week, -month, -date)  # Remove original temporal features and date, keep fuel_type
}

# Function to normalize features using entire training statistics
# For lat/long: use centering only (preserves geographic relationships)
# For density/time: use Z-score normalization
normalize_features <- function(data, stats) {
  data %>%
    mutate(
      # Centering for coordinates (preserves geographic relationships)
      latitude_centered = latitude - stats$latitude_mean,
      longitude_centered = longitude - stats$longitude_mean,
      
      # Z-score normalization for density feature
      nearby_stations_1km_norm = ifelse(stats$nearby_stations_1km_sd == 0, 0, (nearby_stations_1km - stats$nearby_stations_1km_mean) / stats$nearby_stations_1km_sd)
    ) %>%
    select(-latitude, -longitude, -nearby_stations_1km)  # Remove original features
}

# Load station metadata
station_metadata <- dbGetQuery(con, "
  SELECT uuid, latitude, longitude, brand, nearby_stations_1km
  FROM german_stations
")

# Brand categorization based on station counts (data-driven)
# Calculate station counts per brand
brand_counts <- station_metadata %>%
  filter(!is.na(brand) & brand != "") %>%
  group_by(brand) %>%
  summarise(station_count = n(), .groups = 'drop')

cat("Brand categorization:\n")
cat("  Major: >= 700 stations\n")
cat("  Moderate: 100-699 stations\n")
cat("  Small: < 100 stations\n")
cat("  Unknown: Missing brand data\n\n")

# Apply thresholds to categorize brands
brand_categories <- brand_counts %>%
  mutate(
    brand_category = case_when(
      station_count >= 700 ~ "Major",
      station_count >= 100 ~ "Moderate",
      TRUE ~ "Small"
    )
  )

# Join back to station metadata
station_metadata <- station_metadata %>%
  left_join(brand_categories %>% select(brand, brand_category), by = "brand") %>%
  mutate(
    brand_category = ifelse(is.na(brand_category), "Unknown", brand_category)
  )

# Show categorization summary
cat("Station metadata loaded:", nrow(station_metadata), "stations\n")
category_summary <- station_metadata %>%
  group_by(brand_category) %>%
  summarise(
    stations = n(),
    percentage = round(n() / nrow(station_metadata) * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(stations))

cat("\nBrand category distribution:\n")
print(category_summary)

# Time-based train/test split
# Train: Dec 2024 - July 2025
# Test: Aug - Sep 2025 (20%)
train_start <- "2024-12-01"
train_end <- "2025-07-31"
test_start <- "2025-08-01"
test_end <- "2025-09-30"


#### LINEAR REGRESSION


# Initialize model parameters
learning_rate <- 0.001  # Moderate learning rate
chunk_size <- 250000  # Process 250k records at a time

# Calculate total training records to determine max_chunks
total_train_records <- dbGetQuery(con, paste0("
  SELECT COUNT(*) as count
  FROM german_prices p
  JOIN german_stations s ON p.station_uuid = s.uuid
  WHERE p.date >= '", train_start, "' 
    AND p.date <= '", train_end, "'
"))$count

max_chunks <- ceiling(total_train_records / chunk_size)  # Cover all training data

cat("Total training records:", total_train_records, "\n")
cat("Processing", max_chunks, "chunks of", chunk_size, "records each\n")

# CALCULATE GLOBAL NORMALIZATION STATISTICS

# Calculate GLOBAL statistics from all training data 
# Only spatial, density, and time trend features need normalization (cyclical features are already -1 to 1)
global_stats_query <- paste0("
  SELECT 
    AVG(s.latitude) as latitude_mean,
    STDDEV(s.latitude) as latitude_sd,
    AVG(s.longitude) as longitude_mean,
    STDDEV(s.longitude) as longitude_sd,
    AVG(s.nearby_stations_1km) as nearby_stations_1km_mean,
    STDDEV(s.nearby_stations_1km) as nearby_stations_1km_sd
  FROM german_prices p
  JOIN german_stations s ON p.station_uuid = s.uuid
  WHERE p.date >= '", train_start, "' 
    AND p.date <= '", train_end, "'
")

global_stats <- dbGetQuery(con, global_stats_query)

# Store spatial, density, and time trend feature statistics
feature_stats <- list(
  latitude_mean = global_stats$latitude_mean,
  latitude_sd = global_stats$latitude_sd,
  longitude_mean = global_stats$longitude_mean,
  longitude_sd = global_stats$longitude_sd,
  nearby_stations_1km_mean = global_stats$nearby_stations_1km_mean,
  nearby_stations_1km_sd = global_stats$nearby_stations_1km_sd,
  days_since_start_mean = global_stats$days_since_start_mean,
  days_since_start_sd = global_stats$days_since_start_sd
)

# Initialize weights (bias + features)
# Features: hour_sin, hour_cos, dow_sin, dow_cos, month_sin, month_cos,
#           days_since_start_norm, fuel_type (3 levels: e10, e5, diesel), 
#           latitude_centered, longitude_centered, nearby_stations_1km_norm, 
#           brand_category (4 levels: Major, Moderate, Small, Unknown)

n_features <- 14  # 6 cyclical + 2 fuel types (e10 is reference) + 3 spatial + 3 brand categories (Unknown is reference)


# Training metrics
total_samples <- 0
mse_history <- c()
chunk_count <- 0

# Process training data in chunks
for(chunk_idx in 1:max_chunks) {
  # Get chunk of training data
  offset <- (chunk_idx - 1) * chunk_size
  
  chunk_query <- paste0("
    SELECT 
      p.date,
      p.station_uuid,
      p.diesel,
      p.e5, 
      p.e10,
      s.latitude,
      s.longitude,
      s.brand,
      s.nearby_stations_1km
    FROM german_prices p
    JOIN german_stations s ON p.station_uuid = s.uuid
    WHERE p.date >= '", train_start, "' 
      AND p.date <= '", train_end, "'
    LIMIT ", chunk_size, " OFFSET ", offset
  )
  
  chunk_data <- dbGetQuery(con, chunk_query)
  
  # Break if no more data
  if(nrow(chunk_data) == 0) {
    cat("Reached end of training data at chunk", chunk_idx, "\n")
    break
  }
  
  # Handle brand NAs
  chunk_data <- chunk_data %>%
    mutate(
      brand = ifelse(is.na(brand) | brand == "", "Unknown", brand)
    )
  
  # Create long format data
  chunk_long <- chunk_data %>%
    pivot_longer(cols = c(diesel, e5, e10), 
                 names_to = "fuel_type", 
                 values_to = "price") %>%
    filter(price > 0 & price < 10)  # Remove invalid prices
  
  # Add features
  chunk_features <- create_features(chunk_long)
  
  # Add brand category
  chunk_features <- chunk_features %>%
    left_join(station_metadata %>% select(uuid, brand_category), 
              by = c("station_uuid" = "uuid"))
  
  # Normalize features using GLOBAL statistics
  chunk_features <- normalize_features(chunk_features, feature_stats)
  
  # Ensure categorical variables are factors with multiple levels
  chunk_features <- chunk_features %>%
    mutate(
      brand_category = as.factor(brand_category),
      fuel_type = as.factor(fuel_type)
    )
  
  # Set Unknown as reference category to
  chunk_features$brand_category <- relevel(chunk_features$brand_category, ref = "Unknown")
  
  # Remove any rows with NA values
  valid_rows <- complete.cases(chunk_features)
  chunk_features <- chunk_features[valid_rows, ]
  
  # Create feature matrix with cyclical and normalized features
  # Remove -1 to allow reference category (Unknown) to be dropped automatically
  # Test: Add back days_since_start_norm (time feature) - this should break the model
X <- model.matrix(~ hour_sin + hour_cos + dow_sin + dow_cos + month_sin + month_cos +
                                    fuel_type + 
                                    latitude_centered + longitude_centered + nearby_stations_1km_norm + brand_category,
                    data = chunk_features, drop.unused.levels = TRUE) # handle single-level factors
  
  y <- chunk_features$price
  
  if(nrow(X) == 0) {
    cat("Skipping chunk", chunk_idx, "- no valid data\n")
    next
  }
  
  # Initialize weights on first iteration to match actual X dimensions
  if(chunk_idx == 1) {
    weights <- rep(0, ncol(X))
    cat("Initialized weights vector with", ncol(X), "features\n")
    cat("Feature names:", paste(colnames(X), collapse = ", "), "\n")
  }
  
  # Use X directly (already includes intercept column from model.matrix)
  predictions <- as.vector(X %*% weights)
  errors <- y - predictions
  mse <- mean(errors^2)
  
  # Gradient descent update with clipping for numerical stability
  gradient_weights <- -2 * colMeans(X * as.vector(errors))
  
  # Gradient clipping to prevent explosion
  max_grad <- max(abs(gradient_weights), na.rm = TRUE)
  if(max_grad > 1000) {  # Moderate clipping
    gradient_weights <- gradient_weights * (1000 / max_grad)
    cat("  Gradient clipped at chunk", chunk_idx, "- max grad:", round(max_grad, 2), "\n")
  }
  
  # Update weights with clipping
  weight_update <- learning_rate * gradient_weights
  weights <- weights - weight_update
  
  # Weight bounds checking to prevent explosion
  if(max(abs(weights)) > 1000) {
    weights <- weights * (1000 / max(abs(weights)))
    cat("  Weights scaled down at chunk", chunk_idx, "\n")
  }
  
  # Additional weight bounds checking (redundant but safe)
  
  # Update metrics
  total_samples <- total_samples + length(y)
  mse_history <- c(mse_history, mse)
  chunk_count <- chunk_count + 1
  
  # Progress update
  if(chunk_count %% 10 == 0) {
    cat("Chunk", chunk_count, "- Samples:", total_samples, 
        "- MSE:", round(mse, 4), "\n")
    
    # Diagnostic code removed for cleaner output
    # (Feature ranges and weight magnitudes monitoring disabled)
  }
}

cat("Total samples processed:", total_samples, "\n")
final_train_mse <- tail(mse_history, 1)
cat("Final Training MSE:", round(final_train_mse, 4), "\n")
cat("Final weights:", round(weights, 4), "\n\n")

# MODEL TESTING ON 20% TEST SET 

##### Check and reconnect database connection
if(!dbIsValid(con)) {
  cat("Database connection lost, reconnecting...\n")
  con <- dbConnect(duckdb(), db_path, read_only = FALSE)
  dbExecute(con, "SET memory_limit='10GB'")
  dbExecute(con, "SET max_temp_directory_size='20GB'")
  dbExecute(con, "SET threads=4")
  cat("Database reconnected successfully\n")
}

# Calculate test set size
total_test_records <- dbGetQuery(con, paste0("
  SELECT COUNT(*) as count
  FROM german_prices p
  JOIN german_stations s ON p.station_uuid = s.uuid
  WHERE p.date >= '", test_start, "' 
    AND p.date <= '", test_end, "'
"))$count

max_test_chunks <- ceiling(total_test_records / chunk_size)
cat("Test records:", format(total_test_records, big.mark = ","), "\n")
cat("Test chunks:", max_test_chunks, "\n")

# Initialize test metrics
test_predictions <- c()
test_actual <- c()
test_chunk_count <- 0

# Test the model on test data
for(chunk_idx in 1:max_test_chunks) {
  offset <- (chunk_idx - 1) * chunk_size
  
  # Load test chunk
  test_chunk_query <- paste0("
    SELECT 
      p.date,
      p.station_uuid,
      p.diesel,
      p.e5, 
      p.e10,
      s.latitude,
      s.longitude,
      s.brand,
      s.nearby_stations_1km
    FROM german_prices p
    JOIN german_stations s ON p.station_uuid = s.uuid
    WHERE p.date >= '", test_start, "' 
      AND p.date <= '", test_end, "'
    LIMIT ", chunk_size, " OFFSET ", offset
  )
  
  test_chunk_data <- dbGetQuery(con, test_chunk_query)
  
  if(nrow(test_chunk_data) == 0) {
    cat("Reached end of test data at chunk", chunk_idx, "\n")
    break
  }
  
  # Handle brand NAs
  test_chunk_data <- test_chunk_data %>%
    mutate(
      brand = ifelse(is.na(brand) | brand == "", "Unknown", brand)
    )
  
  # Transform to long format
  test_chunk_long <- test_chunk_data %>%
    pivot_longer(cols = c(diesel, e5, e10), 
                 names_to = "fuel_type", 
                 values_to = "price") %>%
    filter(price > 0 & price < 10)  # Remove invalid prices
  
  # Add features
  test_chunk_features <- create_features(test_chunk_long)
  
  # Add brand category
  test_chunk_features <- test_chunk_features %>%
    left_join(station_metadata %>% select(uuid, brand_category), 
              by = c("station_uuid" = "uuid"))
  
  # Normalize features using GLOBAL statistics (same as training)
  test_chunk_features <- normalize_features(test_chunk_features, feature_stats)
  
  # Convert to factors
  test_chunk_features <- test_chunk_features %>%
    mutate(
      brand_category = as.factor(brand_category),
      fuel_type = as.factor(fuel_type)
    )
  
  # Set Unknown as reference category
  test_chunk_features$brand_category <- relevel(test_chunk_features$brand_category, ref = "Unknown")
  
  # Remove NAs
  valid_rows <- complete.cases(test_chunk_features)
  test_chunk_features <- test_chunk_features[valid_rows, ]
  
  if(nrow(test_chunk_features) == 0) {
    cat("Skipping test chunk", chunk_idx, "- no valid data\n")
    next
  }
  
  # Create test feature matrix
  X_test <- model.matrix(~ hour_sin + hour_cos + dow_sin + dow_cos + month_sin + month_cos +
                         fuel_type + 
                         latitude_centered + longitude_centered + nearby_stations_1km_norm + brand_category, 
                        data = test_chunk_features, drop.unused.levels = TRUE)
  
  y_test <- test_chunk_features$price
  
  # Make predictions using X_test directly (no separate bias)
  test_pred <- as.vector(X_test %*% weights)
  
  # Store predictions and actual values
  test_predictions <- c(test_predictions, as.numeric(test_pred))
  test_actual <- c(test_actual, y_test)
  test_chunk_count <- test_chunk_count + 1
  
  # Progress update
  if(test_chunk_count %% 10 == 0) {
    cat("Test chunk", test_chunk_count, "- samples:", length(test_actual), "\n")
  }
}

# Calculate test metrics
if(length(test_predictions) > 0) {
  test_mse <- mean((test_actual - test_predictions)^2)
  test_rmse <- sqrt(test_mse)
  test_mae <- mean(abs(test_actual - test_predictions))
  test_r2 <- 1 - (sum((test_actual - test_predictions)^2) / sum((test_actual - mean(test_actual))^2))
  
  cat("\n=== TEST SET PERFORMANCE ===\n")
  cat("Test samples:", format(length(test_actual), big.mark = ","), "\n")
  cat("Test MSE:", round(test_mse, 4), "\n")
  cat("Test RMSE:", round(test_rmse, 4), "€\n")
  cat("Test MAE:", round(test_mae, 4), "€\n")
  cat("Test R²:", round(test_r2, 4), "\n")
  
  # Compare with training performance
  final_train_mse <- tail(mse_history, 1)
  cat("Final Train MSE:", round(final_train_mse, 4), "\n")
  cat("Final Test MSE:", round(test_mse, 4), "\n")
  cat("Overfitting check (Test MSE / Train MSE):", round(test_mse / final_train_mse, 2), "\n")
} 

# Save model with test results and normalization statistics
model_results <- list(
  weights = weights,
  mse_history = mse_history,
  total_samples = total_samples,
  chunk_count = chunk_count,
  feature_names = colnames(X),
  feature_stats = feature_stats,  # Save global normalization statistics
  train_period = c(train_start, train_end),
  test_period = c(test_start, test_end),
  test_predictions = test_predictions,
  test_actual = test_actual,
  test_mse = if(exists("test_mse")) test_mse else NA,
  test_rmse = if(exists("test_rmse")) test_rmse else NA,
  test_mae = if(exists("test_mae")) test_mae else NA,
  test_r2 = if(exists("test_r2")) test_r2 else NA
)

# Save models
saveRDS(model_results, "german_linear_regression_model.rds")

cat("Model saved to: german_linear_regression_model.rds\n")

# FINAL PERFORMANCE SUMMARY
cat("\n=== FINAL MODEL PERFORMANCE SUMMARY ===\n")
cat("Training MSE:", round(final_train_mse, 4), "\n")
if(exists("test_mse")) {
  cat("Test MSE:", round(test_mse, 4), "\n")
  cat("Test RMSE:", round(test_rmse, 4), "€\n")
  cat("Test R²:", round(test_r2, 4), "\n")
  cat("Model Status: ✅ READY FOR PRODUCTION\n")
} else {
  cat("Model Status: ✅ TRAINED (Testing skipped)\n")
}
cat("=====================================\n")

# Plot training progress
if(length(mse_history) > 1) {
  plot_data <- data.frame(
    chunk = 1:length(mse_history),
    mse = mse_history
  )
  
  p1 <- ggplot(plot_data, aes(x = chunk, y = mse)) +
    geom_line(color = "blue", linewidth = 1) +
    labs(title = "Training Progress",
         subtitle = paste("Final MSE:", round(tail(mse_history, 1), 4)),
         x = "Chunk", y = "Mean Squared Error") +
    theme_minimal() +
    scale_y_log10()
  
  # Feature importance plot
  feature_importance <- data.frame(
    Feature = colnames(X),
    Weight = weights,
    Abs_Weight = abs(weights)
  ) %>%
    arrange(desc(Abs_Weight)) %>%
    head(10)
  
  p2 <- ggplot(feature_importance, aes(x = reorder(Feature, Abs_Weight), y = Abs_Weight)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(title = "Feature Importance",
         x = "Features", y = "Absolute Weight") +
    theme_minimal()
  
  # Test performance plot if available
  if(!is.na(test_mse) && length(test_predictions) > 0) {
    test_data <- data.frame(
      Actual = test_actual,
      Predicted = test_predictions
    )
    
    # Sample if too large
    if(nrow(test_data) > 10000) {
      test_data <- test_data[sample(1:nrow(test_data), 10000), ]
    }
    
    p3 <- ggplot(test_data, aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.3, color = "red") +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
      labs(title = "Test Performance",
           subtitle = paste("R² =", round(test_r2, 3)),
           x = "Actual Price (€)", y = "Predicted Price (€)") +
      theme_minimal()
    
    # Combine all plots
    library(gridExtra)
    combined_plot <- grid.arrange(p1, p2, p3, ncol = 2, nrow = 2)
  } else {
    # Just training plots
    library(gridExtra)
    combined_plot <- grid.arrange(p1, p2, ncol = 1, nrow = 2)
  }
  
  print(combined_plot)
}

# Close database connection
dbDisconnect(con)


# German Fuel Price Prediction Model
# Training using incremental learning approach

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Database path
db_path <- "/Users/alexphan/Desktop/MBAT-Internship-Project/databases/german_fuel_data.duckdb"

# Connect to database
con <- dbConnect(duckdb(), db_path, read_only = FALSE)

# Set DuckDB memory settings
dbExecute(con, "SET memory_limit='16GB'")
dbExecute(con, "SET max_temp_directory_size='20GB'") 
dbExecute(con, "SET threads=8")
dbExecute(con, "SET preserve_insertion_order=false")

# Feature engineering function
create_features <- function(data) {
  data %>%
    mutate(
      # Temporal features
      hour = hour(date),
      day_of_week = wday(date),
      month = month(date),
      
      # Fuel type encoding
      fuel_diesel = ifelse(fuel_type == "diesel", 1, 0),
      fuel_e5 = ifelse(fuel_type == "e5", 1, 0),
      fuel_e10 = ifelse(fuel_type == "e10", 1, 0)
    ) %>%
    select(-fuel_type)  # Remove original fuel_type column
}

# Function to normalize features (Z-score normalization)
normalize_features <- function(data) {
  data %>%
    mutate(
      # Normalize numeric features using data's own statistics
      # Handle division by zero when sd = 0 (all values are the same)
      hour_norm = ifelse(sd(hour, na.rm = TRUE) == 0, 0, (hour - mean(hour, na.rm = TRUE)) / sd(hour, na.rm = TRUE)),
      day_of_week_norm = ifelse(sd(day_of_week, na.rm = TRUE) == 0, 0, (day_of_week - mean(day_of_week, na.rm = TRUE)) / sd(day_of_week, na.rm = TRUE)),
      month_norm = ifelse(sd(month, na.rm = TRUE) == 0, 0, (month - mean(month, na.rm = TRUE)) / sd(month, na.rm = TRUE)),
      latitude_norm = ifelse(sd(latitude, na.rm = TRUE) == 0, 0, (latitude - mean(latitude, na.rm = TRUE)) / sd(latitude, na.rm = TRUE)),
      longitude_norm = ifelse(sd(longitude, na.rm = TRUE) == 0, 0, (longitude - mean(longitude, na.rm = TRUE)) / sd(longitude, na.rm = TRUE)),
      nearby_stations_1km_norm = ifelse(sd(nearby_stations_1km, na.rm = TRUE) == 0, 0, (nearby_stations_1km - mean(nearby_stations_1km, na.rm = TRUE)) / sd(nearby_stations_1km, na.rm = TRUE))
    ) %>%
    select(-hour, -day_of_week, -month, -latitude, -longitude, -nearby_stations_1km)  # Remove original features
}

# Load station metadata for nearby station calculation
station_metadata <- dbGetQuery(con, "
  SELECT uuid, latitude, longitude, brand
  FROM german_stations
")

# Brand categorization
station_metadata <- station_metadata %>%
  mutate(
    brand_category = case_when(
      # Major brands (known large chains)
      brand %in% c("ARAL", "Shell", "ESSO", "TotalEnergies") ~ "Major",
      # Moderate brands (known regional chains)
      brand %in% c("AVIA", "JET", "STAR") ~ "Moderate", 
      # Small brands (everything else with a name)
      !is.na(brand) & brand != "" ~ "Small",
      # Unknown (missing data)
      TRUE ~ "Unknown"
    )
  )

cat("Station metadata loaded:", nrow(station_metadata), "stations\n")

# Time-based train/test split
# Train: Dec 2024 - July 2025 (80%)
# Test: Aug - Sep 2025 (20%)
train_start <- "2024-12-01"
train_end <- "2025-07-31"
test_start <- "2025-08-01"
test_end <- "2025-09-30"

#### LINEAR REGRESSION

# Initialize model parameters
learning_rate <- 0.001
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


# Use simple normalization - calculate from first chunk
feature_stats <- NULL

# Initialize weights (bias + features)
# Features: hour, day_of_week, month,
#           fuel_diesel, fuel_e5, fuel_e10, latitude, longitude, nearby_stations_1km, 
#           brand_category (4 levels: Major, Moderate, Small, Unknown)
n_features <- 13
weights <- rep(0, n_features)
bias <- 0

# Training metrics
total_samples <- 0
mse_history <- c()
chunk_count <- 0

cat("Processing training data in chunks of", chunk_size, "records...\n")

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
  
  # Normalize features
  chunk_features <- normalize_features(chunk_features)
  
  # Ensure categorical variables are factors with multiple levels
  chunk_features <- chunk_features %>%
    mutate(
      brand_category = as.factor(brand_category),
      fuel_diesel = as.factor(fuel_diesel),
      fuel_e5 = as.factor(fuel_e5),
      fuel_e10 = as.factor(fuel_e10)
    )
  
  # Remove any rows with NA values
  valid_rows <- complete.cases(chunk_features)
  chunk_features <- chunk_features[valid_rows, ]
  
  # Create feature matrix with normalized features
  X <- model.matrix(~ hour_norm + day_of_week_norm + month_norm +
                      fuel_diesel + fuel_e5 + fuel_e10 + latitude_norm + longitude_norm + 
                      nearby_stations_1km_norm + brand_category - 1, 
                    data = chunk_features, drop.unused.levels = TRUE) # handle single-level factors
  
  y <- chunk_features$price
  
  if(nrow(X) == 0) {
    cat("Skipping chunk", chunk_idx, "- no valid data\n")
    next
  }
  
  # Add bias column
  X_with_bias <- cbind(1, X)
  
  # Update weights using gradient descent
  predictions <- X_with_bias %*% c(bias, weights)
  errors <- y - predictions
  mse <- mean(errors^2)
  
  # Gradient descent update
  gradient_bias <- -2 * mean(errors)
  gradient_weights <- -2 * colMeans(X * as.vector(errors))
  
  bias <- bias - learning_rate * gradient_bias
  weights <- weights - learning_rate * gradient_weights
  
  # Update metrics
  total_samples <- total_samples + length(y)
  mse_history <- c(mse_history, mse)
  chunk_count <- chunk_count + 1
  
  # Progress update
  if(chunk_count %% 10 == 0) {
    cat("Chunk", chunk_count, "- Samples:", total_samples, 
        "- MSE:", round(mse, 4), "\n")
  }
}

cat("Total samples processed:", total_samples, "\n")
cat("Final MSE:", round(tail(mse_history, 1), 4), "\n")
cat("Final bias:", round(bias, 4), "\n")
cat("Final weights:", round(weights, 4), "\n\n")

# === MODEL TESTING ON 20% TEST SET ===
cat("=== Testing Model on Test Set ===\n")
cat("Test period:", test_start, "to", test_end, "\n")

# Check and reconnect database connection
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
cat("Testing model on test data...\n")
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
    filter(price > 0 & price < 10)
  
  # Add features
  test_chunk_features <- create_features(test_chunk_long)
  
  # Add brand category
  test_chunk_features <- test_chunk_features %>%
    left_join(station_metadata %>% select(uuid, brand_category), 
              by = c("station_uuid" = "uuid"))
  
  # Normalize features
  test_chunk_features <- normalize_features(test_chunk_features)
  
  # Convert to factors
  test_chunk_features <- test_chunk_features %>%
    mutate(
      brand_category = as.factor(brand_category),
      fuel_diesel = as.factor(fuel_diesel),
      fuel_e5 = as.factor(fuel_e5),
      fuel_e10 = as.factor(fuel_e10)
    )
  
  # Remove NAs
  valid_rows <- complete.cases(test_chunk_features)
  test_chunk_features <- test_chunk_features[valid_rows, ]
  
  if(nrow(test_chunk_features) == 0) {
    cat("Skipping test chunk", chunk_idx, "- no valid data\n")
    next
  }
  
  # Create test feature matrix
  X_test <- model.matrix(~ hour_norm + day_of_week_norm + month_norm +
                         fuel_diesel + fuel_e5 + fuel_e10 + latitude_norm + longitude_norm + 
                         nearby_stations_1km_norm + brand_category - 1, 
                        data = test_chunk_features, drop.unused.levels = TRUE)
  
  y_test <- test_chunk_features$price
  
  # Make predictions
  X_test_with_bias <- cbind(1, X_test)
  test_pred <- X_test_with_bias %*% c(bias, weights)
  
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
  cat("Overfitting check (Test MSE / Train MSE):", round(test_mse / final_train_mse, 2), "\n")
  
  if(test_mse / final_train_mse > 1.5) {
    cat("Potential overfitting detected\n")
  } else {
    cat("Good generalization performance\n")
  }
} else {
  cat("No valid test data found\n")
}

# Save model with test results
model_results <- list(
  weights = weights,
  bias = bias,
  mse_history = mse_history,
  total_samples = total_samples,
  chunk_count = chunk_count,
  feature_names = colnames(X),
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
saveRDS(model_results, "models/german_linear_regression_model.rds")

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

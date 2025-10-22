#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(gridExtra)
})

# Configuration ---------------------------------------------------------------

# Input DuckDBs - all countries
german_db <- "databases/german_fuel.duckdb"
austrian_db <- "databases/austrian_fuel_database.duckdb"
slovenian_db <- "databases/slovenian_fuel_database.duckdb"
stations_db <- "databases/stations.duckdb"

# Query window and filters (reduced for faster training)
date_start <- as.POSIXct("2024-12-01 00:00:00", tz = "UTC")
date_end   <- as.POSIXct("2025-01-31 23:59:59", tz = "UTC")

# Training hyperparameters
learning_rate <- 0.001           # Adam base LR (alpha)
weight_decay  <- 1e-4            # L2 regularization strength (lambda)
chunk_size    <- 50000           # Smaller chunks for faster processing
num_epochs    <- 5               # Max epochs
patience      <- 3               # Early stopping patience (epochs)
min_delta     <- 1e-5            # Early stopping minimal improvement in val MSE

# Adam optimizer hyperparameters
beta1 <- 0.9
beta2 <- 0.999
epsilon <- 1e-8

# Brand categorization
international_brands <- c(
  'Shell', 'BP', 'Esso', 'ESSO', 'Total', 'TotalEnergies', 'TOTAL',
  'OMV', 'Eni', 'ENI', 'AgipEni', 'AGIP ENI', 'Q8'
)

national_brands <- c(
  'ARAL', 'Api-Ip', 'PompeBianche', 'Tamoil', 'Petrol'
)

regional_brands <- c(
  'JET', 'AVIA', 'STAR', 'HEM', 'BFT', 'Q1',
  'Raiffeisen', 'Westfalen', 'Hoyer', 'Sprint', 'BayWa'
)

set.seed(42)

# Utilities -------------------------------------------------------------------

create_features <- function(df) {
  df %>%
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
    select(-hour, -day_of_week, -month)
}

normalize_features <- function(df, stats) {
  df %>%
    mutate(
      latitude_centered = latitude - stats$latitude_mean,
      longitude_centered = longitude - stats$longitude_mean,
      nearby_stations_1km_norm = ifelse(stats$nearby_stations_1km_sd == 0, 0,
        (nearby_stations_1km - stats$nearby_stations_1km_mean) / stats$nearby_stations_1km_sd)
    )
}

build_model_matrix <- function(df) {
  # Brand categorization (International/National/Regional/Unknown)
  df$brand <- ifelse(is.na(df$brand) | df$brand == "", "Unknown", df$brand)
  df$brand_category <- case_when(
    df$brand %in% international_brands ~ "International",
    df$brand %in% national_brands ~ "National", 
    df$brand %in% regional_brands ~ "Regional",
    TRUE ~ "Unknown"
  )
  df$brand_category <- as.factor(df$brand_category)
  df$brand_category <- relevel(df$brand_category, ref = "Unknown")
  
  X <- model.matrix(~ hour_sin + hour_cos + dow_sin + dow_cos + month_sin + month_cos +
                      latitude_centered + longitude_centered + nearby_stations_1km_norm +
                      brand_category,
                    data = df, drop.unused.levels = TRUE)
  return(X)
}

compute_mse <- function(y_true, y_pred) {
  mean((y_true - y_pred)^2)
}

# Visualization functions -----------------------------------------------------

create_training_plots <- function(training_metrics, model_name = "Unified Diesel Model") {
  # Create plots directory
  plots_dir <- "analysis/linear_regression/results/plots"
  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 1. Loss curves (chunk-level)
  chunk_data <- data.frame(
    chunk = 1:length(training_metrics$chunk_mse),
    mse = training_metrics$chunk_mse,
    weight_decay = training_metrics$chunk_weight_decay_loss,
    total_loss = training_metrics$chunk_total_loss
  )
  
  p1 <- ggplot(chunk_data, aes(x = chunk)) +
    geom_line(aes(y = mse, color = "MSE"), alpha = 0.7) +
    geom_line(aes(y = weight_decay, color = "Weight Decay"), alpha = 0.7) +
    geom_line(aes(y = total_loss, color = "Total Loss"), alpha = 0.7) +
    labs(title = paste(model_name, "- Loss Curves (Chunk Level)"),
         x = "Chunk", y = "Loss", color = "Loss Type") +
    theme_minimal() +
    scale_y_log10() +
    theme(legend.position = "bottom")
  
  # 2. Epoch-level loss curves
  epoch_data <- data.frame(
    epoch = 1:length(training_metrics$epoch_mse),
    mse = training_metrics$epoch_mse,
    weight_decay = training_metrics$epoch_weight_decay_loss,
    total_loss = training_metrics$epoch_total_loss
  )
  
  p2 <- ggplot(epoch_data, aes(x = epoch)) +
    geom_line(aes(y = mse, color = "MSE"), size = 1.2) +
    geom_line(aes(y = weight_decay, color = "Weight Decay"), size = 1.2) +
    geom_line(aes(y = total_loss, color = "Total Loss"), size = 1.2) +
    geom_point(aes(y = mse, color = "MSE"), size = 2) +
    geom_point(aes(y = weight_decay, color = "Weight Decay"), size = 2) +
    geom_point(aes(y = total_loss, color = "Total Loss"), size = 2) +
    labs(title = paste(model_name, "- Loss Curves (Epoch Level)"),
         x = "Epoch", y = "Loss", color = "Loss Type") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # 3. Weight and gradient norms
  norm_data <- data.frame(
    chunk = 1:length(training_metrics$weight_norms),
    weight_norm = training_metrics$weight_norms,
    gradient_norm = training_metrics$gradient_norms
  )
  
  p3 <- ggplot(norm_data, aes(x = chunk)) +
    geom_line(aes(y = weight_norm, color = "Weight Norm"), alpha = 0.7) +
    geom_line(aes(y = gradient_norm, color = "Gradient Norm"), alpha = 0.7) +
    labs(title = paste(model_name, "- Weight & Gradient Norms"),
         x = "Chunk", y = "Norm", color = "Norm Type") +
    theme_minimal() +
    scale_y_log10() +
    theme(legend.position = "bottom")
  
  # 4. Loss components comparison
  loss_components <- data.frame(
    chunk = 1:length(training_metrics$chunk_mse),
    mse_component = training_metrics$chunk_mse,
    l2_component = training_metrics$chunk_weight_decay_loss
  )
  
  p4 <- ggplot(loss_components, aes(x = chunk)) +
    geom_line(aes(y = mse_component, color = "MSE Component"), alpha = 0.7) +
    geom_line(aes(y = l2_component, color = "L2 Regularization"), alpha = 0.7) +
    labs(title = paste(model_name, "- Loss Components"),
         x = "Chunk", y = "Loss Value", color = "Component") +
    theme_minimal() +
    scale_y_log10() +
    theme(legend.position = "bottom")
  
  # Save individual plots
  ggsave(file.path(plots_dir, "loss_curves_chunk.png"), p1, width = 10, height = 6, dpi = 300)
  ggsave(file.path(plots_dir, "loss_curves_epoch.png"), p2, width = 10, height = 6, dpi = 300)
  ggsave(file.path(plots_dir, "weight_gradient_norms.png"), p3, width = 10, height = 6, dpi = 300)
  ggsave(file.path(plots_dir, "loss_components.png"), p4, width = 10, height = 6, dpi = 300)
  
  # Create combined plot
  combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
  ggsave(file.path(plots_dir, "training_metrics_combined.png"), combined_plot, 
         width = 16, height = 12, dpi = 300)
  
  cat("Training visualizations saved to:", plots_dir, "\n")
  cat("- loss_curves_chunk.png: Chunk-level loss curves\n")
  cat("- loss_curves_epoch.png: Epoch-level loss curves\n") 
  cat("- weight_gradient_norms.png: Weight and gradient norms\n")
  cat("- loss_components.png: MSE vs L2 regularization components\n")
  cat("- training_metrics_combined.png: All plots combined\n")
  
  return(list(
    chunk_plot = p1,
    epoch_plot = p2, 
    norms_plot = p3,
    components_plot = p4,
    combined_plot = combined_plot
  ))
}

# Data access -----------------------------------------------------------------

# Connect to main database and attach all others
con <- dbConnect(duckdb())
dbExecute(con, "SET threads=8")
dbExecute(con, "SET memory_limit='32GB'")

# Attach all databases
german_path <- normalizePath(german_db)
austrian_path <- normalizePath(austrian_db)
slovenian_path <- normalizePath(slovenian_db)
stations_path <- normalizePath(stations_db)

dbExecute(con, paste0("ATTACH '", german_path, "' AS ger"))
dbExecute(con, paste0("ATTACH '", austrian_path, "' AS aut"))
dbExecute(con, paste0("ATTACH '", slovenian_path, "' AS slo"))
dbExecute(con, paste0("ATTACH '", stations_path, "' AS stations_db"))

# Multi-country diesel data query with fuel type mapping
multi_country_query <- sprintf(
  "
  WITH all_diesel_data AS (
    -- German data
    SELECT 
      p.date,
      p.station_uuid,
      p.diesel AS price,
      s.latitude,
      s.longitude,
      s.station_brand AS brand,
      s.nearby_station_1km,
      'Germany' AS country,
      ROW_NUMBER() OVER (ORDER BY RANDOM()) as row_num
    FROM ger.german_prices_wide p
    JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
    WHERE p.date >= TIMESTAMP '%s' AND p.date <= TIMESTAMP '%s'
      AND p.diesel > 0 AND p.diesel < 10
      AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
    
    UNION ALL
    
    -- Austrian data (with fuel type mapping)
    SELECT 
      p.date,
      p.station_uuid,
      p.price,
      s.latitude,
      s.longitude,
      s.station_brand AS brand,
      s.nearby_station_1km,
      'Austria' AS country,
      ROW_NUMBER() OVER (ORDER BY RANDOM()) as row_num
    FROM aut.austrian_prices p
    JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Austria'
    WHERE p.date >= TIMESTAMP '%s' AND p.date <= TIMESTAMP '%s'
      AND p.price > 0 AND p.price < 10
      AND s.latitude BETWEEN 46 AND 49 AND s.longitude BETWEEN 9 AND 17
      AND p.fuel_type IN ('DIESEL', 'diesel', 'fuel_d')
    
    UNION ALL
    
    -- Slovenian data (with fuel type mapping and 80%% random sample)
    SELECT 
      p.date,
      p.station_uuid,
      p.price,
      s.latitude,
      s.longitude,
      s.station_brand AS brand,
      s.nearby_station_1km,
      'Slovenia' AS country,
      ROW_NUMBER() OVER (ORDER BY RANDOM()) as row_num
    FROM slo.slovenian_prices p
    JOIN stations_db.stations s ON CAST(p.station_uuid AS DOUBLE) = CAST(s.station_uuid AS DOUBLE) AND s.country = 'Slovenia'
    WHERE p.price > 0 AND p.price < 10
      AND s.latitude BETWEEN 45 AND 47 AND s.longitude BETWEEN 13 AND 16
      AND p.fuel_type = 'dizel'
      AND RANDOM() < 0.8  -- 80%% random sample
  )
  SELECT 
    date, station_uuid, price, latitude, longitude, brand, nearby_station_1km, country,
    ROW_NUMBER() OVER (ORDER BY RANDOM()) as global_row_num,
    COUNT(*) OVER () as total_count
  FROM all_diesel_data
  ",
  format(date_start, "%Y-%m-%d %H:%M:%S"), format(date_end, "%Y-%m-%d %H:%M:%S"),
  format(date_start, "%Y-%m-%d %H:%M:%S"), format(date_end, "%Y-%m-%d %H:%M:%S")
)

# Get total count first (without loading all data)
count_query <- "
SELECT COUNT(*) as total_count
FROM (
  SELECT 1 FROM ger.german_prices_wide p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
  WHERE p.date >= TIMESTAMP '%s' AND p.date <= TIMESTAMP '%s'
    AND p.diesel > 0 AND p.diesel < 10
    AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
  
  UNION ALL
  
  SELECT 1 FROM aut.austrian_prices p
  JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Austria'
  WHERE p.date >= TIMESTAMP '%s' AND p.date <= TIMESTAMP '%s'
    AND p.price > 0 AND p.price < 10
    AND s.latitude BETWEEN 46 AND 49 AND s.longitude BETWEEN 9 AND 17
    AND p.fuel_type IN ('DIESEL', 'diesel', 'fuel_d')
  
  UNION ALL
  
  SELECT 1 FROM slo.slovenian_prices p
  JOIN stations_db.stations s ON CAST(p.station_uuid AS DOUBLE) = CAST(s.station_uuid AS DOUBLE) AND s.country = 'Slovenia'
  WHERE p.price > 0 AND p.price < 10
    AND s.latitude BETWEEN 45 AND 47 AND s.longitude BETWEEN 13 AND 16
    AND p.fuel_type = 'dizel'
    AND RANDOM() < 0.8
) combined_data
"

total_count <- dbGetQuery(con, sprintf(count_query, 
  format(date_start, "%Y-%m-%d %H:%M:%S"), format(date_end, "%Y-%m-%d %H:%M:%S"),
  format(date_start, "%Y-%m-%d %H:%M:%S"), format(date_end, "%Y-%m-%d %H:%M:%S")
))$total_count

if (total_count == 0) stop("No training data found in the specified window.")

cat("Total multi-country diesel records:", total_count, "\n")
cat("Countries included: Germany, Austria, Slovenia\n")

# Train/validation split (random 80/20 with proper holdout)
train_fraction <- 0.8
train_size <- floor(total_count * train_fraction)
test_size <- total_count - train_size

cat("Training records:", train_size, "\n")
cat("Validation records:", test_size, "\n")

# Compute normalization statistics on a sample of training data
cat("Computing normalization statistics on training sample...\n")
sample_query <- sprintf(
  "
  WITH all_diesel_data AS (
    SELECT 
      s.latitude,
      s.longitude,
      s.nearby_station_1km
    FROM ger.german_prices_wide p
    JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
    WHERE p.date >= TIMESTAMP '%s' AND p.date <= TIMESTAMP '%s'
      AND p.diesel > 0 AND p.diesel < 10
      AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
      AND RANDOM() < 0.01  -- 1%% sample for stats
    
    UNION ALL
    
    SELECT 
      s.latitude,
      s.longitude,
      s.nearby_station_1km
    FROM aut.austrian_prices p
    JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Austria'
    WHERE p.date >= TIMESTAMP '%s' AND p.date <= TIMESTAMP '%s'
      AND p.price > 0 AND p.price < 10
      AND s.latitude BETWEEN 46 AND 49 AND s.longitude BETWEEN 9 AND 17
      AND p.fuel_type IN ('DIESEL', 'diesel', 'fuel_d')
      AND RANDOM() < 0.1  -- 10%% sample for stats
    
    UNION ALL
    
    SELECT 
      s.latitude,
      s.longitude,
      s.nearby_station_1km
    FROM slo.slovenian_prices p
    JOIN stations_db.stations s ON CAST(p.station_uuid AS DOUBLE) = CAST(s.station_uuid AS DOUBLE) AND s.country = 'Slovenia'
    WHERE p.price > 0 AND p.price < 10
      AND s.latitude BETWEEN 45 AND 47 AND s.longitude BETWEEN 13 AND 16
      AND p.fuel_type = 'dizel'
      AND RANDOM() < 0.8
  )
  SELECT 
    AVG(latitude) as latitude_mean,
    AVG(longitude) as longitude_mean,
    AVG(nearby_station_1km) as nearby_stations_1km_mean,
    STDDEV(nearby_station_1km) as nearby_stations_1km_sd
  FROM all_diesel_data
  ",
  format(date_start, "%Y-%m-%d %H:%M:%S"), format(date_end, "%Y-%m-%d %H:%M:%S"),
  format(date_start, "%Y-%m-%d %H:%M:%S"), format(date_end, "%Y-%m-%d %H:%M:%S")
)

norm_stats <- dbGetQuery(con, sample_query)
cat("Normalization stats computed\n")

# Streaming training loop with Adam + L2 + Early stopping ---------------------

# Initialize weights and Adam moments (we'll get feature count from first chunk)
weights <- NULL
m <- NULL
v <- NULL

# Chunking helpers
num_chunks <- ceiling(train_size / chunk_size)
best_val_mse <- Inf
best_weights <- NULL
epochs_no_improve <- 0
global_step <- 0

# Tracking metrics
training_metrics <- list(
  chunk_mse = c(),
  chunk_weight_decay_loss = c(),
  chunk_total_loss = c(),
  epoch_mse = c(),
  epoch_weight_decay_loss = c(),
  epoch_total_loss = c(),
  weight_norms = c(),
  gradient_norms = c()
)

cat(sprintf("Training with Adam (lr=%.4g), L2=%.2g, epochs=%d, chunks=%d\n",
            learning_rate, weight_decay, num_epochs, num_chunks))

# Create preprocessed training table with deterministic random ordering
cat("Creating preprocessed training table...\n")
create_table_query <- sprintf(
  "
  CREATE OR REPLACE TABLE temp_training_data AS
  WITH all_diesel_data AS (
    SELECT 
      p.date,
      p.station_uuid,
      p.diesel AS price,
      s.latitude,
      s.longitude,
      s.station_brand AS brand,
      s.nearby_station_1km
    FROM ger.german_prices_wide p
    JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Germany'
    WHERE p.date >= TIMESTAMP '%s' AND p.date <= TIMESTAMP '%s'
      AND p.diesel > 0 AND p.diesel < 10
      AND s.latitude BETWEEN 47 AND 55 AND s.longitude BETWEEN 5 AND 15
    
    UNION ALL
    
    SELECT 
      p.date,
      p.station_uuid,
      p.price,
      s.latitude,
      s.longitude,
      s.station_brand AS brand,
      s.nearby_station_1km
    FROM aut.austrian_prices p
    JOIN stations_db.stations s ON p.station_uuid = s.station_uuid AND s.country = 'Austria'
    WHERE p.date >= TIMESTAMP '%s' AND p.date <= TIMESTAMP '%s'
      AND p.price > 0 AND p.price < 10
      AND s.latitude BETWEEN 46 AND 49 AND s.longitude BETWEEN 9 AND 17
      AND p.fuel_type IN ('DIESEL', 'diesel', 'fuel_d')
    
    UNION ALL
    
    SELECT 
      p.date,
      p.station_uuid,
      p.price,
      s.latitude,
      s.longitude,
      s.station_brand AS brand,
      s.nearby_station_1km
    FROM slo.slovenian_prices p
    JOIN stations_db.stations s ON CAST(p.station_uuid AS DOUBLE) = CAST(s.station_uuid AS DOUBLE) AND s.country = 'Slovenia'
    WHERE p.price > 0 AND p.price < 10
      AND s.latitude BETWEEN 45 AND 47 AND s.longitude BETWEEN 13 AND 16
      AND p.fuel_type = 'dizel'
      AND RANDOM() < 0.8
  )
  SELECT 
    date, station_uuid, price, latitude, longitude, brand, nearby_station_1km,
    ROW_NUMBER() OVER (ORDER BY RANDOM()) as row_id
  FROM all_diesel_data
  ",
  format(date_start, "%Y-%m-%d %H:%M:%S"), format(date_end, "%Y-%m-%d %H:%M:%S"),
  format(date_start, "%Y-%m-%d %H:%M:%S"), format(date_end, "%Y-%m-%d %H:%M:%S")
)

dbExecute(con, create_table_query)
cat("Preprocessed training table created\n")

# Now stream chunks with NO overlap
streaming_query <- "
SELECT date, station_uuid, price, latitude, longitude, brand, nearby_station_1km
FROM temp_training_data 
WHERE row_id BETWEEN ? AND ?
"

for (epoch in 1:num_epochs) {
  cat(sprintf("\n=== Epoch %d/%d ===\n", epoch, num_epochs))
  epoch_start_time <- Sys.time()
  epoch_total_mse <- 0
  epoch_chunks <- 0
  
  # Process training data in chunks (NO overlap)
  for (chunk_idx in 1:num_chunks) {
    start_row <- (chunk_idx - 1) * chunk_size + 1
    end_row <- min(chunk_idx * chunk_size, train_size)
    
    chunk <- dbGetQuery(con, streaming_query, params = list(start_row, end_row))
    if (nrow(chunk) == 0) next
    
    # Initialize weights on first chunk
    if (is.null(weights)) {
      sample_features <- create_features(chunk[1, ])
      sample_features <- normalize_features(sample_features, norm_stats)
      X_sample <- build_model_matrix(sample_features)
      weights <- runif(ncol(X_sample), -0.01, 0.01)
      m <- rep(0, length(weights))
      v <- rep(0, length(weights))
      best_weights <- weights
      cat("Initialized weights vector with", ncol(X_sample), "features\n")
    }
    
    # Process chunk
    chunk <- create_features(chunk)
    chunk <- normalize_features(chunk, norm_stats)
    X <- build_model_matrix(chunk)
    y <- chunk$price
    if (nrow(X) == 0) next
    
    # Predictions and gradients
    y_hat <- as.vector(X %*% weights)
    error <- y - y_hat
    mse_loss <- mean(error^2)
    weight_decay_loss <- weight_decay * sum(weights^2)
    total_loss <- mse_loss + weight_decay_loss
    
    grad_mse <- -2 * colMeans(X * error)  # d/dw MSE part
    grad_l2 <- 2 * weight_decay * weights # L2 gradient
    grad <- grad_mse + grad_l2

    # Track metrics
    training_metrics$chunk_mse <- c(training_metrics$chunk_mse, mse_loss)
    training_metrics$chunk_weight_decay_loss <- c(training_metrics$chunk_weight_decay_loss, weight_decay_loss)
    training_metrics$chunk_total_loss <- c(training_metrics$chunk_total_loss, total_loss)
    training_metrics$weight_norms <- c(training_metrics$weight_norms, sqrt(sum(weights^2)))
    training_metrics$gradient_norms <- c(training_metrics$gradient_norms, sqrt(sum(grad^2)))

    # Adam update
    global_step <- global_step + 1
    m <- beta1 * m + (1 - beta1) * grad
    v <- beta2 * v + (1 - beta2) * (grad^2)
    m_hat <- m / (1 - beta1^global_step)
    v_hat <- v / (1 - beta2^global_step)
    weights <- weights - learning_rate * m_hat / (sqrt(v_hat) + epsilon)
    
    epoch_total_mse <- epoch_total_mse + mse_loss
    epoch_chunks <- epoch_chunks + 1
    
    if (chunk_idx %% 25 == 0 || chunk_idx == 1) {
      cat("Epoch", epoch, "- Chunk", chunk_idx, "/", num_chunks, 
          "- Records:", nrow(chunk), 
          "- MSE:", round(mse_loss, 6), "\n")
    }
  }
  
  # Epoch summary
  epoch_avg_mse <- epoch_total_mse / epoch_chunks
  training_metrics$epoch_mse <- c(training_metrics$epoch_mse, epoch_avg_mse)
  training_metrics$epoch_weight_decay_loss <- c(training_metrics$epoch_weight_decay_loss, mean(tail(training_metrics$chunk_weight_decay_loss, epoch_chunks)))
  training_metrics$epoch_total_loss <- c(training_metrics$epoch_total_loss, mean(tail(training_metrics$chunk_total_loss, epoch_chunks)))
  
  cat(sprintf("Epoch %d - Avg MSE: %.6f, Chunks: %d\n", epoch, epoch_avg_mse, epoch_chunks))
  
  # Simple validation (sample-based for efficiency)
  if (epoch %% 2 == 0) {  # Validate every 2 epochs
    val_sample_query <- paste0(streaming_query, " LIMIT 10000 OFFSET ", train_size)
    val_sample <- dbGetQuery(con, val_sample_query)
    if (nrow(val_sample) > 0) {
      val_sample <- create_features(val_sample)
      val_sample <- normalize_features(val_sample, norm_stats)
      X_val <- build_model_matrix(val_sample)
      y_val <- val_sample$price
      val_pred <- as.vector(X_val %*% weights)
      val_mse <- compute_mse(y_val, val_pred)
      
      cat(sprintf("Validation MSE: %.6f\n", val_mse))
      
      if (best_val_mse - val_mse > min_delta) {
        best_val_mse <- val_mse
        best_weights <- weights
        epochs_no_improve <- 0
      } else {
        epochs_no_improve <- epochs_no_improve + 1
        if (epochs_no_improve >= patience) {
          cat(sprintf("Early stopping at epoch %d (no improvement %d epochs)\n", epoch, epochs_no_improve))
          break
        }
      }
    }
  }
}

# Persist model ----------------------------------------------------------------

model <- list(
  algorithm = "unified_linear_regression_adam_l2",
  weights = best_weights,
  norm_stats = norm_stats,
  learning_rate = learning_rate,
  weight_decay = weight_decay,
  beta1 = beta1,
  beta2 = beta2,
  epsilon = epsilon,
  num_epochs = num_epochs,
  chunk_size = chunk_size,
  best_val_mse = best_val_mse,
  date_start = date_start,
  date_end = date_end,
  countries = unique(all_data$country),
  total_records = total_records,
  train_records = nrow(train_df),
  val_records = nrow(val_df),
  brand_categories = c("International", "National", "Regional", "Unknown"),
  fuel_type_mapping = "diesel (mapped from DIESEL, diesel, fuel_d, dizel)",
  training_metrics = training_metrics,
  final_weight_norm = sqrt(sum(best_weights^2)),
  final_gradient_norm = sqrt(sum((best_weights - weights)^2))
)

dir.create("analysis/linear_regression/results", recursive = TRUE, showWarnings = FALSE)
saveRDS(model, file = "analysis/linear_regression/results/unified_diesel_linear_regression_adam_l2.rds")
cat("Saved unified multi-country model to analysis/linear_regression/results/unified_diesel_linear_regression_adam_l2.rds\n")

# Create training visualizations
cat("\n=== CREATING TRAINING VISUALIZATIONS ===\n")
plots <- create_training_plots(training_metrics, "Unified Multi-Country Diesel Model")

# Print training summary
cat("\n=== TRAINING SUMMARY ===\n")
cat("Final validation MSE:", round(best_val_mse, 6), "\n")
cat("Final weight norm:", round(sqrt(sum(best_weights^2)), 6), "\n")
cat("Total chunks processed:", length(training_metrics$chunk_mse), "\n")
cat("Total epochs completed:", length(training_metrics$epoch_mse), "\n")
cat("Countries included:", paste(unique(all_data$country), collapse = ", "), "\n")
cat("Total training records:", nrow(train_df), "\n")
cat("Total validation records:", nrow(val_df), "\n")

# Disconnect from database
dbDisconnect(con)



#!/usr/bin/env Rscript

# Optimized Random Forest on German Diesel Data
# Hyperparameter tuning and optimization

library(DBI)
library(duckdb)
library(ranger)
library(ggplot2)
library(dplyr)
library(caret)

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("../../")
}

# Database path
db_path <- "databases/german_fuel_data.duckdb"

# Connect to database
con <- dbConnect(duckdb(), db_path, read_only = TRUE)

cat("Optimized Random Forest on German Diesel Data\n")
cat("============================================\n\n")

# Load training data
cat("Loading training data...\n")
train_data <- dbGetQuery(con, "
  SELECT 
    price,
    hour_sin, hour_cos, dow_sin, dow_cos, month_sin, month_cos,
    latitude_centered, longitude_centered, nearby_stations_1km_norm,
    brand_category
  FROM diesel_train_test_features
  WHERE split_type = 'train'
  LIMIT 500000
")

# Load validation data for tuning
val_data <- dbGetQuery(con, "
  SELECT 
    price,
    hour_sin, hour_cos, dow_sin, dow_cos, month_sin, month_cos,
    latitude_centered, longitude_centered, nearby_stations_1km_norm,
    brand_category
  FROM diesel_train_test_features
  WHERE split_type = 'test'
  LIMIT 50000
")

# Load test data
test_data <- dbGetQuery(con, "
  SELECT 
    price,
    hour_sin, hour_cos, dow_sin, dow_cos, month_sin, month_cos,
    latitude_centered, longitude_centered, nearby_stations_1km_norm,
    brand_category
  FROM diesel_train_test_features
  WHERE split_type = 'test'
  LIMIT 100000
")

cat("Training samples:", nrow(train_data), "\n")
cat("Validation samples:", nrow(val_data), "\n")
cat("Test samples:", nrow(test_data), "\n\n")

# 1. Baseline Random Forest (from previous test)
cat("Training Baseline Random Forest...\n")
start_time <- Sys.time()

baseline_rf <- ranger(
  price ~ .,
  data = train_data,
  num.trees = 100,
  mtry = 3,
  min.node.size = 5,
  num.threads = 4
)

baseline_time <- Sys.time() - start_time
baseline_pred <- predict(baseline_rf, test_data)$predictions
baseline_rmse <- sqrt(mean((test_data$price - baseline_pred)^2))

cat("Baseline Random Forest RMSE:", round(baseline_rmse, 4), "\n\n")

# 2. Optimized Random Forest with more trees
cat("Training Optimized Random Forest (500 trees)...\n")
start_time <- Sys.time()

optimized_rf <- ranger(
  price ~ .,
  data = train_data,
  num.trees = 500,  # More trees for better performance
  mtry = 4,         # Try more variables per split
  min.node.size = 3, # Smaller nodes for more detail
  max.depth = 20,   # Control depth to prevent overfitting
  sample.fraction = 0.8,  # Bootstrap sampling
  num.threads = 4,
  importance = "impurity"
)

optimized_time <- Sys.time() - start_time
optimized_pred <- predict(optimized_rf, test_data)$predictions
optimized_rmse <- sqrt(mean((test_data$price - optimized_pred)^2))

cat("Optimized Random Forest RMSE:", round(optimized_rmse, 4), "\n\n")

# 3. Hyperparameter tuning with caret
cat("Performing hyperparameter tuning...\n")

# Define tuning grid
tune_grid <- expand.grid(
  mtry = c(2, 3, 4, 5, 6),
  splitrule = "variance",
  min.node.size = c(1, 3, 5, 10)
)

# Create control for tuning
ctrl <- trainControl(
  method = "cv",
  number = 3,  # 3-fold CV for speed
  verboseIter = TRUE
)

# Use smaller sample for tuning
tune_sample <- train_data[sample(nrow(train_data), 100000), ]

start_time <- Sys.time()

tuned_rf <- train(
  price ~ .,
  data = tune_sample,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = tune_grid,
  num.trees = 200,  # Moderate number for tuning
  importance = "impurity"
)

tune_time <- Sys.time() - start_time

# Get best parameters
best_mtry <- tuned_rf$bestTune$mtry
best_min_node <- tuned_rf$bestTune$min.node.size

cat("Best parameters from tuning:\n")
cat("  mtry:", best_mtry, "\n")
cat("  min.node.size:", best_min_node, "\n")
cat("  Best CV RMSE:", round(min(tuned_rf$results$RMSE), 4), "\n\n")

# 4. Final model with best parameters
cat("Training Final Tuned Random Forest...\n")
start_time <- Sys.time()

final_rf <- ranger(
  price ~ .,
  data = train_data,
  num.trees = 500,
  mtry = best_mtry,
  min.node.size = best_min_node,
  max.depth = 25,
  sample.fraction = 0.8,
  num.threads = 4,
  importance = "impurity"
)

final_time <- Sys.time() - start_time
final_pred <- predict(final_rf, test_data)$predictions
final_rmse <- sqrt(mean((test_data$price - final_pred)^2))

# Calculate other metrics
final_mae <- mean(abs(test_data$price - final_pred))
final_r2 <- 1 - (sum((test_data$price - final_pred)^2) / sum((test_data$price - mean(test_data$price))^2))

cat("Final Tuned Random Forest Results:\n")
cat("  RMSE:", round(final_rmse, 4), "\n")
cat("  MAE:", round(final_mae, 4), "\n")
cat("  R²:", round(final_r2, 4), "\n")
cat("  Training time:", round(as.numeric(final_time, units = "mins"), 2), "minutes\n\n")

# Performance comparison
cat("Random Forest Performance Comparison:\n")
cat("====================================\n")
cat("Model                    | RMSE   | Improvement\n")
cat("------------------------|--------|-------------\n")
cat("Baseline (100 trees)     |", sprintf("%6.4f", baseline_rmse), "| Baseline\n")
cat("Optimized (500 trees)    |", sprintf("%6.4f", optimized_rmse), "|", sprintf("%+6.2f%%", (baseline_rmse-optimized_rmse)/baseline_rmse*100), "\n")
cat("Tuned (best params)      |", sprintf("%6.4f", final_rmse), "|", sprintf("%+6.2f%%", (baseline_rmse-final_rmse)/baseline_rmse*100), "\n")

# Feature importance from final model
cat("\nFeature Importance (Final Model):\n")
importance_df <- data.frame(
  Feature = names(final_rf$variable.importance),
  Importance = final_rf$variable.importance
) %>%
  arrange(desc(Importance))

print(importance_df)

# Create comparison plot
comparison_data <- data.frame(
  Actual = test_data$price,
  Baseline = baseline_pred,
  Optimized = optimized_pred,
  Final_Tuned = final_pred
) %>%
  sample_n(min(5000, nrow(.)))

p1 <- ggplot(comparison_data, aes(x = Actual)) +
  geom_point(aes(y = Baseline), alpha = 0.3, color = "blue", size = 0.5) +
  geom_point(aes(y = Optimized), alpha = 0.3, color = "red", size = 0.5) +
  geom_point(aes(y = Final_Tuned), alpha = 0.3, color = "green", size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Random Forest Optimization (Blue=Baseline, Red=Optimized, Green=Final)",
       x = "Actual Price", y = "Predicted Price") +
  theme_minimal()

# Save results
results_dir <- "results"
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
}

ggsave(file.path(results_dir, "optimized_random_forest_comparison.png"), p1, 
       width = 12, height = 8, dpi = 300)

# Save detailed results
optimization_results <- data.frame(
  Model = c("Baseline", "Optimized", "Final Tuned"),
  Trees = c(100, 500, 500),
  RMSE = c(baseline_rmse, optimized_rmse, final_rmse),
  Training_Time_Minutes = c(
    as.numeric(baseline_time, units = "mins"),
    as.numeric(optimized_time, units = "mins"),
    as.numeric(final_time, units = "mins")
  ),
  Improvement = c(0, 
                  (baseline_rmse-optimized_rmse)/baseline_rmse*100,
                  (baseline_rmse-final_rmse)/baseline_rmse*100)
)

write.csv(optimization_results, file.path(results_dir, "random_forest_optimization_results.csv"), 
          row.names = FALSE)

write.csv(importance_df, file.path(results_dir, "optimized_feature_importance.csv"), 
          row.names = FALSE)

cat("\nResults saved to results/ directory\n")

# Disconnect from database
dbDisconnect(con, shutdown = TRUE)

cat("Random Forest optimization completed\n")

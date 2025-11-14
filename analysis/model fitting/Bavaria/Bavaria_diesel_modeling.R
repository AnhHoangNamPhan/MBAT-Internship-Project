#!/usr/bin/env Rscript

# Bavaria Diesel Fuel Price Modeling (postcode prefix '8')

if (.Platform$OS.type == "unix") {
  Sys.setenv(R_MAX_VSIZE = "64GB")           
  Sys.setenv(R_MAX_NUM_DLLS = "500")         
  Sys.setenv(R_MAX_MEM_SIZE = "64GB")        
  options(mc.cores = 8)                      
  options(stringsAsFactors = FALSE)          
}

library(DBI)
library(duckdb)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

cat("=== BAVARIA DIESEL FUEL PRICE MODELING ===\n")

# Database paths
stations_db <- "/Users/alexphan/Desktop/MBAT-Internship-Project/databases/stations.duckdb"
german_db <- "/Users/alexphan/Desktop/MBAT-Internship-Project/databases/german_fuel.duckdb"

# Connect to databases with performance optimizations
con_stations <- dbConnect(duckdb(), stations_db, read_only = TRUE)
con_germany <- dbConnect(duckdb(), german_db, read_only = TRUE)

# Optimize DuckDB performance
dbExecute(con_stations, "SET memory_limit='32GB'")
dbExecute(con_stations, "SET threads=8")
dbExecute(con_germany, "SET memory_limit='32GB'")
dbExecute(con_germany, "SET threads=8")

cat("✓ Connected to databases\n")

# =============================================================================
# STEP 1: LOAD STATION METADATA
# =============================================================================
cat("\nSTEP 1: Loading station metadata...\n")

stations_metadata <- dbGetQuery(con_stations, "
  SELECT 
    station_uuid, 
    station_brand,
    latitude,
    longitude,
    zip_code,
    nearby_station_1km
  FROM stations 
  WHERE latitude IS NOT NULL AND longitude IS NOT NULL
    AND latitude != 0 AND longitude != 0
") %>%
  mutate(
    station_uuid = as.character(station_uuid)
  )

cat(sprintf("✓ Loaded %d stations with complete metadata\n", nrow(stations_metadata)))

# =============================================================================
# STEP 2: LOAD DIESEL PRICE DATA FROM BAVARIA (POSTCODE PREFIX '8')
# =============================================================================
cat("\nSTEP 2: Loading diesel price data from Bavaria (postcode prefix '8')...\n")

# Get stations located in Bavaria (postcode prefix '8')
bavaria_stations <- dbGetQuery(con_stations, "
  SELECT station_uuid
  FROM stations 
  WHERE country = 'Germany' 
    AND substr(zip_code, 1, 1) IN ('8')
") %>%
  mutate(station_uuid = as.character(station_uuid))

cat(sprintf(" Bavaria stations (postcode prefix '8'): %d\n", nrow(bavaria_stations)))

# Get prices only for Bavaria stations using the station list
station_list <- paste0("'", bavaria_stations$station_uuid, "'", collapse = ",")

german_prices <- dbGetQuery(con_germany, sprintf("
  SELECT
    DATE_TRUNC('hour', date) AS date,
    station_uuid,
    arg_max(diesel, date) AS price
  FROM german_prices_wide
  WHERE diesel IS NOT NULL AND diesel > 0 AND diesel < 10
    AND station_uuid IN (%s)
  GROUP BY DATE_TRUNC('hour', date), station_uuid
", station_list)) %>%
  mutate(
    station_uuid = as.character(station_uuid),
    date = as.POSIXct(date, tz = "UTC"),
    country = "Germany",
    fuel_type = "diesel"
  )

cat(sprintf("✓ Diesel price records: %d\n", nrow(german_prices)))
cat(sprintf("✓ Data size: %s\n", format(object.size(german_prices), "MB")))

# =============================================================================
# STEP 3: JOIN PRICES WITH STATION METADATA
# =============================================================================
cat("\nSTEP 3: Joining price data with station metadata...\n")

unified_data <- german_prices %>%
  left_join(stations_metadata, by = "station_uuid") %>%
  filter(!is.na(station_brand)) %>%
  group_by(station_uuid) %>%
  filter(n() >= 2) %>%
  ungroup()

cat(sprintf("✓ Records after joining and filtering: %d\n", nrow(unified_data)))
cat(sprintf("✓ Unified data size: %s\n", format(object.size(unified_data), "MB")))

# Clean up intermediate objects to free memory
rm(german_prices)
gc()  # Force garbage collection

# =============================================================================
# STEP 4: BRAND CATEGORIZATION
# =============================================================================

# Count stations per brand and categorize
brand_counts <- unified_data %>%
  filter(!is.na(station_brand) & station_brand != "") %>%
  count(station_brand, name = "n_stations")

top_brands <- brand_counts %>% 
  slice_max(n_stations, n = 10) %>% 
  pull(station_brand)

cat(sprintf("✓ Top 10 brands (get their own categories): %s\n", paste(top_brands, collapse = ", ")))

# Categorize brand 
unified_data <- unified_data %>%
  left_join(brand_counts, by = "station_brand") %>%
  mutate(
    brand_category = case_when(
      station_brand %in% top_brands ~ station_brand,
      n_stations >= 100 ~ "large",
      n_stations >= 50 ~ "medium",
      n_stations >= 10 ~ "small",
      TRUE ~ "tiny"
    ),
    brand_category = as.factor(brand_category)
  )

cat("✓ Brand categories:\n")
print(table(unified_data$brand_category))

# =============================================================================
# STEP 5: ADD TEMPORAL LAGS 
# =============================================================================
cat("\nSTEP 5: Adding temporal lags to full dataset...\n")

# Add temporal lags to the FULL dataset before splitting
# This ensures consistent lag patterns between train and test
unified_data <- unified_data %>%
  arrange(station_uuid, date) %>%
  group_by(station_uuid) %>%
  mutate(
    diesel_lag_1h = lag(price, 1),   # 1 period lag
    diesel_lag_1d = lag(price, 24),  # 24 hours = 1 day
    diesel_lag_1w = lag(price, 168)  # 168 hours = 1 week
  ) %>%
  ungroup()

cat("✓ Added temporal lag variables to full dataset\n")

# Remove redundant columns to save memory
cat("  Removing redundant columns to save memory...\n")
unified_data <- unified_data %>%
  select(-station_uuid, -country, -fuel_type, -station_brand)

# =============================================================================
# STEP 6: TRAIN/VALIDATION/TEST SPLIT
# =============================================================================
cat("\nSTEP 6: Creating train/validation/test split...\n")

# Define training period: Aug 2024 to July 2025
train_start_date <- as.Date("2024-08-01")
train_end_date <- as.Date("2025-07-31")

# Get all data from training period
train_period_data <- unified_data %>%
  filter(date >= train_start_date & date <= train_end_date)


# Split training period into 80% train, 20% validation
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(train_period_data), size = floor(0.8 * nrow(train_period_data)))
train_data <- train_period_data[train_indices, ]
validation_data <- train_period_data[-train_indices, ]

# Hold out test data: all data after training period
test_data <- unified_data %>%
  filter(date > train_end_date)

# Clean up intermediate data
rm(train_period_data, train_indices)

# Remove original unified_data to free memory
rm(unified_data)
gc()

cat(sprintf("✓ Training set: %d records (80%% of Aug 2024 - July 2025)\n", nrow(train_data)))
cat(sprintf("✓ Validation set: %d records (20%% of Aug 2024 - July 2025)\n", nrow(validation_data)))
cat(sprintf("✓ Test set: %d records (after July 2025)\n", nrow(test_data)))

# RMSE function
rmse <- function(predicted, actual) {
  sqrt(mean((predicted - actual)^2, na.rm = TRUE))
}

# Helper function to align factor levels in validation data and test data to match with training data
align_factor_levels <- function(model, newdata) {
  xlevels <- model$xlevels
  if (!is.null(xlevels)) {
    for (var_name in names(xlevels)) {
      if (var_name %in% names(newdata)) {
        # Get the levels the model saw during training
        model_levels <- xlevels[[var_name]]
        # Align newdata to only use those levels
        newdata[[var_name]] <- factor(as.character(newdata[[var_name]]), levels = model_levels)
        # Drop rows with levels not seen during training
        newdata <- newdata[!is.na(newdata[[var_name]]), , drop = FALSE]
      }
    }
  }
  return(newdata)
}

# Define results directory
results_dir <- "results"
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
  cat(sprintf("Created results directory: %s\n", results_dir))
}
save_model_files <- FALSE  # set TRUE to save full model objects (.json/.h5)

# =============================================================================
# STEP 7: LINEAR REGRESSION
# =============================================================================
cat("\nSTEP 7: Linear Regression models fitting ...\n")

# Model 1: Basic time features
cat("\n--- Model 1: Basic time features ---\n")
formula_1 <- price ~
  month(date, label = TRUE) +
  wday(date, label = TRUE) +
  hour(date) |> factor() +
  substr(zip_code, 2, 2) |> factor() # Region level 2

cat("Fitting Model 1...\n")
mdl_1 <- lm(formula_1, data = train_data)
cat(sprintf("Model 1 object size: %s\n", format(object.size(mdl_1), "MB")))

# Store the summary and results
mdl_1_sm <- summary(mdl_1)
mdl_1_bic <- BIC(mdl_1)  # Information criteria 

# In-sample validation error
mdl_1_val <- predict(mdl_1, newdata = validation_data)
mdl_1_val_rmse <- rmse(mdl_1_val, validation_data$price)

# Out-of-sample test error
mdl_1_oos <- predict(mdl_1, newdata = test_data)
mdl_1_oos_rmse <- rmse(mdl_1_oos, test_data$price)

cat(sprintf("Model 1 Validation RMSE: %.4f\n", mdl_1_val_rmse))
cat(sprintf("Model 1 Test RMSE: %.4f\n", mdl_1_oos_rmse))
cat(sprintf("Model 1 R²: %.4f\n", mdl_1_sm$r.squared))
cat(sprintf("Model 1 BIC: %.2f\n", mdl_1_bic))

# Store results before cleanup 
mdl_1_bic_value <- mdl_1_bic
mdl_1_val_rmse_value <- mdl_1_val_rmse
mdl_1_oos_rmse_value <- mdl_1_oos_rmse
mdl_1_r2_value <- mdl_1_sm$r.squared
mdl_1_adj_r2_value <- mdl_1_sm$adj.r.squared  # Adjusted R²
mdl_1_sigma_value <- mdl_1_sm$sigma  # Residual standard error
mdl_1_fstat_value <- mdl_1_sm$fstatistic[1]  # F-statistic
mdl_1_coef_value <- coef(mdl_1)  # Coefficients
mdl_1_coef_table <- mdl_1_sm$coefficients  # Full coefficient table with p-values

# Save Model 1 results externally 
cat("  Saving Model 1 results externally...\n")

# Save extracted metrics (essential statistics only) 
mdl_1_results <- list(
  BIC = mdl_1_bic_value,
  RMSE_Test = mdl_1_oos_rmse_value,
  RMSE_Validation = mdl_1_val_rmse_value,
  R2 = mdl_1_r2_value,
  Adj_R2 = mdl_1_adj_r2_value,
  Sigma = mdl_1_sigma_value,
  F_Statistic = mdl_1_fstat_value,
  Coefficients = mdl_1_coef_value,
  Coefficient_Table = mdl_1_coef_table
)
saveRDS(mdl_1_results, file.path(results_dir, "bavaria_diesel_mdl1_metrics.rds"))

cat(sprintf(" Model 1 metrics saved to: %s\n", file.path(results_dir, "bavaria_diesel_mdl1_metrics.rds")))

# Clean up Model 1 immediately (keep _value variables for comparison later)
cat(" Cleaning up Model 1 to free memory...\n")
rm(mdl_1, mdl_1_sm, mdl_1_val, mdl_1_oos, mdl_1_bic, mdl_1_val_rmse, mdl_1_oos_rmse, mdl_1_results,
   mdl_1_r2_value, mdl_1_adj_r2_value,
   mdl_1_sigma_value, mdl_1_fstat_value, mdl_1_coef_value, mdl_1_coef_table)
gc(verbose = TRUE)

# Model 2: Add regional and brand features
cat("\n--- Model 2: + Regional and brand features ---\n")

formula_2 <- price ~
  month(date, label = TRUE) |> as.character() +
  wday(date, label = TRUE) |> as.character() +
  hour(date) |> factor() +
  substr(zip_code, 2, 2) |> factor() +  
  longitude + latitude +
  brand_category + n_stations +  # Brand effect
  nearby_station_1km   # Local competition effect

cat("Fitting Model 2...\n")
mdl_2 <- lm(formula_2, data = train_data)
cat(sprintf("Model 2 object size: %s\n", format(object.size(mdl_2), "MB")))

# Store the summary and results
mdl_2_sm <- summary(mdl_2)
mdl_2_bic <- BIC(mdl_2)  # Information criteria

# In-sample validation error (align factor levels before prediction)
validation_data_aligned_2 <- align_factor_levels(mdl_2, validation_data)
mdl_2_val <- predict(mdl_2, newdata = validation_data_aligned_2)
mdl_2_val_rmse <- rmse(mdl_2_val, validation_data_aligned_2$price)

# Out-of-sample test error (align factor levels before prediction)
test_data_aligned_2 <- align_factor_levels(mdl_2, test_data)
mdl_2_oos <- predict(mdl_2, newdata = test_data_aligned_2)
mdl_2_oos_rmse <- rmse(mdl_2_oos, test_data_aligned_2$price)

cat(sprintf("Model 2 Validation RMSE: %.4f\n", mdl_2_val_rmse))
cat(sprintf("Model 2 Test RMSE: %.4f\n", mdl_2_oos_rmse))
cat(sprintf("Model 2 R²: %.4f\n", mdl_2_sm$r.squared))
cat(sprintf("Model 2 BIC: %.2f\n", mdl_2_bic))

# Store results before cleanup 
mdl_2_bic_value <- mdl_2_bic
mdl_2_val_rmse_value <- mdl_2_val_rmse
mdl_2_oos_rmse_value <- mdl_2_oos_rmse
mdl_2_r2_value <- mdl_2_sm$r.squared
mdl_2_adj_r2_value <- mdl_2_sm$adj.r.squared  # Adjusted R²
mdl_2_sigma_value <- mdl_2_sm$sigma  # Residual standard error
mdl_2_fstat_value <- mdl_2_sm$fstatistic[1]  # F-statistic
mdl_2_coef_value <- coef(mdl_2)  # Coefficients
mdl_2_coef_table <- mdl_2_sm$coefficients  # Full coefficient table with p-values

# Save Model 2 results externally 
cat("  Saving Model 2 results externally...\n")

# Save extracted metrics (essential statistics only) 
mdl_2_results <- list(
  BIC = mdl_2_bic_value,
  RMSE_Test = mdl_2_oos_rmse_value,
  RMSE_Validation = mdl_2_val_rmse_value,
  R2 = mdl_2_r2_value,
  Adj_R2 = mdl_2_adj_r2_value,
  Sigma = mdl_2_sigma_value,
  F_Statistic = mdl_2_fstat_value,
  Coefficients = mdl_2_coef_value,
  Coefficient_Table = mdl_2_coef_table
)
saveRDS(mdl_2_results, file.path(results_dir, "bavaria_diesel_mdl2_metrics.rds"))

cat(sprintf("    Model 2 metrics saved to: %s\n", file.path(results_dir, "bavaria_diesel_mdl2_metrics.rds")))

# Clean up Model 2 immediately (keep _value variables for comparison later)
cat("  Cleaning up Model 2 to free memory...\n")
rm(mdl_2, mdl_2_sm, mdl_2_val, mdl_2_oos, mdl_2_bic, mdl_2_val_rmse, mdl_2_oos_rmse, mdl_2_results,
   validation_data_aligned_2, test_data_aligned_2,
   mdl_2_r2_value, mdl_2_adj_r2_value,
   mdl_2_sigma_value, mdl_2_fstat_value, mdl_2_coef_value, mdl_2_coef_table)
gc(verbose = TRUE)

# Model 3: Add temporal information --- lags of the price
cat("\n--- Model 3: + Temporal lags and polynomial spatial terms ---\n")

formula_3 <- price ~
  diesel_lag_1h +
  diesel_lag_1d + 
  diesel_lag_1w +
  month(date, label = TRUE) |> as.character() +
  wday(date, label = TRUE) |> as.character() +
  hour(date) |> factor() +
  substr(zip_code, 2, 2) |> factor() +  
  poly(longitude, 3) + poly(latitude, 3) +  # Polynomial terms
  brand_category +
  n_stations + log(n_stations) + # Brand scale effect (linear and log) 
  nearby_station_1km # Local competition effect 

cat("Fitting Model 3...\n")
mdl_3 <- lm(formula_3, data = train_data)
cat(sprintf("Model 3 object size: %s\n", format(object.size(mdl_3), "MB")))

# Store the summary and results
mdl_3_sm <- summary(mdl_3)
mdl_3_bic <- BIC(mdl_3)  # Information criteria

# In-sample validation error (align factor levels before prediction, as model 3 have lag features)
validation_data_aligned_3 <- align_factor_levels(mdl_3, validation_data)
mdl_3_val <- predict(mdl_3, newdata = validation_data_aligned_3)
mdl_3_val_rmse <- rmse(mdl_3_val, validation_data_aligned_3$price)

# Out-of-sample test error (align factor levels before prediction)
test_data_aligned_3 <- align_factor_levels(mdl_3, test_data)
mdl_3_oos <- predict(mdl_3, newdata = test_data_aligned_3)
mdl_3_oos_rmse <- rmse(mdl_3_oos, test_data_aligned_3$price)

cat(sprintf("Model 3 Validation RMSE: %.4f\n", mdl_3_val_rmse))
cat(sprintf("Model 3 Test RMSE: %.4f\n", mdl_3_oos_rmse))
cat(sprintf("Model 3 R²: %.4f\n", mdl_3_sm$r.squared))
cat(sprintf("Model 3 BIC: %.2f\n", mdl_3_bic))

# Store results before cleanup 
mdl_3_bic_value <- mdl_3_bic
mdl_3_val_rmse_value <- mdl_3_val_rmse
mdl_3_oos_rmse_value <- mdl_3_oos_rmse
mdl_3_r2_value <- mdl_3_sm$r.squared
mdl_3_adj_r2_value <- mdl_3_sm$adj.r.squared  # Adjusted R²
mdl_3_sigma_value <- mdl_3_sm$sigma  # Residual standard error
mdl_3_fstat_value <- mdl_3_sm$fstatistic[1]  # F-statistic
mdl_3_coef_value <- coef(mdl_3)  # Coefficients
mdl_3_coef_table <- mdl_3_sm$coefficients  # Full coefficient table with p-values

# Save Model 3 results externally 
cat("  Saving Model 3 results externally...\n")

# Save extracted metrics (essential statistics only) 
mdl_3_results <- list(
  BIC = mdl_3_bic_value,
  RMSE_Test = mdl_3_oos_rmse_value,
  RMSE_Validation = mdl_3_val_rmse_value,
  R2 = mdl_3_r2_value,
  Adj_R2 = mdl_3_adj_r2_value,
  Sigma = mdl_3_sigma_value,
  F_Statistic = mdl_3_fstat_value,
  Coefficients = mdl_3_coef_value,
  Coefficient_Table = mdl_3_coef_table
)
saveRDS(mdl_3_results, file.path(results_dir, "bavaria_diesel_mdl3_metrics.rds"))

cat(sprintf("    Model 3 metrics saved to: %s\n", file.path(results_dir, "bavaria_diesel_mdl3_metrics.rds")))

# Clean up Model 3 immediately
cat("  Cleaning up Model 3 to free memory...\n")
rm(mdl_3, mdl_3_sm, mdl_3_val, mdl_3_oos, mdl_3_bic, mdl_3_val_rmse, mdl_3_oos_rmse, mdl_3_results,
   validation_data_aligned_3, test_data_aligned_3,
   mdl_3_r2_value, mdl_3_adj_r2_value,
   mdl_3_sigma_value, mdl_3_fstat_value, mdl_3_coef_value, mdl_3_coef_table)
gc(verbose = TRUE)

# =============================================================================
# LINEAR MODELS COMPARISON
# =============================================================================
cat("\n--- Model Comparison (Loading from saved results) ---\n")

# Load all model results from saved RDS files
cat("  Loading model results from saved files...\n")
mdl_1_res <- readRDS(file.path(results_dir, "bavaria_diesel_mdl1_metrics.rds"))
mdl_2_res <- readRDS(file.path(results_dir, "bavaria_diesel_mdl2_metrics.rds"))
mdl_3_res <- readRDS(file.path(results_dir, "bavaria_diesel_mdl3_metrics.rds"))

# Create comparison table
model_comparison <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  RMSE_Validation = c(
    mdl_1_res$RMSE_Validation,
    mdl_2_res$RMSE_Validation,
    mdl_3_res$RMSE_Validation
  ),
  RMSE_Test = c(
    mdl_1_res$RMSE_Test,
    mdl_2_res$RMSE_Test,
    mdl_3_res$RMSE_Test
  ),
  R2 = c(
    mdl_1_res$R2,
    mdl_2_res$R2,
    mdl_3_res$R2
  ),
  BIC = c(
    mdl_1_res$BIC,
    mdl_2_res$BIC,
    mdl_3_res$BIC
  ),
  stringsAsFactors = FALSE
)

print(model_comparison)

# =============================================================================
# STEP 8: GENERALIZED ADDITIVE MODEL (GAM)
# =============================================================================
cat("\nSTEP 8: Generalized Additive Model (GAM) fitting...\n")

library(mgcv)

# Precompute cyclic-friendly numeric time fields for train, validation, and test
train_data$hour_num <- as.integer(hour(train_data$date))
train_data$wday_num <- as.integer(wday(train_data$date))
train_data$month_num <- as.integer(month(train_data$date))
train_data$zip1 <- factor(substr(train_data$zip_code, 2, 2))

validation_data$hour_num <- as.integer(hour(validation_data$date))
validation_data$wday_num <- as.integer(wday(validation_data$date))
validation_data$month_num <- as.integer(month(validation_data$date))
validation_data$zip1 <- factor(substr(validation_data$zip_code, 2, 2), levels = levels(train_data$zip1))

test_data$hour_num <- as.integer(hour(test_data$date))
test_data$wday_num <- as.integer(wday(test_data$date))
test_data$month_num <- as.integer(month(test_data$date))
test_data$zip1 <- factor(substr(test_data$zip_code, 2, 2), levels = levels(train_data$zip1))

# GAM formula with smooths; use bam for big data
gam_formula <- price ~
  s(diesel_lag_1h) + 
  s(diesel_lag_1d) + 
  s(diesel_lag_1w) +
  s(month_num, bs = "cc", k = 12) +
  s(wday_num, bs = "cc", k = 7) +
  s(hour_num, bs = "cc", k = 24) +
  s(longitude, latitude, bs = "tp", k = 100) +
  brand_category +
  s(n_stations) +
  s(nearby_station_1km, k = 6) +
  zip1

cat("Fitting GAM model (bam, REML)...\n")
gam_mdl <- mgcv::bam(gam_formula, data = train_data, method = "fREML", discrete = TRUE)

# Align factor levels for validation data
validation_data_aligned_gam <- align_factor_levels(gam_mdl, validation_data)

# Predict on validation data
gam_pred_val <- predict(gam_mdl, newdata = validation_data_aligned_gam)

# Handle NA values: filter out NA predictions and prices before calculating metrics
valid_idx_val <- !is.na(gam_pred_val) & !is.na(validation_data_aligned_gam$price)
gam_pred_val_clean <- gam_pred_val[valid_idx_val]
gam_price_val_clean <- validation_data_aligned_gam$price[valid_idx_val]

cat(sprintf("  Validation: %d valid pairs out of %d total\n", sum(valid_idx_val), length(gam_pred_val)))

# Calculate validation RMSE and R²
gam_val_rmse <- sqrt(mean((gam_pred_val_clean - gam_price_val_clean)^2))
gam_val_r2 <- cor(gam_pred_val_clean, gam_price_val_clean)^2

cat(sprintf("GAM Model Validation RMSE: %.4f\n", gam_val_rmse))
cat(sprintf("GAM Model Validation R²: %.4f\n", gam_val_r2))

# Align factor levels for test data
test_data_aligned_gam <- align_factor_levels(gam_mdl, test_data)

# Predict on test data
gam_pred_test <- predict(gam_mdl, newdata = test_data_aligned_gam)

# Handle NA values: filter out NA predictions and prices before calculating metrics
valid_idx_test <- !is.na(gam_pred_test) & !is.na(test_data_aligned_gam$price)
gam_pred_test_clean <- gam_pred_test[valid_idx_test]
gam_price_test_clean <- test_data_aligned_gam$price[valid_idx_test]

cat(sprintf("  Test: %d valid pairs out of %d total\n", sum(valid_idx_test), length(gam_pred_test)))

# Calculate test RMSE and R²
gam_test_rmse <- sqrt(mean((gam_pred_test_clean - gam_price_test_clean)^2))
gam_test_r2 <- cor(gam_pred_test_clean, gam_price_test_clean)^2

cat(sprintf("GAM Model Test RMSE: %.4f\n", gam_test_rmse))
cat(sprintf("GAM Model Test R²: %.4f\n", gam_test_r2))

# Save results with both validation and test metrics
gam_results <- list(
  RMSE_Validation = gam_val_rmse,
  RMSE_Test = gam_test_rmse,
  R2_Validation = gam_val_r2,
  R2_Test = gam_test_r2,
  Summary = summary(gam_mdl)
)
saveRDS(gam_results, file.path(results_dir, "bavaria_diesel_gam_metrics.rds"))
cat(sprintf("✓ GAM model results saved to: %s\n", file.path(results_dir, "bavaria_diesel_gam_metrics.rds")))

# Save GAM smooth term plots individually

# Get number of smooth terms
n_smooths <- length(gam_mdl$smooth)
cat(sprintf("  Found %d smooth terms to plot\n", n_smooths))

# Plot each smooth term separately
for (i in 1:n_smooths) {
  term_name <- gam_mdl$smooth[[i]]$label
  cat(sprintf("  Plotting term %d: %s\n", i, term_name))
  
  plot_path <- file.path(results_dir, sprintf("bavaria_diesel_gam_term%d_%s.png", i, 
                                              gsub("[^A-Za-z0-9]", "_", term_name)))
  
  tryCatch({
    png(plot_path, width = 1000, height = 700, res = 72)
    plot(gam_mdl, select = i, shade = TRUE)  # Plot only term i
    dev.off()
    cat(sprintf("    ✓ Saved: %s\n", basename(plot_path)))
  }, error = function(e) {
    try(dev.off(), silent = TRUE)
    cat(sprintf("    ✗ ERROR plotting term %d (%s): %s\n", i, term_name, e$message))
    if (i == 7) {  # Term 7 is the spatial smooth
      cat("    → NOTE: Spatial smooth failed. Consider reducing k from 100 to 50.\n")
    }
  })
}

cat("✓ GAM smooth term plots saved individually\n")

# Clean up GAM model to free memory
cat("  Cleaning up GAM model to free memory...\n")
rm(gam_mdl, gam_pred_val, gam_pred_test, gam_val_rmse, gam_test_rmse, gam_val_r2, gam_test_r2,
   gam_results, validation_data_aligned_gam, test_data_aligned_gam,
   valid_idx_val, valid_idx_test, gam_pred_val_clean, gam_price_val_clean,
   gam_pred_test_clean, gam_price_test_clean)
gc(verbose = TRUE)

# =============================================================================
# STEP 9: XGBOOST MODELS
# =============================================================================
cat("\nSTEP 9: XGBoost models ...\n")

# Load xgboost library
  library(xgboost)

# Create terms_3 for ML models (using formula 3 from linear model, as it has the best performance)
terms_3 <- terms(formula_3, data = train_data)
cat("  ✓ Created terms_3 (locked design matrix from training data)\n")

# Convert data frames to numeric matrices
cat("  Training XGBoost Model 3 with early stopping (monitor validation RMSE)...\n")
cat("  Building design matrices from train/validation/test splits...\n")

train_matrix_3 <- model.matrix(terms_3, data = train_data)
valid_matrix_3 <- model.matrix(terms_3, data = validation_data)
test_matrix_3  <- model.matrix(terms_3, data = test_data)

# Get training column names as standard reference
train_cols <- colnames(train_matrix_3)

# Helper function to align a matrix to training columns
align_matrix_cols <- function(mat, train_cols) {
  mat_cols <- colnames(mat)
  
  # Step 1: Remove extra columns (keep only those in train_cols that exist in mat)
  mat <- mat[, intersect(mat_cols, train_cols), drop = FALSE]
  
  # Step 2: Add missing columns (with zeros)
  missing_cols <- setdiff(train_cols, colnames(mat))
  if (length(missing_cols) > 0) {
    missing_mat <- matrix(0, nrow = nrow(mat), ncol = length(missing_cols), 
                          dimnames = list(NULL, missing_cols))
    mat <- cbind(mat, missing_mat)
  }
  
  # Step 3: Reorder to match train_cols exactly
  # But add a safety check first
  if (!all(train_cols %in% colnames(mat))) {
    still_missing <- setdiff(train_cols, colnames(mat))
    stop(sprintf("ERROR: Missing columns after alignment. Missing: %s", 
                 paste(still_missing, collapse = ", ")))
  }
  
  # Now safe to reorder (all columns exist)
  mat <- mat[, train_cols, drop = FALSE]
  
  return(mat)
}

# Apply alignment to validation and test matrices
valid_matrix_3 <- align_matrix_cols(valid_matrix_3, train_cols)
test_matrix_3  <- align_matrix_cols(test_matrix_3, train_cols)
cat("  ✓ Column names aligned across train/validation/test matrices\n")

# Extract the target variable (price) from each split
train_response_vec_3 <- model.frame(terms_3, data = train_data) |> model.response()
valid_response_vec_3 <- model.frame(terms_3, data = validation_data) |> model.response()
test_response_3      <- model.frame(terms_3, data = test_data)     |> model.response()

# Create DMatrix objects
dtrain_3 <- xgb.DMatrix(data = train_matrix_3, label = train_response_vec_3)
dvalid_3 <- xgb.DMatrix(data = valid_matrix_3, label = valid_response_vec_3)
watchlist_3 <- list(train = dtrain_3, eval = dvalid_3)  # For early stopping monitoring

# Train gradient boosting model with early stopping to prevent overfitting
cat("  Training XGBoost model (max 500 iterations, early stopping after 50 rounds)...\n")
xmdl_3_es <- xgb.train(
  params = list(objective = "reg:squarederror", eval_metric = "rmse"),
  data = dtrain_3,
  nrounds = 500,                    # Maximum iterations
  watchlist = watchlist_3,           # Monitor train and validation sets
  early_stopping_rounds = 50,        # Stop if no improvement for 50 rounds
  verbose = 0                       
)
eval_log_3 <- xmdl_3_es$evaluation_log  # Training history for plotting

# Get validation RMSE from training history
# eval_log_3$eval_rmse contains validation RMSE at each iteration
xmdl_3_val_rmse <- eval_log_3$eval_rmse[xmdl_3_es$best_iteration]
cat(sprintf("  Best iteration: %d | XGBoost Model 3 Validation RMSE (from training): %.4f\n", 
            xmdl_3_es$best_iteration, xmdl_3_val_rmse))

# Make out-of-sample predictions using the best iteration 
cat("  Making predictions on test set...\n")
xmdl_3_oos <- predict(xmdl_3_es, newdata = xgb.DMatrix(test_matrix_3), ntreelimit = xmdl_3_es$best_iteration)
xmdl_3_rmse <- sqrt(mean((xmdl_3_oos - test_response_3)^2))
cat(sprintf("  Best iteration: %d | XGBoost Model 3 Test RMSE: %.4f\n", xmdl_3_es$best_iteration, xmdl_3_rmse))

# Compute feature importance
xgb_importance_3 <- xgb.importance(model = xmdl_3_es)

# Plot training progress: RMSE over iterations for train vs validation
xgb_plot_path <- file.path(results_dir, "bavaria_diesel_xgb3_learning_curve.png")
cat("  Generating XGBoost learning curve plot...\n")
png(xgb_plot_path, width = 900, height = 600)
plot(eval_log_3$iter, eval_log_3$train_rmse, type = "l", col = "steelblue", lwd = 2,
     xlab = "Iteration", ylab = "RMSE", main = "XGBoost Model 3 Learning Curve")
lines(eval_log_3$iter, eval_log_3$eval_rmse, col = "tomato", lwd = 2)
legend("topright", legend = c("train", "validation"), col = c("steelblue", "tomato"), lwd = 2)
dev.off()
cat(sprintf("✓ XGBoost learning curve plot saved to: %s\n", xgb_plot_path))

# --- SHAP beeswarm plot -----------------------------------------------------
# Optionally save full model (if save_model_files = TRUE)
if (isTRUE(save_model_files)) {
  try(xgb.save(xmdl_3_es, fname = file.path(results_dir, "bavaria_diesel_xmdl3.model")), silent = TRUE)
}

# Compute SHAP values to explain individual predictions
cat("  Computing SHAP values on a test sample...\n")
set.seed(123)
shap_sample_n <- min(500000, nrow(test_matrix_3))
sample_idx <- sample(seq_len(nrow(test_matrix_3)), shap_sample_n)
test_sample_mat <- test_matrix_3[sample_idx, , drop = FALSE]
test_sample_y <- test_response_3[sample_idx]

shap_contrib <- predict(xmdl_3_es, newdata = xgb.DMatrix(test_sample_mat), ntreelimit = xmdl_3_es$best_iteration, predcontrib = TRUE)
shap_df <- as.data.frame(shap_contrib[, -ncol(shap_contrib), drop = FALSE])  # Drop bias term (last column)
shap_mean_abs <- sort(colMeans(abs(shap_df)), decreasing = TRUE)  # Average absolute contribution per feature

saveRDS(list(SHAP_MeanAbs = shap_mean_abs, SHAP_Sample = shap_df[1:min(1000, nrow(shap_df)), ]),
        file.path(results_dir, "bavaria_diesel_xgb3_shap.rds"))
cat("    SHAP summary saved to: bavaria_diesel_xgb3_shap.rds\n")

shap_long <- shap_df |> mutate(Row = row_number()) |> pivot_longer(-Row, names_to = "Feature", values_to = "SHAP")
value_long <- as_tibble(as.matrix(test_sample_mat)) |> mutate(Row = seq_len(n())) |> pivot_longer(-Row, names_to = "Feature", values_to = "Value")
shap_plot <- shap_long |> left_join(value_long, by = c("Row", "Feature"))
shap_rank <- shap_plot |> group_by(Feature) |> summarise(MeanAbs = mean(abs(SHAP))) |> arrange(desc(MeanAbs)) |> slice_head(n = 20)
shap_plot <- shap_plot |> semi_join(shap_rank, by = "Feature") |> mutate(Feature = factor(Feature, levels = rev(shap_rank$Feature)))
shap_beeswarm_path <- file.path(results_dir, "bavaria_diesel_xgb3_shap_beeswarm.png")
png(shap_beeswarm_path, width = 1400, height = 900, res = 180)
print(
  ggplot(shap_plot, aes(x = SHAP, y = Feature)) +
    geom_violin(
      aes(group = Feature),
      fill = "grey92",
      colour = "#b0b0b0",
      alpha = 0.85,
      width = 0.9,
      adjust = 1.2,   
      trim = FALSE
    ) +
    geom_point(
      aes(colour = Value),
      position = position_jitter(height = 0.12, width = 0),
      size = 0.6,
      alpha = 0.65
    ) +
    scale_colour_gradient(low = "purple", high = "gold") +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey40") +
    labs(
      title = "SHAP beeswarm (Bavaria XGBoost)",
      x = "SHAP value",
      y = NULL,
      colour = "Feature value"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 12),
      legend.position = "right"
    )
)
dev.off()
cat(sprintf("    SHAP beeswarm saved to: %s\n", shap_beeswarm_path))

# Save all metrics and results to RDS file for later analysis
cat("  Saving XGBoost Model 3 results externally...\n")
xmdl_3_results <- list(
  RMSE_Validation = xmdl_3_val_rmse,
  RMSE_Test = xmdl_3_rmse,
  Best_Iteration = xmdl_3_es$best_iteration,
  Evaluation_Log = eval_log_3,
  Feature_Importance = as.data.frame(xgb_importance_3)
)
saveRDS(xmdl_3_results, file.path(results_dir, "bavaria_diesel_xmdl3_metrics.rds"))
cat(sprintf(" XGBoost Model 3 metrics saved to: %s\n", file.path(results_dir, "bavaria_diesel_xmdl3_metrics.rds")))

# Clean up XGBoost objects immediately after saving
cat("  Cleaning up XGBoost Model 3 objects to free memory...\n")
rm(xmdl_3_es, xmdl_3_oos, xmdl_3_val_rmse, xmdl_3_rmse, xgb_importance_3, eval_log_3, xmdl_3_results)
rm(train_matrix_3, valid_matrix_3, test_matrix_3, train_response_vec_3, valid_response_vec_3, test_response_3)
rm(dtrain_3, dvalid_3, watchlist_3, train_cols)
rm(shap_contrib, shap_df, shap_mean_abs, test_sample_mat, test_sample_y, shap_sample_n, sample_idx)
gc(verbose = TRUE)

# =============================================================================
# STEP 10: DEEP NEURAL NETWORK MODEL 
# =============================================================================

library(keras)
library(tensorflow)

# Prepare design matrices using terms_3 so NN matches the XGBoost feature set
train_matrix_nn3 <- model.matrix(terms_3, data = train_data)
valid_matrix_nn3 <- model.matrix(terms_3, data = validation_data)
test_matrix_nn3  <- model.matrix(terms_3, data = test_data)

train_response_nn3 <- model.frame(terms_3, data = train_data) |> model.response()
valid_response_nn3 <- model.frame(terms_3, data = validation_data) |> model.response()
test_response_nn3  <- model.frame(terms_3, data = test_data) |> model.response()

# Align validation/test matrices to the training columns
nn_train_cols <- colnames(train_matrix_nn3)
valid_matrix_nn3 <- align_matrix_cols(valid_matrix_nn3, nn_train_cols)
test_matrix_nn3  <- align_matrix_cols(test_matrix_nn3, nn_train_cols)

# Standardize features (z-score) using training statistics
cat("  Standardizing data for neural networks...\n")
scaler_mean_nn3 <- apply(train_matrix_nn3, 2, mean)
scaler_sd_nn3   <- apply(train_matrix_nn3, 2, sd)

# Guard against zero-variance features
zero_var_idx <- which(scaler_sd_nn3 == 0 | is.na(scaler_sd_nn3))
if (length(zero_var_idx) > 0) {
  zero_var_names <- colnames(train_matrix_nn3)[zero_var_idx]
  cat(sprintf("  NOTE: %d zero-variance features adjusted: %s\n",
              length(zero_var_idx), paste(zero_var_names, collapse = ", ")))
  scaler_sd_nn3[zero_var_idx] <- 1
}

train_matrix_nn3_scaled <- scale(train_matrix_nn3, center = scaler_mean_nn3, scale = scaler_sd_nn3)
rm(train_matrix_nn3); gc(verbose = FALSE)

valid_matrix_nn3_scaled <- scale(valid_matrix_nn3, center = scaler_mean_nn3, scale = scaler_sd_nn3)
rm(valid_matrix_nn3); gc(verbose = FALSE)

test_matrix_nn3_scaled  <- scale(test_matrix_nn3,  center = scaler_mean_nn3, scale = scaler_sd_nn3)

# Remove rows with missing values after scaling to avoid NA predictions
keep_train <- complete.cases(train_matrix_nn3_scaled) & is.finite(train_response_nn3)
keep_valid <- complete.cases(valid_matrix_nn3_scaled) & is.finite(valid_response_nn3)
keep_test  <- complete.cases(test_matrix_nn3_scaled)  & is.finite(test_response_nn3)

train_matrix_nn3_scaled <- train_matrix_nn3_scaled[keep_train, , drop = FALSE]
train_response_nn3 <- train_response_nn3[keep_train]

valid_matrix_nn3_scaled <- valid_matrix_nn3_scaled[keep_valid, , drop = FALSE]
valid_response_nn3 <- valid_response_nn3[keep_valid]

test_matrix_nn3_scaled <- test_matrix_nn3_scaled[keep_test, , drop = FALSE]
test_response_nn3 <- test_response_nn3[keep_test]

# Build a simple dense network: 128 → 64 → 32 → 1
cat("  Building neural network architecture...\n")
nn_mdl_3 <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = "relu", input_shape = ncol(train_matrix_nn3_scaled)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

# Compile with legacy Adam (faster on Apple Silicon), MSE loss and MAE metric
cat("  Compiling neural network model...\n")
nn_mdl_3 %>% compile(
  optimizer = tf$keras$optimizers$legacy$Adam(learning_rate = 0.001),
  loss = "mse",
  metrics = "mae"
)

# Train with early stopping on validation loss
cat("  Training Deep Neural Network Model 3...\n")
nn_mdl_3_history <- nn_mdl_3 %>% fit(
  x = train_matrix_nn3_scaled,
  y = train_response_nn3,
  epochs = 100,
  batch_size = 2048,
  validation_data = list(valid_matrix_nn3_scaled, valid_response_nn3),
  callbacks = list(callback_early_stopping(monitor = "val_loss", patience = 5, restore_best_weights = TRUE)),
  verbose = 0
)

# Validation RMSE from training history
best_epoch <- which.min(nn_mdl_3_history$metrics$val_loss)
nn_mdl_3_val_rmse <- sqrt(nn_mdl_3_history$metrics$val_loss[best_epoch])
cat(sprintf("Deep Neural Network Model 3 Validation RMSE (epoch %d): %.4f\n",
            best_epoch, nn_mdl_3_val_rmse))

# Predict on the test set
cat("  Making predictions for Deep Neural Network Model 3...\n")
nn_mdl_3_oos <- as.numeric(predict(nn_mdl_3, test_matrix_nn3_scaled))
rm(test_matrix_nn3); gc(verbose = FALSE)

# Calculate RMSE and R²
nn_mdl_3_rmse <- sqrt(mean((nn_mdl_3_oos - test_response_nn3)^2))
nn_mdl_3_r2   <- cor(test_response_nn3, nn_mdl_3_oos)^2

cat(sprintf("Deep Neural Network Model 3 RMSE: %.4f\n", nn_mdl_3_rmse))
cat(sprintf("Deep Neural Network Model 3 R²: %.4f\n", nn_mdl_3_r2))

# Learning-curve plot (train vs validation loss)
nn_plot_path <- file.path(results_dir, "bavaria_diesel_nn3_learning_curve.png")
if (all(is.finite(nn_mdl_3_history$metrics$loss)) || all(is.finite(nn_mdl_3_history$metrics$val_loss))) {
  png(nn_plot_path, width = 900, height = 600)
  plot(nn_mdl_3_history$metrics$loss, type = "l", lwd = 2, col = "steelblue",
       xlab = "Epoch", ylab = "Loss (MSE)", main = "NN Model 3 Learning Curve")
  lines(nn_mdl_3_history$metrics$val_loss, lwd = 2, col = "tomato")
  legend("topright", legend = c("train", "validation"), col = c("steelblue", "tomato"),
         lwd = 2, bty = "n")
  dev.off()
  cat(sprintf("✓ Neural Network learning curve plot saved to: %s\n", nn_plot_path))
}

# Optionally save full Keras model
if (isTRUE(save_model_files)) {
  keras::save_model_hdf5(nn_mdl_3, file.path(results_dir, "bavaria_diesel_nn_mdl3_full.h5"))
}

# Save all metrics and results to RDS file for later analysis
cat("  Saving Deep Neural Network Model 3 results externally...\n")

# Save extracted metrics
nn_history_df <- NULL
if (!is.null(nn_mdl_3_history$metrics)) {
  nn_history_df <- as.data.frame(nn_mdl_3_history$metrics)
}

nn_mdl_3_results <- list(
  RMSE_Validation = nn_mdl_3_val_rmse,
  RMSE_Test = nn_mdl_3_rmse,
  R2_Test = nn_mdl_3_r2,
  History = nn_history_df
)
saveRDS(nn_mdl_3_results, file.path(results_dir, "bavaria_diesel_nn_mdl3_metrics.rds"))

cat(sprintf("    Deep Neural Network Model 3 metrics saved to: %s\n", file.path(results_dir, "bavaria_diesel_nn_mdl3_metrics.rds")))

# Clean up Deep Neural Network Model 3 immediately
cat("  Cleaning up Deep Neural Network Model 3 to free memory...\n")
rm(nn_mdl_3, nn_mdl_3_history, nn_mdl_3_oos, nn_mdl_3_val_rmse,
   train_response_nn3, test_response_nn3, valid_response_nn3,
   train_matrix_nn3_scaled, test_matrix_nn3_scaled, valid_matrix_nn3_scaled,
   scaler_mean_nn3, scaler_sd_nn3, nn_mdl_3_results, nn_history_df,
   nn_plot_path)
gc(verbose = TRUE)

# =============================================================================
# STEP 12: FINAL MODEL COMPARISON
# =============================================================================
cat("\nSTEP 12: Final model comparison...\n")

# Load all results for comparison
cat("  Loading saved model results for comparison...\n")
mdl1_res <- readRDS(file.path(results_dir, "bavaria_diesel_mdl1_metrics.rds"))
mdl2_res <- readRDS(file.path(results_dir, "bavaria_diesel_mdl2_metrics.rds"))
mdl3_res <- readRDS(file.path(results_dir, "bavaria_diesel_mdl3_metrics.rds"))
gam_res <- readRDS(file.path(results_dir, "bavaria_diesel_gam_metrics.rds"))
xgb_res <- readRDS(file.path(results_dir, "bavaria_diesel_xmdl3_metrics.rds"))
nn_res <- readRDS(file.path(results_dir, "bavaria_diesel_nn_mdl3_metrics.rds"))

get_metric <- function(res, name) {
  value <- res[[name]]
  if (is.null(value) || length(value) == 0) {
    return(NA_real_)
  }
  as.numeric(value)[1]
}

final_comparison <- data.frame(
  Model = c("Linear 1", "Linear 2", "Linear 3", 
            "GAM (bam)", "XGBoost (formula_3)", "Deep NN (formula_3)"),
  RMSE_Validation = c(
    get_metric(mdl1_res, "RMSE_Validation"),
    get_metric(mdl2_res, "RMSE_Validation"),
    get_metric(mdl3_res, "RMSE_Validation"),
    get_metric(gam_res, "RMSE_Validation"),
    get_metric(xgb_res, "RMSE_Validation"),
    get_metric(nn_res, "RMSE_Validation")
  ),
  RMSE_Test = c(
    get_metric(mdl1_res, "RMSE_Test"),
    get_metric(mdl2_res, "RMSE_Test"),
    get_metric(mdl3_res, "RMSE_Test"),
    get_metric(gam_res, "RMSE_Test"),
    get_metric(xgb_res, "RMSE_Test"),
    get_metric(nn_res, "RMSE_Test")
  ),
  R2_Test = c(
    get_metric(mdl1_res, "R2"),
    get_metric(mdl2_res, "R2"),
    get_metric(mdl3_res, "R2"),
    get_metric(gam_res, "R2_Test"),
    NA,
    get_metric(nn_res, "R2_Test")
  ),
  BIC = c(
    get_metric(mdl1_res, "BIC"),
    get_metric(mdl2_res, "BIC"),
    get_metric(mdl3_res, "BIC"),
    NA,
    NA,
    NA
  ),
  stringsAsFactors = FALSE
)

cat("\n=== CROSS-MODEL COMPARISON (Validation/Test RMSE) ===\n")
print(final_comparison)

# Best model based on Test RMSE (lower is better)
best_model_idx <- which.min(final_comparison$RMSE_Test)
cat(sprintf("\n✓ Best performing model (by Test RMSE): %s (RMSE: %.4f)\n", 
            final_comparison$Model[best_model_idx], 
            final_comparison$RMSE_Test[best_model_idx]))


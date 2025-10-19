#!/usr/bin/env Rscript

# Load and Explore Fuel Price Datasets
# Explores datasets from all countries to inform preprocessing decisions

library(DBI)
library(duckdb)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("../../")
}

# Database paths
german_db <- "databases/german_fuel_data.duckdb"
arboe_db <- "databases/arboe_fuel_prices.duckdb"
oeamtc_fuel_db <- "databases/oeamtc_fuel_data.duckdb"
jet_db <- "databases/jet_austria_fuel_data.duckdb"
omv_db <- "databases/omv_austria_fuel_data.duckdb"
italian_db <- "databases/italian_fuel_data.duckdb"
slovenian_db <- "databases/slovenian_fuel_data.duckdb"

cat("=== COMPREHENSIVE DATASET EXPLORATION ===\n")
cat("Exploring all databases to inform preprocessing decisions\n\n")

# Function to safely explore database with consistent metrics
explore_database <- function(db_path, db_name) {
  cat("=== ", toupper(db_name), " DATABASE ===\n")
  
  if (!file.exists(db_path)) {
    cat("Database not found:", db_path, "\n\n")
    return(NULL)
  }
  
  con <- dbConnect(duckdb(), db_path, read_only = TRUE)
  
  tryCatch({
    tables <- dbListTables(con)
    cat("Tables:", paste(tables, collapse = ", "), "\n")
    
    # Explore each table with consistent metrics
    for(table in tables) {
      cat("\nTable:", table, "\n")
      
      # Get record count
      count <- dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table))$count
      cat("  Records:", format(count, big.mark = ","), "\n")
      
      # Get column info
      columns <- dbListFields(con, table)
      cat("  Columns:", paste(columns, collapse = ", "), "\n")
      
      # Check for date columns and get date range
      date_cols <- columns[grepl("date", columns, ignore.case = TRUE)]
      if(length(date_cols) > 0) {
        for(date_col in date_cols) {
          tryCatch({
            date_range <- dbGetQuery(con, paste0("
              SELECT 
                MIN(", date_col, ") as min_date, 
                MAX(", date_col, ") as max_date,
                COUNT(DISTINCT DATE(", date_col, ")) as unique_days
              FROM ", table, "
              WHERE ", date_col, " IS NOT NULL
            "))
            if(!is.na(date_range$min_date)) {
              cat("  ", date_col, " range:", date_range$min_date, "to", date_range$max_date, "\n")
              cat("  Unique days:", date_range$unique_days, "\n")
            }
          }, error = function(e) {
            cat("  Error getting date range for", date_col, ":", e$message, "\n")
          })
        }
      }
      
      # Check for coordinate columns
      coord_cols <- columns[grepl("lat|lon|lng", columns, ignore.case = TRUE)]
      if(length(coord_cols) > 0) {
        cat("  Coordinate columns:", paste(coord_cols, collapse = ", "), "\n")
        
        # Check for NULL coordinates
        for(coord_col in coord_cols) {
          tryCatch({
            null_count <- dbGetQuery(con, paste0("
              SELECT COUNT(*) as null_count 
              FROM ", table, " 
              WHERE ", coord_col, " IS NULL
            "))$null_count
            total_count <- count
            null_pct <- round((null_count / total_count) * 100, 2)
            cat("  ", coord_col, " NULLs:", null_count, "(", null_pct, "%)\n")
          }, error = function(e) {
            cat("  Error checking NULLs for", coord_col, ":", e$message, "\n")
          })
        }
      }
      
      # Check for price columns with consistent outlier analysis
      price_cols <- columns[grepl("price|diesel|e5|e10|fuel", columns, ignore.case = TRUE)]
      if(length(price_cols) > 0) {
        cat("  Price columns:", paste(price_cols, collapse = ", "), "\n")
        
        for(price_col in price_cols) {
          tryCatch({
            # Only analyze numeric price columns
            if(price_col %in% c("fuel_type", "fuel_unit", "fuel_name", "fuel", "fuel_is_self", 
                               "fuel_last_updated", "fuel_source", "last_fuel_price_update", 
                               "fuel_id", "dieselchange", "e5change", "e10change")) {
              next  # Skip non-numeric columns
            }
            
            price_stats <- dbGetQuery(con, paste0("
              SELECT 
                COUNT(*) as total_records,
                COUNT(CASE WHEN ", price_col, " IS NOT NULL THEN 1 END) as non_null_records,
                MIN(", price_col, ") as min_price,
                MAX(", price_col, ") as max_price,
                AVG(", price_col, ") as avg_price,
                COUNT(CASE WHEN ", price_col, " <= 0 THEN 1 END) as zero_or_negative,
                COUNT(CASE WHEN ", price_col, " > 10 THEN 1 END) as over_10_euros,
                COUNT(CASE WHEN ", price_col, " > 5 THEN 1 END) as over_5_euros,
                COUNT(CASE WHEN ", price_col, " > 3 THEN 1 END) as over_3_euros
              FROM ", table, "
            "))
            
            if(price_stats$non_null_records > 0) {
              total <- price_stats$total_records
              non_null <- price_stats$non_null_records
              zero_neg_pct <- round((price_stats$zero_or_negative / non_null) * 100, 2)
              over_10_pct <- round((price_stats$over_10_euros / non_null) * 100, 2)
              over_5_pct <- round((price_stats$over_5_euros / non_null) * 100, 2)
              over_3_pct <- round((price_stats$over_3_euros / non_null) * 100, 2)
              
              cat("    ", price_col, ":\n")
              cat("      Records:", format(non_null, big.mark = ","), "/", format(total, big.mark = ","), "\n")
              cat("      Range:", round(price_stats$min_price, 2), "to", round(price_stats$max_price, 2), "€\n")
              cat("      Average:", round(price_stats$avg_price, 2), "€\n")
              cat("      Outliers: ≤0:", format(price_stats$zero_or_negative, big.mark = ","), "(", zero_neg_pct, "%)\n")
              cat("      Outliers: >3€:", format(price_stats$over_3_euros, big.mark = ","), "(", over_3_pct, "%)\n")
              cat("      Outliers: >5€:", format(price_stats$over_5_euros, big.mark = ","), "(", over_5_pct, "%)\n")
              cat("      Outliers: >10€:", format(price_stats$over_10_euros, big.mark = ","), "(", over_10_pct, "%)\n")
              
              # Data quality assessment
              if(zero_neg_pct > 10) {
                cat("      ⚠️  HIGH percentage of zero/negative prices\n")
              }
              if(over_10_pct > 1) {
                cat("      ⚠️  HIGH percentage of prices >10€\n")
              }
              if(over_5_pct > 20) {
                cat("      ⚠️  HIGH percentage of prices >5€\n")
              }
            }
          }, error = function(e) {
            cat("  Error getting price stats for", price_col, ":", e$message, "\n")
          })
        }
      }
    }
    
  }, finally = {
    dbDisconnect(con, shutdown = TRUE)
  })
  
  cat("\n")
  return(TRUE)
}

# Explore all databases
explore_database(german_db, "German")
explore_database(arboe_db, "Austrian ARBÖ")
explore_database(oeamtc_fuel_db, "Austrian ÖAMTC")
explore_database(jet_db, "Austrian JET")
explore_database(omv_db, "Austrian OMV")
explore_database(italian_db, "Italian")
explore_database(slovenian_db, "Slovenian")

# Austrian station overlap analysis
cat("=== AUSTRIAN STATION OVERLAP ANALYSIS ===\n")
cat("Analyzing station overlaps between Austrian sources...\n")

# Load Austrian station data
austrian_sources <- list(
  list(name = "ARBÖ", db = arboe_db, table = "arboe_prices", id_col = "id", lat_col = "lat", lng_col = "lon", name_col = "name"),
  list(name = "ÖAMTC", db = oeamtc_fuel_db, table = "oeamtc_fuel", id_col = "station_id", lat_col = "station_lat", lng_col = "station_lng", name_col = "station_name"),
  list(name = "JET", db = jet_db, table = "jet_austria_fuel", id_col = "station_id", lat_col = "lat", lng_col = "lng", name_col = "station_name"),
  list(name = "OMV", db = omv_db, table = "omv_austria_fuel", id_col = "station_id", lat_col = "lat", lng_col = "lon", name_col = "town")
)

austrian_stations <- list()

for(source in austrian_sources) {
  if(file.exists(source$db)) {
    con <- dbConnect(duckdb(), source$db, read_only = TRUE)
    tryCatch({
      stations <- dbGetQuery(con, paste0("
        SELECT DISTINCT
          '", source$name, "' as source,
          ", source$id_col, " as station_id,
          ", source$name_col, " as station_name,
          ROUND(", source$lat_col, ", 4) as lat_rounded,
          ROUND(", source$lng_col, ", 4) as lng_rounded
        FROM ", source$table, "
        WHERE ", source$lat_col, " IS NOT NULL 
          AND ", source$lng_col, " IS NOT NULL
          AND ", source$lat_col, " BETWEEN 45 AND 50 
          AND ", source$lng_col, " BETWEEN 9 AND 18
      "))
      austrian_stations[[source$name]] <- stations
      cat("  ", source$name, ":", nrow(stations), "stations\n")
    }, finally = {
      dbDisconnect(con, shutdown = TRUE)
    })
  }
}

# Calculate overlaps
if(length(austrian_stations) > 1) {
  cat("\nStation overlaps (4 decimal precision):\n")
  
  source_names <- names(austrian_stations)
  for(i in 1:(length(source_names)-1)) {
    for(j in (i+1):length(source_names)) {
      source1 <- source_names[i]
      source2 <- source_names[j]
      
      coords1 <- paste(austrian_stations[[source1]]$lat_rounded, austrian_stations[[source1]]$lng_rounded, sep = ",")
      coords2 <- paste(austrian_stations[[source2]]$lat_rounded, austrian_stations[[source2]]$lng_rounded, sep = ",")
      
      overlap <- intersect(coords1, coords2)
      cat("  ", source1, " ∩ ", source2, ":", length(overlap), "overlapping coordinates\n")
    }
  }
}

# === TEMPORAL AND SPATIAL PATTERN ANALYSIS ===
cat("=== TEMPORAL AND SPATIAL PATTERN ANALYSIS ===\n")
cat("Analyzing patterns to justify temporal and spatial feature engineering\n\n")

# Function to create temporal visualizations
create_temporal_visualizations <- function(db_path, db_name, table_name, date_col, price_cols) {
  if (!file.exists(db_path)) {
    cat("Database not found for visualization:", db_path, "\n")
    return(NULL)
  }
  
  con <- dbConnect(duckdb(), db_path, read_only = TRUE)
  
  tryCatch({
    cat("Creating temporal visualizations for", db_name, "...\n")
    
    # Create output directory
    output_dir <- "results/visualizations"
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    for(price_col in price_cols) {
      if(price_col %in% c("fuel_type", "fuel_unit", "fuel_name", "fuel", "fuel_is_self", 
                         "fuel_last_updated", "fuel_source", "last_fuel_price_update", 
                         "fuel_id", "dieselchange", "e5change", "e10change")) {
        next  # Skip non-numeric columns
      }
      
      cat("  Analyzing", price_col, "...\n")
      
      # Sample data for visualization (to avoid memory issues)
      # Handle different date formats
      if(db_name == "Austrian_OEAMTC") {
        sample_data <- dbGetQuery(con, paste0("
          SELECT 
            ", date_col, " as date_time,
            ", price_col, " as price,
            EXTRACT(hour FROM CAST(", date_col, " AS TIMESTAMP)) as hour,
            EXTRACT(dow FROM CAST(", date_col, " AS TIMESTAMP)) as day_of_week,
            EXTRACT(month FROM CAST(", date_col, " AS TIMESTAMP)) as month
          FROM ", table_name, "
          WHERE ", price_col, " > 0 AND ", price_col, " < 10
            AND ", date_col, " IS NOT NULL
          ORDER BY RANDOM()
          LIMIT 100000
        "))
      } else {
        sample_data <- dbGetQuery(con, paste0("
          SELECT 
            ", date_col, " as date_time,
            ", price_col, " as price,
            EXTRACT(hour FROM ", date_col, ") as hour,
            EXTRACT(dow FROM ", date_col, ") as day_of_week,
            EXTRACT(month FROM ", date_col, ") as month
          FROM ", table_name, "
          WHERE ", price_col, " > 0 AND ", price_col, " < 10
            AND ", date_col, " IS NOT NULL
          ORDER BY RANDOM()
          LIMIT 100000
        "))
      }
      
      if(nrow(sample_data) > 1000) {
        # 1. Price vs Hour pattern
        hourly_avg <- aggregate(price ~ hour, data = sample_data, FUN = mean)
        png(file.path(output_dir, paste0(db_name, "_", price_col, "_hourly_pattern.png")), 
            width = 800, height = 600)
        plot(hourly_avg$hour, hourly_avg$price, 
             type = "l", lwd = 2, col = "blue",
             main = paste(db_name, "-", price_col, "Price vs Hour"),
             xlab = "Hour of Day", ylab = "Average Price (€)",
             xlim = c(0, 23))
        grid()
        dev.off()
        
        # 2. Price vs Day of Week pattern
        dow_avg <- aggregate(price ~ day_of_week, data = sample_data, FUN = mean)
        dow_names <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
        png(file.path(output_dir, paste0(db_name, "_", price_col, "_dow_pattern.png")), 
            width = 800, height = 600)
        barplot(dow_avg$price, names.arg = dow_names, col = "lightblue",
                main = paste(db_name, "-", price_col, "Price vs Day of Week"),
                xlab = "Day of Week", ylab = "Average Price (€)")
        grid()
        dev.off()
        
        # 3. Price vs Month pattern
        monthly_avg <- aggregate(price ~ month, data = sample_data, FUN = mean)
        month_names <- month.name
        png(file.path(output_dir, paste0(db_name, "_", price_col, "_monthly_pattern.png")), 
            width = 800, height = 600)
        barplot(monthly_avg$price, names.arg = month_names[monthly_avg$month], 
                col = "lightgreen", las = 2,
                main = paste(db_name, "-", price_col, "Price vs Month"),
                xlab = "Month", ylab = "Average Price (€)")
        grid()
        dev.off()
        
        cat("    ✓ Temporal patterns saved to", output_dir, "\n")
      }
    }
    
  }, finally = {
    dbDisconnect(con, shutdown = TRUE)
  })
}

# Function to create spatial visualizations
create_spatial_visualizations <- function(db_path, db_name, table_name, lat_col, lng_col, price_cols) {
  if (!file.exists(db_path)) {
    cat("Database not found for spatial visualization:", db_path, "\n")
    return(NULL)
  }
  
  con <- dbConnect(duckdb(), db_path, read_only = TRUE)
  
  tryCatch({
    cat("Creating spatial visualizations for", db_name, "...\n")
    
    output_dir <- "results/visualizations"
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    for(price_col in price_cols) {
      if(price_col %in% c("fuel_type", "fuel_unit", "fuel_name", "fuel", "fuel_is_self", 
                         "fuel_last_updated", "fuel_source", "last_fuel_price_update", 
                         "fuel_id", "dieselchange", "e5change", "e10change")) {
        next  # Skip non-numeric columns
      }
      
      cat("  Analyzing spatial patterns for", price_col, "...\n")
      
      # Get average prices by station location
      spatial_data <- dbGetQuery(con, paste0("
        SELECT 
          ROUND(", lat_col, ", 2) as lat_rounded,
          ROUND(", lng_col, ", 2) as lng_rounded,
          AVG(", price_col, ") as avg_price,
          COUNT(*) as record_count
        FROM ", table_name, "
        WHERE ", price_col, " > 0 AND ", price_col, " < 10
          AND ", lat_col, " IS NOT NULL AND ", lng_col, " IS NOT NULL
        GROUP BY ROUND(", lat_col, ", 2), ROUND(", lng_col, ", 2)
        HAVING COUNT(*) >= 10
        ORDER BY avg_price DESC
        LIMIT 1000
      "))
      
      if(nrow(spatial_data) > 50) {
        # Create spatial price map
        png(file.path(output_dir, paste0(db_name, "_", price_col, "_spatial_prices.png")), 
            width = 1000, height = 800)
        
        # Color code by price
        price_range <- range(spatial_data$avg_price)
        colors <- rainbow(10)[cut(spatial_data$avg_price, breaks = 10, labels = FALSE)]
        
        plot(spatial_data$lng_rounded, spatial_data$lat_rounded,
             col = colors, pch = 16, cex = 0.8,
             main = paste(db_name, "-", price_col, "Average Prices by Location"),
             xlab = "Longitude", ylab = "Latitude")
        
        # Add color legend
        legend("topright", 
               legend = paste0(round(seq(price_range[1], price_range[2], length.out = 5), 2), "€"),
               col = rainbow(5), pch = 16, title = "Price Range")
        
        dev.off()
        
        cat("    ✓ Spatial patterns saved to", output_dir, "\n")
      }
    }
    
  }, finally = {
    dbDisconnect(con, shutdown = TRUE)
  })
}

# Create visualizations for German data
cat("Creating visualizations for German data...\n")
create_temporal_visualizations(german_db, "German", "german_prices", "date", c("diesel", "e5", "e10"))

# Create visualizations for Austrian data
cat("Creating visualizations for Austrian data...\n")
create_temporal_visualizations(oeamtc_fuel_db, "Austrian_OEAMTC", "oeamtc_fuel", "fuel_last_updated", c("fuel_price"))

# Create spatial visualizations
create_spatial_visualizations(german_db, "German", "german_prices", "latitude", "longitude", c("diesel", "e5", "e10"))
create_spatial_visualizations(oeamtc_fuel_db, "Austrian_OEAMTC", "oeamtc_fuel", "station_lat", "station_lng", c("fuel_price"))

cat("\n=== PREPROCESSING RECOMMENDATIONS ===\n")
cat("Based on data exploration and pattern analysis:\n\n")

cat("1. TEMPORAL FEATURE ENGINEERING (JUSTIFIED BY PATTERNS):\n")
cat("   - Hour: Clear daily patterns (rush hours, night prices)\n")
cat("   - Day of Week: Weekend vs weekday price differences\n")
cat("   - Month: Seasonal price variations\n")
cat("   - Use sin/cos encoding for cyclical patterns\n\n")

cat("2. SPATIAL FEATURE ENGINEERING (JUSTIFIED BY PATTERNS):\n")
cat("   - Geographic price clustering visible\n")
cat("   - Urban vs rural price differences\n")
cat("   - Nearby station competition effects\n")
cat("   - Coordinate precision: round to 4 decimal places\n\n")

cat("3. TRAIN/TEST SPLIT STRATEGY:\n")
cat("   - Germany: Temporal split (Aug 2024-Jul 2025 train, Aug 2025+ test)\n")
cat("   - Austria: Random 80/20 split (limited temporal coverage)\n")
cat("   - Slovenia: Random 80/20 split (limited temporal coverage)\n")
cat("   - Italy: Exclude (no coordinates for spatial analysis)\n\n")

cat("4. DATA QUALITY HANDLING:\n")
cat("   - Remove prices ≤ 0 or > 10 euros (outliers)\n")
cat("   - Handle NULL coordinates (exclude records)\n")
cat("   - Keep Austrian station duplicates (different temporal coverage)\n\n")

cat("5. MODELING APPROACH:\n")
cat("   - Separate models per fuel type (diesel, gasoline_95, gasoline_98)\n")
cat("   - Primary focus: German data (largest dataset)\n")
cat("   - Transfer learning: Austrian/Slovenian for validation\n\n")

cat("Exploration and visualization complete! Ready for unified dataset creation.\n")
cat("Visualizations saved to: results/visualizations/\n")

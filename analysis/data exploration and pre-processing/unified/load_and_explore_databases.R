#!/usr/bin/env Rscript

# Load and Explore DuckDB Databases
# Explores fuel price databases for Germany, Austria, and Slovenia
# Also analyzes the unified stations database

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(tidyr)
  library(scales)
})

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("../../")
}

# Database configuration
databases <- list(
  german = list(
    path = "databases/german_fuel.duckdb",
    size_gb = 22.4,
    tables = c("german_prices_wide", "german_stations")
  ),
  austrian = list(
    path = "databases/austrian_fuel_database.duckdb", 
    size_gb = 0.92,
    tables = c("austrian_prices", "austrian_stations")
  ),
  slovenian = list(
    path = "databases/slovenian_fuel_database.duckdb",
    size_gb = 0.011,
    tables = c("slovenian_prices", "slovenian_stations")
  ),
  stations = list(
    path = "databases/stations.duckdb",
    size_gb = 0.0045,
    tables = c("stations", "v_aus", "v_ger", "v_slo", "v_neighbors", "v_pairs")
  )
)

cat("Database Exploration\n")
cat("===================\n")
cat("Databases:", length(databases), "\n")
cat("Total tables:", sum(sapply(databases, function(x) length(x$tables))), "\n\n")

# Helper function to connect to database
connect_db <- function(db_path) {
  if (!file.exists(db_path)) {
    cat("Database not found:", db_path, "\n")
    return(NULL)
  }
  
  con <- dbConnect(duckdb(), db_path, read_only = TRUE)
  return(con)
}

# Helper function to get table info
get_table_info <- function(con, table_name) {
  tryCatch({
    # Get row count
    count_query <- paste0("SELECT COUNT(*) as count FROM ", table_name)
    row_count <- dbGetQuery(con, count_query)$count
    
    # Get column info
    col_info <- dbGetQuery(con, paste0("DESCRIBE ", table_name))
    
    # Get sample data
    sample_data <- dbGetQuery(con, paste0("SELECT * FROM ", table_name, " LIMIT 5"))
    
    return(list(
      row_count = row_count,
      columns = col_info,
      sample = sample_data
    ))
  }, error = function(e) {
    cat("Error accessing table", table_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Helper function to analyze price data
analyze_price_data <- function(con, table_name, country_name) {
  cat("Analyzing price data for", country_name, "\n")
  
  tryCatch({
    # Date range
    date_range <- dbGetQuery(con, paste0("
      SELECT 
        MIN(date) as min_date,
        MAX(date) as max_date,
        COUNT(DISTINCT DATE(date)) as unique_days
      FROM ", table_name
    ))
    
    cat("  Date range:", date_range$min_date, "to", date_range$max_date, "\n")
    cat("  Unique days:", date_range$unique_days, "\n")
    
    # Station count
    station_count <- dbGetQuery(con, paste0("
      SELECT COUNT(DISTINCT station_uuid) as unique_stations
      FROM ", table_name
    ))
    cat("  Unique stations:", format(station_count$unique_stations, big.mark = ","), "\n")
    
    # Price statistics (adapt based on table structure)
    if (country_name == "Germany") {
      # German has wide format: diesel, gasoline_95, gasoline_98
      price_stats <- dbGetQuery(con, paste0("
        SELECT 
          AVG(diesel) as avg_diesel,
          AVG(gasoline_95) as avg_gasoline_95,
          AVG(gasoline_98) as avg_gasoline_98,
          MIN(diesel) as min_diesel,
          MAX(diesel) as max_diesel
        FROM ", table_name, "
        WHERE diesel > 0
      "))
      cat("  Diesel: avg €", round(price_stats$avg_diesel, 3), 
          ", range €", round(price_stats$min_diesel, 3), "-€", round(price_stats$max_diesel, 3), "\n")
      cat("  Gasoline 95: avg €", round(price_stats$avg_gasoline_95, 3), "\n")
      cat("  Gasoline 98: avg €", round(price_stats$avg_gasoline_98, 3), "\n")
      
    } else {
      # Austrian/Slovenian have long format: fuel_type, price
      fuel_types <- dbGetQuery(con, paste0("
        SELECT DISTINCT fuel_type
        FROM ", table_name, "
        WHERE price > 0
      "))
      
      for (fuel in fuel_types$fuel_type) {
        fuel_stats <- dbGetQuery(con, paste0("
          SELECT 
            AVG(price) as avg_price,
            MIN(price) as min_price,
            MAX(price) as max_price,
            COUNT(*) as records
          FROM ", table_name, "
          WHERE fuel_type = '", fuel, "' AND price > 0
        "))
        cat("  ", fuel, ": avg €", round(fuel_stats$avg_price, 3),
            ", range €", round(fuel_stats$min_price, 3), "-€", round(fuel_stats$max_price, 3),
            " (", format(fuel_stats$records, big.mark = ","), " records)\n")
      }
    }
    
    # Recent data check
    recent_data <- dbGetQuery(con, paste0("
      SELECT COUNT(*) as recent_count
      FROM ", table_name, "
      WHERE date >= CURRENT_DATE - INTERVAL '7 days'
    "))
    cat("  Recent data (last 7 days):", format(recent_data$recent_count, big.mark = ","), "records\n")
    
  }, error = function(e) {
    cat("  Error analyzing price data:", e$message, "\n")
  })
}

# Helper function to analyze station data
analyze_station_data <- function(con, table_name, country_name) {
  cat("Analyzing station data for", country_name, "\n")
  
  tryCatch({
    # Basic station count
    station_count <- dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table_name))
    cat("  Total stations:", format(station_count$count, big.mark = ","), "\n")
    
    # Brand distribution
    brand_stats <- dbGetQuery(con, paste0("
      SELECT 
        brand,
        COUNT(*) as station_count
      FROM ", table_name, "
      WHERE brand IS NOT NULL
      GROUP BY brand
      ORDER BY station_count DESC
      LIMIT 10
    "))
    
    if (nrow(brand_stats) > 0) {
      cat("  Top brands:\n")
      for (i in 1:min(5, nrow(brand_stats))) {
        cat("    ", brand_stats$brand[i], ":", format(brand_stats$station_count[i], big.mark = ","), "stations\n")
      }
    }
    
    # Geographic coverage
    geo_coverage <- dbGetQuery(con, paste0("
      SELECT 
        COUNT(*) as total_stations,
        COUNT(latitude) as stations_with_coords,
        COUNT(DISTINCT city) as unique_cities
      FROM ", table_name
    ))
    
    coord_pct <- round((geo_coverage$stations_with_coords / geo_coverage$total_stations) * 100, 1)
    cat("  Geographic coverage:", geo_coverage$stations_with_coords, "/", geo_coverage$total_stations,
        "stations (", coord_pct, "%)\n")
    cat("  Cities covered:", geo_coverage$unique_cities, "\n")
    
  }, error = function(e) {
    cat("  Error analyzing station data:", e$message, "\n")
  })
}

# Main exploration loop
for (db_name in names(databases)) {
  db_info <- databases[[db_name]]
  
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("DATABASE:", toupper(db_name), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("Path:", db_info$path, "\n")
  cat("Size:", db_info$size_gb, "GB\n")
  cat("Tables:", paste(db_info$tables, collapse = ", "), "\n\n")
  
  # Connect to database
  con <- connect_db(db_info$path)
  if (is.null(con)) {
    cat("Skipping database", db_name, "\n\n")
    next
  }
  
  # Explore each table
  for (table_name in db_info$tables) {
    cat("TABLE:", table_name, "\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    
    table_info <- get_table_info(con, table_name)
    if (is.null(table_info)) {
      cat("Could not access table\n\n")
      next
    }
    
    # Basic table info
    cat("Rows:", format(table_info$row_count, big.mark = ","), "\n")
    cat("Columns:", nrow(table_info$columns), "\n")
    
    # Show column structure
    cat("Column structure:\n")
    for (i in 1:nrow(table_info$columns)) {
      col <- table_info$columns[i, ]
      cat("  ", col$column_name, " (", col$column_type, ")", 
          ifelse(col$null == "NO", " [NOT NULL]", ""),
          ifelse(col$key == "PRI", " [PRIMARY KEY]", ""), "\n")
    }
    
    # Specialized analysis based on table type
    if (grepl("prices", table_name, ignore.case = TRUE)) {
      analyze_price_data(con, table_name, db_name)
    } else if (grepl("stations", table_name, ignore.case = TRUE)) {
      analyze_station_data(con, table_name, db_name)
    }
    
    cat("\n")
  }
  
  # Disconnect
  dbDisconnect(con, shutdown = TRUE)
  cat("Database", db_name, "exploration completed\n\n")
}

# Cross-database analysis
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("CROSS-DATABASE ANALYSIS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Connect to all databases for cross-analysis
connections <- list()
for (db_name in names(databases)) {
  con <- connect_db(databases[[db_name]]$path)
  if (!is.null(con)) {
    connections[[db_name]] <- con
  }
}

if (length(connections) > 0) {
  cat("Total data volume across all databases:\n")
  
  total_records <- 0
  total_stations <- 0
  
  for (db_name in names(connections)) {
    con <- connections[[db_name]]
    
    # Count total records
    for (table in databases[[db_name]]$tables) {
      if (grepl("prices", table, ignore.case = TRUE)) {
        count <- dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table))$count
        total_records <- total_records + count
        cat("  ", db_name, " prices:", format(count, big.mark = ","), "records\n")
      } else if (grepl("stations", table, ignore.case = TRUE)) {
        count <- dbGetQuery(con, paste0("SELECT COUNT(*) as count FROM ", table))$count
        total_stations <- total_stations + count
        cat("  ", db_name, " stations:", format(count, big.mark = ","), "stations\n")
      }
    }
  }
  
  cat("\nSUMMARY STATISTICS:\n")
  cat("  Total price records:", format(total_records, big.mark = ","), "\n")
  cat("  Total stations:", format(total_stations, big.mark = ","), "\n")
  cat("  Total database size:", sum(sapply(databases, function(x) x$size_gb)), "GB\n")
  cat("  Countries covered:", length(databases) - 1, "(Germany, Austria, Slovenia)\n")
  
  # Disconnect all
  for (con in connections) {
    dbDisconnect(con, shutdown = TRUE)
  }
}

cat(paste(rep("\n", 60), collapse = ""), "\n")
cat("DATABASE EXPLORATION COMPLETED\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Summary:\n")
cat("  Explored", length(databases), "databases\n")
cat("  Analyzed", sum(sapply(databases, function(x) length(x$tables))), "tables\n")
cat("  Total data volume:", sum(sapply(databases, function(x) x$size_gb)), "GB\n")
cat("  Ready for analysis and modeling\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

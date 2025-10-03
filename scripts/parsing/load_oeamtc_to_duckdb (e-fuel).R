# =============================================================================
# Comprehensive E-Fuel Data Parser for OEAMTC JSON Files
# Extracts all meaningful variables from e-fuel charging stations
# =============================================================================

# ---------- Load Libraries ----------
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(duckdb)
library(DBI)
library(fs)

# ---------- Helper Functions ----------
`%or%` <- function(x, y) if (is.null(x)) y else x

safe_as_numeric <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (is.numeric(x) && length(x) >= 1) return(as.numeric(x[1]))
  if (is.character(x) && length(x) >= 1) return(suppressWarnings(as.numeric(x[1])))
  if (is.list(x) && length(x) >= 1) return(safe_as_numeric(x[[1]]))
  NA_real_
}

extract_station_name <- function(names_list) {
  if (is.null(names_list) || length(names_list) == 0) return(NA_character_)
  types <- purrr::map_chr(names_list, ~ (.x$type %or% NA_character_))
  idx <- which(types == "NAME")
  if (length(idx) == 0) return(NA_character_)
  names_list[[idx[1]]]$value %or% NA_character_
}

extract_lat_lon <- function(data) {
  # Check geoData.coordinates (e-fuel files)
  if (!is.null(data$geoData) && !is.null(data$geoData$coordinates) && 
      length(data$geoData$coordinates) > 0) {
    lon <- data$geoData$coordinates[[1]]$longitude %or% NA_real_
    lat <- data$geoData$coordinates[[1]]$latitude %or% NA_real_
    return(c(lat, lon))
  }
  # Fallback to direct coordinates
  if (!is.null(data$coordinates) && length(data$coordinates) > 0) {
    lon <- data$coordinates[[1]]$longitude %or% NA_real_
    lat <- data$coordinates[[1]]$latitude %or% NA_real_
    return(c(lat, lon))
  }
  c(NA_real_, NA_real_)
}

# Extract charging point information
extract_charging_points <- function(charging_points_list) {
  if (is.null(charging_points_list) || length(charging_points_list) == 0) {
    return(list(
      total_points = 0,
      max_capacity_kw = NA_real_,
      min_capacity_kw = NA_real_,
      avg_capacity_kw = NA_real_,
      cost_types = NA_character_,
      payment_methods = NA_character_,
      plug_types = NA_character_,
      vehicle_types = NA_character_,
      roaming_available = NA
    ))
  }
  
  # Extract capacities
  capacities <- purrr::map_dbl(charging_points_list, ~ safe_as_numeric(.x$capacityKw))
  capacities <- capacities[!is.na(capacities)]
  
  # Extract cost types
  cost_types <- unique(purrr::map_chr(charging_points_list, ~ (.x$cost %or% "UNKNOWN")))
  
  # Extract payment methods
  all_payments <- unlist(purrr::map(charging_points_list, ~ (.x$payments %or% list())))
  payment_methods <- unique(all_payments)
  
  # Extract plug types
  all_plugs <- unlist(purrr::map(charging_points_list, ~ (.x$plugs %or% list())))
  plug_types <- unique(all_plugs)
  
  # Extract vehicle types
  all_vehicles <- unlist(purrr::map(charging_points_list, ~ (.x$vehicles %or% list())))
  vehicle_types <- unique(all_vehicles)
  
  # Check roaming availability
  roaming_flags <- purrr::map_lgl(charging_points_list, ~ (.x$roaming %or% FALSE))
  roaming_available <- any(roaming_flags, na.rm = TRUE)
  
  list(
    total_points = length(charging_points_list),
    max_capacity_kw = if (length(capacities) > 0) max(capacities) else NA_real_,
    min_capacity_kw = if (length(capacities) > 0) min(capacities) else NA_real_,
    avg_capacity_kw = if (length(capacities) > 0) mean(capacities) else NA_real_,
    cost_types = if (length(cost_types) > 0) paste(cost_types, collapse = ", ") else NA_character_,
    payment_methods = if (length(payment_methods) > 0) paste(payment_methods, collapse = ", ") else NA_character_,
    plug_types = if (length(plug_types) > 0) paste(plug_types, collapse = ", ") else NA_character_,
    vehicle_types = if (length(vehicle_types) > 0) paste(vehicle_types, collapse = ", ") else NA_character_,
    roaming_available = roaming_available
  )
}

# Extract price information from nested structure
extract_prices <- function(prices_list) {
  if (is.null(prices_list) || length(prices_list) == 0) {
    return(list(
      price_per_kwh = NA_real_,
      unit = NA_character_,
      price_last_updated = NA_character_,
      fuel_source = NA_character_
    ))
  }
  
  # Handle nested structure: prices[[i]]$prices[[j]]
  all_prices <- list()
  for (container in prices_list) {
    inner_prices <- container$prices %or% list()
    all_prices <- c(all_prices, inner_prices)
  }
  
  if (length(all_prices) == 0) {
    return(list(
      price_per_kwh = NA_real_,
      unit = NA_character_,
      price_last_updated = NA_character_,
      fuel_source = NA_character_
    ))
  }
  
  # Take the first price entry (most e-fuel stations have one price)
  price_entry <- all_prices[[1]]
  
  list(
    price_per_kwh = safe_as_numeric(price_entry$price),
    unit = price_entry$unit %or% NA_character_,
    price_last_updated = price_entry$lastUpdated %or% NA_character_,
    fuel_source = price_entry$fuelSource %or% NA_character_
  )
}

# ---------- Configuration ----------
json_dir   <- "~/Desktop/MBAT-Internship-Project/scraped_data/data_alt"
db_path    <- "~/Desktop/MBAT-Internship-Project/databases/oeamtc_efuel_data.duckdb"
table_name <- "oeamtc_efuel"

# ---------- Database Connection ----------
connect_duckdb <- function() {
  DBI::dbConnect(duckdb::duckdb(dbdir = path.expand(db_path)))
}

con <- connect_duckdb()
on.exit({
  if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
}, add = TRUE)

# ---------- Create Table with Unique Constraint ----------
create_table_if_not_exists <- function(table_name) {
  # Check connection validity first
  if (!DBI::dbIsValid(con)) {
    cat("🔄 Reconnecting to DuckDB...\n")
    con <<- connect_duckdb()
  }
  
  # Check if table exists
  if (!DBI::dbExistsTable(con, table_name)) {
    cat("📋 Creating table:", table_name, "\n")
    
    # Create table with proper schema
    create_sql <- paste0("
      CREATE TABLE ", table_name, " (
        station_id VARCHAR,
        station_name VARCHAR,
        station_address VARCHAR,
        station_brand VARCHAR,
        station_operator VARCHAR,
        station_phone VARCHAR,
        station_website VARCHAR,
        station_email VARCHAR,
        station_facilities VARCHAR,
        station_services VARCHAR,
        station_opening_hours VARCHAR,
        station_operation_modes VARCHAR,
        station_payment_methods VARCHAR,
        station_roaming_available BOOLEAN,
        station_green_energy BOOLEAN,
        station_free_parking BOOLEAN,
        station_active BOOLEAN,
        station_oeamtc_epower_station BOOLEAN,
        total_charging_points INTEGER,
        charging_point_ids VARCHAR,
        max_capacity_kw REAL,
        min_capacity_kw REAL,
        avg_capacity_kw REAL,
        cost_types VARCHAR,
        payment_methods VARCHAR,
        plug_types VARCHAR,
        vehicle_types VARCHAR,
        roaming_available BOOLEAN,
        price_per_kwh REAL,
        unit VARCHAR,
        price_last_updated VARCHAR,
        fuel_source VARCHAR,
        manually_approved BOOLEAN,
        last_imported VARCHAR,
        last_imported_human VARCHAR,
        entry_etag VARCHAR,
        station_lat REAL,
        station_lng REAL,
        file_source VARCHAR,
        PRIMARY KEY (station_id, file_source, last_imported)
      )
    ")
    
    DBI::dbExecute(con, create_sql)
    cat("✅ Table created successfully\n")
  } else {
    cat("📋 Table", table_name, "already exists\n")
  }
}

# Create table
create_table_if_not_exists(table_name)

# ---------- Find E-Fuel Files ----------
efuel_files <- fs::dir_ls(json_dir, regexp = "(^|/)e-fuel_.*\\.json$")

if (length(efuel_files) == 0) {
  stop("No e-fuel_*.json files found in: ", json_dir)
}

cat("Found", length(efuel_files), "e-fuel files to process\n")

# ---------- Main Processing Loop ----------
total_rows <- 0
processed_files <- 0
failed_files <- 0

for (file in efuel_files) {
  cat("\n📄 Processing:", basename(file), "\n")
  
  # Read and validate JSON
  raw <- try(jsonlite::fromJSON(file, simplifyVector = FALSE), silent = TRUE)
  
  if (inherits(raw, "try-error")) {
    cat("⚠️ Failed to read JSON:", basename(file), "\n")
    failed_files <- failed_files + 1
    next
  }
  
  # Check for error responses (like "Bad Request")
  if (!is.null(raw$status) && raw$status >= 400) {
    cat("⚠️ API error in file:", basename(file), "-", raw$detail %or% "Unknown error", "\n")
    failed_files <- failed_files + 1
    next
  }
  
  # Validate structure
  if (is.null(raw$results) || length(raw$results) == 0) {
    cat("⚠️ No results found in:", basename(file), "\n")
    failed_files <- failed_files + 1
    next
  }
  
  # Parse data
  df <- purrr::map_dfr(raw$results, function(entry) {
    data   <- entry$data %or% list()
    header <- data$header %or% list()
    entry_header <- entry$header %or% list()
    
    # Station information
    station_id           <- header$id %or% NA_character_
    station_name         <- extract_station_name(header$names %or% list())
    station_last_updated <- header$lastUpdated %or% NA_character_
    manually_approved    <- (header$quality$manuallyApproved %or% FALSE)
    categories           <- paste(header$categories %or% list(), collapse = ", ")
    oeamtc_epower_station <- (header$oeamtcEPowerStation %or% FALSE)
    
    # Import metadata
    last_imported        <- entry_header$lastImported %or% NA_character_
    last_imported_human  <- entry_header$lastImportedHuman %or% NA_character_
    entry_etag          <- entry_header$etag %or% NA_character_
    
    # Station characteristics
    green_energy         <- (data$greenEnergy %or% FALSE)
    free_parking         <- (data$freeParking %or% FALSE)
    station_active       <- (data$stationActive %or% FALSE)
    
    # Coordinates
    coords <- extract_lat_lon(data)
    lat <- suppressWarnings(as.numeric(coords[1]))
    lon <- suppressWarnings(as.numeric(coords[2]))
    
    # Charging points information
    charging_points <- data$chargingPoints %or% list()
    cp_info <- extract_charging_points(charging_points)
    
    # Price information
    prices <- data$prices %or% list()
    price_info <- extract_prices(prices)
    
    # Create comprehensive record
    tibble(
      # Station identifiers
      id                   = station_id,
      name                 = station_name,
      categories           = categories,
      
      # Price information
      price_per_kwh        = price_info$price_per_kwh,
      unit                 = price_info$unit,
      price_last_updated   = price_info$price_last_updated,
      fuel_source          = price_info$fuel_source,
      
      # Station metadata
      station_last_updated = station_last_updated,
      manually_approved    = manually_approved,
      last_imported        = last_imported,
      last_imported_human  = last_imported_human,
      entry_etag          = entry_etag,
      
      # Station characteristics
      green_energy         = green_energy,
      free_parking         = free_parking,
      station_active       = station_active,
      oeamtc_epower_station = oeamtc_epower_station,
      
      # Location
      latitude             = lat,
      longitude            = lon,
      
      # Charging infrastructure
      total_charging_points = cp_info$total_points,
      max_capacity_kw      = cp_info$max_capacity_kw,
      min_capacity_kw      = cp_info$min_capacity_kw,
      avg_capacity_kw      = cp_info$avg_capacity_kw,
      cost_types           = cp_info$cost_types,
      payment_methods      = cp_info$payment_methods,
      plug_types           = cp_info$plug_types,
      vehicle_types        = cp_info$vehicle_types,
      roaming_available    = cp_info$roaming_available,
      
      # File source
      file_source          = basename(file)
    )
  })
  
  # Write to database
  if (nrow(df) > 0) {
    # Check connection validity
    if (!DBI::dbIsValid(con)) {
      cat("🔄 Reconnecting to DuckDB...\n")
      con <<- connect_duckdb()
    }
    
    # Check for duplicates before inserting
    duplicate_count <- 0
    for (i in 1:nrow(df)) {
      # Check connection validity before each query
      if (!DBI::dbIsValid(con)) {
        cat("🔄 Reconnecting to DuckDB...\n")
        con <<- connect_duckdb()
      }
      
      row <- df[i, ]
      check_sql <- paste0("
        SELECT COUNT(*) as count FROM ", table_name, " 
        WHERE station_id = ? AND file_source = ? AND last_imported = ?
      ")
      result <- DBI::dbGetQuery(con, check_sql, params = list(
        row$station_id, row$file_source, row$last_imported
      ))
      if (result$count > 0) duplicate_count <- duplicate_count + 1
    }
    
    if (duplicate_count > 0) {
      cat("🔄 Found", duplicate_count, "existing rows, updating...\n")
    }
    
    # Write data (DuckDB will handle duplicates with PRIMARY KEY constraint)
    DBI::dbWriteTable(con, table_name, df, append = TRUE)
    cat("✅ Written", nrow(df), "rows to", table_name, "\n")
    total_rows <- total_rows + nrow(df)
    processed_files <- processed_files + 1
  } else {
    cat("⚠️ No data to write from:", basename(file), "\n")
    failed_files <- failed_files + 1
  }
}

# ---------- Summary ----------
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("📊 E-FUEL PROCESSING SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n")
cat("✅ Successfully processed:", processed_files, "files\n")
cat("⚠️ Failed files:", failed_files, "\n")
cat("📈 Total rows written:", total_rows, "\n")
cat("💾 Database location:", db_path, "\n")
cat("📋 Table name:", table_name, "\n")

# ---------- Verify Data ----------
if (DBI::dbIsValid(con)) {
  try({
    count_query <- paste("SELECT COUNT(*) as total_rows FROM", table_name)
    result <- DBI::dbGetQuery(con, count_query)
    cat("🔍 Total rows in database:", result$total_rows, "\n")
    
    # Show charging capacity distribution
    capacity_query <- paste("SELECT 
      CASE 
        WHEN max_capacity_kw <= 3 THEN 'Low (≤3kW)'
        WHEN max_capacity_kw <= 11 THEN 'Medium (4-11kW)'
        WHEN max_capacity_kw <= 22 THEN 'High (12-22kW)'
        ELSE 'Very High (>22kW)'
      END as capacity_range,
      COUNT(*) as count
      FROM", table_name, 
                            "WHERE max_capacity_kw IS NOT NULL 
      GROUP BY capacity_range 
      ORDER BY count DESC")
    capacity_dist <- DBI::dbGetQuery(con, capacity_query)
    if (nrow(capacity_dist) > 0) {
      cat("\n⚡ Charging capacity distribution:\n")
      print(capacity_dist)
    }
    
    # Show cost types
    cost_query <- paste("SELECT cost_types, COUNT(*) as count FROM", table_name, 
                        "WHERE cost_types IS NOT NULL GROUP BY cost_types ORDER BY count DESC")
    cost_dist <- DBI::dbGetQuery(con, cost_query)
    if (nrow(cost_dist) > 0) {
      cat("\n💰 Cost types:\n")
      print(cost_dist)
    }
    
    # Show plug types
    plug_query <- paste("SELECT plug_types, COUNT(*) as count FROM", table_name, 
                        "WHERE plug_types IS NOT NULL GROUP BY plug_types ORDER BY count DESC LIMIT 10")
    plug_dist <- DBI::dbGetQuery(con, plug_query)
    if (nrow(plug_dist) > 0) {
      cat("\n🔌 Top plug types:\n")
      print(plug_dist)
    }
    
    # Show station characteristics
    char_query <- paste("SELECT 
      SUM(CASE WHEN green_energy = true THEN 1 ELSE 0 END) as green_energy_stations,
      SUM(CASE WHEN free_parking = true THEN 1 ELSE 0 END) as free_parking_stations,
      SUM(CASE WHEN station_active = true THEN 1 ELSE 0 END) as active_stations,
      SUM(CASE WHEN roaming_available = true THEN 1 ELSE 0 END) as roaming_stations
      FROM", table_name)
    char_stats <- DBI::dbGetQuery(con, char_query)
    if (nrow(char_stats) > 0) {
      cat("\n🏢 Station characteristics:\n")
      print(char_stats)
    }
    
    # Show coordinate coverage
    coords_query <- paste("SELECT COUNT(*) as with_coords FROM", table_name, 
                          "WHERE latitude IS NOT NULL AND longitude IS NOT NULL")
    coords_result <- DBI::dbGetQuery(con, coords_query)
    cat("\n📍 Stations with coordinates:", coords_result$with_coords, "\n")
    
    # Show price statistics
    price_query <- paste("SELECT 
      COUNT(*) as stations_with_prices,
      AVG(price_per_kwh) as avg_price_per_kwh,
      MIN(price_per_kwh) as min_price_per_kwh,
      MAX(price_per_kwh) as max_price_per_kwh
      FROM", table_name, 
                         "WHERE price_per_kwh IS NOT NULL")
    price_stats <- DBI::dbGetQuery(con, price_query)
    if (nrow(price_stats) > 0) {
      cat("\n💵 Price statistics:\n")
      print(price_stats)
    }
    
  }, silent = TRUE)
}

cat("\n✅ Comprehensive e-fuel data parsing completed!\n")

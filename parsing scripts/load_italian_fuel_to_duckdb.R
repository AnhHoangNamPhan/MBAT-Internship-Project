# =============================================================================
# Italian Fuel Data Parser - Duplicate Prevention Version
# Parses fuel_region-*.json files from data_ita directory
# Prevents duplicates by using INSERT OR REPLACE
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

# Extract region number from filename
extract_region <- function(filename) {
  region_match <- regmatches(filename, regexpr("region-\\d+", filename))
  if (length(region_match) > 0) {
    return(as.numeric(gsub("region-", "", region_match)))
  }
  NA_real_
}

# ---------- Configuration ----------
json_dir   <- "~/Desktop/MBAT-Internship-Project/data_ita"
db_path    <- "~/Desktop/MBAT-Internship-Project/db/italian_fuel_data.duckdb"
table_name <- "italian_fuel"

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
        station_id REAL,
        station_name VARCHAR,
        station_address VARCHAR,
        station_brand VARCHAR,
        fuel_id REAL,
        fuel_name VARCHAR,
        fuel_price REAL,
        fuel_is_self BOOLEAN,
        insert_date VARCHAR,
        distance REAL,
        region_number REAL,
        center_lat REAL,
        center_lng REAL,
        station_lat REAL,
        station_lng REAL,
        file_source VARCHAR,
        PRIMARY KEY (station_id, fuel_id, insert_date, file_source)
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

# ---------- Find Italian Fuel Files ----------
fuel_files <- fs::dir_ls(json_dir, regexp = "(^|/)fuel_region-.*\\.json$")

if (length(fuel_files) == 0) {
  stop("No fuel_region-*.json files found in: ", json_dir)
}

cat("Found", length(fuel_files), "Italian fuel files to process\n")

# ---------- Main Processing Loop ----------
total_rows <- 0
processed_files <- 0
failed_files <- 0
duplicate_rows <- 0

for (file in fuel_files) {
  cat("\n📄 Processing:", basename(file), "\n")
  
  # Read and validate JSON
  raw <- try(jsonlite::fromJSON(file, simplifyVector = FALSE), silent = TRUE)
  
  if (inherits(raw, "try-error")) {
    cat("⚠️ Failed to read JSON:", basename(file), "\n")
    failed_files <- failed_files + 1
    next
  }
  
  # Check for success flag
  if (!isTRUE(raw$success)) {
    cat("⚠️ API returned success=false in:", basename(file), "\n")
    failed_files <- failed_files + 1
    next
  }
  
  # Validate structure
  if (is.null(raw$results) || length(raw$results) == 0) {
    cat("⚠️ No results found in:", basename(file), "\n")
    failed_files <- failed_files + 1
    next
  }
  
  # Extract metadata
  center_lat <- safe_as_numeric(raw$center$lat)
  center_lng <- safe_as_numeric(raw$center$lng)
  region_number <- extract_region(basename(file))
  
  # Parse data
  current_file <- basename(file)  # Capture file name at this point
  df <- purrr::map_dfr(raw$results, function(station) {
    # Station information
    station_id <- station$id %or% NA_real_
    station_name <- station$name %or% NA_character_
    station_address <- station$address %or% NA_character_
    station_brand <- station$brand %or% NA_character_
    insert_date <- station$insertDate %or% NA_character_
    distance <- safe_as_numeric(station$distance)
    
    # Location
    station_lat <- safe_as_numeric(station$location$lat)
    station_lng <- safe_as_numeric(station$location$lng)
    
    # Fuels
    fuels <- station$fuels %or% list()
    
    if (length(fuels) == 0) {
      # No fuels - skip this station
      return(tibble())
    }
    
    # Process each fuel entry
    purrr::map_dfr(fuels, function(fuel) {
      tibble(
        # Station identifiers
        station_id           = station_id,
        station_name         = station_name,
        station_address      = station_address,
        station_brand        = station_brand,
        
        # Fuel information
        fuel_id              = fuel$id %or% NA_real_,
        fuel_name            = fuel$name %or% NA_character_,
        fuel_price           = safe_as_numeric(fuel$price),
        fuel_is_self         = fuel$isSelf %or% NA,
        
        # Station metadata
        insert_date          = insert_date,
        distance             = distance,
        region_number        = region_number,
        center_lat           = center_lat,
        center_lng           = center_lng,
        
        # Location
        station_lat          = station_lat,
        station_lng          = station_lng,
        
        # File source
        file_source          = current_file
      )
    })
  })
  
  # Write to database with duplicate prevention
  if (nrow(df) > 0) {
    # Check connection validity
    if (!DBI::dbIsValid(con)) {
      cat("🔄 Reconnecting to DuckDB...\n")
      con <<- connect_duckdb()
    }
    
    # Use INSERT OR REPLACE to prevent duplicates
    tryCatch({
      # First, check how many rows would be duplicates
      existing_count <- 0
      for (i in 1:nrow(df)) {
        # Check connection validity before each query
        if (!DBI::dbIsValid(con)) {
          cat("🔄 Reconnecting to DuckDB...\n")
          con <<- connect_duckdb()
        }
        
        row <- df[i, ]
        check_sql <- paste0("
          SELECT COUNT(*) as count FROM ", table_name, " 
          WHERE station_id = ? AND fuel_id = ? AND insert_date = ? AND file_source = ?
        ")
        result <- DBI::dbGetQuery(con, check_sql, params = list(
          row$station_id, row$fuel_id, row$insert_date, row$file_source
        ))
        if (result$count > 0) existing_count <- existing_count + 1
      }
      
      if (existing_count > 0) {
        cat("🔄 Found", existing_count, "existing rows, updating...\n")
        duplicate_rows <- duplicate_rows + existing_count
      }
      
      # Insert or replace data using INSERT OR REPLACE
      for (i in 1:nrow(df)) {
        row <- df[i, ]
        insert_sql <- paste0("
          INSERT OR REPLACE INTO ", table_name, " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ")
        DBI::dbExecute(con, insert_sql, params = list(
          row$station_id, row$station_name, row$station_address, row$station_brand,
          row$fuel_id, row$fuel_name, row$fuel_price, row$fuel_is_self,
          row$insert_date, row$distance, row$region_number, row$center_lat, row$center_lng,
          row$station_lat, row$station_lng, row$file_source
        ))
      }
      cat("✅ Written", nrow(df), "rows to", table_name, "\n")
      total_rows <- total_rows + nrow(df)
      processed_files <- processed_files + 1
      
    }, error = function(e) {
      cat("❌ Error writing to database:", e$message, "\n")
      failed_files <- failed_files + 1
    })
  } else {
    cat("⚠️ No data to write from:", basename(file), "\n")
    failed_files <- failed_files + 1
  }
}

# ---------- Summary ----------
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("📊 ITALIAN FUEL PROCESSING SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n")
cat("✅ Successfully processed:", processed_files, "files\n")
cat("⚠️ Failed files:", failed_files, "\n")
cat("📈 Total rows written:", total_rows, "\n")
cat("🔄 Duplicate rows handled:", duplicate_rows, "\n")
cat("💾 Database location:", db_path, "\n")
cat("📋 Table name:", table_name, "\n")

# ---------- Verify Data ----------
if (DBI::dbIsValid(con)) {
  try({
    count_query <- paste("SELECT COUNT(*) as total_rows FROM", table_name)
    result <- DBI::dbGetQuery(con, count_query)
    cat("🔍 Total rows in database:", result$total_rows, "\n")
    
    # Show fuel types
    fuel_types_query <- paste("SELECT fuel_name, COUNT(*) as count FROM", table_name, 
                             "WHERE fuel_name IS NOT NULL GROUP BY fuel_name ORDER BY count DESC")
    fuel_types <- DBI::dbGetQuery(con, fuel_types_query)
    if (nrow(fuel_types) > 0) {
      cat("\n⛽ Fuel types found:\n")
      print(fuel_types)
    }
    
    # Show brands
    brands_query <- paste("SELECT station_brand, COUNT(*) as count FROM", table_name, 
                         "WHERE station_brand IS NOT NULL GROUP BY station_brand ORDER BY count DESC LIMIT 10")
    brands <- DBI::dbGetQuery(con, brands_query)
    if (nrow(brands) > 0) {
      cat("\n🏢 Top brands:\n")
      print(brands)
    }
    
    # Show regions
    regions_query <- paste("SELECT region_number, COUNT(*) as count FROM", table_name, 
                          "WHERE region_number IS NOT NULL GROUP BY region_number ORDER BY region_number")
    regions <- DBI::dbGetQuery(con, regions_query)
    if (nrow(regions) > 0) {
      cat("\n🗺️ Regions covered:\n")
      print(regions)
    }
    
    # Show price statistics
    price_query <- paste("SELECT 
      COUNT(*) as stations_with_prices,
      AVG(fuel_price) as avg_price,
      MIN(fuel_price) as min_price,
      MAX(fuel_price) as max_price
      FROM", table_name, 
      "WHERE fuel_price IS NOT NULL")
    price_stats <- DBI::dbGetQuery(con, price_query)
    if (nrow(price_stats) > 0) {
      cat("\n💵 Price statistics:\n")
      print(price_stats)
    }
    
    # Show coordinate coverage
    coords_query <- paste("SELECT COUNT(*) as with_coords FROM", table_name, 
                         "WHERE station_lat IS NOT NULL AND station_lng IS NOT NULL")
    coords_result <- DBI::dbGetQuery(con, coords_query)
    cat("\n📍 Stations with coordinates:", coords_result$with_coords, "\n")
    
  }, silent = TRUE)
}

cat("\n✅ Italian fuel data parsing completed!\n")

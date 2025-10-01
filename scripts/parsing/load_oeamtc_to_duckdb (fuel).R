# =============================================================================
# Comprehensive Fuel Data Parser for OEAMTC JSON Files
# Extracts all meaningful variables including facilities, opening hours, etc.
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
  # Check geoData.coordinates first (fuel files)
  if (!is.null(data$geoData) && !is.null(data$geoData$coordinates) && 
      length(data$geoData$coordinates) > 0) {
    lon <- data$geoData$coordinates[[1]]$longitude %or% NA_real_
    lat <- data$geoData$coordinates[[1]]$latitude %or% NA_real_
    return(c(lat, lon))
  }
  # Fallback to direct coordinates (e-fuel files)
  if (!is.null(data$coordinates) && length(data$coordinates) > 0) {
    lon <- data$coordinates[[1]]$longitude %or% NA_real_
    lat <- data$coordinates[[1]]$latitude %or% NA_real_
    return(c(lat, lon))
  }
  c(NA_real_, NA_real_)
}

# Extract opening hours as a summary string
extract_opening_hours <- function(openings_list) {
  if (is.null(openings_list) || length(openings_list) == 0) return(NA_character_)
  
  opening_hours <- openings_list[[1]]$openingHours %or% list()
  if (length(opening_hours) == 0) return(NA_character_)
  
  days <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
  hours_summary <- character()
  
  for (day in days) {
    if (!is.null(opening_hours[[day]]) && length(opening_hours[[day]]) > 0) {
      day_hours <- opening_hours[[day]][[1]]
      from <- day_hours$from %or% "closed"
      to <- day_hours$to %or% "closed"
      hours_summary <- c(hours_summary, paste0(substr(day, 1, 3), ":", from, "-", to))
    }
  }
  
  if (length(hours_summary) > 0) {
    return(paste(hours_summary, collapse = "; "))
  }
  NA_character_
}

# Extract payment options as comma-separated string
extract_payment_options <- function(facilities) {
  if (is.null(facilities$paymentOptions) || length(facilities$paymentOptions) == 0) {
    return(NA_character_)
  }
  
  payment_types <- purrr::map_chr(facilities$paymentOptions, ~ (.x$type %or% ""))
  payment_types <- payment_types[payment_types != ""]
  
  if (length(payment_types) > 0) {
    return(paste(payment_types, collapse = ", "))
  }
  NA_character_
}

# Extract services as comma-separated string
extract_services <- function(facilities) {
  if (is.null(facilities$services) || length(facilities$services) == 0) {
    return(NA_character_)
  }
  
  services <- facilities$services
  if (length(services) > 0) {
    return(paste(services, collapse = ", "))
  }
  NA_character_
}

# ---------- Configuration ----------
json_dir   <- "~/Desktop/MBAT-Internship-Project/data_alt"
db_path    <- "~/Desktop/MBAT-Internship-Project/db/oeamtc_fuel_data_comprehensive.duckdb"
table_name <- "oeamtc_fuel_comprehensive"

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
        fuel_type VARCHAR,
        fuel_price REAL,
        fuel_unit VARCHAR,
        fuel_last_updated VARCHAR,
        fuel_source VARCHAR,
        manually_approved BOOLEAN,
        last_imported VARCHAR,
        last_imported_human VARCHAR,
        entry_etag VARCHAR,
        station_lat REAL,
        station_lng REAL,
        file_source VARCHAR,
        PRIMARY KEY (station_id, file_source, last_imported, fuel_type)
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

# ---------- Find Fuel Files ----------
fuel_files <- fs::dir_ls(json_dir, regexp = "(^|/)fuel_.*\\.json$")

if (length(fuel_files) == 0) {
  stop("No fuel_*.json files found in: ", json_dir)
}

cat("Found", length(fuel_files), "fuel files to process\n")

# ---------- Main Processing Loop ----------
total_rows <- 0
processed_files <- 0
failed_files <- 0

for (file in fuel_files) {
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
    
    # Import metadata
    last_imported        <- entry_header$lastImported %or% NA_character_
    last_imported_human  <- entry_header$lastImportedHuman %or% NA_character_
    entry_etag          <- entry_header$etag %or% NA_character_
    
    # Operator information
    operator <- data$operator %or% list()
    company_name <- operator$companyName %or% NA_character_
    branch_name  <- operator$branchName %or% NA_character_
    phone        <- operator$phone %or% NA_character_
    url          <- operator$url %or% NA_character_
    
    # Coordinates
    coords <- extract_lat_lon(data)
    lat <- suppressWarnings(as.numeric(coords[1]))
    lon <- suppressWarnings(as.numeric(coords[2]))
    
    # Facilities and services
    facilities <- data$facilities %or% list()
    payment_options <- extract_payment_options(facilities)
    services <- extract_services(facilities)
    
    # Opening hours
    openings <- data$openings %or% list()
    opening_hours <- extract_opening_hours(openings)
    
    # Prices - handle nested structure
    prices <- data$prices %or% list()
    
    if (length(prices) == 0) {
      # No prices - return station info only
      return(tibble(
        # Station identifiers
        id                   = station_id,
        name                 = station_name,
        
        # Price information
        fuel_type            = NA_character_,
        price                = NA_real_,
        unit                 = NA_character_,
        price_last_updated   = NA_character_,
        fuel_source          = NA_character_,
        
        # Station metadata
        station_last_updated = station_last_updated,
        manually_approved    = manually_approved,
        last_imported        = last_imported,
        last_imported_human  = last_imported_human,
        entry_etag          = entry_etag,
        
        # Operator information
        company_name         = company_name,
        branch_name          = branch_name,
        phone                = phone,
        url                  = url,
        
        # Location
        latitude             = lat,
        longitude            = lon,
        
        # Facilities and services
        payment_options      = payment_options,
        services             = services,
        opening_hours        = opening_hours,
        
        # File source
        file_source          = basename(file)
      ))
    }
    
    # Process nested price structure: data$prices[[i]]$prices[[j]]
    purrr::map_dfr(prices, function(price_container) {
      inner_prices <- price_container$prices %or% list()
      if (length(inner_prices) == 0) return(tibble())
      
      purrr::map_dfr(inner_prices, function(p) {
        tibble(
          # Station identifiers
          id                   = station_id,
          name                 = station_name,
          
          # Price information
          fuel_type            = p$fuel %or% NA_character_,
          price                = safe_as_numeric(p$price),
          unit                 = p$unit %or% NA_character_,
          price_last_updated   = p$lastUpdated %or% NA_character_,
          fuel_source          = p$fuelSource %or% NA_character_,
          
          # Station metadata
          station_last_updated = station_last_updated,
          manually_approved    = manually_approved,
          last_imported        = last_imported,
          last_imported_human  = last_imported_human,
          entry_etag          = entry_etag,
          
          # Operator information
          company_name         = company_name,
          branch_name          = branch_name,
          phone                = phone,
          url                  = url,
          
          # Location
          latitude             = lat,
          longitude            = lon,
          
          # Facilities and services
          payment_options      = payment_options,
          services             = services,
          opening_hours        = opening_hours,
          
          # File source
          file_source          = basename(file)
        )
      })
    })
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
        WHERE station_id = ? AND file_source = ? AND last_imported = ? AND fuel_type = ?
      ")
      result <- DBI::dbGetQuery(con, check_sql, params = list(
        row$station_id, row$file_source, row$last_imported, row$fuel_type
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
cat("📊 PROCESSING SUMMARY\n")
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
    
    # Show sample of fuel types
    fuel_types_query <- paste("SELECT fuel_type, COUNT(*) as count FROM", table_name, 
                              "WHERE fuel_type IS NOT NULL GROUP BY fuel_type ORDER BY count DESC")
    fuel_types <- DBI::dbGetQuery(con, fuel_types_query)
    if (nrow(fuel_types) > 0) {
      cat("\n📊 Fuel types found:\n")
      print(fuel_types)
    }
    
    # Show sample of companies
    companies_query <- paste("SELECT company_name, COUNT(*) as count FROM", table_name, 
                             "WHERE company_name IS NOT NULL GROUP BY company_name ORDER BY count DESC LIMIT 10")
    companies <- DBI::dbGetQuery(con, companies_query)
    if (nrow(companies) > 0) {
      cat("\n🏢 Top companies:\n")
      print(companies)
    }
    
    # Show sample of services
    services_query <- paste("SELECT services, COUNT(*) as count FROM", table_name, 
                            "WHERE services IS NOT NULL GROUP BY services ORDER BY count DESC LIMIT 10")
    services <- DBI::dbGetQuery(con, services_query)
    if (nrow(services) > 0) {
      cat("\n🛠️ Top services:\n")
      print(services)
    }
    
    # Show coordinate coverage
    coords_query <- paste("SELECT COUNT(*) as with_coords FROM", table_name, 
                          "WHERE latitude IS NOT NULL AND longitude IS NOT NULL")
    coords_result <- DBI::dbGetQuery(con, coords_query)
    cat("\n📍 Stations with coordinates:", coords_result$with_coords, "\n")
    
  }, silent = TRUE)
}

cat("\n✅ Comprehensive fuel data parsing completed!\n")

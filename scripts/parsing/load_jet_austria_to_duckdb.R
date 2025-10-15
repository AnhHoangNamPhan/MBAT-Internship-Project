# Load necessary libraries
library(jsonlite)
library(dplyr)
library(tibble)
library(DBI)
library(duckdb)
library(fs)

# ---------- Configuration ----------
data_dir <- "/Users/alexphan/Desktop/MBAT-Internship-Project/scraped_data/data_jet_at"
db_dir <- "/Users/alexphan/Desktop/MBAT-Internship-Project/db"
db_path <- file.path(db_dir, "jet_austria_fuel_data.duckdb")
table_name <- "jet_austria_fuel"

# Ensure database directory exists
if (!dir.exists(db_dir)) {
  dir.create(db_dir, recursive = TRUE)
}

# Function to connect to DuckDB
connect_duckdb <- function() {
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
}

# Initialize connection
con <- connect_duckdb()

# Custom operator for null handling
`%or%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.atomic(a) && all(is.na(a)))) {
    b
  } else {
    a
  }
}

# Helper function to safely convert to numeric
safe_as_numeric <- function(x) {
  if (is.null(x) || length(x) == 0) {
    NA_real_
  } else {
    as.numeric(x)
  }
}

# Helper function to extract date from filename
extract_date_from_filename <- function(filename) {
  # Expected format: jet_prices_YYYY-MM-DD.json
  # Extract YYYY-MM-DD
  date_match <- regmatches(filename, regexec("jet_prices_(\\d{4}-\\d{2}-\\d{2})\\.json", filename))
  if (length(date_match[[1]]) > 1) {
    return(date_match[[1]][2])
  }
  NA_character_
}

# ---------- Create Table with Unique Constraint ----------
create_table_if_not_exists <- function(table_name) {
  # Check connection validity first
  if (!DBI::dbIsValid(con)) {
    cat(" Reconnecting to DuckDB...\n")
    con <<- connect_duckdb()
  }
  
  # Check if table exists
  if (!DBI::dbExistsTable(con, table_name)) {
    cat(" Creating table:", table_name, "\n")
    
    # Create table with proper schema
    create_sql <- paste0("
      CREATE TABLE ", table_name, " (
        station_id VARCHAR,
        station_name VARCHAR,
        operator VARCHAR,
        street VARCHAR,
        postcode VARCHAR,
        city VARCHAR,
        website VARCHAR,
        lat REAL,
        lng REAL,
        phone VARCHAR,
        opening_mon VARCHAR,
        opening_tue VARCHAR,
        opening_wed VARCHAR,
        opening_thu VARCHAR,
        opening_fri VARCHAR,
        opening_sat VARCHAR,
        opening_sun VARCHAR,
        has_lpg INTEGER,
        has_cng INTEGER,
        fuel_type VARCHAR,
        fuel_price REAL,
        last_fuel_price_update VARCHAR,
        file_date VARCHAR,
        file_source VARCHAR,
        PRIMARY KEY (station_id, fuel_type, file_date)
      )
    ")
    
    DBI::dbExecute(con, create_sql)
    cat(" Table created successfully\n")
  } else {
    cat("˜‘ Table", table_name, "already exists\n")
  }
}

# Call function to create table if it doesn't exist
create_table_if_not_exists(table_name)

# Get list of all JSON files
json_files <- list.files(data_dir, pattern = "\\.json$", full.names = TRUE)

if (length(json_files) == 0) {
  stop("No JSON files found in the specified directory: ", data_dir)
}

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("€ Starting JET Austria Fuel Data Parsing and Loading\n")
cat(paste(rep("=", 50), collapse=""), "\n\n")

total_rows <- 0
processed_files <- 0
failed_files <- 0

# Process each JSON file
for (file in json_files) {
  cat("Processing:", basename(file), "\n")
  
  # Capture file name early to prevent scope issues
  current_file <- basename(file)
  file_date <- extract_date_from_filename(current_file)

  # Read and validate JSON
  raw <- try(jsonlite::fromJSON(file, simplifyVector = FALSE), silent = TRUE)

  if (inherits(raw, "try-error") || is.null(raw) || length(raw) == 0) {
    cat("Œ Error reading or parsing JSON from", basename(file), "\n")
    failed_files <- failed_files + 1
    next
  }

  # Extract data from each station
  all_stations <- list()
  station_counter <- 0
  
  for (station in raw) {
    station_counter <- station_counter + 1
    
    # Extract station information
    station_id <- station$id %or% NA_character_
    station_name <- station$name %or% NA_character_
    operator <- station$operator %or% NA_character_
    street <- station$street %or% NA_character_
    postcode <- station$postcode %or% NA_character_
    city <- station$city %or% NA_character_
    website <- station$website %or% NA_character_
    lat <- safe_as_numeric(station$lat)
    lng <- safe_as_numeric(station$lng)
    phone <- station$phone %or% NA_character_
    has_lpg <- station$hasLpg %or% 0
    has_cng <- station$hasCng %or% 0
    last_fuel_price_update <- station$lastFuelPriceUpdate %or% NA_character_
    
    # Extract opening times
    opening_times <- station$openingTimes %or% list()
    opening_mon <- opening_times$mon %or% NA_character_
    opening_tue <- opening_times$tue %or% NA_character_
    opening_wed <- opening_times$wed %or% NA_character_
    opening_thu <- opening_times$thu %or% NA_character_
    opening_fri <- opening_times$fri %or% NA_character_
    opening_sat <- opening_times$sat %or% NA_character_
    opening_sun <- opening_times$sun %or% NA_character_

    # Extract fuel prices
    fuel_prices <- station$fuelPrices %or% list()
    
    # Process each fuel type
    if (length(fuel_prices) > 0) {
      for (fuel_type in names(fuel_prices)) {
        price <- safe_as_numeric(fuel_prices[[fuel_type]])
        
        # Only include rows with valid prices
        if (!is.na(price)) {
          station_row <- list(
            station_id = station_id,
            station_name = station_name,
            operator = operator,
            street = street,
            postcode = postcode,
            city = city,
            website = website,
            lat = lat,
            lng = lng,
            phone = phone,
            opening_mon = opening_mon,
            opening_tue = opening_tue,
            opening_wed = opening_wed,
            opening_thu = opening_thu,
            opening_fri = opening_fri,
            opening_sat = opening_sat,
            opening_sun = opening_sun,
            has_lpg = has_lpg,
            has_cng = has_cng,
            fuel_type = fuel_type,
            fuel_price = price,
            last_fuel_price_update = last_fuel_price_update,
            file_date = file_date,
            file_source = current_file
          )
          all_stations[[length(all_stations) + 1]] <- station_row
        }
      }
    }
  }

  # Convert to data frame
  if (length(all_stations) > 0) {
    df <- do.call(rbind, lapply(all_stations, function(x) {
      data.frame(x, stringsAsFactors = FALSE)
    }))
    
    # Insert or replace data using INSERT OR REPLACE
    tryCatch({
      for (i in 1:nrow(df)) {
        row <- df[i, ]
        insert_sql <- paste0("
          INSERT OR REPLACE INTO ", table_name, " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        ")
        DBI::dbExecute(con, insert_sql, params = list(
          row$station_id, row$station_name, row$operator, row$street,
          row$postcode, row$city, row$website, row$lat, row$lng,
          row$phone, row$opening_mon, row$opening_tue, row$opening_wed,
          row$opening_thu, row$opening_fri, row$opening_sat, row$opening_sun,
          row$has_lpg, row$has_cng, row$fuel_type, row$fuel_price,
          row$last_fuel_price_update, row$file_date, row$file_source
        ))
      }
      cat(" Written", nrow(df), "rows to", table_name, "\n")
      total_rows <- total_rows + nrow(df)
      processed_files <- processed_files + 1
    }, error = function(e) {
      cat("Œ Error writing to database for", basename(file), ":", e$message, "\n")
      failed_files <- failed_files + 1
    })
  } else {
    cat(" No valid data to write from", basename(file), "\n")
  }
}

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat(" PROCESSING SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n")
cat(" Files processed successfully:", processed_files, "\n")
cat("Œ Files failed:", failed_files, "\n")
cat(" Total files:", length(json_files), "\n")
cat(" Total rows written:", total_rows, "\n")

# Optional: Query some statistics
cat("\nˆ Price statistics for JET Austria fuel:\n")
price_query <- paste0("
  SELECT
    fuel_type,
    COUNT(DISTINCT station_id) AS num_stations,
    AVG(fuel_price) AS avg_price,
    MIN(fuel_price) AS min_price,
    MAX(fuel_price) AS max_price
  FROM ", table_name, "
  WHERE fuel_price IS NOT NULL
  GROUP BY fuel_type
  ORDER BY fuel_type
")
price_stats <- DBI::dbGetQuery(con, price_query)
if (nrow(price_stats) > 0) {
  print(price_stats)
} else {
  cat("No price data available.\n")
}

# Disconnect from DuckDB
DBI::dbDisconnect(con, shutdown = TRUE)
cat("\nDisconnected from DuckDB. Database saved to:", db_path, "\n")

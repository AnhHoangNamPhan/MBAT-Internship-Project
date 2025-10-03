# ---------- minimal dependencies ----------
library(jsonlite)
library(dplyr)
library(tibble)
library(duckdb)
library(fs)
library(purrr)

# ---------- fallback operator ----------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------- paths ----------
geojson_dir <- "~/Desktop/MBAT-Internship-Project/data"
db_path     <- "~/Desktop/MBAT-Internship-Project/databases/arboe_fuel_prices.duckdb"
table_name  <- "arboe_prices"

# ---------- setup ----------
dir_create(dirname(db_path))
con <- dbConnect(duckdb(db_path))

# ---------- Create Table with Unique Constraint ----------
create_table_if_not_exists <- function(table_name) {
  # Check connection validity first
  if (!dbIsValid(con)) {
    cat("🔄 Reconnecting to DuckDB...\n")
    con <<- dbConnect(duckdb(db_path))
  }
  
  # Check if table exists
  if (!dbExistsTable(con, table_name)) {
    cat("📋 Creating table:", table_name, "\n")
    
    # Create table with proper schema
    create_sql <- paste0("
      CREATE TABLE ", table_name, " (
        id VARCHAR,
        name VARCHAR,
        fuel_s98 REAL,
        fuel_s95 REAL,
        fuel_n REAL,
        fuel_d REAL,
        fuel_g REAL,
        zip VARCHAR,
        land VARCHAR,
        bundesland VARCHAR,
        city VARCHAR,
        city2 VARCHAR,
        strasse VARCHAR,
        date VARCHAR,
        time VARCHAR,
        file_source VARCHAR,
        lon REAL,
        lat REAL,
        PRIMARY KEY (id, file_source, date, time)
      )
    ")
    
    dbExecute(con, create_sql)
    cat("✅ Table created successfully\n")
  } else {
    cat("📋 Table", table_name, "already exists\n")
  }
}

# Create table
create_table_if_not_exists(table_name)

# ---------- list all geojson files ----------
geojson_files <- dir_ls(geojson_dir, glob = "*.geojson")

# ---------- helper: safely extract lon/lat ----------
extract_coordinates <- function(geometry) {
  if (!is.list(geometry) || is.null(geometry$coordinates)) {
    return(tibble(lon = NA_real_, lat = NA_real_))
  }
  
  coords <- geometry$coordinates
  if (!is.numeric(coords[[1]]) || !is.numeric(coords[[2]])) {
    return(tibble(lon = NA_real_, lat = NA_real_))
  }
  
  tibble(
    lon = as.numeric(coords[[1]]),
    lat = as.numeric(coords[[2]])
  )
}

# ---------- process each file ----------
for (file in geojson_files) {
  cat("📂 Processing:", file, "\n")
  
  data <- try(fromJSON(file, simplifyVector = FALSE), silent = TRUE)
  if (inherits(data, "try-error") || is.null(data$features)) {
    cat("⚠️ Failed to read or no features in", file, "\n")
    next
  }
  
  features <- data$features
  if (!is.list(features) || length(features) == 0) {
    cat("⚠️ No valid features in", file, "\n")
    next
  }
  
  # Map all features into a tibble
  df <- map_dfr(features, function(feature) {
    props <- feature$properties %||% list()
    geom  <- feature$geometry %||% list()
    
    coords <- extract_coordinates(geom)
    
    tibble(
      id          = props$id         %||% NA_character_,
      name        = props$name       %||% NA_character_,
      fuel_s98    = as.numeric(props$S98 %||% NA),
      fuel_s95    = as.numeric(props$S95 %||% NA),
      fuel_n      = as.numeric(props$N   %||% NA),
      fuel_d      = as.numeric(props$D   %||% NA),
      fuel_g      = as.numeric(props$G   %||% NA),
      zip         = props$zip        %||% NA_character_,
      land        = props$land       %||% NA_character_,
      bundesland  = props$bundesland %||% NA_character_,
      city        = props$city       %||% NA_character_,
      city2       = props$city2      %||% NA_character_,
      strasse     = props$strasse    %||% NA_character_,
      date        = props$date       %||% NA_character_,
      time        = props$time       %||% NA_character_,
      file_source = basename(file),
      lon         = coords$lon,
      lat         = coords$lat
    )
  })
  
  if (nrow(df) > 0) {
    # Check for duplicates before inserting
    duplicate_count <- 0
    for (i in 1:nrow(df)) {
      # Check connection validity before each query
      if (!dbIsValid(con)) {
        cat("🔄 Reconnecting to DuckDB...\n")
        con <<- dbConnect(duckdb(db_path))
      }
      
      row <- df[i, ]
      check_sql <- paste0("
        SELECT COUNT(*) as count FROM ", table_name, " 
        WHERE id = ? AND file_source = ? AND date = ? AND time = ?
      ")
      result <- dbGetQuery(con, check_sql, params = list(
        row$id, row$file_source, row$date, row$time
      ))
      if (result$count > 0) duplicate_count <- duplicate_count + 1
    }
    
    if (duplicate_count > 0) {
      cat("🔄 Found", duplicate_count, "existing rows, updating...\n")
    }
    
    # Insert data (DuckDB will handle duplicates with PRIMARY KEY constraint)
    dbWriteTable(con, table_name, df, append = TRUE)
    cat("✅ Written", nrow(df), "rows to", table_name, "\n")
  } else {
    cat("⚠️ Skipping write: no valid rows in", basename(file), "\n")
  }
}

# ---------- done ----------
dbDisconnect(con)
cat("✅ All files loaded into DuckDB at:", db_path, "\n")
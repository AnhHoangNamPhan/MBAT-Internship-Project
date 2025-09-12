# ---------- minimal dependencies ----------
library(jsonlite)
library(dplyr)
library(tibble)
library(duckdb)
library(fs)
library(purrr)

# ---------- paths ----------
geojson_dir <- "~/Desktop/MBAT-Internship-Project/data"
db_path     <- "~/Desktop/MBAT-Internship-Project/db/arboe_fuel_prices.duckdb"
table_name  <- "arboe_prices"

# ---------- setup ----------
dir_create(dirname(db_path))

# ---------- connect ----------
con <- dbConnect(duckdb(db_path))

# ---------- list all geojson files ----------
geojson_files <- dir_ls(geojson_dir, glob = "*.geojson")

# ---------- helper: safely extract lon/lat ----------
extract_coordinates <- function(geometry) {
  if (!is.list(geometry) || is.null(geometry$coordinates)) {
    return(tibble(lon = NA_real_, lat = NA_real_))
  }
  
  coords <- geometry$coordinates
  if (!is.numeric(coords) || length(coords) != 2) {
    return(tibble(lon = NA_real_, lat = NA_real_))
  }
  
  tibble(
    lon = as.numeric(coords[[1]]),
    lat = as.numeric(coords[[2]])
  )
}

# ---------- load and append each file ----------
for (file in geojson_files) {
  cat("📂 Processing:", file, "\n")
  
  data <- try(fromJSON(file), silent = TRUE)
  if (inherits(data, "try-error")) {
    cat("⚠️ Failed to read", file, "\n")
    next
  }
  
  features <- data$features
  if (is.null(features) || length(features) == 0) {
    cat("⚠️ No features in", file, "\n")
    next
  }
  
  # Build final data frame
  df <- map_dfr(seq_along(features), function(i) {
    feature <- features[[i]]
    
    # Ensure feature is a list and has required elements
    if (!is.list(feature) || is.null(feature$properties)) {
      return(tibble())
    }
    
    props <- feature$properties
    coords <- extract_coordinates(feature$geometry)
    
    tibble(
      id          = props$id,
      name        = props$name,
      fuel_s98    = as.numeric(props$S98),
      fuel_s95    = as.numeric(props$S95),
      fuel_n      = as.numeric(props$N),
      fuel_d      = as.numeric(props$D),
      fuel_g      = as.numeric(props$G),
      zip         = props$zip,
      land        = props$land,
      bundesland  = props$bundesland,
      city        = props$city,
      city2       = props$city2,
      strasse     = props$strasse,
      date        = props$date,
      time        = props$time,
      file_source = basename(file),
      lon         = coords$lon,
      lat         = coords$lat
    )
  })
  
  # ✅ WRITE to DuckDB
  if (nrow(df) > 0) {
    dbWriteTable(con, table_name, df, append = TRUE)
  } else {
    cat("⚠️ Skipping write: data frame is empty for", file, "\n")
  }
}

# ---------- done ----------
dbDisconnect(con)
cat("✅ All files loaded into DuckDB at:", db_path, "\n")
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
db_path     <- "~/Desktop/MBAT-Internship-Project/db/arboe_fuel_prices.duckdb"
table_name  <- "arboe_prices"

# ---------- setup ----------
dir_create(dirname(db_path))
con <- dbConnect(duckdb(db_path))

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
    dbWriteTable(con, table_name, df, append = TRUE)
  } else {
    cat("⚠️ Skipping write: no valid rows in", basename(file), "\n")
  }
}

# ---------- done ----------
dbDisconnect(con)
cat("✅ All files loaded into DuckDB at:", db_path, "\n")
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

# ---------- 1. Get all JET station public IDs in Austria ----------
url <- "https://www.jet-austria.at/api/v1/stations_within_bounds"

response <- request(url) |>
  req_url_query(
    n = 52.646204153791714,
    e = 23.134118968085264,
    s = 42.227821354168384,
    w = 3.557481031914733
  ) |>
  req_headers(
    accept = "application/json, text/javascript, */*; q=0.01",
    referer = "https://www.jet-austria.at/tankstellen-suche"
  ) |>
  req_options(ssl_verifypeer = FALSE) |>
  req_perform()

# Parse JSON to get station IDs
data <- response |> resp_body_json()
public_ids <- sapply(data$stations, function(x) x$publicId)

# ---------- 2. Fetch detailed info for each station ----------
base_url <- "https://www.jet-austria.at/api/stations/id"

get_station_info <- function(station_id) {
  station_url <- paste0(base_url, "/", station_id)
  message("📡 Fetching: ", station_url)
  
  tryCatch({
    res <- request(station_url) |>
      req_headers(
        accept = "application/json, text/javascript, */*; q=0.01",
        referer = "https://www.jet-austria.at/tankstellen-suche"
      ) |>
      req_options(ssl_verifypeer = FALSE) |>
      req_perform()
    
    json <- res |> resp_body_json()
    return(json)
  }, error = function(e) {
    message("❌ Failed for ID: ", station_id)
    return(NULL)
  })
}

cat("⛽ Fetching all station details...\n")
station_details <- map(public_ids, get_station_info)

# Filter out failed ones
station_details <- Filter(Negate(is.null), station_details)

# ---------- 3. Save to JSON ----------
if (length(station_details) > 0) {
  today <- format(Sys.Date(), "%Y-%m-%d")
  file_name <- paste0("jet_prices_", today, ".json")
  write_json(station_details, file_name, pretty = TRUE, auto_unbox = TRUE)
  cat("✅ Saved to file:", file_name, "\n")
} else {
  cat("⚠️ No data collected. Nothing saved.\n")
}
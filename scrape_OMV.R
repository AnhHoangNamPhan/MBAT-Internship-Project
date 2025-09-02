# ---------- minimal dependencies ----------
library(jsonlite)
library(stringr)
library(tesseract)
library(base64enc)
library(readr)
library(tibble)
library(httr)
library(fs)
library(dplyr)

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a

# ---------- configuration ----------
station_list_csv <- "~/Desktop/MBAT-Internship-Project/data_OMV_at/stations_AUT.csv"
out_dir          <- "~/Desktop/MBAT-Internship-Project/data_OMV_at"
csv_path         <- file.path(out_dir, "prices_AUT.csv")
dir_create(path.expand(out_dir))

country_iso <- "AUT"
lang        <- "DE"
eng         <- tesseract(c("deu", "eng"))

SLEEP_SEC   <- 0.35
TIMEOUT_SEC <- 10

# ---------- load stations ----------
stations <- read_csv(station_list_csv, show_col_types = FALSE)
station_ids <- stations$station_id

# ---------- loop through stations ----------
for (station_id in station_ids) {
  stamp     <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  resp_path <- file.path(out_dir, sprintf("response_%s_%s.json", station_id, stamp))
  png_path  <- file.path(out_dir, sprintf("prices_%s_%s.png", station_id, stamp))
  
  # ---- send POST request ----
  res <- tryCatch({
    POST(
      "https://app.wigeogis.com/kunden/omv/data/details.php",
      add_headers(
        "content-type" = "application/x-www-form-urlencoded",
        "referer"      = "https://www.omv.at/"
      ),
      body = list(
        LNG = lang,
        CTRISO = country_iso,
        VEHICLE = "",
        MODE = "",
        BRAND = "",
        ID = station_id,
        DISTANCE = "0"
      ),
      encode = "form",
      timeout(TIMEOUT_SEC)
    )
  }, error = function(e) NULL)
  
  if (is.null(res) || http_error(res)) {
    message("Failed request for station: ", station_id)
    next
  }
  
  writeBin(content(res, "raw"), path.expand(resp_path))
  raw_txt <- read_file(path.expand(resp_path))
  raw_txt <- gsub("\\\\", "", raw_txt)
  b64 <- str_extract(raw_txt, "data:image/png;base64,[A-Za-z0-9+/=]+")
  if (is.na(b64)) {
    message("No image in station: ", station_id)
    next
  }
  
  b64_clean <- sub("^data:image/png;base64,", "", b64)
  writeBin(base64decode(b64_clean), path.expand(png_path))
  
  # ---- parse metadata ----
  j <- tryCatch(fromJSON(path.expand(resp_path)), error = function(e) NULL)
  if (is.null(j)) next
  
  sid      <- j$siteDetails$sid        %||% station_id
  town     <- j$siteDetails$town_l     %||% NA_character_
  postcode <- j$siteDetails$postcode   %||% NA_character_
  lat      <- as.numeric(j$siteDetails$y_coordinates %||% NA)
  lon      <- as.numeric(j$siteDetails$x_coordinates %||% NA)
  
  # ---- run OCR ----
  txt       <- ocr(path.expand(png_path), engine = eng)
  lines     <- unlist(strsplit(txt, "\n"))
  eur_lines <- grep("EUR", lines, value = TRUE)
  
  num  <- stringr::str_match(eur_lines, "([0-9]+[\\.,][0-9]+)\\s*EUR")[,2]
  num  <- as.numeric(gsub(",", ".", num))
  fuel <- stringr::str_trim(gsub("\\s*[0-9]+[\\.,][0-9]+\\s*EUR.*", "", eur_lines))
  
  result <- tibble(
    station_id    = sid,
    country_iso   = country_iso,
    town          = town,
    postcode      = postcode,
    lat           = lat,
    lon           = lon,
    fuel          = fuel,
    price_eur     = num,
    raw_line      = eur_lines,
    ocr_text_path = png_path,
    response_path = resp_path,
    scraped_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  
  # ---- save to CSV ----
  if (file_exists(csv_path)) {
    write_csv(result, csv_path, append = TRUE)
  } else {
    write_csv(result, csv_path)
  }
  
  message("✓ Scraped ", sid, " — found ", nrow(result), " prices")
  Sys.sleep(SLEEP_SEC)
}
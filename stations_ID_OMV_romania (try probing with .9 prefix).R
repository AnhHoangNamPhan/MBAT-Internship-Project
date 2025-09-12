# ---- minimal deps ----
library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(stringr)

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a

# ---- CONFIG ----
country_code <- "RO"  # Change to "RS" for Serbia
lang         <- "RO"  # Or "SR" for Serbia if applicable

# ✨ Output CSV matches original file
out_csv <- path.expand(sprintf("~/Desktop/MBAT-Internship-Project/data_OMV_%s/stations_%s.csv", 
                               tolower(country_code), country_code))

dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)

# Set probing range
SLEEP_SEC   <- 0.35
TIMEOUT_SEC <- 10
CHUNK_SIZE  <- 500
START_ID    <- 1
END_ID      <- 9999

# ---- probe one station ID ----
probe_one <- function(id_str) {
  res <- tryCatch({
    POST(
      "https://app.wigeogis.com/kunden/omv/data/details.php",
      add_headers(
        "content-type" = "application/x-www-form-urlencoded",
        "referer"      = "https://www.omv.at/"
      ),
      body = list(LNG = lang, CTRISO = country_code,
                  VEHICLE = "", MODE = "", BRAND = "",
                  ID = id_str, DISTANCE = "0"),
      encode = "form",
      timeout(TIMEOUT_SEC)
    )
  }, error = function(e) return(structure(list(error = TRUE), class = "list")))
  
  if (isTRUE(res$error) || http_error(res)) return("error")
  
  txt <- rawToChar(content(res, "raw"))
  txt <- gsub("\\\\", "", txt)
  if (!grepl("data:image/png;base64,", txt, fixed = TRUE)) return(NULL)
  
  j <- tryCatch(fromJSON(txt), error = function(e) NULL)
  tibble::tibble(
    station_id = j$siteDetails$sid          %||% id_str,
    brand_id   = j$siteDetails$brand_id     %||% NA_character_,
    town       = j$siteDetails$town_l       %||% NA_character_,
    postcode   = j$siteDetails$postcode     %||% NA_character_,
    lat        = suppressWarnings(as.numeric(j$siteDetails$y_coordinates %||% NA)),
    lon        = suppressWarnings(as.numeric(j$siteDetails$x_coordinates %||% NA))
  )
}

# ---- probe a chunk with logging ----
probe_range <- function(from_id, to_id) {
  # ✨ Change suffix to .9
  ids <- sprintf("%s.%04d.9", country_code, from_id:to_id)
  
  # Skip already-saved station IDs
  if (file.exists(out_csv)) {
    saved <- tryCatch(read_csv(out_csv, show_col_types = FALSE)$station_id, error = function(e) character(0))
    ids   <- setdiff(ids, saved)
  }
  
  n_total <- length(ids)
  if (!n_total) {
    message(sprintf("Chunk %d-%d: nothing to do (already saved).", from_id, to_id))
    return(invisible(NULL))
  }
  
  n_attempt <- 0L; n_valid <- 0L; n_error <- 0L
  buffer <- vector("list", 0)
  FLUSH_EVERY <- 25
  
  message(sprintf("Chunk %d-%d: %d candidates", from_id, to_id, n_total))
  t0 <- Sys.time()
  
  for (id in ids) {
    Sys.sleep(SLEEP_SEC)
    n_attempt <- n_attempt + 1L
    rec <- probe_one(id)
    
    if (identical(rec, "error")) {
      n_error <- n_error + 1L
      next
    }
    if (!is.null(rec)) {
      n_valid <- n_valid + 1L
      buffer[[length(buffer) + 1L]] <- rec
      if (length(buffer) >= FLUSH_EVERY) {
        df <- dplyr::bind_rows(buffer)
        if (file.exists(out_csv)) write_csv(df, out_csv, append = TRUE) else write_csv(df, out_csv)
        buffer <- vector("list", 0)
      }
    }
  }
  
  # final flush
  if (length(buffer)) {
    df <- dplyr::bind_rows(buffer)
    if (file.exists(out_csv)) write_csv(df, out_csv, append = TRUE) else write_csv(df, out_csv)
  }
  
  # remove any duplicates
  if (file.exists(out_csv)) {
    st <- read_csv(out_csv, show_col_types = FALSE) |>
      distinct(station_id, .keep_all = TRUE)
    write_csv(st, out_csv)
    total_saved <- nrow(st)
  } else {
    total_saved <- 0L
  }
  
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  message(sprintf(
    "Chunk %d-%d done in %ss | attempts=%d, valid=%d, errors=%d | total_saved=%d",
    from_id, to_id, dt, n_attempt, n_valid, n_error, total_saved
  ))
}

# ---- driver: run in chunks ----
ranges <- seq(START_ID, END_ID, by = CHUNK_SIZE)
for (from_id in ranges) {
  to_id <- min(from_id + CHUNK_SIZE - 1L, END_ID)
  probe_range(from_id, to_id)
}
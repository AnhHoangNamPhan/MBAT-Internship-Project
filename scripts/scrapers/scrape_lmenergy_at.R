#!/usr/bin/env Rscript

# LM Energy Austria Data Scraper (ChatGPT Version - Improved)
# This script scrapes complete LM Energy data including prices and locations
# from a single API call using the correct JSON structure
# 
# Run this script daily to get complete, up-to-date data
# Usage: Rscript scrape_lmenergy_at.R

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)
library(readr)

# Set up logging
setup_logging <- function() {
  # Create logs directory
  log_dir <- "~/Desktop/MBAT-Internship-Project/cache_lmenergy_at"
  dir.create(path.expand(log_dir), recursive = TRUE, showWarnings = FALSE)
  
  # Create log file with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(path.expand(log_dir), paste0("lm_energy_daily_", timestamp, ".log"))
  
  # Set up logging to both console and file
  log_con <- file(log_file, "w")
  
  # Function to log messages
  log_message <- function(msg) {
    timestamp_msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", msg)
    cat(timestamp_msg, "\n")
    cat(timestamp_msg, "\n", file = log_con)
    flush(log_con)
  }
  
  return(list(log_con = log_con, log_message = log_message, log_file = log_file))
}

# Helper function for null coalescing
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Main scraping function
scrape_lm_energy_complete <- function(
    url = "https://www.lm-energy.at/wp-json/wp/v2/pages?slug=tankstellennetz",
    out_csv = paste0("~/Desktop/MBAT-Internship-Project/scraped_data/data_lm_at/lm_energy_at_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
) {
  dir.create(dirname(path.expand(out_csv)), recursive = TRUE, showWarnings = FALSE)
  
  # Fetch JSON
  resp <- httr::GET(url, timeout(15))
  stop_for_status(resp)
  data <- httr::content(resp, as = "text", encoding = "UTF-8")
  json <- fromJSON(data, simplifyVector = FALSE)
  
  # Extract stations from the correct structure
  page <- json[[1]]
  webblocks <- page$acf$webblocks
  
  stations <- list()
  
  for (block in webblocks) {
    if (block$acf_fc_layout == "stations_map") {
      for (p in block$posts) {
        prices <- p$prices
        if (!is.null(prices)) {
          last_update <- as.numeric(prices$last_update)
          last_update_iso <- as_datetime(last_update, tz = "UTC")
          
          for (entry in prices$data) {
            # Skip entries with no price data
            if (is.null(entry$value) || entry$value == "" || entry$label == "Keine Preisdaten verfügbar") {
              next
            }
            
            stations <- append(stations, list(
              tibble(
                scrape_time = Sys.time(),
                station_id = p$id %||% NA_integer_,
                station_name = p$name %||% NA_character_,
                station_nr = p$acf$station_nr %||% NA_character_,
                address = p$acf$location$address %||% NA_character_,
                latitude = p$acf$location$lat %||% NA_real_,
                longitude = p$acf$location$lng %||% NA_real_,
                city = p$acf$location$city %||% NA_character_,
                state = p$acf$location$state %||% NA_character_,
                postal_code = p$acf$location$post_code %||% NA_character_,
                country = p$acf$location$country %||% NA_character_,
                fuel_type = entry$label %||% NA_character_,
                price_eur = as.numeric(gsub(",", ".", entry$value)) %||% NA_real_,
                last_update = last_update_iso,
                # Additional metadata
                station_type = p$acf$type$name %||% NA_character_,
                phone = p$acf$phone %||% NA_character_,
                email = p$acf$email %||% NA_character_,
                status = p$acf$status %||% NA_character_
              )
            ))
          }
        }
      }
    }
  }
  
  if (length(stations) == 0) {
    message("❌ No station data found")
    return(invisible(NULL))
  }
  
  df <- bind_rows(stations)
  
  # Write to CSV
  write_csv(df, path.expand(out_csv))
  
  message("✅ LM Energy: scraped ", nrow(df), " price records from ", length(unique(df$station_name)), " stations")
  message("📁 Output saved to: ", path.expand(out_csv))
  
  return(df)
}

# Main execution function
run_daily_scrape <- function() {
  # Set up logging
  logger <- setup_logging()
  log_msg <- logger$log_message
  log_con <- logger$log_con
  log_file <- logger$log_file
  
  start_time <- Sys.time()
  log_msg("🚀 Starting LM Energy Data Scrape (ChatGPT Version)")
  log_msg(paste("📁 Log file:", log_file))
  log_msg(paste("📊 Project directory:", path.expand("~/Desktop/MBAT-Internship-Project")))
  log_msg("")
  
  tryCatch({
    # Run scraping
    complete_data <- scrape_lm_energy_complete()
    
    end_time <- Sys.time()
    duration <- round(as.numeric(end_time - start_time, units = "mins"), 2)
    
    log_msg("")
    log_msg("✅ Daily scrape completed successfully!")
    log_msg(paste("📁 Data saved to:", path.expand(paste0("~/Desktop/MBAT-Internship-Project/scraped_data/data_lm_at/lm_energy_at_", format(Sys.Date(), "%Y-%m-%d"), ".csv"))))
    log_msg(paste("📋 Log saved to:", log_file))
    
    # Show file sizes
    output_file <- path.expand(paste0("~/Desktop/MBAT-Internship-Project/scraped_data/data_lm_at/lm_energy_at_", format(Sys.Date(), "%Y-%m-%d"), ".csv"))
    if (file.exists(output_file)) {
      file_size <- file.info(output_file)$size
      file_size_kb <- round(file_size / 1024, 2)
      log_msg("")
      log_msg("📊 File sizes:")
      log_msg(paste("  ", basename(output_file), ":", file_size_kb, "KB"))
    } else {
      log_msg("❌ Output file not found")
    }
    
    log_msg("")
    log_msg("🎉 LM Energy data collection complete!")
    log_msg(paste("📊 Final dataset:", nrow(complete_data), "records,", ncol(complete_data), "columns"))
    log_msg(paste("⏱️  Completed in:", duration, "minutes"))
    
    # Show sample data
    log_msg("")
    log_msg("📋 Sample data:")
    print(head(complete_data, 3))
    
    close(log_con)
    return(complete_data)
    
  }, error = function(e) {
    log_msg("")
    log_msg("❌ Daily scrape failed!")
    log_msg(paste("📋 Check log file:", log_file))
    log_msg(paste("🚨 Error:", e$message))
    close(log_con)
    stop(e)
  })
}

# Run the daily scrape
if (!interactive()) {
  run_daily_scrape()
}
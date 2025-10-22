  
  library("rvest")
  library("xml2")
  
  setwd("~/Desktop/MBAT-Internship-Project/")
  dir.create("log_files/cache", FALSE, recursive = TRUE)
  dir.create("scraped_data/data_arboe_at", FALSE, recursive = TRUE)
  
  id <- format(Sys.time(), "%Y-%m-%d_%H:%M")
  link <- "https://www.arboe.at/leistungen/spritpreis-und-e-tankstellenfinder/spritpreise-oesterreich"
  
  # Read the page and acquire the scripts ---
  page <- read_html(link)
  xml2::write_xml(page, paste0("log_files/cache/page-", id, ".xml"))
# Get scripts by element
  scripts <- page %>% html_elements("script")
  # Get the script including the JSON and save it as JS
  scripts[grep("json", scripts)] %>%
    html_text() %>%
    writeLines(paste0("log_files/cache/script-", id, ".js"))
  
  # Parse the JS script ---
  js <- readLines(paste0("log_files/cache/script-", id, ".js"))
  # Get the boundaries of the JSON (in a brute way)
  start <- grep("json:", js) # The start
  end <- grep("^}", js) # The end
  # end2 <- grep("stations:", js)[1L] - 2L
  
  # Store as GeoJSON
  result <- try(
    c("{", js[seq(start + 1, end)]) %>%
      writeLines(paste0("scraped_data/data_arboe_at/prices-", id, ".geojson")),
      silent = TRUE
  )
  if(inherits(result, "try-error")) {
    # 1. Try a different way
    # 2. Save the cache
    # 3. Send a mail to complain
  } else {
    # Report via push to Uptime Kuma
    curl::curl_fetch_memory("http://kuma.kuschnig.eu/api/push/QVF9SujRDm?status=up&msg=OK&ping=")
  }
  
  # ZIP up the files ---
  zip(paste0("scraped_data/data_arboe_at/prices-", id, "_geojson.zip"), paste0("scraped_data/data_arboe_at/prices-", id, ".geojson"))
  file.remove(paste0("scraped_data/data_arboe_at/prices-", id, ".geojson"))
  
  # ZIP up all files in the cache
  files_cache <- list.files("log_files/cache", ".[xml|js]$")
  zip(paste0("log_files/cache/cache-", id, ".zip"), paste0("log_files/cache/", files_cache))
  file.remove(paste0("log_files/cache/", files_cache))
  
  # Read the JSON
  # c("{", js[seq(start + 1, end)]) %>%
  #   jsonlite::fromJSON()

library(httr)
library(jsonlite)
library(rvest)
library(stringr)
library(dplyr)
library(readr)

scrape_lm_energy <- function(
    url = "https://www.lm-energy.at/wp-json/wp/v2/page/tankstellennetz",
    out_csv = "~/Desktop/MBAT-Internship-Project/data_AT/lm_energy_prices.csv"
) {
  dir.create(dirname(path.expand(out_csv)), recursive = TRUE, showWarnings = FALSE)
  
  res <- GET(url, timeout(15))
  stop_for_status(res)
  j <- content(res, "parsed", type = "application/json")
  
  # CASE A: ACF/custom fields with structured data
  # (Uncomment/adjust if you see these fields when you inspect `str(j)`.)
  # if (!is.null(j$acf)) {
  #   df <- tibble::as_tibble(j$acf$stations) |>
  #     mutate(scraped_at = Sys.time())
  #   readr::write_csv(df, path.expand(out_csv), append = file.exists(path.expand(out_csv)))
  #   return(df)
  # }
  
  # CASE B: Parse the HTML inside content.rendered
  html <- read_html(j$content$rendered %||% "")
  # grab text blocks; LM’s page often lists stations and prices in a single markup block
  text <- html %>% html_text2()
  
  # Find lines that look like “Diesel 1,699 €” / “Super 95 1,739 €”, etc.
  # Tolerate dot or comma decimal; allow optional € / EUR
  lines <- unlist(strsplit(text, "\n"))
  lines <- str_squish(lines)
  lines <- lines[nzchar(lines)]
  
  # crude extractor: pick lines that contain a fuel keyword + a price
  fuel_pat <- "(Diesel|Super\\s*95|Super\\s*Plus\\s*98|Autogas|E10|B7|B\\d+)"
  price_pat <- "([0-9]+[\\.,][0-9]{2})\\s*(€|EUR)?"
  hits <- lines[str_detect(lines, fuel_pat) & str_detect(lines, price_pat)]
  
  if (!length(hits)) {
    message("LM Energy: no price-looking lines found — inspect the page structure or Network tab.")
    return(invisible(NULL))
  }
  
  df <- tibble(
    raw = hits,
    fuel = str_match(hits, fuel_pat)[,2] %>% str_trim(),
    price = str_match(hits, price_pat)[,2] %>% str_replace(",", ".") %>% as.numeric(),
    scraped_at = Sys.time()
  )
  
  write_csv(df, path.expand(out_csv), append = file.exists(path.expand(out_csv)))
  df
}

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a


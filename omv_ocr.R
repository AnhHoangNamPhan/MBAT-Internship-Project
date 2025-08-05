library("tesseract")

# OCR the image
text <- ocr("stuff/omv_image.png", engine = tesseract("deu"))

# Method 1: Extract all prices using gsub
prices <- gsub(
  ".*?([0-9]+\\.[0-9]+) EUR.*",
  "\\1",
  grep("[0-9]+\\.[0-9]+ EUR", strsplit(text, "\n")[[1]], value = TRUE)
)
prices

# Method 2: More robust - find lines with EUR, then extract numbers
eur_lines <- grep("EUR", strsplit(text, "\n")[[1]], value = TRUE)
price_values <- as.numeric(gsub(".*?([0-9]+\\.[0-9]+).*", "\\1", eur_lines))
price_values

# Method 3: Extract fuel names and prices together
fuel_lines <- grep("[A-Za-z].*[0-9]+\\.[0-9]+ EUR", strsplit(text, "\n")[[1]], value = TRUE)
# Clean fuel names (remove price part)
fuel_names <- gsub("\\s+[0-9]+\\.[0-9]+ EUR.*", "", fuel_lines)
# Extract just the prices
fuel_prices <- as.numeric(gsub(".*?([0-9]+\\.[0-9]+) EUR.*", "\\1", fuel_lines))


# OMV Scraper

- Scraping OMV data is slightly more involved than the others
- However, we can get a lot of prices this way:
    - OMV Romania and Serbia also provide prices in this way – just use `ID=RO.1176.9` or `RS.2410.8`
    - Avanti also works this way, e.g. `ID=AT.9003.8`
    - As does Hofer Diskont, e.g. `ID=AT.4012.8`
- The steps are:
    - We need to get a bunch of station IDs
        - It looks like they follow `AT.[0-9]{4}.8` – it's not clear whether we can cleanly obtain the four digits though (same for `RS` and `RO`).
    - Then we can request info with the ID
    - We need OCR to obtain prices from the info (encoded image)

###### Steps
We can get info via:
```bash
curl 'https://app.wigeogis.com/kunden/omv/data/details.php' \
     -H 'content-type: application/x-www-form-urlencoded' \
     -H 'referer: https://www.omv.at/' \
     --data-raw 'LNG=DE&CTRISO=AUT&VEHICLE=&MODE=&BRAND=&ID=AT.3542.8&DISTANCE=0'
 ```
which returns some info and a base64 encoded PNG with prices.

<details>
<summary>The prices are stored in the PNG – that needs to be decoded and read.</summary>
 The returned value escapes `\/` – if we remove that, we can split off the `data:image/png;base64` part of the encoded string into and export in R via `base64decode(clean_base64) |> writeBin("image.png")`. The data then looks like:

```
data:image/png;base64,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
```
</details>

We can use OCR to obtain the prices. Here's an LLM draft:

```{r}
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
```

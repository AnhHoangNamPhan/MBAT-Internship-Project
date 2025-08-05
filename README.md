
# Tankerkaiser Fuel Prices

Database of fuel prices, focused on Central Europe.

## Main tasks

- [ ] Obtain more complete fuel price data (companies, countries)
- [ ] Create a usable database out of the scraped data

## Background

- German data is readily available in full detail, provided by [Tankerkönig](https://dev.azure.com/tankerkoenig/tankerkoenig-data)
- Austrian partial data by *E-Control* is provided by ARBÖ and ÖAMTC
  - Current prices of the five cheapest stations per region
    - [ ] Some companies provide their own data – obtain that
  - There is historical data (text) per state on [spritvergleich.at](https://archiv.spritvergleich.at)
    -[ ] Obtaining this should also be useful
  - Scraped via `scrape_arboe.R` (into `data`) and `scrape_oeamtc.sh` (into `data_alt`)
    - Scraping both helps with redundancy
    - ÖAMTC also features other data, including e-fuel
       - [ ] Should also check other data that would be available
- Italian data may also be truncated (similar to Austria)
  - [ ] Check how complete the data is
  - Scraped via `scrape_italia.sh` into `data_ita`
- Slovenian data should be complete
  - Scraped via `scrape_slovenia.sh` into `data_slo`
- Other countries haven't been straightforward
  - Switzerland ([see](https://www.comparis.ch/benzin-preise)) doesn't seem to have good data
  - Same for Czechia, Slovakia, Hungary, ...
  
## Reading the data

- Data is scraped and stored more or less directly (as compressed JSON)
- Usually, the JSON contains a geospatial point location – but directly reading as a proper GeoJSON doesn't work
- A terrible attempt at collating files is in `read.R`

## Company data

### Austria

[WKO has infos](https://www.wko.at/oe/industrie/mineraloelindustrie/tankstellenstatistik) on the Austrian market.

- [ ] Eni doesn't seem to provide prices – double-check that
- [ ] [Shell](https://www.shell.at/tanken/shell-tankstellensuche.html) should be relatively easy to obtain
- [ ] [BP](https://www.bp.com/de_at/austria/home/produkte-und-services/bp-in-ihrer-naehe.html) should be relatively easy to obtain
- [ ] OMV, Avanti, and Diskont have the same format with images that can be read
- [ ] [Jet](https://www.jet-austria.at/tankstellen/klagenfurt/rosentalerstrasse-102) should work, but may be tedious
- [ ] [Avia](https://www.avia.at/tanken), [LM](https://www.lm-energy.at/tankstellennetz/) and [Rudolf](https://www.rudolf-ag.at/tankstelle-mischzapfsaeule.php) (only 1) should work
- [ ] Turmöl, Genol (Lagerhaus), SOCAR, Rumpold, Leitner, IQ don't seem to provide prices – double-check that
- [ ] Are there any others that may be relevant?

#### Notes on Scraping

- Shell and BP are similar
    - Shell locations (and IDs) in two ways:
        - Within bounds: `https://shellgsllocator.geoapp.me/api/v2/locations/within_bounds?sw%5B%5D=46.083518&sw%5B%5D=13.291695&ne%5B%5D=48.769586&ne%5B%5D=16.565621&locale=de_AT&format=json&driving_distances=false`
        - Nearest to: `https://shellgsllocator.geoapp.me/api/v2/locations/nearest_to?lat=48.164233&lng=16.390912&limit=50&locale=de_AT&format=json&driving_distances=false`
        - With a given ID we can get more info (including, often, prices – note the ISO codes):
          - `https://find.shell.com/at/fuel/10022649.json`
          - `https://find.shell.com/it/fuel/10126758.json`
  - BP IDs from (note the similarity):
    - `https://bpretaillocator.geoapp.me/api/v2/locations/within_bounds?sw%5B%5D=48.160166&sw%5B%5D=16.330328&ne%5B%5D=48.199427&ne%5B%5D=16.373072&locale=de_AT&format=json`
    - Prices from `https://bpretaillocator.geoapp.me/api/v2/locations/AT-754?locale=de_AT&format=json`
- Jet
    - Given an ID, we can get a JSON: `https://www.jet-austria.at/api/stations/id/579dd833ff`
    - We can probably just scrape the ~156 station IDs from `https://www.jet-austria.at/tankstellen-suche`
- LM Energy
    - We can get that from `https://www.lm-energy.at/wp-json/wp/v2/page/tankstellennetz`
- Avia
    - The prices are in `https://www.avia.at/tanken/tankstellenfinder-diesel-super-95-super-plus-98-biofrei-48638.html`
- OMV, Avanti, and Diskont are slightly more involved, but work in the same way – see [here](info/omv_scraper.md)

# Regional Mapping Functions for Fuel Price Analysis
# Maps stations to broad regions for Germany, Austria, and Slovenia

library(dplyr)

# =============================================================================
# GERMANY - 4 REGIONS (Postal Code Based)
# =============================================================================

map_german_region <- function(zip_code) {
  if (is.na(zip_code) || zip_code == "") return(NA_character_)
  
  first_digit <- substr(zip_code, 1, 1)
  case_when(
    first_digit %in% c("2", "3") ~ "North",      # Schleswig-Holstein, Hamburg, Lower Saxony
    first_digit %in% c("4", "5", "6") ~ "West",  # NRW, Rhineland-Palatinate
    first_digit %in% c("0", "7") ~ "Central",     # Saxony, Thuringia, Baden-Württemberg
    first_digit %in% c("1", "8", "9") ~ "South",  # Brandenburg/Berlin, Bavaria, Franken
    TRUE ~ NA_character_  # No Unknown category - return NA for unmapped
  )
}

# Vectorized version for applying to columns
map_german_region_vec <- Vectorize(map_german_region)

# =============================================================================
# AUSTRIA - 3 REGIONS (Priority-Based: State > City > Longitude)
# =============================================================================

# State to region mapping
map_austrian_state_to_region <- function(state) {
  if (is.na(state) || state == "") return(NA_character_)
  
  case_when(
    state %in% c("Burgenland", "Niederösterreich", "Wien") ~ "East",
    state %in% c("Oberösterreich", "Salzburg", "Steiermark") ~ "Central",
    state %in% c("Tirol", "Vorarlberg", "Kärnten") ~ "West",
    TRUE ~ NA_character_
  )
}

# Comprehensive city to region mapping
map_austrian_city_to_region <- function(city) {
  if (is.na(city) || city == "") return(NA_character_)
  
  # Clean city name (remove extra spaces, convert to lowercase)
  clean_city <- tolower(trimws(city))
  
  # EAST REGION CITIES
  east_cities <- c(
    # Wien (Vienna)
    "wien", "vienna", "innere stadt", "leopoldstadt", "landstraße", "wieden", 
    "margareten", "mariahilf", "neubau", "josefstadt", "alsergrund", "favoriten",
    "simmering", "meidling", "hietzing", "penzing", "rudolfsheim-fünfhaus", 
    "ottakring", "hernals", "währing", "döbling", "brigittenau", "floridsdorf",
    "donaustadt", "liesing",
    
    # Niederösterreich (Lower Austria)
    "st. pölten", "sankt pölten", "st pölten", "sankt poelten", "st poelten",
    "wiener neustadt", "klosterneuburg", "baden", "mödling", "schwechat",
    "korneuburg", "neunkirchen", "amstetten", "kapfenberg", "oberwart",
    "hollabrunn", "tulln", "wolkersdorf", "krems", "stockerau", "mistelbach",
    "gänserndorf", "bruck an der leitha", "korneuburg", "hollabrunn",
    
    # Burgenland
    "eisensadt", "neusiedl am see", "oberwart", "mattersburg", "güssing",
    "jennersdorf", "oberpullendorf", "neusiedl", "rust", "podersdorf"
  )
  
  # CENTRAL REGION CITIES
  central_cities <- c(
    # Oberösterreich (Upper Austria)
    "linz", "wels", "steyr", "leonding", "traun", "ansfelden", "leonding",
    "braunau am inn", "freistadt", "gmunden", "grieskirchen", "kirchdorf",
    "linz-land", "perg", "ried im innkreis", "rohrbach", "schärding",
    "steyr-land", "urfahr-umgebung", "vöcklabruck", "wels-land",
    
    # Salzburg
    "salzburg", "hallein", "saalfelden", "zell am see", "bischofshofen",
    "st. johann im pongau", "sankt johann im pongau", "st johann im pongau",
    "tamsweg", "mittersill", "neukirchen", "oberndorf", "st. gilgen",
    "sankt gilgen", "st gilgen", "bad hofgastein", "bad gastein",
    
    # Steiermark (Styria)
    "graz", "leoben", "kapfenberg", "bruck an der mur", "knaben",
    "leibnitz", "liezen", "murau", "murtal", "südoststeiermark",
    "voitsberg", "weiz", "hartberg-fürstenfeld", "deutschlandsberg",
    "feldbach", "gratkorn", "judenburg", "kindberg", "knittelfeld"
  )
  
  # WEST REGION CITIES
  west_cities <- c(
    # Tirol
    "innsbruck", "kufstein", "kitzbühel", "st. johann in tirol", 
    "sankt johann in tirol", "st johann in tirol", "wörgl", "hall in tirol",
    "imst", "landeck", "reutte", "schwaz", "kitzbühel", "mayrhofen",
    "seefeld", "st. anton am arlberg", "sankt anton am arlberg",
    "st anton am arlberg", "ischgl", "sölden", "zillertal",
    
    # Vorarlberg
    "bregenz", "dornbirn", "feldkirch", "hohenems", "lauterach",
    "hard", "götzis", "bludenz", "rankweil", "lauterach", "wolfurt",
    "hohenems", "altach", "meiningen", "röthis", "sulz", "weiler",
    
    # Kärnten (Carinthia)
    "klagenfurt", "villach", "spittal an der drau", "wolfsberg",
    "völkermarkt", "st. veit an der glan", "sankt veit an der glan",
    "st veit an der glan", "feldkirchen", "hermagor", "st. veit",
    "sankt veit", "st veit", "velden", "pörtschach", "millstatt"
  )
  
  # Check which region the city belongs to
  if (clean_city %in% east_cities) {
    return("East")
  } else if (clean_city %in% central_cities) {
    return("Central")
  } else if (clean_city %in% west_cities) {
    return("West")
  } else {
    return(NA_character_)  # Return NA for unmapped cities
  }
}

# Longitude to region mapping (fallback)
map_austrian_longitude_to_region <- function(longitude) {
  if (is.na(longitude)) return(NA_character_)
  
  case_when(
    longitude < 13.5 ~ "West",      # Western Austria (Tirol, Vorarlberg, western Kärnten)
    longitude <= 15.0 ~ "Central",  # Central Austria (Oberösterreich, Salzburg, Steiermark)
    longitude > 15.0 ~ "East",      # Eastern Austria (Wien, Niederösterreich, Burgenland)
    TRUE ~ NA_character_
  )
}

# Main Austrian mapping function (Priority-based)
map_austrian_region <- function(state, city, longitude) {
  # Priority 1: State mapping
  state_region <- map_austrian_state_to_region(state)
  if (!is.na(state_region)) {
    return(state_region)
  }
  
  # Priority 2: City mapping
  city_region <- map_austrian_city_to_region(city)
  if (!is.na(city_region)) {
    return(city_region)
  }
  
  # Priority 3: Longitude mapping (100% coverage)
  return(map_austrian_longitude_to_region(longitude))
}

# Vectorized versions
map_austrian_region_vec <- Vectorize(map_austrian_region)

# =============================================================================
# SLOVENIA - 3 REGIONS (Longitude Based)
# =============================================================================

map_slovenian_region <- function(longitude) {
  if (is.na(longitude)) return(NA_character_)
  
  case_when(
    longitude < 14.5 ~ "West_Slovenia",
    longitude <= 15.5 ~ "Central_Slovenia",
    longitude > 15.5 ~ "East_Slovenia",
    TRUE ~ NA_character_
  )
}

# Vectorized version
map_slovenian_region_vec <- Vectorize(map_slovenian_region)

# =============================================================================
# SUMMARY FUNCTIONS
# =============================================================================

# Get regional mapping summary
get_regional_mapping_summary <- function(stations_df) {
  summary <- stations_df %>%
    group_by(country, region) %>%
    summarise(
      station_count = n(),
      .groups = 'drop'
    ) %>%
    arrange(country, region)
  
  return(summary)
}

# Check mapping coverage
check_mapping_coverage <- function(stations_df) {
  coverage <- stations_df %>%
    group_by(country) %>%
    summarise(
      total_stations = n(),
      mapped_stations = sum(!is.na(region)),
      unmapped_stations = sum(is.na(region)),
      coverage_percent = round(mapped_stations / total_stations * 100, 2),
      .groups = 'drop'
    )
  
  return(coverage)
}

cat("Regional mapping functions loaded successfully!\n")
cat("Available functions:\n")
cat("- map_german_region() - 4 regions based on postal codes\n")
cat("- map_austrian_region() - 3 regions (State > City > Longitude priority)\n")
cat("- map_slovenian_region() - 3 regions based on longitude\n")
cat("- get_regional_mapping_summary() - Get mapping statistics\n")
cat("- check_mapping_coverage() - Check coverage percentages\n")

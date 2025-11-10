#!/usr/bin/env Rscript

# Overview: generate a station coverage map for Germany, Austria, and Slovenia
# Steps:
#   1. Locate project root and DuckDB warehouse.
#   2. Pull station coordinates (lat/lon) for the three countries.
#   3. Download and load GADM level-1 boundaries for state outlines (if unavailable locally).
#   4. Convert station points to sf, create country centroids for labels.
#   5. Plot: solid country borders, dashed state borders, colored station points, and country labels.
#   6. Save to analysis/presentation/visualizations/station_distribution_map.png.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
})

find_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..", "../../../..")
  for (cand in candidates) {
    candidate_db <- file.path(cand, "databases", "stations.duckdb")
    if (file.exists(candidate_db)) {
      return(normalizePath(cand))
    }
  }
  stop("Unable to locate project root containing databases/stations.duckdb")
}

enhance_gadm_boundaries <- function(iso_code, data_dir) {
  file_path <- file.path(data_dir, sprintf("gadm41_%s_1.geojson", iso_code))
  if (!file.exists(file_path)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    url <- sprintf("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_%s_1.json", iso_code)
    message(sprintf("Downloading GADM admin layer for %s...", iso_code))
    download.file(url, destfile = file_path, mode = "wb", quiet = TRUE)
  }
  st_read(file_path, quiet = TRUE)
}

project_root <- find_project_root()
stations_db <- file.path(project_root, "databases", "stations.duckdb")
data_dir <- file.path(project_root, "analysis", "presentation", "data")
output_dir <- file.path(project_root, "analysis", "presentation", "visualizations")
output_path <- file.path(output_dir, "station_distribution_map.png")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== Generating station distribution map ===\n")

con <- dbConnect(duckdb(), stations_db, read_only = TRUE)
stations_df <- dbGetQuery(con, "
  SELECT station_uuid, country, latitude, longitude
  FROM stations
  WHERE country IN ('Germany','Austria','Slovenia')
    AND latitude IS NOT NULL AND longitude IS NOT NULL
")
dbDisconnect(con, shutdown = TRUE)

if (nrow(stations_df) == 0) {
  stop("No station coordinates were retrieved. Check the stations.duckdb database.")
}

stations_sf <- st_as_sf(stations_df, coords = c("longitude", "latitude"), crs = 4326)

countries_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name %in% c("Germany", "Austria", "Slovenia"))

admin_sf <- bind_rows(
  enhance_gadm_boundaries("DEU", data_dir),
  enhance_gadm_boundaries("AUT", data_dir),
  enhance_gadm_boundaries("SVN", data_dir)
)

country_labels <- countries_sf %>%
  mutate(label_lon = st_coordinates(st_centroid(geometry))[,1],
         label_lat = st_coordinates(st_centroid(geometry))[,2]) %>%
  st_drop_geometry()

station_map <- ggplot() +
  geom_sf(data = countries_sf, fill = "grey97", color = "black", size = 1) +
  geom_sf(data = admin_sf, fill = NA, color = "grey50", size = 0.6, linetype = "dashed") +
  geom_sf(data = stations_sf, aes(color = country), size = 0.2, alpha = 0.75, show.legend = TRUE) +
  geom_text(data = country_labels, aes(x = label_lon, y = label_lat, label = name),
            color = "#222222", fontface = "bold", size = 4) +
  scale_color_manual(
    values = c(
      "Germany" = "#5b2cff",
      "Austria" = "#ff6f2c",
      "Slovenia" = "#2ca25f"
    ),
    name = "Country"
  ) +
  coord_sf(xlim = c(5, 18), ylim = c(45, 56), expand = FALSE) +
  labs(
    title = "Station coverage across scraped countries",
    subtitle = "Point locations for all stations in the harmonised warehouse (October 2025)",
    caption = "State boundaries sourced from GADM 4.1",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(size = 9, color = "grey40")
  )

ggsave(output_path, station_map, width = 9, height = 7, dpi = 300, bg = "white")

cat(sprintf("Station distribution map saved to %s\n", output_path))

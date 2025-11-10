#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(ggplot2)
  library(sf)
})

find_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..", "../../../..")
  for (cand in candidates) {
    if (file.exists(file.path(cand, "databases", "stations.duckdb"))) {
      return(normalizePath(cand))
    }
  }
  stop("Project root not found")
}

root <- find_project_root()
stations_db <- file.path(root, "databases", "stations.duckdb")
prices_db <- file.path(root, "databases", "german_fuel.duckdb")
data_dir <- file.path(root, "analysis", "presentation", "data")
output_dir <- file.path(root, "analysis", "presentation", "visualizations")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load German districts (GADM level 2)
distr_path <- file.path(data_dir, "gadm41_DEU_2.geojson")
if (!file.exists(distr_path)) {
  download.file("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_DEU_2.json",
                destfile = distr_path, mode = "wb", quiet = TRUE)
}
districts <- st_read(distr_path, quiet = TRUE) |> st_transform(4326)

# Connect to DuckDB (read-only attachments)
con <- dbConnect(duckdb())
dbExecute(con, sprintf("ATTACH '%s' AS stations_db (READ_ONLY)", stations_db))
dbExecute(con, sprintf("ATTACH '%s' AS ger_db (READ_ONLY)", prices_db))

start_ts <- "2024-08-01 00:00:00"
end_ts <- "2025-07-31 23:59:59"

# Compute station-level average diesel price across all Germany
station_avg <- dbGetQuery(con, sprintf(
  "SELECT s.station_uuid, s.latitude, s.longitude, AVG(p.diesel) AS avg_diesel\n     FROM stations_db.stations s\n     JOIN ger_db.german_prices_wide p ON s.station_uuid = p.station_uuid\n     WHERE s.country = 'Germany'\n       AND s.latitude IS NOT NULL AND s.longitude IS NOT NULL\n       AND p.diesel > 0 AND p.diesel < 10\n       AND p.date BETWEEN TIMESTAMP '%s' AND TIMESTAMP '%s'\n     GROUP BY s.station_uuid, s.latitude, s.longitude",
  start_ts, end_ts))

dbDisconnect(con, shutdown = TRUE)

points_sf <- st_as_sf(station_avg, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

make_heatmap <- function(state_name, title, filename) {
  state_poly <- districts |> filter(NAME_1 == state_name)
  assigned <- st_join(points_sf, state_poly, join = st_within, left = FALSE)
  if (nrow(assigned) == 0) {
    warning(sprintf("No station averages assigned to %s", state_name))
    return()
  }
  district_avg <- assigned |> group_by(NAME_2) |> summarise(avg_price = mean(avg_diesel, na.rm = TRUE), .groups = 'drop') |> st_drop_geometry()
  plot_sf <- state_poly |> left_join(district_avg, by = "NAME_2")
  p <- ggplot(plot_sf) +
    geom_sf(aes(fill = avg_price), color = "white", size = 0.2) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Avg diesel (€)") +
    coord_sf() +
    labs(title = title, subtitle = "Average diesel price (Aug 2024 – Jul 2025)") +
    theme_minimal() +
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
  ggsave(file.path(output_dir, filename), p, width = 7, height = 6, dpi = 300, bg = "white")
  message(sprintf("Saved %s", filename))
}

make_heatmap("Bayern", "Bavaria: Average diesel price by district", "bavaria_district_heatmap.png")
make_heatmap("Nordrhein-Westfalen", "North Rhine-Westphalia: Average diesel price by district", "nrw_district_heatmap.png")

message("District heatmaps generated.")

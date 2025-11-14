# Compose 3x3 GAM partial-effect panels for Bavaria and NRW
# Usage: Rscript compose_gam_panels.R

suppressPackageStartupMessages({
  library(magick)
  library(glue)
})

project_root <- "/Users/alexphan/Desktop/MBAT-Internship-Project"
model_fit_root <- file.path(project_root, "analysis", "model fitting")

nrw_dir <- list.dirs(model_fit_root, recursive = FALSE, full.names = TRUE)
nrw_dir <- nrw_dir[grepl("North Rhine", nrw_dir)]
if (length(nrw_dir) != 1L) {
  stop("Unable to uniquely identify the NRW results directory.")
}

result_dirs <- list(
  Bavaria = file.path(model_fit_root, "Bavaria", "results"),
  NRW     = file.path(nrw_dir, "results")
)

find_term_pngs <- function(dir) {
  files <- list.files(dir, pattern = "gam_term[0-9]+.*\\.png$", full.names = TRUE)
  if (length(files) != 9L) {
    stop(glue("Expected 9 GAM term PNGs in {dir}, found {length(files)}."))
  }
  files[order(files)]
}

load_and_enhance <- function(path) {
  img <- magick::image_read(path)
  img <- magick::image_resize(img, "1600x1600")
  img <- magick::image_contrast(img, sharpen = TRUE)
  img <- magick::image_modulate(img, brightness = 95, saturation = 130)
  img <- magick::image_fx(img, expression = "pow(u,0.85)")
  img <- magick::image_fx(img, expression = "u*0.9")
  img
}

make_panel <- function(region, dir) {
  message(glue("Processing {region}"))
  pngs <- find_term_pngs(dir)
  images <- lapply(pngs, load_and_enhance)
  rows <- split(images, ceiling(seq_along(images) / 3))
  row_imgs <- lapply(rows, function(row) {
    magick::image_append(Reduce(c, row), stack = FALSE)
  })
  panel <- magick::image_append(Reduce(c, row_imgs), stack = TRUE)
  panel <- magick::image_border(panel, color = "white", geometry = "20x20")
  out_path <- file.path(dir, glue("{tolower(region)}_diesel_gam_terms_panel.png"))
  magick::image_write(panel, path = out_path, format = "png")
  message(glue("Saved panel to {out_path}"))
}

invisible(mapply(make_panel, names(result_dirs), result_dirs))

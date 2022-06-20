# flag if app is running locally or on a server
is_local <- Sys.getenv("SHINY_PORT") == ""

# get data

## if app is running on the server, overwrite data folder with newest data from GitHub
## this ensures the server always has the newest data when rebooting
## when running locally (e.g., for development and testing), this behaviour is skipped
## for local use, data_updater.R can be run manually if the newest data are desired
## (data will be downloaded automatically if there is no existing local data directory)
if (!is_local | !dir.exists("data")) {
  source("data_updater.R")
}

# load data

## list files
files <- list.files("data", full.names = TRUE)

## load province/territory & health region data
pt_hr_files <- c("data/pt.csv", "data/health_regions.csv")
files <- files[!files %in% pt_hr_files]
for (f in pt_hr_files) {
  name <- basename(tools::file_path_sans_ext(f))
  assign(name, {
    x <- readr::read_csv(f, progress = FALSE, show_col_types = FALSE)})
}

## load other files
for (f in files) {
  ext <- tools::file_ext(f)
  name <- basename(tools::file_path_sans_ext(f))
  if (ext == "csv") {
    assign(name, {
      x <- readr::read_csv(f, progress = FALSE, show_col_types = FALSE)
      # apexcharter will complain if series are not the same length, so use tidyr::complete
      # also, join population/name data
      if ("sub_region_2" %in% names(x)) {
        x <- tidyr::complete(x, name, tidyr::nesting(region, sub_region_1, sub_region_2), date)
      } else if ("sub_region_1" %in% names(x)) {
        x <- tidyr::complete(x, name, tidyr::nesting(region, sub_region_1), date)
        x <- dplyr::left_join(x, health_regions[c("region", "hruid", "name_short", "pop")],
                              by = c("region" = "region", "sub_region_1" = "hruid"))
      } else {
        x <- tidyr::complete(x, name, region, date)
        x <- dplyr::left_join(x, pt[c("region", "pop")], by = "region")
      }
      x
      })
    rm(x)
  } else if (ext == "json") {
    assign(name, jsonlite::fromJSON(f))
  } else if (ext == "geojson") {
    assign(paste0(name, "_geo"), {x <- sf::read_sf(f); suppressWarnings(sf::st_crs(x) <- 3347); x})
    rm(x)
  } else {
    assign(name, readLines(f))
  }
}

# set constants

## PT colour palette
palette_pt <- list(
  "AB" = "#00E676",
  "BC" = "#304FFE",
  "MB" = "#FF80AB",
  "NB" = "#76FF03",
  "NL" = "#B388FF",
  "NS" = "#00E5FF",
  "NT" = "#00ACC1",
  "NU" = "#FFFF00",
  "ON" = "#FF6F00",
  "PE" = "#1B5E20",
  "QC" = "#D50000",
  "SK" = "#AA00FF",
  "YT" = "#D500F9"
)

# load analytics (if available)
analytics <- if (file.exists("google-analytics.html")) {
  shiny::tags$head(shiny::includeHTML("google-analytics.html"))
} else {}
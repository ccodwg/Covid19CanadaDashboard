# create data directory, if it doesn't already exist
dir.create("data", showWarnings = FALSE)

# download data from GitHub

# update time
curl::curl_download("https://raw.githubusercontent.com/ccodwg/CovidTimelineCanada/main/update_time.txt", "data/update_time.txt")

# geo data
curl::curl_download("https://raw.githubusercontent.com/ccodwg/CovidTimelineCanada/main/geo/pt.csv", "data/pt.csv")
curl::curl_download("https://raw.githubusercontent.com/ccodwg/CovidTimelineCanada/main/geo/pt.geojson", "data/pt.geojson")
curl::curl_download("https://raw.githubusercontent.com/ccodwg/CovidTimelineCanada/main/geo/health_regions.csv", "data/health_regions.csv")
curl::curl_download("https://raw.githubusercontent.com/ccodwg/CovidTimelineCanada/main/geo/health_regions.geojson", "data/health_regions.geojson")

# health region data
curl::curl_download(paste0("https://github.com/ccodwg/CovidTimelineCanada/raw/main/data/hr/deaths_hr.csv"), paste0("data/deaths_hr.csv"))

# province/territory data
for (d in c(
  "deaths",
  "hospitalizations",
  "icu"
  )) {
  curl::curl_download(paste0("https://github.com/ccodwg/CovidTimelineCanada/raw/main/data/pt/", d, "_pt.csv"), paste0("data/", d, "_pt.csv"))
}

# wastewater data
curl::curl_download("https://raw.githubusercontent.com/ccodwg/CovidTimelineCanada/main/raw_data/active_ts/can/can_wastewater_copies_per_ml_subhr_ts.csv","data/wastewater_data.csv")
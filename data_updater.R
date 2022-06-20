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
for (d in c(
  "cases",
  "deaths"
  )) {
  curl::curl_download(paste0("https://github.com/ccodwg/CovidTimelineCanada/raw/main/data/hr/", d, "_hr.csv"), paste0("data/", d, "_hr.csv"))
}

# province/territory data
for (d in c(
  "cases",
  "deaths",
  "tests_completed",
  "vaccine_coverage_dose_1",
  "vaccine_coverage_dose_2",
  "vaccine_coverage_dose_3",
  "vaccine_coverage_dose_4"
  )) {
  curl::curl_download(paste0("https://github.com/ccodwg/CovidTimelineCanada/raw/main/data/pt/", d, "_pt.csv"), paste0("data/", d, "_pt.csv"))
}

# metadata
for (d in c("cases", "deaths")) {
  curl::curl_download(paste0("https://github.com/ccodwg/CovidTimelineCanada/raw/main/data/can/", d, "_can_completeness.json"), paste0("data/", d, "_can_completeness.json"))
}

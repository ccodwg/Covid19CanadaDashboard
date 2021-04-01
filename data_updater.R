# load libraries
library(dplyr)
library(RJSONIO)

# create data directory, if it doesn't already exist
dir.create("data", showWarnings = FALSE)

# download most recent data from GitHub and unzip into temporary folder
temp <- tempfile()
tempd <- tempdir()
download.file("https://github.com/ccodwg/Covid19Canada/archive/master.zip", temp, mode = "wb")
unzip(temp, exdir = tempd)

# copy into local data directories

## copy data
if (dir.exists(paste(tempd, "Covid19Canada-master", sep = "/"))) {
  for (f in list.files(paste(tempd, "Covid19Canada-master", sep = "/"), recursive = TRUE, full.names = TRUE)[basename(list.files(paste(tempd, "Covid19Canada-master", sep = "/"), recursive = TRUE)) %in% c(
    "update_time.txt",
    "cases_2020.csv", "cases_2021.csv", "mortality_2020.csv", "mortality_2021.csv",
    "cases_timeseries_prov.csv", "mortality_timeseries_prov.csv", "recovered_timeseries_prov.csv", "testing_timeseries_prov.csv", "active_timeseries_prov.csv",
    "cases_timeseries_hr.csv", "mortality_timeseries_hr.csv",
    "cases_timeseries_canada.csv", "mortality_timeseries_canada.csv", "recovered_timeseries_canada.csv", "testing_timeseries_canada.csv", "active_timeseries_canada.csv",
    "vaccine_administration_timeseries_prov.csv", "vaccine_administration_timeseries_canada.csv",
    "vaccine_distribution_timeseries_prov.csv", "vaccine_distribution_timeseries_canada.csv",
    "vaccine_completion_timeseries_prov.csv", "vaccine_completion_timeseries_canada.csv",
    "sk_new_cases_timeseries_hr.csv","sk_new_mortality_timeseries_hr.csv"
    )]) {
    file_destination <- paste0("data/", basename(f))
    message("Copying: ", file_destination)
    file.copy(f, file_destination, overwrite = TRUE)
  }
}

## copy other files
if (dir.exists(paste(tempd, "Covid19Canada-master", sep = "/"))) {
  for (f in list.files(paste(tempd, "Covid19Canada-master", sep = "/"), recursive = TRUE, full.names = TRUE)[basename(list.files(paste(tempd, "Covid19Canada-master", sep = "/"), recursive = TRUE)) %in% c(
    "age_map_cases.csv", "age_map_mortality.csv", "prov_map.csv", "hr_map.csv","hr_map_sk_new.csv"
  )]) {
    file_destination <- paste0("data/", basename(f))
    message("Copying: ", file_destination)
    file.copy(f, file_destination, overwrite = TRUE)
  }
}

# download hospitalization data
hosp <- fromJSON("https://api.covid19tracker.ca/summary/split")$data %>%
  lapply(function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  }) %>%
  {as.data.frame(t(do.call("cbind", .)), stringsAsFactors = FALSE)} %>%
  rename(
    prov_short = province,
    hosp_cases = total_hospitalizations,
    hosp_cases_change = change_hospitalizations) %>%
  inner_join(
    data.frame("prov_short" = c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU"),
               "province" = c("NL", "PEI", "Nova Scotia", "New Brunswick", "Quebec", "Ontario", "Manitoba",
                              "Saskatchewan", "Alberta", "BC", "Yukon", "NWT", "Nunavut"),
               stringsAsFactors = FALSE),
    by = "prov_short"
  ) %>%
  select(province, hosp_cases, hosp_cases_change)

# write hospitalization data
write.csv(hosp, "data/hosp.csv", row.names = FALSE)

# delete temporary files
unlink(temp) # delete GitHub download
unlink(paste(tempd, "Covid19Canada-master", sep = "/"), recursive = TRUE) # delete unzipped files

# write latest commit message
cat("\n", file = "data/news.txt")
news <- try(fromJSON("https://api.github.com/repos/ccodwg/Covid19Canada/events"))
if (!"try-error" %in% class(news)) {
  news <- news$payload$commits
  news <- news[lengths(news) != 0] # remove null elements
  news <- news[[1]]$message # get most recent commit message
  cat(paste0(news, "\n"), file = "data/news.txt")
}
# load libraries
library(dplyr)
library(tidyr)
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
    "vaccine_completion_timeseries_prov.csv", "vaccine_completion_timeseries_canada.csv"
    )]) {
    file_destination <- paste0("data/", basename(f))
    message("Copying: ", file_destination)
    file.copy(f, file_destination, overwrite = TRUE)
  }
}

## copy other files
if (dir.exists(paste(tempd, "Covid19Canada-master", sep = "/"))) {
  for (f in list.files(paste(tempd, "Covid19Canada-master", sep = "/"), recursive = TRUE, full.names = TRUE)[basename(list.files(paste(tempd, "Covid19Canada-master", sep = "/"), recursive = TRUE)) %in% c(
    "age_map_cases.csv", "age_map_mortality.csv", "prov_map.csv", "hr_map.csv"
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
               "province" = c("NL", "PEI", "Nova Scotia", "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta", "BC", "Yukon", "NWT", "Nunavut"),
               stringsAsFactors = FALSE),
    by = "prov_short"
  ) %>%
  select(province, hosp_cases, hosp_cases_change)

# write hospitalization data
write.csv(hosp, "data/hosp.csv", row.names = FALSE)


## VOCs
phac <- read.csv(
  "https://health-infobase.canada.ca/src/data/covidLive/covid19-epiSummary-voc.csv",
  stringsAsFactors = FALSE) %>%
  ## keep relevant columns
  select(report_date, prov, b117, b1351, p1) %>%
  ## rename columns
  rename(date_report = report_date, province = prov, B117 = b117, B1351 = b1351, P1 = p1) %>%
  ## convert dates
  mutate(
    date_report = as.Date(date_report)
  ) %>%
  ## PEI is sometimes named "PE" and sometimes "PEI"; rename "CA" to "Canada"
  mutate(
    province = case_when(
      province == "PEI" ~ "PE",
      province == "CA" ~ "Canada",
      TRUE ~ province
    )
  ) %>%
  ## fill in missing dates
  complete(., expand(., date_report, province), fill = list(B117 = 0, B1351 = 0, P1 = 0)) %>%
  ## arrange
  arrange(date_report, province)
# wide to long for easier plotting
phac_plot <-
  pivot_longer(
    phac,
    cols = c(B117, B1351, P1),
    names_to = "variant",
    values_to = "cumulative_variant_cases"
  )

phac_plot <- phac_plot %>%
  group_by(province,variant) %>%
  arrange(date_report, .by_group = TRUE) %>%
  mutate(variant_cases = cumulative_variant_cases - lag(cumulative_variant_cases, default = first(cumulative_variant_cases)))

phac_plot$province[phac_plot$province=="AB"] <- "Alberta"
phac_plot$province[phac_plot$province=="MB"] <- "Manitoba"
phac_plot$province[phac_plot$province=="NB"] <- "New Brunswick"
phac_plot$province[phac_plot$province=="NS"] <- "Nova Scotia"
phac_plot$province[phac_plot$province=="NU"] <- "Nunavut"
phac_plot$province[phac_plot$province=="NT"] <- "NWT"
phac_plot$province[phac_plot$province=="ON"] <- "Ontario"
phac_plot$province[phac_plot$province=="PE"] <- "PEI"
phac_plot$province[phac_plot$province=="QC"] <- "Quebec"
phac_plot$province[phac_plot$province=="SK"] <- "Saskatchewan"
phac_plot$province[phac_plot$province=="YT"] <- "Yukon"

## VOC Date data needs to be in the following format: "%d-%m-%Y"
phac_plot$date_report <- format(phac_plot$date_report,"%d-%m-%Y")

write.csv(phac_plot[(phac_plot$province!="Canada"),],"data/variants_timeseries_prov.csv", row.names = F)
write.csv(phac_plot[(phac_plot$province=="Canada"),],"data/variants_timeseries_canada.csv",row.names = F)


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
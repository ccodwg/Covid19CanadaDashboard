# load libraries

## dashboard
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyalert) # display pop-ups
library(metathis)
library(rsconnect)

## data manipulation
library(Hmisc) # load before other packages due to namespace conflicts
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(zoo)
library(sf)
library(rvest)
library(RJSONIO)
library(mgcv)

## data visualization
library(ggplot2)
library(scales)
library(ggrepel)
library(RColorBrewer)
library(plotly)
library(DT)
library(leaflet)

# flag if app is running locally or on a server
is_local <- Sys.getenv('SHINY_PORT') == ""

# load data

## if app is running on the server, overwrite data folder with newest data from GitHub
## this ensures the server always has the newest data when rebooting
## when running locally (e.g., for development and testing), this behaviour is skipped
## for local use, data_updater.R can be run manually if the newest data are desired
## (data will be downloaded automatically if there is no existing local data directory)
if (!is_local | !dir.exists("data")) {
  source("data_updater.R")
}

## read update time and update notes
update_time <- readLines("data/update_time.txt")
update_date <- as.Date(update_time)
data_notes <- paste(readLines("data/data_notes.txt"), collapse = "\n")

## list data files to be loaded
files <- matrix(
  c(
    "ts_cases", "data/cases_timeseries_prov.csv",
    "ts_mortality", "data/mortality_timeseries_prov.csv",
    "ts_recovered", "data/recovered_timeseries_prov.csv",
    "ts_testing", "data/testing_timeseries_prov.csv",
    "ts_active", "data/active_timeseries_prov.csv",
    "ts_cases_hr", "data/cases_timeseries_hr.csv",
    "ts_mortality_hr", "data/mortality_timeseries_hr.csv",
    "ts_cases_new_hr","data/sk_new_cases_timeseries_hr.csv",
    "ts_mortality_new_hr","data/sk_new_mortality_timeseries_hr.csv",
    "ts_cases_canada", "data/cases_timeseries_canada.csv",
    "ts_mortality_canada", "data/mortality_timeseries_canada.csv",
    "ts_recovered_canada", "data/recovered_timeseries_canada.csv",
    "ts_testing_canada", "data/testing_timeseries_canada.csv",
    "ts_active_canada", "data/active_timeseries_canada.csv",
    "ts_vaccine_administration", "data/vaccine_administration_timeseries_prov.csv",
    "ts_vaccine_distribution", "data/vaccine_distribution_timeseries_prov.csv",
    "ts_vaccine_completion", "data/vaccine_completion_timeseries_prov.csv",
    "ts_vaccine_additionaldoses", "data/vaccine_additionaldoses_timeseries_prov.csv",
    "ts_vaccine_administration_canada", "data/vaccine_administration_timeseries_canada.csv",
    "ts_vaccine_distribution_canada", "data/vaccine_distribution_timeseries_canada.csv",
    "ts_vaccine_completion_canada", "data/vaccine_completion_timeseries_canada.csv",
    "ts_vaccine_additionaldoses_canada", "data/vaccine_additionaldoses_timeseries_canada.csv",
    "hosp", "data/hosp.csv",
    "map_hr", "data/hr_map.csv",
    "map_prov", "data/prov_map.csv",
    "hr_map_sk_new", "data/hr_map_sk_new.csv",
    "info_testing", "text/info_testing.csv"
  ),
  byrow = TRUE,
  ncol = 2
)

## load files
for (i in 1:nrow(files)) {
  assign(
    files[i, 1],
    read.csv(files[i, 2],
             stringsAsFactors = FALSE) %>%
      ### convert date variables
      mutate(across(matches("date|week"), as.Date, format = "%d-%m-%Y"))
  )
}

# process data

## ADDED: merge SK new HR with rest of HR data

ts_cases_new_hr <- bind_rows(ts_cases_hr[(ts_cases_hr$province!="Saskatchewan"),],ts_cases_new_hr)
ts_mortality_new_hr <- bind_rows(ts_mortality_hr[(ts_mortality_hr$province!="Saskatchewan"),],ts_mortality_new_hr)

## get cumulative cases and mortality for SK at start date (2020-08-04)

ts_cases_new_add_hr <- ts_cases_new_hr[(ts_cases_new_hr$province=="Saskatchewan" &
                                          ts_cases_new_hr$date_report=="2020-08-04"),]
ts_mortality_new_add_hr <- ts_mortality_new_hr[(ts_mortality_new_hr$province=="Saskatchewan" &
                                                  ts_mortality_new_hr$date_death_report=="2020-08-04"),]

# add cumulative cases to case count
ts_cases_new_add_hr$cases <- ts_cases_new_add_hr$cumulative_cases
ts_mortality_new_add_hr$deaths <- ts_mortality_new_add_hr$cumulative_deaths

# bind cumulative numbers to data frame
ts_cases_new_add_hr <- bind_rows(ts_cases_new_add_hr,ts_cases_new_hr[!(ts_cases_new_hr$province=="Saskatchewan" &
                                                                         ts_cases_new_hr$date_report=="2020-08-04"),])
ts_mortality_new_add_hr <- bind_rows(ts_mortality_new_add_hr,ts_mortality_new_hr[!(ts_mortality_new_hr$province=="Saskatchewan" &
                                                                                ts_mortality_new_hr$date_death_report=="2020-08-04"),])


## add province short codes, full names, and populations to datasets
for (i in c("ts_cases", "ts_mortality", "ts_recovered", "ts_testing", "ts_active", "ts_vaccine_administration", "ts_vaccine_distribution", "ts_vaccine_completion")) {
  assign(
    i,
    get(i) %>%
      left_join(
        map_prov,
        by = "province"
      )
  )
}

# define common variables
date_min <- min(ts_cases$date_report, na.rm = TRUE)
date_max <- max(ts_cases$date_report, na.rm = TRUE)

# define common plot elements

## colour palette - province full names
palette_province <- c(
  "Canada" = "#000000",
  "Alberta" = "#00E676",
  "BC" = "#304FFE",
  "Manitoba" = "#FF80AB",
  "New Brunswick" = "#76FF03",
  "NL" = "#B388FF",
  "Nova Scotia" = "#00E5FF",
  "Ontario" = "#FF6F00",
  "PEI" = "#1B5E20",
  "Quebec" = "#D50000",
  "Saskatchewan" = "#AA00FF",
  "Nunavut" = "#FFFF00",
  "NWT" = "#00ACC1",
  "Yukon" = "#D500F9"
)

## colour palette - province short names
palette_province_short <- setNames(
  palette_province,
  c(
    "CAN",
    "AB",
    "BC",
    "MB",
    "NB",
    "NL",
    "NS",
    "ON",
    "PE",
    "QC",
    "SK",
    "NU",
    "NT",
    "YT"
  )
)

## plotly display options

### plotly legend
plotly_legend <- list(
  orientation = "h",
  yanchor = "bottom",
  y = 1.02,
  xanchor = "right",
  x = 1
)

### hide plotly buttons
plotly_buttons <- c(
  "zoomIn2d",
  "zoomOut2d",
  "pan2d",
  "resetScale2d",
  "autoScale2d",
  "zoom2d"
)

### hide plotly axis
axis_hide <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  fixedrange = TRUE
)

# construct overview table
table_overview <- ts_cases %>%
  group_by(province) %>%
  filter(date_report == date_max) %>%
  left_join(
    ts_mortality %>%
      group_by(province) %>%
      filter(date_death_report == date_max),
    by = "province"
  ) %>%
  left_join(
    ts_recovered %>%
      group_by(province) %>%
      filter(date_recovered == date_max),
    by = "province"
  ) %>%
  left_join(
    ts_testing %>%
      group_by(province) %>%
      filter(date_testing == date_max),
    by = "province"
  ) %>%
  left_join(
    ts_active %>%
      select(province, date_active, active_cases, active_cases_change) %>%
      group_by(province) %>%
      filter(date_active == date_max),
    by = "province"
  ) %>%
  left_join(
    hosp %>%
      select(province, hosp_cases, hosp_cases_change),
    by = "province"
  ) %>%
  left_join(
    ts_vaccine_administration %>%
      group_by(province) %>%
      filter(date_vaccine_administered == date_max) %>%
      select(province, avaccine, cumulative_avaccine),
    by = "province"
  ) %>%
  left_join(
    ts_vaccine_completion %>%
      group_by(province) %>%
      filter(date_vaccine_completed == date_max) %>%
      select(province, cvaccine, cumulative_cvaccine),
    by = "province"
  ) %>%
  left_join(
    ts_vaccine_additionaldoses %>%
      group_by(province) %>%
      filter(date_vaccine_additionaldoses == date_max) %>%
      select(province, additionaldosesvaccine, cumulative_additionaldosesvaccine),
    by = "province"
  ) %>%
  left_join(
    map_prov %>%
      select(province, pop),
    by = "province"
  ) %>%
  select(province, cases, cumulative_cases,
         active_cases, active_cases_change,
         avaccine, cumulative_avaccine,
         cvaccine, cumulative_cvaccine,
         additionaldosesvaccine, cumulative_additionaldosesvaccine,
         hosp_cases, hosp_cases_change,
         deaths, cumulative_deaths,
         recovered, cumulative_recovered,
         testing, cumulative_testing,
         pop) %>%
  bind_rows(
    data.frame(
      "province" = "Canada",
      "cases" = ts_cases_canada %>% filter(date_report == date_max) %>% pull(cases),
      "cumulative_cases" = ts_cases_canada %>% filter(date_report == date_max) %>% pull(cumulative_cases),
      "active_cases" = ts_active_canada %>% filter(date_active == date_max) %>% pull(active_cases),
      "active_cases_change" = ts_active_canada %>% filter(date_active == date_max) %>% pull(active_cases_change),
      "cumulative_avaccine" = ts_vaccine_administration_canada %>% filter(date_vaccine_administered == date_max) %>% pull(cumulative_avaccine),
      "avaccine" = ts_vaccine_administration_canada %>% filter(date_vaccine_administered == date_max) %>% pull(avaccine),
      "cumulative_cvaccine" = ts_vaccine_completion_canada %>% filter(date_vaccine_completed == date_max) %>% pull(cumulative_cvaccine),
      "cvaccine" = ts_vaccine_completion_canada %>% filter(date_vaccine_completed == date_max) %>% pull(cvaccine),
      "cumulative_additionaldosesvaccine" = ts_vaccine_additionaldoses_canada %>% filter(date_vaccine_additionaldoses == date_max) %>% pull(cumulative_additionaldosesvaccine),
      "additionaldosesvaccine" = ts_vaccine_additionaldoses_canada %>% filter(date_vaccine_additionaldoses == date_max) %>% pull(additionaldosesvaccine),
      "hosp_cases" = sum(hosp$hosp_cases),
      "hosp_cases_change" = sum(hosp$hosp_cases_change),
      "deaths" = ts_mortality_canada %>% filter(date_death_report == date_max) %>% pull(deaths),
      "cumulative_deaths" = ts_mortality_canada %>% filter(date_death_report == date_max) %>% pull(cumulative_deaths),
      "recovered" = ts_recovered_canada %>% filter(date_recovered == date_max) %>% pull(recovered),
      "cumulative_recovered" = ts_recovered_canada %>% filter(date_recovered == date_max) %>% pull(cumulative_recovered),
      "testing" = ts_testing_canada %>% filter(date_testing == date_max) %>% pull(testing),
      "cumulative_testing" = ts_testing_canada %>% filter(date_testing == date_max) %>% pull(cumulative_testing),
      "pop" = sum(map_prov$pop, na.rm = TRUE),
      stringsAsFactors = FALSE)
  ) %>%
  replace_na(list(cases = 0,
                  cumulative_cases = 0,
                  active_cases = 0,
                  active_cases_change = 0,
                  avaccine = 0,
                  cumulative_avaccine = 0,
                  cvaccine = 0,
                  cumulative_cvaccine = 0,
                  additionaldosesvaccine = 0,
                  cumulative_additionaldosesvaccine = 0,
                  hosp_cases = 0,
                  hosp_cases_change = 0,
                  deaths = 0,
                  cumulative_deaths = 0,
                  recovered = 0,
                  cumulative_recovered = 0,
                  testing = 0,
                  cumulative_testing = 0)) %>%
  mutate(
    cases_per_100k = cases / pop * 100000,
    cumulative_cases_per_100k = cumulative_cases / pop * 100000,
    active_cases_per_100k = active_cases / pop * 100000,
    hosp_per_100k = hosp_cases / pop * 100000,
    cumulative_deaths_per_100k = cumulative_deaths / pop * 100000,
    cumulative_testing_per_100k = cumulative_testing / pop * 100000) %>%
  arrange(desc(cases)) %>%
  select(
    province,
    cumulative_cases, cases, cases_per_100k, cumulative_cases_per_100k,
    active_cases, active_cases_change, active_cases_per_100k,
    avaccine, cumulative_avaccine,
    cvaccine, cumulative_cvaccine,
    additionaldosesvaccine, cumulative_additionaldosesvaccine,
    hosp_cases, hosp_cases_change, hosp_per_100k,
    cumulative_deaths, deaths, cumulative_deaths_per_100k,
    cumulative_recovered, recovered,
    cumulative_testing, testing, cumulative_testing_per_100k,
  ) %>%
  rename(
    `Province` = province,
    `Cumulative cases` = cumulative_cases,
    `Cases (new)` = cases,
    `Cases (new) per 100k` = cases_per_100k,
    `Cumulative cases per 100k` = cumulative_cases_per_100k,
    `Active cases` = active_cases,
    `Active cases (change)` = active_cases_change,
    `Active cases per 100k` = active_cases_per_100k,
    `Vaccine doses administered (new)` = avaccine,
    `Cumulative vaccine doses administered` = cumulative_avaccine,
    `People w/ 2 doses (new)` = cvaccine,
    `Cumulative people w/ 2 doses` = cumulative_cvaccine,
    `Additional doses (new)` = additionaldosesvaccine,
    `Cumulative additional doses` = cumulative_additionaldosesvaccine,
    `Hospitalized per 100k` = hosp_per_100k,
    `Hospitalized` = hosp_cases,
    `Hospitalized (change)` = hosp_cases_change,
    `Hospitalized per 100k` = hosp_per_100k,
    `Cumulative recovered` = cumulative_recovered,
    `Recovered (new)` = recovered,
    `Cumulative deaths` = cumulative_deaths,
    `Deaths (new)` = deaths,
    `Cumulative deaths per 100k` = cumulative_deaths_per_100k,
    `Cumulative testing` = cumulative_testing,
    `Testing (new)` = testing,
    `Cumulative testing per 100k` = cumulative_testing_per_100k
  ) %>%
  ungroup

# prepare info tables

## info: testing
info_testing <- info_testing %>%
  ### capitalize column names
  rename_with(capitalize) %>%
  ### create proper hyperlinks to sources
  mutate(
    Source = paste0("<a href='", Source, "' target='_blank'>", "Link","</a>")
  )

# load 

# load spatial data

## load simple province map
geo_prov_simple <- st_read("geo/natural_earth/ne_canada_provinces_simple.geojson", quiet = TRUE) %>%
  ### join province names
  left_join(
    map_prov,
    by = c("name" = "province_full")
  )

# get a random question from the opencovid.ca FAQ page
faq <- read_html("https://opencovid.ca/work/data-faq/") %>%
  html_nodes("h5") %>%
  html_text() %>%
  `[`(sample(x = 1:length(.), size = 1))

# load analytics (if available)
analytics <- if (file.exists("google-analytics.html")) {
  tags$head(includeHTML("google-analytics.html"))
} else {}
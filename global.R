# load libraries

## dashboard
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyBS)
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
news <- paste(readLines("data/news.txt"), collapse = "\n")

## list data files to be loaded
files <- matrix(
  c(
    "cases_2020", "data/cases_2020.csv",
    "cases_2021", "data/cases_2021.csv",
    "mortality_2020", "data/mortality_2020.csv",
    "mortality_2021", "data/mortality_2021.csv",
    "ts_cases", "data/cases_timeseries_prov.csv",
    "ts_variants", "data/variants_timeseries_prov.csv",
    "ts_mortality", "data/mortality_timeseries_prov.csv",
    "ts_recovered", "data/recovered_timeseries_prov.csv",
    "ts_testing", "data/testing_timeseries_prov.csv",
    "ts_active", "data/active_timeseries_prov.csv",
    "ts_cases_hr", "data/cases_timeseries_hr.csv",
    "ts_mortality_hr", "data/mortality_timeseries_hr.csv",
    "ts_cases_canada", "data/cases_timeseries_canada.csv",
    "ts_variants_canada", "data/variants_timeseries_canada.csv",
    "ts_mortality_canada", "data/mortality_timeseries_canada.csv",
    "ts_recovered_canada", "data/recovered_timeseries_canada.csv",
    "ts_testing_canada", "data/testing_timeseries_canada.csv",
    "ts_active_canada", "data/active_timeseries_canada.csv",
    "ts_vaccine_administration", "data/vaccine_administration_timeseries_prov.csv",
    "ts_vaccine_distribution", "data/vaccine_distribution_timeseries_prov.csv",
    "ts_vaccine_completion", "data/vaccine_completion_timeseries_prov.csv",
    "ts_vaccine_administration_canada", "data/vaccine_administration_timeseries_canada.csv",
    "ts_vaccine_distribution_canada", "data/vaccine_distribution_timeseries_canada.csv",
    "ts_vaccine_completion_canada", "data/vaccine_completion_timeseries_canada.csv",
    "hosp", "data/hosp.csv",
    "map_hr", "data/hr_map.csv",
    "map_prov", "data/prov_map.csv",
    "map_age_cases", "data/age_map_cases.csv",
    "map_age_mortality", "data/age_map_mortality.csv",
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

## merge individual-level data
cases <- bind_rows(cases_2020, cases_2021)
mortality <- bind_rows(mortality_2020, mortality_2021)


## add province short codes, full names, and populations to datasets
for (i in c("cases", "ts_variants", "mortality", "ts_cases", "ts_mortality", "ts_recovered", "ts_testing", "ts_active", "ts_vaccine_administration", "ts_vaccine_distribution", "ts_vaccine_completion")) {
  assign(
    i,
    get(i) %>%
      left_join(
        map_prov,
        by = "province"
      )
  )
}


# if VOC data missing, add 0 to dates missing so that dashboard works

dash.date <- max(cases_2021$date_report)
voc.date <- max(ts_variants$date_report)

if(dash.date != voc.date) {
  ts_variants_add <- ts_variants[(ts_variants$date_report==voc.date),]
  ts_variants_add$date_report <- dash.date
  ts_variants_add$variant_cases <- 0
  ts_variants_add$cumulative_variant_cases <- ts_variants_add$cumulative_variant_cases
  ts_variants <- rbind(ts_variants,ts_variants_add)
  
  ts_variants_can_add <- ts_variants_canada[(ts_variants_canada$date_report==voc.date),]
  ts_variants_can_add$date_report <- dash.date
  ts_variants_can_add$variant_cases <- 0
  ts_variants_can_add$cumulative_variant_cases <- ts_variants_can_add$cumulative_variant_cases
  ts_variants_canada <- rbind(ts_variants_canada,ts_variants_can_add)
}


## make separate df for variant cases (to pull data for summary tables)

ts_b117_variants_canada <- ts_variants_canada[(ts_variants_canada$variant=="B117"),]
ts_b1351_variants_canada <- ts_variants_canada[(ts_variants_canada$variant=="B1351"),]
ts_p1_variants_canada <- ts_variants_canada[(ts_variants_canada$variant=="P1"),]

## variant cases - daily and cumulative

ts_b117_variants <- ts_variants[(ts_variants$variant=="B117"),]
ts_b117_variants_add <- ts_b117_variants[(ts_b117_variants$date_report=="2021-02-04"),]
ts_b117_variants_add$variant_cases <- ts_b117_variants_add$cumulative_variant_cases
ts_b117_variants_add$date_report[ts_b117_variants_add$date_report=="2021-02-04"] <- "2021-02-03"
ts_b117_variants_add$variant_cases <- ts_b117_variants_add$cumulative_variant_cases
ts_b117_variants <- bind_rows(ts_b117_variants_add,ts_b117_variants)
ts_b117_variants <- ts_b117_variants %>% rename(b117_variant_cases = variant_cases,
                                                b117_cumulative_variant_cases = cumulative_variant_cases)

ts_b1351_variants <- ts_variants[(ts_variants$variant=="B1351"),]
ts_b1351_variants_add <- ts_b1351_variants[(ts_b1351_variants$date_report=="2021-02-04"),]
ts_b1351_variants_add$variant_cases <- ts_b1351_variants_add$cumulative_variant_cases
ts_b1351_variants_add$date_report[ts_b1351_variants_add$date_report=="2021-02-04"] <- "2021-02-03"
ts_b1351_variants_add$variant_cases <- ts_b1351_variants_add$cumulative_variant_cases
ts_b1351_variants <- bind_rows(ts_b1351_variants_add,ts_b1351_variants)
ts_b1351_variants <- ts_b1351_variants %>% rename(b1351_variant_cases = variant_cases,
                                                  b1351_cumulative_variant_cases = cumulative_variant_cases)

ts_p1_variants <- ts_variants[(ts_variants$variant=="P1"),]
ts_p1_variants_add <- ts_p1_variants[(ts_p1_variants$date_report=="2021-02-04"),]
ts_p1_variants_add$variant_cases <- ts_p1_variants_add$cumulative_variant_cases
ts_p1_variants_add$date_report[ts_p1_variants_add$date_report=="2021-02-04"] <- "2021-02-03"
ts_p1_variants_add$variant_cases <- ts_p1_variants_add$cumulative_variant_cases
ts_p1_variants <- bind_rows(ts_p1_variants_add,ts_p1_variants)
ts_p1_variants <- ts_p1_variants %>% rename(p1_variant_cases = variant_cases,
                                            p1_cumulative_variant_cases = cumulative_variant_cases)

ts_variants_wide <- cbind(ts_b117_variants,ts_b1351_variants,ts_p1_variants)
ts_variants_wide <- ts_variants_wide[c("province","date_report","province_full","province_short","pop",
                             "b117_variant_cases","b117_cumulative_variant_cases",
                             "b1351_variant_cases","b1351_cumulative_variant_cases",
                             "p1_variant_cases","p1_cumulative_variant_cases")]

ts_variants_wide$variant_cases <- ts_variants_wide$b117_variant_cases + ts_variants_wide$b1351_variant_cases + ts_variants_wide$p1_variant_cases
ts_variants_wide$cumulative_variant_cases <- ts_variants_wide$b117_cumulative_variant_cases + ts_variants_wide$b1351_cumulative_variant_cases + ts_variants_wide$p1_cumulative_variant_cases

ts_variants_wide <- merge(ts_variants_wide, ts_cases[c("province","date_report","cases","cumulative_cases")], by = c("province","date_report"))

ts_variants_wide$cases_adj <- (ts_variants_wide$cases-ts_variants_wide$variant_cases)

## process case data
cases <- cases %>%
  mutate(
    age = factor(
      recode(age, !!!setNames(map_age_cases$age_display, map_age_cases$age)),
      c(
        "<20",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80-89",
        "90-99",
        "100+",
        "NR"
      )
    ),
    sex = factor(sex, c("Male", "Female", "Not Reported")),
    travel_history_country = ifelse(travel_yn == 1 & travel_history_country == "", "Not Reported", travel_history_country),
    route = factor(
      case_when(
        travel_yn == "1" ~ "Travel",
        locally_acquired == "Close Contact" ~ "Close Contact",
        locally_acquired == "Community" ~ "Community",
        TRUE ~ "Not Reported"
      ),
      levels = c("Not Reported", "Community", "Close Contact", "Travel")
    )
  )

## process mortality data
mortality <- mortality %>%
  mutate(
    age = factor(
      recode(age, !!!setNames(map_age_mortality$age_display, map_age_mortality$age)),
      c(
        "<20",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80-89",
        "90-99",
        "100+",
        "NR"
      )
    ),
    sex = factor(sex, c("Male", "Female", "Not Reported"))
  )

# define common variables
date_min <- min(cases$date_report, na.rm = TRUE)
date_max <- max(cases$date_report, na.rm = TRUE)

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
    ts_variants_wide %>%
      select(province, date_report, b117_variant_cases, b1351_variant_cases, p1_variant_cases,
             b117_cumulative_variant_cases, b1351_cumulative_variant_cases, p1_cumulative_variant_cases,) %>%
      group_by(province) %>%
      filter(date_report == date_max),
    by = "province"
  ) %>%
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
    map_prov %>%
      select(province, pop),
    by = "province"
  ) %>%
  select(province, cases,
         b117_variant_cases, b1351_variant_cases, p1_variant_cases,
         b117_cumulative_variant_cases, b1351_cumulative_variant_cases, p1_cumulative_variant_cases,
         cumulative_cases,
         active_cases, active_cases_change,
         avaccine, cumulative_avaccine,
         cvaccine, cumulative_cvaccine,
         hosp_cases, hosp_cases_change,
         deaths, cumulative_deaths,
         recovered, cumulative_recovered,
         testing, cumulative_testing,
         pop) %>%
  bind_rows(
    data.frame(
      "province" = "Canada",
      "cases" = ts_cases_canada %>% filter(date_report == date_max) %>% pull(cases),
      "b117_variant_cases" = ts_b117_variants_canada %>% filter(date_report == date_max) %>% pull(variant_cases),
      "b1351_variant_cases" = ts_b1351_variants_canada %>% filter(date_report == date_max) %>% pull(variant_cases),
      "p1_variant_cases" = ts_p1_variants_canada %>% filter(date_report == date_max) %>% pull(variant_cases),
      "cumulative_cases" = ts_cases_canada %>% filter(date_report == date_max) %>% pull(cumulative_cases),
      "b117_cumulative_variant_cases" = ts_b117_variants_canada %>% filter(date_report == date_max) %>% pull(cumulative_variant_cases),
      "b1351_cumulative_variant_cases" = ts_b1351_variants_canada %>% filter(date_report == date_max) %>% pull(cumulative_variant_cases),
      "p1_cumulative_variant_cases" = ts_p1_variants_canada %>% filter(date_report == date_max) %>% pull(cumulative_variant_cases),
      "active_cases" = ts_active_canada %>% filter(date_active == date_max) %>% pull(active_cases),
      "active_cases_change" = ts_active_canada %>% filter(date_active == date_max) %>% pull(active_cases_change),
      "cumulative_avaccine" = ts_vaccine_administration_canada %>% filter(date_vaccine_administered == date_max) %>% pull(cumulative_avaccine),
      "avaccine" = ts_vaccine_administration_canada %>% filter(date_vaccine_administered == date_max) %>% pull(avaccine),
      "cumulative_cvaccine" = ts_vaccine_completion_canada %>% filter(date_vaccine_completed == date_max) %>% pull(cumulative_cvaccine),
      "cvaccine" = ts_vaccine_completion_canada %>% filter(date_vaccine_completed == date_max) %>% pull(cvaccine),     
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
                  b117_variant_cases = 0,
                  b1351_variant_cases = 0,
                  p1_variant_cases = 0,
                  cumulative_cases = 0,
                  b117_cumulative_variant_cases = 0,
                  b1351_cumulative_variant_cases = 0,
                  p1_cumulative_variant_cases = 0,
                  active_cases = 0,
                  active_cases_change = 0,
                  avaccine = 0,
                  cumulative_avaccine = 0,
                  cvaccine = 0,
                  cumulative_cvaccine = 0,
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
    b117_variant_cases, b1351_variant_cases, p1_variant_cases,
    b117_cumulative_variant_cases, b1351_cumulative_variant_cases, p1_cumulative_variant_cases,
    active_cases, active_cases_change, active_cases_per_100k,
    avaccine, cumulative_avaccine,
    cvaccine, cumulative_cvaccine,
    hosp_cases, hosp_cases_change, hosp_per_100k,
    cumulative_deaths, deaths, cumulative_deaths_per_100k,
    cumulative_recovered, recovered,
    cumulative_testing, testing, cumulative_testing_per_100k,
  ) %>%
  rename(
    `Province` = province,
    `Cumulative cases` = cumulative_cases,
    `Cumulative B117 Variant Cases` = b117_cumulative_variant_cases,
    `Cumulative B1351 Variant Cases` = b1351_cumulative_variant_cases,
    `Cumulative P1 Variant Cases` = p1_cumulative_variant_cases,
    `Cases (new)` = cases,
    `B117 Variant Cases (new)` = b117_variant_cases,
    `B1351 Variant Cases (new)` = b1351_variant_cases,
    `P1 Variant Cases (new)` = p1_variant_cases,
    `Cases (new) per 100k` = cases_per_100k,
    `Cumulative cases per 100k` = cumulative_cases_per_100k,
    `Active cases` = active_cases,
    `Active cases (change)` = active_cases_change,
    `Active cases per 100k` = active_cases_per_100k,
    `Vaccine doses administered (new)` = avaccine,
    `Cumulative vaccine doses administered` = cumulative_avaccine,
    `People fully vaccinated (new)` = cvaccine,
    `Cumulative people fully vaccinated` = cumulative_cvaccine,
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
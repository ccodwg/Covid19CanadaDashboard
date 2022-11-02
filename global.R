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

### FINDS start state for this calendar year (simplified function of MMWRweek package)
### Start of week is Monday

start_week_calc <- function(week, year){
  begin_date <- function(year) {
    jan1 <- as.Date(paste0(year, "-01-01"))
    wday <- as.numeric(lubridate::wday(jan1, week_start = 1))
    date <- jan1 - (wday-1) + 7*(wday>4)
    return(date)
  }
  jan1 <- begin_date(year)
  return(jan1 + (week-1)*7)
}

##### ----- FUNCTION to process data: PROVINCE/ TERRITORIES
### Variable names are standardized but include option to identify different value (cumulative vs daily)
### TYPE == type of data: "mean" (wastewater); or "total" (mortality, hospitalizations)
### ALSO ADDS week 53 to week 52 data (and if week 53 is NAN, then == 0 for addition)

process_pt_data <- function(data, val, type){
  
  names(data)[names(data) == val] <- "val"
  
  if(type == "mean"){
    
  data <- data %>% 
    dplyr::mutate(year = lubridate::year(date), week = lubridate::week(date)) %>%
    dplyr::group_by(region, year, week) %>% dplyr::summarise(mean = mean(val, na.rm = T), .groups = "keep") 
  
  data <- data %>% dplyr::rename(val = mean) %>%
    dplyr::mutate(date = start_week_calc(week, year))
  
  }
  
  if(type == "total"){
    
    data <- data %>% 
      dplyr::mutate(year = lubridate::year(date), week = lubridate::week(date)) %>%
      dplyr::group_by(region, year, week) %>% dplyr::summarise(total = sum(val, na.rm = T), .groups = "keep") 
    
    data <- data %>% dplyr::rename(val = total) %>%
      dplyr::mutate(date = start_week_calc(week, year))
  }

  ### Separates out week 53 so it can be added to week 52
  w53 <- data[(data$week==53),]
  w53$week <- 52
  w53 <- w53[c("week","year","region","val")]
  colnames(w53) <- c("week","year","region","val_add")
  
  data <- data[(data$week!=53),]
  data <- merge(data,w53, by = c("week","year","region"), all.x = T)
  data$val <- rowSums(data[c("val","val_add")], na.rm = T)
  
  data <- data[c("week","year","date","region","val")]
  names(data)[names(data) == "val"] <- val
  
  ### ADD AREAS - FACTOR ACCORDINGLY
  
  data$area <- NA
  data$area[data$region=="NL" | data$region=="NS" | data$region=="PE" | data$region=="NB"] <- "Atlantic Canada"
  data$area[data$region=="ON" | data$region=="QC"] <- "Central Canada"
  data$area[data$region=="MB" | data$region=="AB" | data$region=="SK" | data$region=="BC"] <- "Western / Prairie Provinces"
  data$area[data$region=="NT" | data$region=="NU" | data$region=="YT"] <- "Northern Canada"
  
  return(data)
  
}


process_hr_data <- function(data, hr, pt, val, type){
  
  names(data)[names(data) == val] <- "val"
  names(data)[names(data) == hr] <- "hr"
  names(data)[names(data) == pt] <- "pt"
  
  areas <- data[c("hr","pt")]
  areas$unique <- duplicated(areas$hr)
  areas <- areas[(areas$unique==FALSE),]
  
  if(type == "mean"){
    
    data <- data %>%
      dplyr::mutate(year = lubridate::year(date), week = lubridate::week(date)) %>%
      dplyr::group_by(hr, year, week) %>% dplyr::summarise(mean = mean(val, na.rm = T), .groups = "keep") 
    
    data <- data %>% dplyr::rename(val = mean) %>%
      dplyr::mutate(date = start_week_calc(week, year))
    
  }
  
  if(type == "total"){
    
    data <- data %>%
      dplyr::mutate(year = lubridate::year(date), week = lubridate::week(date)) %>%
      dplyr::group_by(hr, year, week) %>% dplyr::summarise(total = sum(val, na.rm = T), .groups = "keep") 
    
    data <- data %>% dplyr::rename(val = total) %>%
      dplyr::mutate(date = start_week_calc(week, year))
  
  }

  ### Separates out week 53 so it can be added to week 52
  w53 <- data[(data$week==53),]
  w53$week <- 52
  w53 <- w53[c("week","year","hr","val")]
  colnames(w53) <- c("week","year","hr","val_add")
  
  data <- data[(data$week!=53),]
  data <- merge(data,w53, by = c("week","year","hr"), all.x = T)
  data$val <- rowSums(data[c("val","val_add")], na.rm = T)
  
  data <- data[c("week","year","date","hr","val")]
  names(data)[names(data) == "val"] <- val
  
  ### ADD AREAS - FACTOR ACCORDINGLY
  
  areas$area <- NA
  areas$area[areas$pt=="NL" | areas$pt=="NS" | areas$pt=="PE" | areas$pt=="NB"] <- "Atlantic Canada"
  areas$area[areas$pt=="ON" | areas$pt=="QC"] <- "Central Canada"
  areas$area[areas$pt=="MB" | areas$pt=="AB" | areas$pt=="SK" | areas$pt=="BC"] <- "Western / Prairie Provinces"
  areas$area[areas$pt=="NT" | areas$pt=="NU" | areas$pt=="YT"] <- "Northern Canada"
  
  atlantic_can <- areas$hr[areas$area=="Atlantic Canada"]
  central_can <- areas$hr[areas$area=="Central Canada"]
  western_can <- areas$hr[areas$area=="Western / Prairie Provinces"]
  northern_can <- areas$hr[areas$area=="Northern Canada"]
  
  data$area[data$hr %in% atlantic_can] <- "Atlantic Canada"
  data$area[data$hr %in% central_can] <- "Central Canada"
  data$area[data$hr %in% western_can] <- "Western / Prairie Provinces"
  data$area[data$hr %in% northern_can] <- "Northern Canada"
  
  names(data)[names(data) == "hr"] <- "region" # rename to region for simplicity
  
  return(data)
  
}

##### ----- WASTEWATER DATA

#!!!# For now, fix missing regional data:
wastewater_data$region[wastewater_data$sub_region_1=="Saskatoon"] <- "SK"
wastewater_data$region[wastewater_data$sub_region_1=="Prince Albert"] <- "SK"
wastewater_data$region[wastewater_data$sub_region_1=="North Battleford"] <- "SK"
wastewater_data$region[wastewater_data$sub_region_1=="Moncton"] <- "NB"

wastewater_pt <- process_pt_data(wastewater_data,"value","mean")
wastewater_hr <- process_hr_data(wastewater_data,"sub_region_1","region","value","mean")

wastewater_pt$area <- factor(wastewater_pt$area, levels = c("Western / Prairie Provinces","Central Canada","Atlantic Canada"))
wastewater_hr$area <- factor(wastewater_hr$area, levels = c("Western / Prairie Provinces","Central Canada","Atlantic Canada"))

##### ----- HOSPITALIZATION DATA

hospitalization_data <- process_pt_data(hospitalizations_pt,"value","total")
hospitalization_data$area <- factor(hospitalization_data$area, levels = c("Western / Prairie Provinces","Central Canada","Atlantic Canada","Northern Canada"))

##### ----- MORTALITY DATA

mortality_data <- process_pt_data(deaths_pt,"value_daily","total")
mortality_data$area <- factor(mortality_data$area, levels = c("Western / Prairie Provinces","Central Canada","Atlantic Canada","Northern Canada"))

# load analytics (if available)
analytics <- if (file.exists("google-analytics.html")) {
  shiny::tags$head(shiny::includeHTML("google-analytics.html"))
} else {}
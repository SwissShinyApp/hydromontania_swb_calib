## this is only to aquire weather data
library(here)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(jsonlite)
library(ggplot2)
library(jsonlite)
library(glue)
library(lubridate)
library(leaflet)
library(sf)
library(htmlwidgets)
# read station file


###--- GETTING WEATHER DATA FUNCTIONS ----

# station table
GET_station_table <- function(country, parameter){
  url <- glue("https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/find_station?location={country}&parameters={parameter}")
  a<-httr::GET(url) %>%httr::content( type = "text", encoding="UTF-8") %>%
    data.table::fread(sep = ";") %>% as.data.frame() %>%
    tidyr::separate(`Location Lat,Lon`, into = c("lat", "lon"), sep = ",", convert = TRUE) %>%
    dplyr::mutate(date = `Start Date`, end = `End Date`) %>%
    dplyr::select(all_of(c("ID Hash", "wmo_id" ="WMO ID", "alt_id" = "Alternative IDs","Name", "lon", "lat", "date", "end")))
}

# call statio data
call_station_data <- function(wmo_id, alt_id, parameter, date, end, ... ){
  parse_time <- function(date_string){
    lubridate::ymd(date_string) %>% paste("23:59:59Z", sep = "T")
  }
  
  end <- end - days(1)
  
  if (is.na(wmo_id)){
    id <- paste0("id_", alt_id)
  } else {
    id <- paste0("wmo_", wmo_id)
  }
  #https://api.meteomatics.com/2025-03-09T00Z--2025-03-10T00Z:PT1H/t_2m:C,wind_speed_10m:ms/wmo_066700/html?source=mix-obs&on_invalid=fill_with_invalid
  #https://api.meteomatics.com/2025-03-10T11:05:00.000+01:00--2025-03-17T11:05:00.000+01:00:P1D/t_2m:C/51.5073219,-0.1276474/csv?model=mix
  url <- glue("https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/{parse_time(as.Date(date))}--{parse_time(as.Date(end))}:P1D/{parameter}/{id}/csv?source=mix-obs&on_invalid=fill_with_invalid")
  X <- tryCatch({
    httr::GET(url) %>% httr::content( type = "text", encoding="UTF-8") %>%
      data.table::fread(sep = ";") %>% 
      as.data.frame() %>%
      rename_with(.cols = everything(),~ c("date", "rain_station")) %>%
      dplyr::mutate(date = date + lubridate::seconds(1))
  }, 
  error = function(e) {
    # Handle errors
    print("error")
    return(NA)  # Return NULL if there's an error
  })
  
  return(X)
  
}

# call the model data
call_model_data_station <- function(lon, lat, date, end, parameter, ... ){
  
  parse_time <- function(date_string){
    lubridate::ymd(date_string) %>% paste("23:59:59Z", sep = "T")
  }
  
  end <- end - days(1)
  # https://api.meteomatics.com/2025-03-10T11:05:00.000+01:00--2025-03-17T11:05:00.000+01:00:P1D/t_2m:C/51.5073219,-0.1276474/csv?model=mix
  url <- glue("https://{Sys.getenv('mm_user')}:{Sys.getenv('mm_password')}@api.meteomatics.com/{parse_time(as.Date(date))}--{parse_time(as.Date(end))}:P1D/{parameter}/{lon},{lon}/csv?model=mix")
  X <- tryCatch({
    httr::GET(url) %>% httr::content( type = "text", encoding="UTF-8") %>%
      data.table::fread(sep = ";") %>% 
      as.data.frame() %>%
      rename_with(.cols = everything(),~ c("date", "rain_mmmix")) %>%
      dplyr::mutate(date = date + lubridate::seconds(1))
  }, 
  error = function(e) {
    # Handle errors
    print("error")
    return(NA)  # Return NULL if there's an error
  })
  return(X)
}


# manages the data calls per loc id
RUN_data_extraction <- function(X, loc_id, ...){
  
  station_data <- X %>% purrr::pmap(., .f = call_station_data, parameter = parameter) %>% setNames(X$station_index)
  model_at_station <- X %>% purrr::pmap(., .f = call_model_data_station, parameter = parameter) %>% setNames(paste0("model_at_", X$station_index))
  
  loc_lon <- X$loc_lon[1]
  loc_lat <- X$loc_lat[1]
  date <- min(X$date)
  end <- max(X$end)
  model_at_sensor <- call_model_data_station(lon = loc_lon, lat = loc_lat, date = date, end = end, parameter)
  
  list(
    loc_id = loc_id,
    loc_lon = loc_lon, loc_lat = loc_lat,
    date = date, end = end,
    meta_data = X,
    station_data = station_data,
    model_at_station = model_at_station,
    model_at_sensor = model_at_sensor
  )
  
}


###--- PLOTTERS FUNCTIONS ----

CREATE_leaflet <- function(X){

  X <- X %>% 
    dplyr::select(everything(), lon = ends_with("lon"), lat = ends_with("lat")) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)



  map <- leaflet::leaflet() %>% 
    leaflet::addProviderTiles(providers$Esri.WorldImagery) %>%
    # leaflet::setView(lng = cnt_ln, lat = cnt_lt, zoom = 6) %>%
    leaflet::addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters") %>%
    leaflet::addMarkers(
      data = X,
      popup = ~paste0(
        "<b>", loc_id, "</b><br/>",
        "Name: ", loc_name, "<br/>"
      )
    )
  return(map)
}


ADD_stations <- function(X, map){

  X <- X %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  m2 <- map %>% 
    leaflet::addCircleMarkers(
    data = X,
    radius = 4,

    stroke = TRUE,
    color = "red",
    weight = 2,
    opacity = 0.5,
    popup = ~paste0(
      "<b>Name: ", Name, "</b><br/>",
      "Dist km: ", round(distance_m / 1000, 2), "<br/>",
      "<b>to Sensor: ", loc_name, "</b><br/>",
      "wmo_id: ", `wmo_id`, "<br/>",
      "Alternative IDs: ", `alt_id`, "<br/>",
      "From: ", date, "<br/>",
      "To: ", end, "<br/>"
    )
  )
  return(m2)
  
}

###--- FINDING CLOSEST STATIONS FUNCTIONS ----

calculate_distance <- function(lon, lat, loc_lon, loc_lat, ...) {
  point1 <- st_sfc(st_point(c(lon, lat)), crs = 4326)
  point2 <- st_sfc(st_point(c(loc_lon, loc_lat)), crs = 4326)
  as.numeric(st_distance(point1, point2))
}

euclidean_distance <- function(A, B) {
  sqrt(outer(A[, 1], B[, 1], FUN = "-")^2 + 
         outer(A[, 2], B[, 2], FUN = "-")^2)
}

find_closest <- function (sensors, stations, n_station = 3, max_dist = 20000){

  sensor_m <- sensors %>% sf::st_drop_geometry() %>% dplyr::select(loc_lon, loc_lat) %>% as.matrix()
  station_m <- cbind(stations$lon, stations$lat)
  stations <- stations %>% mutate(station_index = row_number())
  
  dist_matrix <- euclidean_distance(sensor_m, station_m)
  
  X <- dist_matrix %>%
    as.data.frame() %>% 
    mutate(loc_id = sensors$loc_id) %>%
    tidyr::pivot_longer(-loc_id, names_to = "station_index", values_to = "approx_distance") %>%
    mutate(station_index = as.integer(gsub("V", "", station_index))) %>%
    group_by(loc_id) %>%
    dplyr::slice_min(order_by = approx_distance, n = n_station) %>%
    ungroup()%>%
    left_join(stations, by = join_by(station_index)) %>%
    left_join(sensors, by = join_by(loc_id)) %>%
    dplyr::mutate(distance_m = purrr::pmap_dbl(., .f =calculate_distance)  ) %>%
    dplyr::filter(distance_m <= max_dist)

  return(X)
}


###--- TOP LINE ----

# getting locs and inspect
sensors <- read.csv(here("data", "stations_check_2022.csv"))
m1 <- CREATE_leaflet(sensors)
saveWidget(m1, here("output_stations", "sensors.html"), selfcontained = TRUE)

## passwords to be deleted
Sys.setenv("mm_user" = "")
Sys.setenv("mm_password" = "")


# define parameters country and call station table
parameter <- "precip_24h:mm"
country <- "uk"

# get the stations. also clean doubles
stations <- GET_station_table(country, parameter) %>%
  mutate(tempid = paste0(wmo_id, alt_id)) %>%
  group_by(tempid) %>%
  slice_max(end, with_ties = F) %>%
  ungroup() %>%
  dplyr::select(-tempid)

# find the closest stations for each loc // 5 with max 50km // returns a long table
match_table <- find_closest(sensors, stations, n_station = 5, max_dist = 50000)

# extract the data for each loc_id
X <- match_table %>% 
  group_by(loc_id) %>%
  group_map(~ RUN_data_extraction(.x, .y)) 
Y <- setNames(X, unique(match_table$loc_id))
saveRDS(Y, here("output_stations", "complete_data_set.rds"))  






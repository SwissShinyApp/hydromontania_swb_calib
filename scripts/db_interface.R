### DOCUMENTATION: 
### This script is the Interface between R code and the DB.
### For safety reasons, always use these functions, rather than coding queries by yourself.


### DEPENDENCIES: Packages for DATA --------------------------

# Load Packages
library(here)
library(dplyr)
library(dbplyr)
library(tidyr)
library(lubridate)
library(DBI)
library(RPostgres)



## ------------------- DB HELPERS -----------------------

# makes a db pool connection to the DB
make_db_conn <- function(){
  DBI::dbConnect(
    RPostgres::Postgres(), 
    dbname = Sys.getenv("db_name"), 
    host = Sys.getenv("db_host"), 
    port = as.integer(Sys.getenv("db_port")), 
    user = Sys.getenv("db_user"),
    password = Sys.getenv("db_password")
  )
}

# QUERY: DB Execute Query with Variables for Updating // uses SQL glue for safety
execute_query <- function(base_qry, con, row, row2 = NULL, row3 = NULL, col = NULL, value = NULL, ...){
  res<-tryCatch({
    qry <- glue::glue_sql(base_qry, .con = con)
    DBI::dbExecute(con, DBI::sqlInterpolate(DBI::ANSI(), qry))
  }, error = function(e) {
    -1
  })
}

# meta query // gets the table attributes
db_table_parameters_postgres <- function(tbl, con, dbname){
  sql<-"SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = ?dbname AND TABLE_NAME = ?tbl;"
  query <- sqlInterpolate(con = con, sql, tbl = tbl, dbname =  dbname)
  
  res <- try(DBI::dbGetQuery(con, query),  silent = TRUE)
  ret<-FALSE
  if (class(res) != "try-error"){
    
    if (nrow(res) > 0){
      ### HERE I CAN GIVE BACK MANY DIFFERENT PARAMETERS LIKE VARTYPE AND LENGHT TO MAKE DATA CHECKING
      ret <-list(
        all_cls = res %>% dplyr::pull(column_name),
        not_null = res %>% dplyr::filter(is_nullable == "NO")%>%dplyr::pull(column_name)
      )
    }
  }
  return(ret)
}


## ------------------- PREPARING DATA TO AND FROM THE DB -----------------------

# Given an imported and sliced data frame from the DB, this function checks if all data are complete.
# it checks the expected n of days for each combination of variable and fields
# it returns only field ids
field_data_complete <- function(X, min_date, max_date, variable){
  l_days <- as.integer(difftime(max_date, min_date, units = "days"))
  a<- X %>% 
    dplyr::group_by(field_id, var_id) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>% 
    dplyr::mutate(is_complete = as.integer(n == l_days)) %>%
    dplyr::group_by(field_id) %>%
    dplyr::summarise(is_complete = sum(is_complete) == length(variable)) %>%
    dplyr::filter(is_complete) %>%
    dplyr::pull(field_id)
}


# For forecast data we will have multiple data entries for a same date, each forecasted and registered
# different days (t+6 days, t+5 days, ...). These entries for the same date are identified by the dimension_id.
# This function takes the most recent forecasted data.
filter_recent_forecast <- function(X) {
  X %>%
    dplyr::group_by(field_id, date, var_id) %>%
    dplyr::arrange(dimension_id, by_group = TRUE) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()
}


# Imports a set of meteo data. It filters them by field_id, variable, min_date, max_date, dimension
# if flag "check" is TRUE, then it checks for completeness of data and returns only complete fields
IMPORT_meteo_data <- function(field, variable, min_date, max_date, dimension = 2, check = TRUE){
  X <- GET_meteo_data(field = field,
                      variable = variable, 
                      dimension = dimension, 
                      min_date = min_date, 
                      max_date = max_date
  ) %>% 
    filter_recent_forecast()
  if (isFALSE(check)){
    return(X)
  } else {
    complete_fields <- X %>% 
      filter(!is.na(value)) %>% 
      field_data_complete(min_date, max_date, variable)
    Y <- X %>% dplyr::filter(field_id %in% complete_fields)
    return(Y)
  }
}

# Takes a data frame as it comes from the DB and reframes it to a style, which is used in the code
# uses the "var_short" as key to translate from standard varnames to arbitrary names used in the code
REFRAME_as_wide <- function(X){
  variable <- GET_variables()
  X %>%
    dplyr::group_by(field_id) %>%
    dplyr::left_join(variable, by = dplyr::join_by(var_id)) %>%
    dplyr::select(field_id, var_short, date, value) %>%
    tidyr::pivot_wider(names_from = "var_short", values_from = "value") %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::ungroup()
}

# Takes a data frame as it comes from the R code and reframes it to be used in the DB.
# uses the "var_short" as key to translate from standard varnames to arbitrary names used in the code
REFRAME_as_long <- function(X){
  variable <- GET_variables()
  if (!"dimension_id" %in% names(X)) X$dimension_id <- NA
  X %>% 
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = -c("field_id", "date", "dimension_id"), names_to = "var_short", values_to = "value") %>%
    dplyr::left_join(variable, by = dplyr::join_by(var_short)) %>%
    dplyr::mutate(depth = 0, 
                  dimension_id = ifelse(is.na(dimension_id), 2, dimension_id)) %>%
    dplyr::select(all_of(c("field_id", "var_id", "dimension_id", "date","depth","value")))
}

# Adds metadata for a field (like lon, lat). X must have field_id. 
ADD_field_info <- function(X, return_cols = c("field_lon", "field_lat")){
  fields <- GET_fields(field = field_to_run)
  X %>%
    dplyr::left_join(fields, by = dplyr::join_by(field_id)) %>%
    dplyr::select(any_of(c(colnames(X), return_cols)))
}


## ------------------- DB STORAGE QUERIES -----------------------

# this checks if the email from cognito is already in the db // otherwise returns default
check_get_email_id <- function(proxy_user){
  con <- make_db_conn()
  email <- con %>% 
    dplyr::tbl("user_D") %>%
    dplyr::filter(email_id == proxy_user) %>%
    dplyr::pull(email_id)
  DBI::dbDisconnect(con)
  if (isTRUE(length(email) == 1)){
    return(email)
  } else {
    return(FALSE)
  }
}

# Updates entries in existing rows iterates over update table using pool.
UPDATE_DB_table <- function(data, base_qry){
  con <- make_db_conn()
  res <- data %>% dplyr::mutate(res = purrr::pmap(., base_qry = base_qry, con = con, .f = execute_query))
  DBI::dbDisconnect(con)
  return(res)
}

# Delete a table entry with 1 or 2 or 3 row criteria
DELETE_table_row <- function(delete_query, row, row2 = NULL, row3 = NULL){
  data <- tibble(
    row = row,
    row2 = row2,
    row3 = row3
  )
  con <- make_db_conn()
  res <- data %>% dplyr::mutate(res = purrr::pmap(., base_qry = delete_query, con = con, .f = execute_query))
  DBI::dbDisconnect(con)
  return(res)
}

# Adds entire rows to a DB table
ADD_DB_table <- function(data, tbl, dbname = "hydromontania"){
  
  con <- make_db_conn()
  # make sure I select the right columns n32 wt
  tbl_params <- db_table_parameters_postgres(tbl = tbl, con = con, dbname = dbname)

  res <- tryCatch({
    # make sure i only store cols which are there
    data <- data %>% dplyr::select(any_of(tbl_params$all_cls))
    old <- dplyr::tbl(con, dbplyr::in_schema("hydromontania", tbl)) %>% collect()
    to_store <- anti_join(data, old, by = tbl_params$not_null)
    RPostgres::dbWriteTable(con, name = Id(schema = "hydromontania", table = tbl), value = to_store, append = TRUE, overwrite = FALSE, row.names=FALSE)
    ## transform T/F into 1/0 >> is needed because RmySQL driver
    # data <- data %>% dplyr::mutate(dplyr::across(dplyr::where(is.logical), as.numeric))
  }, error = function(e) {
    FALSE
  })
  DBI::dbDisconnect(con)
  return(res)
}


## ------------------- DB SPECIFIC IMPORT QUERIES -----------------------
# All these functions are save and lazy queries. It means that they create a filtered query and only
# import (collect) what we really need.This saves memory space.

# Main Meteo Data Importer // takes optional arguments for slicing. If argument is ignored, then the entire dimension space is returned
GET_meteo_data <- function(field = NA, variable = NA, dimension = NA, min_date = NA, max_date = NA, depth = 0, ...){
  con <- make_db_conn()
  res <- tryCatch({
    X <- dplyr::tbl(con, dbplyr::in_schema("hydromontania", "field_data_X"))
    if (all(!is.na(field))){
      X <- X %>% dplyr::filter(field_id %in% field)
    }
    if (all(!is.na(variable))){
      X <- X %>% dplyr::filter(var_id %in% variable)
    }
    if (all(!is.na(dimension))){
      X <- X %>% dplyr::filter(dimension_id %in% dimension)
    }
    if (all(!is.na(depth))){
      mn <- min(depth)
      mx <- max(depth)
      X <- X %>% dplyr::filter(dplyr::between(depth, mn, mx))
    }
    if (all(!is.na(min_date), lubridate::is.Date(min_date))){
      X <- X %>% dplyr::filter(date > min_date)
    }
    if (all(!is.na(max_date), lubridate::is.Date(max_date))){
      X <- X %>% dplyr::filter(date <= max_date)
    }
    if (all(!is.na(max_date), lubridate::is.Date(max_date))){
      X <- X %>% dplyr::filter(date <= max_date)
    }
    res <- X %>% dplyr::collect()
    res
  }, error = function(err) {
    showNotification("Attention: something went wrong with GET_meteo_data.", duration = 5, type = "error")
    NULL
  })
  DBI::dbDisconnect(con)
  return(res)
}

# Soil Data Importer 
GET_soil_data <- function(field = NA, variable = NA, dimension = NA, min_date = NA, max_date = NA, depth = 0, ...){
  con <- make_db_conn()
  res <- tryCatch({
    X <- dplyr::tbl(con, dbplyr::in_schema("hydromontania", "test_soil_params_X"))
    if (all(!is.na(field))){
      X <- X %>% dplyr::filter(field_id %in% field)
    }
    if (all(!is.na(variable))){
      X <- X %>% dplyr::filter(var_id %in% variable)
    }
    # if (all(!is.na(dimension))){
    #   X <- X %>% dplyr::filter(dimension_id %in% dimension)
    # }
    # if (all(!is.na(depth))){
    #   mn <- min(depth)
    #   mx <- max(depth)
    #   X <- X %>% dplyr::filter(dplyr::between(depth, mn, mx))
    # }
    # if (all(!is.na(min_date), lubridate::is.Date(min_date))){
    #   X <- X %>% dplyr::filter(date > min_date)
    # }
    # if (all(!is.na(max_date), lubridate::is.Date(max_date))){
    #   X <- X %>% dplyr::filter(date <= max_date)
    # }
    X <- X %>%
      group_by(field_id, var_id) %>%
      # slice_max(date, n = 1)
      summarise(value = mean(value, na.rm = TRUE))
    X <- X %>% dplyr::collect()
    variable <- GET_variables()
    res <- X %>%
      dplyr::group_by(field_id) %>%
      dplyr::left_join(variable, by = dplyr::join_by(var_id)) %>%
      dplyr::select(field_id, var_short, value) %>%
      tidyr::pivot_wider(names_from = "var_short", values_from = "value") %>%
      dplyr::arrange(field_id, .by_group = TRUE) %>%
      dplyr::ungroup()
    
    res
  }, error = function(err) {
    # showNotification("Attention: something went wrong with GET_soil_data.", duration = 5, type = "error")
    print("Attention: something went wrong with GET_soil_data.")
    NULL
  })
  DBI::dbDisconnect(con)
  return(res)
}

# Imports field information. Can be filtered by field id or farm id. Empty means everything is returned.
GET_fields <- function(field = NA, farm = NA){
  con <- make_db_conn()
  res <- tryCatch({
    X <- dplyr::tbl(con, dbplyr::in_schema("hydromontania", "field_F"))
    if (all(!is.na(field))){
      X <- X %>% dplyr::filter(field_id %in% field)
    }
    if (all(!is.na(farm))){
      X <- X %>% dplyr::filter(farm_id %in% farm)
    }
    res <- X %>% 
      dplyr::select(any_of(c("field_id", "field_name", "field_lon", "field_lat", "field_elevation", "farm_id"))) %>%
      dplyr::collect()
    res
  }, error = function(err) {
    showNotification("Attention: something went wrong with GET_meteo_data.", duration = 5, type = "error")
    NULL
  })
  DBI::dbDisconnect(con)
  return(res)
}

# imports variables information
GET_variables <- function(){
  con <- make_db_conn()
  res <- tryCatch({
    dplyr::tbl(con, dbplyr::in_schema("hydromontania", "variable_D")) %>% 
      dplyr::collect()
  }, error = function(err) {
    showNotification("Attention: something went wrong with GET_variables", duration = 5, type = "error")
    NULL
  })
  DBI::dbDisconnect(con)
  return(res)
}

# imports the data dimensions
GET_dimensions <- function(){
  con <- make_db_conn()
  res <- tryCatch({
    dplyr::tbl(con, dbplyr::in_schema("hydromontania", "dimension_D")) %>% 
      dplyr::collect()
  }, error = function(err) {
    showNotification("Attention: something went wrong with GET_dimensions", duration = 5, type = "error")
    NULL
  })
  DBI::dbDisconnect(con)
  return(res)
}


## ------------------- OLD STUFF -----------------------
# 
# # obtains the function to transform everything incl 100/
# get_transform_func <- function(dimension_id, params){
#   func_name <- params$sys_params$data_dimension %>% dplyr::filter(dimension_id == !!dimension_id) %>%
#     mutate(func_name = case_when(
#       imperial_units == 1 ~paste(var_id, "imperial", sep = "_"), 
#       TRUE ~ paste(var_id, "metric", sep = "_")
#     )
#     ) %>% pull(func_name)
#   if (func_name %in% names(params$sys_params$transform_function)){
#     return(params$sys_params$transform_function[[func_name]])
#   } else {
#     return(function(x){x/100})
#   }
# }
# 
# # Retrieves the child polygons to a given poly_key and with a target type and returns them as an sf object.
# GET_poly_geo <- function(poly_key = NA, type) {
#   if ((!is.na(poly_key) && is.na(as.numeric(poly_key)))|| is.na(as.numeric(type))) {
#     showNotification("Invalid input / Safety reasons.", duration = 5, type = "error")
#     return(NULL)
#   }
#   # Construct the query
#   if (is.na(poly_key) || type == 0) {
#     query <- sprintf("CALL get_polygons(NULL, %d);", as.numeric(type))
#   } else {
#     query <- sprintf("CALL get_polygons(%d, %d);", as.numeric(poly_key), as.numeric(type))
#   }
#   
#   con <- get_MariaDB_conn("terradb")
#   result <- tryCatch({
#     res <- dbGetQuery(con, query)
#     res$geometry <- st_as_sfc(res$geom, crs = 4326)
#     st_as_sf(res, sf_column_name = "geometry") %>% dplyr::select(-geom)
#   }, error = function(err) {
#     showNotification("Invalid input / No data.", duration = 5, type = "warning")
#     NULL
#   })
#   dbDisconnect(con)
#   return(result)
# }
# 
# # Imports Alerts // dbplyr
# GET_alert_tbl <-function(n_days = 60) {
#   
#   classify_alerts <- function(X){
#     X %>% dplyr::arrange(desc(percentile)) %>%
#       mutate(class = case_when(percentile >= 0.99 ~"Severe",
#                                percentile >= 0.95 & percentile < 0.99 ~"Elevated",
#                                percentile >= 0.85 & percentile < 0.95 ~"Moderate",
#                                TRUE ~"Normal"
#       ),
#       
#       )
#   }
#   # join
#   usr <- Sys.getenv("terra_user")
#   clnms <- c("alert_id","dimension_id","poly_key","event_date","event_duration","event_type","event_txt","frequency","severity")
#   country <- GET_geo_tbl(0) %>%  dplyr::select(poly_key, country_name = name)
#   admin_1 <- GET_geo_tbl(1)%>%  dplyr::select(poly_key, admin_1_name = name)
#   admin_2 <- GET_geo_tbl(2)%>%  dplyr::select(poly_key, admin_2_name = name)
#   event_type_ref <- client_db_pool %>% dplyr::tbl("event_type_D")
#   user_data_alert_ref <- client_db_pool %>% dplyr::tbl("user_data_alert_R") #%>% dplyr::select(email_dims = email_id, dimension_id) #%>% collect()
#   
#   # This part queries on DB
#   X <- client_db_pool %>% 
#     dplyr::tbl("data_alert_F") %>%
#     dplyr::left_join(event_type_ref, by = join_by(event_type_id)) %>%
#     mutate(
#       country_key = floor(poly_key / 1000000) * 1000000,
#       admin_1_key = floor(poly_key / 1000) * 1000,
#       admin_2_key = poly_key
#     ) %>%
#     dplyr::left_join(user_data_alert_ref, by = join_by(country_key == poly_key, event_type_id)) %>%
#     dplyr::filter(email_id == usr) %>%
#     left_join(country, by = c("country_key" = "poly_key")) %>%
#     left_join(admin_1, by = c("admin_1_key" = "poly_key")) %>%
#     left_join(admin_2, by = c("admin_2_key" = "poly_key")) %>%
#     collect() %>%
#     dplyr::arrange(desc(event_date)) %>%
#     classify_alerts()
#   
#   # temporary filtering // once we have up to date data we can use only sysdate - ndays
#   today <- max(X$event_date, na.rm = T)
#   Y <- X %>% dplyr::filter(event_date > (today - lubridate::days(30))) %>% slice(1:1000)
# }
# 
# # Imports User // dbplyr
# GET_user_tbl <-function() {
#   
#   usr <- Sys.getenv("terra_user")
#   country_ref <- GET_geo_tbl("0")
#   
#   client_db_pool %>% dplyr::tbl("user_D") %>%
#     left_join(country_ref, by = join_by(user_country == poly_key)) %>%
#     dplyr::filter(email_id == usr) %>%
#     collect() %>% 
#     dplyr::mutate(user_country = name) %>%
#     dplyr::select(-any_of(c("code","name","type")))
# }
# 
# # Imports Alerts // dbplyr
# GET_alert_criteria_tbl <-function() {
#   
#   # join
#   usr <- Sys.getenv("terra_user")
#   clnms <- c("alert_id","dimension_id","poly_key","event_date","event_duration","event_type","event_txt","frequency","severity")
#   country <- GET_geo_tbl(0) %>%  dplyr::select(poly_key, country_name = name)
#   event_type_ref <- client_db_pool %>% dplyr::tbl("event_type_D")
#   
#   
#   # This part queries on DB
#   X<- client_db_pool %>% dplyr::tbl("user_data_alert_R") %>%
#     dplyr::left_join(event_type_ref, by = join_by(event_type_id)) %>%
#     dplyr::left_join(country, by = join_by(poly_key)) %>%
#     dplyr::filter(email_id == usr) %>%
#     dplyr::select(email_id, poly_key, country_name, event_type_id, event_type_name) %>%
#     collect()
# }
# 



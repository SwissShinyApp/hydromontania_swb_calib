
library(here)
library(stringr)
library(ggplot2)
library(zoo)
library(purrr)
source(here("scripts", "helper_etp_swb.R"), local = FALSE)


# run swb model -----------------------------------------------------------


# Defining the parameters for this run
max_date <- Sys.Date()
min_date <- max_date - lubridate::years(10) 

loc_to_run <- c(1:100)

vars <- c(11, 19)

soil_params_grid <- expand.grid(
  CN = c(30, 70),
  DC = c(0.3, 0.5, 0.7),
  MUF = c(0.05, 0.2),
  WATfc = c(26, 100, 200)
) %>% 
  mutate(WATwp = case_when(
    WATfc == 26 ~ 2,
    WATfc == 100 ~ 60,
    WATfc == 200 ~ 120),
    soil_params_id = row_number()
  )


con <- make_db_conn()

X1 <- dplyr::tbl(con, dbplyr::in_schema("hydromontania", "calibration_weather_X")) %>% 
  dplyr::filter(loc_id %in% loc_to_run) %>% 
  dplyr::filter(var_id %in% vars) %>% 
  dplyr::filter(date > min_date) %>% 
  dplyr::filter(date <= max_date) %>% 
  dplyr::collect()

variable <- GET_variables()

a <- X1 %>% 
  dplyr::group_by(loc_id) %>%
  dplyr::mutate(l_days = as.integer(difftime(max(date), min(date), units = "days")) + 1) %>% 
  ungroup() %>% 
  dplyr::group_by(loc_id, var_id, l_days) %>%
  dplyr::summarise(n = n(),
                   .groups = "drop") %>% 
  dplyr::mutate(is_complete = as.integer(n == l_days)) %>%
  dplyr::group_by(loc_id) %>%
  dplyr::summarise(is_complete = sum(is_complete) == length(vars)) %>%
  dplyr::filter(is_complete)# %>%

X <- X1 %>% 
  dplyr::group_by(loc_id) %>%
  dplyr::left_join(variable, by = dplyr::join_by(var_id)) %>%
  dplyr::select(loc_id, var_short, date, value) %>%
  tidyr::pivot_wider(names_from = "var_short", values_from = "value") %>%
  dplyr::arrange(date, .by_group = TRUE) %>%
  dplyr::ungroup() %>% 
  dplyr::cross_join(soil_params_grid)
# ADD_field_info(c("field_lon", "field_lat", "field_elevation"))
  
  # Calculates the SWB by field and join to X1 to get the dimension_id
swb_modeled <- X %>%
  dplyr::group_by(loc_id, soil_params_id) %>%
  dplyr::group_modify(.f = RUN_swb_calc) %>%
  dplyr::ungroup() 

# swb_modeled %>% write_rds(here("data", "swb_calibration", "swb_calibr_params_grid.rds"))
swb_modeled <- read_rds(here("data", "swb_calibration", "swb_calibr_params_grid.rds"))
# soil_params_grid %>% write_rds(here("data", "swb_calibration", "soil_params_grid.rds"))


# get measured data -------------------------------------------------------



sm_tens <- dplyr::tbl(con, dbplyr::in_schema("hydromontania", "calibration_data_X")) %>% 
  dplyr::filter(loc_id %in% loc_to_run) %>% 
  dplyr::filter(var == "sm_tens") %>% 
  dplyr::collect()  

review_reps <- sm_tens %>% 
  filter(depth == 20) %>% 
  count(loc_id, date) %>% 
  group_by(loc_id) %>% 
  summarise(max_n = max(n),
            mean_n = mean(n)) 

sm_tens_sum <- sm_tens %>% 
  group_by(loc_id, date) %>% 
  summarise(sm_tens_mean = mean(value),
            depths = list(sort(unique(depth)))) %>% 
  ungroup()

loc_depths <- sm_tens_sum %>% 
  group_by(loc_id) %>% 
  summarise(depth = paste0("0-", str_flatten(unique(unlist(depths)), "-"))) %>% 
  ungroup()

sm_tens_sum <- sm_tens_sum %>% 
  select(-depths) %>% 
  left_join(loc_depths, by = "loc_id") %>% 
  left_join(sm_tens %>% 
              filter(depth == 20) %>% 
              select(loc_id, date, sm_tens_20 = value),
              by = c("loc_id", "date")) %>% 
  mutate(date = as_date(date))


DBI::dbDisconnect(con)




# volumetric / tension soil moisture --------------------------------------


#   write_sf("locations.geojson")

sm_vol <- dplyr::tbl(con, dbplyr::in_schema("hydromontania", "calibration_data_X")) %>% 
  dplyr::filter(loc_id %in% loc_to_run) %>% 
  dplyr::filter(var == "sm_vol") %>% 
  dplyr::collect()  



sm_vol_filt <- sm_vol %>% 
  filter(between(value, 0.01, 100)) %>%
  mutate(date = as_date(date))

sm_comparison <- sm_vol_filt %>% 
  select(loc_id, date, sm_vol = value) %>% 
  left_join(sm_tens_sum %>% select(loc_id, date, sm_tens_20, sm_tens_mean), by = c("loc_id", "date")) %>% 
  filter(
    between(sm_tens_mean, 0.01, 100),
    between(sm_tens_20, 0.01, 100)
  )

sm_comparison$loc_id %>% unique()
sm_comparison %>% 
  ggplot(aes(sm_vol, sm_tens_20)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) + 
  facet_wrap(~ loc_id, scales = "free")  +
  theme_minimal()

sm_comparison %>% 
  ggplot(aes(sm_vol, sm_tens_mean)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) + 
  facet_wrap(~ loc_id, scales = "free")  +
  theme_minimal()

sm_comparison %>% 
  mutate(loc_id = factor(loc_id)) %>% 
  ggplot(aes(sm_vol, sm_tens_20, color = loc_id)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) + 
  theme_minimal()


# modeled-measured intial exploration --------------------------------------------

sm_thresholds <- c(0.01, 100)
# sm_thresholds <- c(10, 70)

sm_tens_sum_filt <- sm_tens_sum %>% 
  filter(
    between(sm_tens_mean, sm_thresholds[1], sm_thresholds[2]),
    between(sm_tens_20, sm_thresholds[1], sm_thresholds[2])
  )

# drop first rows as swb model starts from unknown value
swb_modeled_slice <- swb_modeled %>% 
  group_by(loc_id, soil_params_id) %>% 
  arrange(date, .by_group = TRUE) %>% 
  slice(21:n()) %>% 
  ungroup()

results <- swb_modeled_slice %>%
  left_join(sm_tens_sum_filt, by = c("loc_id", "date")) %>%
  group_by(loc_id, soil_params_id) %>%
  summarise(cor_avail_mean = cor(AVAIL, sm_tens_mean, use = "complete.obs"),
            cor_avail_20 = cor(AVAIL, sm_tens_20, use = "complete.obs"),
            cor_hydrostate_mean = cor(hydro_state, sm_tens_mean, use = "complete.obs"),
            cor_hydrostate_20 = cor(hydro_state, sm_tens_20, use = "complete.obs")) %>%
  ungroup()

results_spearman <- swb_modeled_slice %>%
  left_join(sm_tens_sum_filt, by = c("loc_id", "date")) %>%
  group_by(loc_id, soil_params_id) %>%
  summarise(spear_avail_mean = cor(AVAIL, sm_tens_mean, method = "spearman", use = "complete.obs"),
            spear_avail_20 = cor(AVAIL, sm_tens_20, method = "spearman", use = "complete.obs"),
            spear_hydrostate_mean = cor(hydro_state, sm_tens_mean, method = "spearman", use = "complete.obs"),
            spear_hydrostate_20 = cor(hydro_state, sm_tens_20, method = "spearman", use = "complete.obs")) %>%
  ungroup()

best_params <- results %>%
  group_by(loc_id) %>%
  summarise(best_sp_avail = soil_params_id[which.max(abs(cor_avail_mean))],
            best_sp_avail_corr = max(abs(cor_avail_mean)),
            best_sp_hydro = soil_params_id[which.max(abs(cor_hydrostate_mean))],
            best_sp_hydro_corr = max(abs(cor_hydrostate_mean))) %>%
  ungroup()

best_params_spearman <- results_spearman %>%
  group_by(loc_id) %>%
  summarise(best_sp_avail = soil_params_id[which.max(abs(spear_avail_mean))],
            best_sp_avail_corr = max(abs(spear_avail_mean)),
            best_sp_hydro = soil_params_id[which.max(abs(spear_hydrostate_mean))],
            best_sp_hydro_corr = max(abs(spear_hydrostate_mean))) %>%
  ungroup()


top_20_locs <- best_params_spearman %>%
  arrange(desc(best_sp_avail_corr)) %>%
  slice(1:20) %>%
  select(loc_id, soil_params_id = best_sp_avail)

top_20_data <- swb_modeled_slice %>%
  semi_join(top_20_locs) %>% 
  left_join(sm_tens_sum_filt, by = c("loc_id", "date")) 

top_20_data %>% 
  ggplot(aes(x = AVAIL, y = sm_tens_mean)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) + 
  facet_wrap(~ loc_id, scales = "free") +
  labs(title = "Scatterplot of AVAIL vs Soil Moisture Tension (sm_tens_mean)",
       x = "Model AVAIL",
       y = "Soil Moisture Tension (sm_tens_mean)") +
  theme_minimal()

worst_20_locs <- best_params %>%
  arrange(best_sp_avail_corr) %>%
  slice(1:20) %>%
  select(loc_id, soil_params_id = best_sp_avail)

worst_20_data <- swb_modeled_slice %>%
  semi_join(worst_20_locs) %>% 
  left_join(sm_tens_sum_filt, by = c("loc_id", "date")) 

worst_20_data %>% 
  ggplot(aes(x = AVAIL, y = sm_tens_mean)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) + 
  facet_wrap(~ loc_id, scales = "free") +
  labs(title = "Scatterplot of AVAIL vs Soil Moisture Tension (sm_tens_mean)",
       x = "Model AVAIL",
       y = "Soil Moisture Tension (sm_tens_mean)") +
  theme_minimal()


# add locations metadata --------------------------------------------------


con <- make_db_conn()
# 
locations_tbl <- dplyr::tbl(con, dbplyr::in_schema("hydromontania", "calibration_locs_F")) %>%
  collect()
locations <- locations_tbl %>%
  st_as_sf(coords = c("loc_lon", "loc_lat"), crs = 4326) #%>%

sss_map <- st_read(con, layer = Id(schema = "hydromontania", table = "sss_map_G"))

locations_int <- locations %>% 
  st_join(sss_map)


DBI::dbDisconnect(con)

# some locations are inside cities/towns and spatial join returns NA for soil properties
# we will join those rows with the closest non NA polygon in SSS map

na_rows <- locations_int %>%
  filter(is.na(z)) %>% 
  select(loc_id:topography)

valid_polygons <- sss_map %>%
  filter(!is.na(z))

nearest_indices <- st_nearest_feature(na_rows, valid_polygons)

nearest_polygons <- valid_polygons[nearest_indices, ]

na_rows_fixed <- na_rows %>%
  bind_cols(nearest_polygons %>% st_set_geometry(NULL))

locations_int_fixed <- locations_int %>%
  filter(!is.na(z)) %>%
  bind_rows(na_rows_fixed)



# time series approach ----------------------------------------------------

# soil moisture tension sensors saturate at both high and low humidities.
# also for an irrigation project the most relevant range of soil moisture values are the  intermediate values, where we must decide to irrigate

# so, we will define chunks of time series where soil moisture reading are at intermediate levels of soil moisture
# we will attempt then to optimize the soil water balance model soil params to maximize the correlation with soil moisture in those chunks.

sm_tens_expanded <- sm_tens_sum %>%
  group_by(loc_id) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  ungroup()

# filter only values of sm tens within the 10 to 70 range
sm_tens_expanded <- sm_tens_expanded %>%
  mutate(valid = if_else(sm_tens_mean >= 10 & sm_tens_mean <= 70, TRUE, FALSE, missing = NA))

# allow up to 2 consecutive values outside the defined range
sm_tens_expanded <- sm_tens_expanded %>%
  group_by(loc_id) %>%
  mutate(
    is_start = ifelse(
      valid & 
        (lag(valid, 1, default = FALSE) == FALSE | is.na(lag(valid, 1, default = FALSE))) & 
        (lag(valid, 2, default = FALSE) == FALSE | is.na(lag(valid, 2, default = FALSE))),
      1, 0
    ),
    is_end = ifelse(
      valid & 
        (lead(valid, 1, default = FALSE) == FALSE | is.na(lead(valid, 1, default = FALSE))) & 
        (lead(valid, 2, default = FALSE) == FALSE | is.na(lead(valid, 2, default = FALSE))),
      1, 0
    ),
    is_start = replace_na(is_start, 0),
    is_end = replace_na(is_end, 0),
    ts_id = cumsum(is_start)
  ) %>%
  ungroup() %>% 
  filter(ts_id > 0)


chunks <- sm_tens_expanded %>%
  group_by(loc_id, ts_id) %>%
  mutate(
    after_end = cumsum(is_end) > 0
  ) %>%
  filter(!(after_end & is_end == 0)) %>%   # Remove rows after the first is_end within the chunk
  mutate(n_chunk = n()) %>% 
  filter(n_chunk >= 5) %>% 
  mutate(
    sm_tens_mean = ifelse(between(sm_tens_mean, 0.01, 99), sm_tens_mean, NA),
    sm_tens_20 = ifelse(between(sm_tens_20, 0.01, 99), sm_tens_20, NA),
    sm_tens_mean  = na.approx(sm_tens_mean, na.rm = F),
    sm_tens_20  = na.approx(sm_tens_20, na.rm = F)
  ) %>% 
  select(-c(is_start, is_end, valid, after_end)) %>% 
  ungroup()

chunk_ids <- chunks %>% 
  group_by(loc_id, ts_id) %>% 
  summarise() %>% 
  ungroup() %>% 
  mutate(chunk_id = row_number())

chunks <- chunks %>% 
  group_by(loc_id, ts_id) %>% 
  mutate(day = row_number()) %>% 
  ungroup() %>% 
  left_join(chunk_ids)

filtered_chunks <- chunks %>%
  group_by(chunk_id) %>%
  filter(
    all(sm_tens_mean[1:2] < 25, na.rm = TRUE),
    all(sm_tens_mean[(n()-1):n()] > 55, na.rm = TRUE)
  ) %>%
  ungroup()

filtered_chunks <- filtered_chunks %>% 
  left_join(swb_modeled_slice) %>% 
  drop_na(AVAIL) %>% 
  group_by(chunk_id) %>% 
  mutate(n_chunk = n_distinct(day)) %>% 
  filter(n_chunk >= 5) %>% 
  ungroup()



chunks %>% write_rds(here("data", "swb_calibration", "time_series_chunks.rds"))

# time series correlation analysis ----------------------------------------

# we'll focus on ts where soil dries progressively to find how AVAIL varies on that same period
# filter chunks where sm_tens start on low values and end on high values


### first lets check the correlation for one single soil params set: soil_params_id = 13
correlations <- filtered_chunks %>%
  filter(soil_params_id == 33) %>% 
  group_by(loc_id, chunk_id, soil_params_id) %>%
  summarize(
    corr = cor(sm_tens_mean, AVAIL, use = "complete.obs", method = "pearson")
  ) %>%
  ungroup()

mean(correlations$corr, na.rm = TRUE)

correlations_loc <- correlations %>% 
  group_by(loc_id) %>% 
  summarise(mean_corr = mean(corr, na.rm = TRUE),
            sd_corr = sd(corr, na.rm = TRUE))

correlations_loc %>% 
  ggplot(aes(loc_id, mean_corr)) + 
  geom_col() + 
  theme_light()


correlations_selection <- correlations %>% 
  left_join(filtered_chunks) %>% 
  filter(n_chunk > 10) %>%
  filter(corr < -.7)

correlations_selection %>% pull(chunk_id) %>% unique() %>% length()
chunk_sample <- correlations_selection %>% 
  distinct(chunk_id) %>% 
  slice_sample(n = 25) 

correlations_selection %>% 
  filter(chunk_id %in% chunk_sample$chunk_id) %>% 
  ggplot() + 
  geom_line(aes(x = day, y = AVAIL, group = chunk_id), color = "blue2") +
  geom_line(aes(x = day, y = sm_tens_mean, group = chunk_id), color = "red3") +
  facet_wrap(~ chunk_id) + 
  theme_light()

max_lag <- 3

cross_corr <- filtered_chunks %>%
  filter(soil_params_id == 33) %>% 
  group_by(loc_id, ts_id, soil_params_id) %>%
  summarize(
    ccf_max = min(ccf(sm_tens_mean, AVAIL, lag.max = max_lag, plot = FALSE)$acf),
    lag_at_max = which.min(ccf(sm_tens_mean, AVAIL, lag.max = max_lag, plot = FALSE)$acf) - 1 - max_lag
  ) %>%
  ungroup()

mean(cross_corr$ccf_max)

cross_correlations_loc <- cross_corr %>% 
  group_by(loc_id) %>% 
  summarise(mean_corr = mean(ccf_max, na.rm = TRUE),
            sd_corr = sd(ccf_max, na.rm = TRUE))

cross_correlations_loc %>% 
  ggplot(aes(loc_id, mean_corr)) + 
  geom_col() + 
  theme_light()

correlations_loc %>% 
  mutate(metric = "corr") %>% 
  bind_rows(cross_correlations_loc %>% 
              mutate(metric = "ccf")) %>% 
  mutate(metric = factor(metric)) %>% 
  ggplot(aes(loc_id, mean_corr, fill = metric)) + 
  geom_col(position = "dodge") + 
  theme_light()

cross_corr %>% 
  ggplot(aes(ccf_max)) + 
  geom_density() + 
  theme_light()

cross_corr %>% 
  ggplot(aes(lag_at_max)) + 
  geom_histogram() + 
  theme_light()

### DTW
# install.packages("dtw")
library(dtw)

safe_dtw <- safely(function(x, y) dtw(x, y)$distance)

dtw <- filtered_chunks %>%
  filter(soil_params_id == 33) %>% 
  group_by(loc_id, chunk_id, soil_params_id) %>%
  mutate(
    sm_tens_inverted = -sm_tens_mean,
    sm_tens_normalized = scale(sm_tens_inverted),
    avail_normalized = scale(AVAIL)
  ) %>% 
  summarize(
    dtw_distance = list(safe_dtw(sm_tens_normalized, avail_normalized))
  ) %>%
  ungroup()

dtw <- dtw %>% 
  unnest(dtw_distance) %>% 
  group_by(chunk_id, soil_params_id) %>% 
  slice_head(n = 1) %>% 
  drop_na(dtw_distance) %>% 
  mutate(dtw_distance = as.numeric(dtw_distance)) %>% 
  ungroup()

dtw_loc <- dtw %>% 
  group_by(loc_id) %>% 
  summarise(mean_dtw_dist = mean(dtw_distance, na.rm = TRUE),
            sd_dtw_dist = sd(dtw_distance, na.rm = TRUE)) %>% 
  ungroup()

dtw_loc %>% 
  ggplot(aes(loc_id, mean_dtw_dist)) + 
  geom_col() + 
  theme_light()

dtw_selection <- dtw %>% 
  left_join(filtered_chunks) %>% 
  filter(n_chunk > 10) %>%
  filter(dtw_distance < 10)

dtw_selection %>% 
  ggplot(aes(dtw_distance)) + 
  geom_density()

dtw_selection %>% pull(chunk_id) %>% unique() %>% length()
chunk_sample <- dtw_selection %>% 
  distinct(chunk_id) %>% 
  slice_sample(n = 25) 
  
scale_factor <- .5
dtw_selection %>% 
  filter(chunk_id %in% chunk_sample$chunk_id) %>% 
  ggplot() + 
  geom_line(aes(x = day, y = AVAIL * scale_factor, group = chunk_id), color = "blue2") +
  geom_line(aes(x = day, y = sm_tens_mean, group = chunk_id), color = "red3") +
  facet_wrap(~ chunk_id) + 
  scale_y_continuous(
    name = "AVAIL",  # Primary y-axis label
    sec.axis = sec_axis(~./scale_factor, name = "sm_tens_mean")  # Secondary y-axis label
  ) +
  theme_light()



# compare soil params -----------------------------------------------------

correlations <- filtered_chunks %>%
  # filter(soil_params_id == 13) %>% 
  group_by(loc_id, chunk_id, soil_params_id) %>%
  summarize(
    corr = cor(sm_tens_mean, AVAIL, use = "complete.obs", method = "pearson")
  ) %>%
  ungroup()

correlations_loc_params <- correlations %>% 
  group_by(loc_id, soil_params_id) %>% 
  summarise(mean_corr = mean(corr, na.rm = TRUE),
            sd_corr = sd(corr, na.rm = TRUE))

# mean corr by soil params set
correlations_loc_params %>% 
  group_by(soil_params_id) %>% 
  summarise(mean_corr = mean(mean_corr, na.rm = TRUE)) %>% 
  ggplot(aes(soil_params_id, mean_corr)) + 
  geom_col() + 
  theme_light()

correlations_sp_count <- correlations_loc_params %>% 
  group_by(loc_id) %>% 
  mutate(rank = rank(mean_corr, ties.method = "random")) %>% 
  filter(rank == 1) %>% 
  count(soil_params_id) %>% 
  arrange(soil_params_id)

correlations_sp_count %>% 
  ggplot(aes(soil_params_id, n)) +
  geom_col() + 
  theme_light()

correlations_sp <- correlations_loc_params %>% 
  group_by(loc_id) %>% 
  mutate(rank = rank(mean_corr, ties.method = "random")) %>% 
  filter(rank == 1) %>%  
  group_by(soil_params_id) %>% 
  summarise(mean_corr_sp = mean(mean_corr, na.rm = TRUE),
            sd = sd(mean_corr, na.rm = TRUE),
            n  = n())

# mean correlation and sd by soil params set for best set per location
correlations_sp %>% 
  ggplot(aes(soil_params_id, mean_corr_sp)) +
  geom_col() + 
  geom_errorbar(aes(ymin=mean_corr_sp-sd, ymax=mean_corr_sp+sd), width=.2)
  theme_light()
  
best_sp_by_loc <- correlations %>% 
  group_by(loc_id, chunk_id) %>% 
  mutate(rank = rank(corr, ties.method = "random")) %>% 
  group_by(loc_id, soil_params_id) %>% 
  summarise(
    n_chunks = n(),
    median_rank = median(rank),
    mean_corr = mean(corr, na.rm = TRUE)
  ) %>% 
  group_by(loc_id) %>% 
  arrange(median_rank, .by_group = TRUE) %>% 
  slice_head(n = 1)
  
best_sp_by_loc %>% 
  ggplot(aes(loc_id, median_rank)) + 
  geom_col() + 
  theme_light()

best_sp_by_loc %>% 
  ggplot(aes(loc_id, mean_corr)) + 
  geom_col() + 
  theme_light()

best_sp_by_loc %>% 
  ggplot(aes(median_rank, mean_corr)) + 
  geom_point() +
  geom_smooth(se =FALSE)
  theme_light()

best_sp_by_loc %>% 
  ggplot(aes(mean_corr)) + 
  geom_density() + 
  theme_light()

best_sp_by_loc %>% 
  ggplot(aes(median_rank)) + 
  geom_density() + 
  theme_light()

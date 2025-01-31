
# Redefine calcWatBal from AquaBEHER to use soil parameters dynamically
calcWatBal <- function(data) {
  
  # CN = 65, DC = 0.55, MUF = 0.1, WATfc = NULL, WATwp = NULL, 
  CN <- data$CN[1]
  DC <- data$DC[1]
  MUF <- data$MUF[1]
  soilWHC <- data$WATfc[1]
  WATwp <- data$WATwp[1]
  # soilWHC <- data$SoilWHC[1]
  
  # Initialize parameters
  S <- 25400 / CN - 254  # Maximum abstraction (for run off)
  IA <- 0.2 * S  # Initial Abstraction (for run off)
  
  # if (is.null(WATfc)) WATfc <- soilWHC
  # if (is.null(WATwp)) WATwp <- 0.15 * soilWHC  
  # Preprocess data is in proper format. Init output vars
  data <- data %>% 
    mutate(
      date = as_date(date),
      Rain = ifelse(Rain < 1, 0, Rain),
      RUNOFF = NA,
      DRAIN = NA,
      TRAN = NA,
      AVAIL = NA,
      R = NA
    )
  
  # Initialize WAT0 for the first day
  WATini <- (soilWHC + WATwp) / 2
  
  # Calculate water balance for each day
  for (day in 1:nrow(data)) {
    
    if (day == 1) {
      
      # WAT0 <- 0
      WAT0 <- WATini
      
      # Change in water before drainage (Precipitation - Runoff)
      
      if (data$Rain[day] > IA){
        data$RUNOFF[day] <- (data$Rain[day]-0.2*S)^2/(data$Rain[day]+0.8*S)
      }else{
        data$RUNOFF[day] <- 0
      }
      
      data$RUNOFF[day] <- max(data$RUNOFF[day], 0)
      
      # Calculating the amount of deep drainage
      
      if ((WAT0+data$Rain[day]-data$RUNOFF[day]) > soilWHC){
        data$DRAIN[day] <- DC*(WAT0+data$Rain[day]-data$RUNOFF[day]-soilWHC)
      }else{
        data$DRAIN[day] <- 0
      }
      
      data$DRAIN[day] <- max(data$DRAIN[day], 0)
      
      # Calculating the amount of water lost by transpiration (after drainage)
      
      data$TRAN[day] <- min(MUF*(WAT0+data$Rain[day]-data$RUNOFF[day]-
                                   data$DRAIN[day]- WATwp), data$Eto[day])
      data$TRAN[day] <- max(data$TRAN[day], 0)
      data$TRAN[day] <- min(data$TRAN[day], soilWHC)
      
      data$R[day] <- data$TRAN[day] / data$Eto[day]
      data$TRAN[day] <- max(data$TRAN[day], (data$R[day] * data$Eto[day]))
      
      data$AVAIL[day] <- WAT0 + (data$Rain[day]-data$RUNOFF[day]-
                                   data$DRAIN[day] - data$TRAN[day])
      data$AVAIL[day] <- min(data$AVAIL[day], soilWHC)
      data$AVAIL[day] <- max(data$AVAIL[day], 0)
      
    } else {
      
      WAT0 <- data$AVAIL[day-1]
      
      # Change in water before drainage (Precipitation - Runoff)
      
      if (data$Rain[day] > IA){
        data$RUNOFF[day] <- (data$Rain[day]-0.2*S)^2/(data$Rain[day]+0.8*S)
      }else{
        data$RUNOFF[day] <- 0
      }
      
      data$RUNOFF[day] <- max(data$RUNOFF[day], 0)
      
      # Calculating the amount of deep drainage
      
      if ((WAT0+data$Rain[day]-data$RUNOFF[day]) > soilWHC){
        data$DRAIN[day] <- DC*(WAT0+data$Rain[day]-data$RUNOFF[day]-soilWHC)
      }else{
        data$DRAIN[day] <- 0
      }
      
      data$DRAIN[day] <- max(data$DRAIN[day], 0)
      
      # Calculating the amount of water lost by transpiration (after drainage)
      
      data$TRAN[day] <- min(MUF*(WAT0+data$Rain[day]-data$RUNOFF[day]-
                                   data$DRAIN[day]- WATwp), data$Eto[day])
      data$TRAN[day] <- max(data$TRAN[day], 0)
      data$TRAN[day] <- min(data$TRAN[day], soilWHC)
      
      data$R[day] <- data$TRAN[day] / data$Eto[day]
      data$TRAN[day] <- max(data$TRAN[day], (data$R[day] * data$Eto[day]))
      
      data$AVAIL[day] <- WAT0 + (data$Rain[day]-data$RUNOFF[day]-
                                   data$DRAIN[day] - data$TRAN[day])
      data$AVAIL[day] <- min(data$AVAIL[day], soilWHC)
      data$AVAIL[day] <- max(data$AVAIL[day], 0)
      
    }
    
  }
  
  data$R <- round(data$R, 3)
  data$AVAIL <- round(data$AVAIL, 3)
  data$TRAN <- round(data$TRAN, 3)
  data$DRAIN <- round(data$DRAIN, 3)
  data$RUNOFF <- round(data$RUNOFF, 3)
  data$hydro_state = data$AVAIL / soilWHC
  data$hydro_state2 = (data$AVAIL -  WATwp)/ (soilWHC - WATwp)
  return(data)
  
}

TRANSFORM_as_df <- function(X){
  X %>%
    dplyr::mutate(
      # Lat = field_lat,
      # Lon = field_lon,
      # Elev = field_elevation,
      Rain = `Sum.Precipitation`,
      Eto = ET0_pm
    ) %>%
    dplyr::select(all_of(c("date", "Rain","Eto", "CN", "DC", "MUF", "WATfc", "WATwp")))
}


RUN_swb_calc <- function(X, y, ...){
  X %>% TRANSFORM_as_df() %>%
    calcWatBal() %>%
    dplyr::select(all_of(c("date", "R", "AVAIL", "TRAN", "DRAIN", "RUNOFF", "hydro_state", "hydro_state2")))
}


# error metrics used for calibration
pearson_correlation <- function(data, col1, col2) {
  cor(data[[col1]], data[[col2]], method = "pearson")
}

pearson_diff_correlation <- function(data, col1, col2) {
  diff_data <- data %>%
    mutate(
      diff_col1 = c(NA, diff(.data[[col1]])),
      diff_col2 = c(NA, diff(.data[[col2]]))
    ) %>%
    drop_na()
  
  cor(diff_data$diff_col1, diff_data$diff_col2, method = "pearson")
}

spearman_correlation <- function(data, col1, col2) {
  cor(data[[col1]], data[[col2]], method = "spearman")
}

integral_based_distance <- function(data, col1, col2) {
  abs_diff <- abs(data[[col1]] - data[[col2]])
  trapz(1:nrow(data), abs_diff) # Numerically integrates the absolute differences
}

ks_test <- function(data, col1, col2) {
  ks.test(data[[col2]], data[[col1]])$statistic %>% 
    as.numeric()
}

linear_regression_errors <- function(data, col1, col2) {
  # Fit the regression model
  model <- lm(as.formula(paste(col2, "~", col1)), data = data)
  data <- data %>%
    mutate(
      predicted = predict(model),
      residuals = .data[[col1]] - predicted
    )
  
  data.frame(
    lm_rsquared = summary(model)$r.squared,
    lm_mae = mae_vec(data[[col1]], data$predicted), # MAE on actual values
    lm_mape = mape_vec(data[[col1]], data$predicted) # MAPE on actual vs predicted
  )
}



run_all_metrics <- function(data, groundtruth_col, index_col) {
  pearson <- pearson_correlation(data, groundtruth_col, index_col)
  pearson_diff <- pearson_diff_correlation(data, groundtruth_col, index_col)
  spearman <- spearman_correlation(data, groundtruth_col, index_col)
  # cosine_sim <- cosine_similarity(data, groundtruth_col, index_col)
  integral_dist <- integral_based_distance(data, groundtruth_col, index_col)
  ks_stat <- ks_test(data, groundtruth_col, index_col)
  linear_errors <- linear_regression_errors(data, groundtruth_col, index_col)
  # var_errors <- var_model_errors(data, groundtruth_col, index_col)
  tibble(
    pearson = pearson,
    pearson_diff = pearson_diff,
    spearman = spearman,
    # cosine_similarity = cosine_sim,
    integral_distance = integral_dist,
    ks_stat = ks_stat,
    linear_errors = linear_errors
    # var_errors = var_errors
  ) %>% 
    unnest_wider(linear_errors)
}

objective_function <- function(params, loc_id_data, sm_obs) {
  # params = c(CN, DC, MUF, WATfc, WATwp)
  
  # Run the SWB model with current parameters for this loc_id (pseudo-code)
  loc_id_data <- loc_id_data %>%
    mutate(CN = params[1], 
           DC = params[2], 
           MUF = params[3], 
           WATfc = params[4], 
           WATwp = WATfc * params[5])
  
  # Call the SWB model function to get the modeled soil water (replace with actual function)
  swb_output <- RUN_swb_calc(loc_id_data)
  
  # Discard first 20 days until model stabilizes
  swb_output <- swb_output[21:nrow(swb_output),]
  
  # Calculate RMSE between observed and modeled available water (swb_output$AVAIL)
  output_join <- sm_obs %>% 
    left_join(swb_output, by = "date") %>% 
    mutate(hydro_state = rollmean(hydro_state, k = 5, fill = NA, align = "center")) %>% 
    drop_na(hydro_state)
  
  # rmse <- sqrt(mean((output_join$AVAIL - output_join$sm_mm_smooth)^2, na.rm = TRUE))
  # browser()
  pearson_diff = pearson_diff_correlation(output_join, "sm", "hydro_state")
  lm_errors = linear_regression_errors(output_join, "sm", "hydro_state")
  
  # Normalize metrics
  # norm_pearson <- (avg_metrics$avg_pearson - (-0.1)) / (0.3 - (-0.1))  # Normalize to [0, 1]
  # norm_mape <- avg_metrics$avg_lm_mape / 2                            # Normalize to [0, 1]
  
  # # Handle out-of-bounds values
  # norm_pearson <- pmax(0, pmin(1, norm_pearson))
  # norm_pearson_inverted <- 1 - norm_pearson
  # norm_integral <- pmax(0, pmin(1, norm_integral))
  # norm_mape <- pmax(0, pmin(1, norm_mape))
  
  # Composite score with weights
  composite_score <- pearson_diff * 0.5 +  # Weight Pearson (maximize)
    (1 - (min(100, lm_errors$lm_mape) / 100)) * -0.5                        # Weight MAPE (minimize)
  
  
  return(composite_score)
}

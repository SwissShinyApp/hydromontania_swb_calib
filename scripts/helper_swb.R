
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
      Rain = ifelse(Rain < 2, 0, Rain),
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

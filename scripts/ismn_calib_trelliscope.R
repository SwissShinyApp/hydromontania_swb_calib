library(here)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(lubridate)
library(yardstick)
library(trelliscopejs)
library(plotly)


best_params_avg <- read_rds(here("data", "swb_calibration", "best_params_ismn.rds"))
swb_grid <- read_rds(here("data", "swb_calibration", "swb_calibr_params_grid_vol_ismn.rds"))
soil_params_grid <- read_rds(here("data", "swb_calibration", "soil_params_grid_ismn.rds"))
swb_results_metrics <- read_rds(here("data", "swb_calibration", "swb_calibr_metrics_ismn.rds"))


swb_best <- swb_grid %>% 
  inner_join(best_params_avg %>% distinct(loc_id, soil_params_id))

unlink("trelliscope_output", recursive = TRUE)
dir.create("trelliscope_output", showWarnings = FALSE)

# Function to create an interactive plot for each loc_id
loc_plot <- function(data) {
  plot_ly(data = data) %>%
    add_lines(x = ~date, y = ~sm_mean, key = ~sm_mean, name = "Soil Moisture", yaxis = "y1", line = list(color = "red")) %>%
    add_lines(x = ~date, y = ~hydro_state, key = ~hydro_state, , name = "Hydro State", yaxis = "y2", line = list(color = "#0868ac")) %>%
    layout(
      title = paste("Location:", unique(data$loc_id)),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Soil Moisture", side = "left", showgrid = FALSE),
      yaxis2 = list(
        title = "Hydro State",
        side = "right",
        overlaying = "y",
        showgrid = FALSE
      )#,
      # legend = list(x = 0.1, y = 0.9)
    )
}

# Compute cognostics (Pearson, R², MAPE)
# swb_best_facet <- swb_best %>%
#   group_nest(loc_id, keep = TRUE) %>%
#   left_join(
#     swb_results_metrics %>% 
#       filter(metric %in% c("pearson", "lm_rsquared", "lm_mape")) %>%
#       pivot_wider(names_from = metric, values_from = value),
#     by = "loc_id"
#   ) %>%
#   mutate(
#     panel = map_plot(data, loc_plot),  # Create plot panels
#     pearson_cog = cog(pearson, desc = "Pearson correlation"),
#     r_squared_cog = cog(lm_rsquared, desc = "R-squared of linear model"),
#     mape_cog = cog(lm_mape, desc = "Mean Absolute Percentage Error")
#   ) 

swb_best_facet <- swb_best %>%
  group_nest(loc_id, keep = TRUE) %>%
  mutate(panel = map_plot(data, loc_plot))  # Generate plots

swb_best_facet$panel[[160]]

swb_best_facet %>%
  filter(loc_id < 210) %>% 
  trelliscope(
    name = "Soil Moisture vs. SWB Hydro State",
    nrow = 4, ncol = 8,
    state = list(labels = c("loc_id")),
    path = "trelliscope_output"
  )



swb_best <- swb_grid %>% 
  inner_join(best_params_avg %>% distinct(loc_id, soil_params_id))


swb_metrics_cog <- best_params_avg %>%
  filter(metric %in% c("pearson", "lm_rsquared", "lm_mape")) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  distinct(loc_id, .keep_all = TRUE) %>%
  mutate(
    pearson_cog = cog(pearson, desc = "Pearson correlation"),
    r_squared_cog = cog(lm_rsquared, desc = "R-squared of linear model"),
    mape_cog = cog(lm_mape, desc = "Mean Absolute Percentage Error")
  )

# Merge Cognostics with Main Data
swb_best <- swb_best %>%
  left_join(swb_metrics_cog, by = "loc_id")

ggplot_chart <- swb_best %>% 
  filter(loc_id < 210) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = sm_mean, color = "Soil Moisture")) +
  geom_line(aes(y = hydro_state, color = "Hydro State")) +
  scale_color_manual(values = c("Soil Moisture" = "#cc0000", "Hydro State" = "#0868ac")) +
  theme_minimal() +
  labs(title = "Soil Moisture vs. Hydro State", x = "Date", y = "Value", color = "Legend") +
  facet_trelliscope(~loc_id, name = "Soil Moisture vs. Hydro State", nrow = 5, ncol = 8,
                    state = list(labels = c("loc_id")),
                    path = "trelliscope_output")

ggplot_chart

library(tidyverse)

# TIDES FUNCTIONS -----------
sine_wave <- function(t_vec, amplitude, period, phase_degrees) {
  phase_radians = phase_degrees * 2 * pi / 360
  wave <- amplitude * sin(2 * pi * t_vec / period - phase_radians)
}

my_theme <- theme_bw()

plot_tides <- function(t_vec, tide_height, n_days_to_plot) {
  tide_df <- data.frame(t = t_vec, tide = tide_height) %>%
    mutate(t_days = t/24)
  
  # Plot the simple tide
  ggplot(tide_df %>% filter(t_days < n_days_to_plot), aes(x = t_days, y = tide)) + 
    geom_line() +
    my_theme
}

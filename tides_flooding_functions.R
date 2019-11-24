library(tidyverse)

# TIDES FUNCTIONS -----------
sine_wave <- function(t_vec, amplitude, period, phase_degrees) {
  phase_radians = phase_degrees * 2 * pi / 360
  wave <- amplitude * sin(2 * pi * t_vec / period - phase_radians)
}

my_theme <- theme_bw()

plot_tides_simple2 <- function(tide_df, components = c('M2','S2','O1','K1','N2','M4'), n_days = 30, superimpose = TRUE) {

  temp <- tide_df %>%
    select(c(t_days, date_year, components)) %>%
    filter(t_days < n_days) %>%
    pivot_longer(cols = components, names_to = 'comp', values_to = 'magnitude') #%>%
  
  if(max(temp$date_year) - min(temp$date_year) > 4) {
    USE_YEAR = TRUE
  } else {
    USE_YEAR = FALSE
  }
  
  if(USE_YEAR == TRUE) {
    temp_p <- temp %>% ggplot(aes(x = date_year, y = magnitude))
  } else {
    temp_p <- temp %>% ggplot(aes(x = t_days, y = magnitude))
  }
  temp_p <- temp_p + 
    geom_line(aes(color = comp)) +
    my_theme +
    theme(legend.position = 'none') +
    facet_grid(rows = vars(comp))
  temp_p
}

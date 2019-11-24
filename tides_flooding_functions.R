library(tidyverse)

# TIDES FUNCTIONS -----------
sine_wave <- function(t_vec, amplitude, period, phase_degrees) {
  phase_radians = phase_degrees * 2 * pi / 360
  wave <- amplitude * sin(2 * pi * t_vec / period - phase_radians)
}

my_theme <- theme_bw()

plot_tides <- function(t_vec, components_list, n_days, superimpose = TRUE) {
  
  # initial data frame:
  tide_df <- data.frame(t = t_vec) %>%
    mutate(t_days = t/24)
  
  for(i in 1:length(components_list)) {
    # assign(paste0('component_',i), components_list[i])
    varname <- paste0('component_',i)
    tide_df <- tide_df %>%
      mutate(!!varname := components_list[i])
  }
  
  if (superimpose = TRUE) {
    
  }
  
  
  assign(paste0('Component_'))
  
  
  
  if(superimpose == TRUE) {
    tide_height =
  }
  tide_height = 
  
    
}
plot_tides_simple <- function(t_vec, tide_height, n_days) {
  tide_df <- data.frame(t = t_vec, tide = tide_height) %>%
    mutate(t_days = t/24)
  
  # Plot the simple tide
  ggplot(tide_df %>% filter(t_days < n_days_to_plot), aes(x = t_days, y = tide)) + 
    geom_line() +
    my_theme
}

plot_tides_simple2 <- function(tide_df, components = c('M2','S2','O1','K1','N2','M4'), n_days = 30, superimpose = TRUE) {
  temp <- tide_df %>%
    select(c(t_days, components)) %>%
    filter(t_days < n_days) %>%
    pivot_longer(cols = components, names_to = 'comp', values_to = 'magnitude') #%>%
  
  temp %>% ggplot(aes(x = t_days, y = magnitude)) + 
    geom_line(aes(color = comp)) +
    my_theme +
    facet_grid(rows = vars(comp))
}

#### FLOODING, MIAMI FL ------------------------------------------------


#### Set-up & load packages --------------------------------------------
library(tidyverse)

# Create a theme for plotting
my_theme <- theme_bw()

#### Create Initial Function -------------------------------------------
N_YEARS <- 80
N_HOUR_IN_YEAR <- 24 * 365 - 0.25*1
t = (0:(N_HOUR_IN_YEAR * N_YEARS)) # [hours]

# tide:
LUNAR_TIDAL_PERIOD <- 12.42 # [hours] source: wikipedia
LUNAR_AVG_TIDE_MAGNITUDE <- 0.33 # [m]
lunar_tide = LUNAR_AVG_TIDE_MAGNITUDE * sin((2*pi/LUNAR_TIDAL_PERIOD) * t)

SOLAR_TIDAL_PERIOD <- 12 # [hours]
SOLAR_AVG_TIDE_MAGNITUDE <- 0.46 * LUNAR_AVG_TIDE_MAGNITUDE # [m] source: https://oceanservice.noaa.gov/education/tutorial_tides/media/supp_tide02.html
solar_tide = SOLAR_AVG_TIDE_MAGNITUDE * sin((2*pi/SOLAR_TIDAL_PERIOD) * t - 3) # staggered from the lunar tide

# Other possible adds:
# perigee = closest point of moon to earth
# closest point of earth to sun
# whatever contributes to the late summer/fall king tides in South Florida

tide <- data.frame(t = t, tide = lunar_tide + solar_tide) %>%
  mutate(t_days = t/24)

# Plot the simple tide
ggplot(tide %>% filter(t_days < 32), aes(x = t_days, y = tide)) + 
  geom_line() +
  my_theme



# Add flood event forcing --------------------------------------------------
set.seed(54) # set seed to 54 (random)
# create the seeds for each period (year)
seeds_by_year <- floor(runif(N_YEARS, min = 1, max = 101))
for (i in 1:N_YEARS) {
  
  n_events <- floor(abs(rnorm(n = 1, mean = 3, sd = 2.5))) # This works to an average of 3 to 4 flood events
  set.seed(seeds_by_year[i])
  seeds_for_timing <- floor(runif(n_events), 102, 201)
  for (n in 1:n_events) {
    set.seed(seeds_for_timing[n])
    temp_severity <- runif()
  }
}


# Sea Level Rise
SLR_RATE <- 2.39 * 10e-3 # [m/yr] source: NOAA https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8723170
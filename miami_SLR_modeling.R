#### TIDES & FLOODING, MIAMI FL ------------------------------------------------

N_YEARS <- 80
N_HOUR_IN_YEAR <- 24 * 365 #- 0.25*1 for now, not considering leap years...
t = (0:(N_HOUR_IN_YEAR * N_YEARS)) # [hours]

# Harmonic Constituents of Tide come from https://tidesandcurrents.noaa.gov/harcon.html?unit=0&timezone=1&id=8723165&name=MIAMI%2C+BISCAYNE+BAY&state=FL
#  for Biscayne Bay, Miami, FL

# M2:
# S2: 
# N2: larger lunar eliptic semidiurnal constituent
# K1, O1 are effect of moon's declination

# X = sine_wave(t_vec, amplitude, period, phase_degrees)
M2 = sine_wave(t, 0.322, 12.4206, 256.0)
S2 = sine_wave(t, 0.057, 12,      282.7)
O1 = sine_wave(t, 0.028, 25.8193, 214.3)
K1 = sine_wave(t, 0.033, 23,9345, 184.4)
N2 = sine_wave(t, 0.071, 12.6583, 242.2)
M4 = sine_wave(t, 0.003,  6.2103, 33.2)

plot_tides(t, M2, 30)


#### Set-up & load packages --------------------------------------------
library(tidyverse)

# Create a theme for plotting
my_theme <- theme_bw()

#### Create Equilibrium Amplitude Fucntion -------------------------------------------
# Only considers Principal Tidal Constituents (Sun and Moon)


# tide:
LUNAR_TIDAL_PERIOD <- 12.4206 # [hours] source: wikipedia, 262B notes 2/28/18 p.3
LUNAR_AVG_TIDE_MAGNITUDE <- 0.243 # [m]
# M2 = lunar semidiurnal
M2 <- LUNAR_AVG_TIDE_MAGNITUDE * sin((2*pi/LUNAR_TIDAL_PERIOD) * t)

SOLAR_TIDAL_PERIOD <- 12 # [hours]
SOLAR_AVG_TIDE_MAGNITUDE <- 0.46 * LUNAR_AVG_TIDE_MAGNITUDE # [m] source: https://oceanservice.noaa.gov/education/tutorial_tides/media/supp_tide02.html
# S2 = solar semidiurnal
S2 <- SOLAR_AVG_TIDE_MAGNITUDE * sin((2*pi/SOLAR_TIDAL_PERIOD) * t - 3) # staggered from the lunar tide

# lunisolar:
K1 <- 0.141565 * sin((2*pi/23.9344)) # 262B
# principal lunar:
O1 <- 0.100514 * sin((2*pi/25.8194)) 
# Also see: https://www.sciencedirect.com/topics/earth-and-planetary-sciences/tidal-constituent



# Other possible adds:
# perigee = closest point of moon to earth
# closest point of earth to sun
# whatever contributes to the late summer/fall king tides in South Florida

tide_df <- data.frame(t = t, tide = lunar_tide + solar_tide) %>%
  mutate(t_days = t/24)

# Plot the simple tide
ggplot(tide_df %>% filter(t_days < 32), aes(x = t_days, y = tide)) + 
  geom_line() +
  my_theme



# Add flood event forcing --------------------------------------------------

# Add a single event at t = 100:105
tide_plus <- tide_df %>%
  mutate(forcing = case_when(t_days >= 12 & t_days <= 12.5 ~ 0.25,
                          TRUE ~ 0)) %>%
  mutate(sea_level = tide + forcing)

# Plot the simple tide with 1 flood event
ggplot(tide_plus %>% filter(t_days < 32), aes(x = t_days, y = sea_level)) + 
  geom_line() +
  my_theme


# Randomized Events ------------------------------------------------------



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
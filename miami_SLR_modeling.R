#### TIDES & FLOODING, MIAMI FL ------------------------------------------------

N_YEARS <- 80
N_HOUR_IN_YEAR <- 24 * 365 #- 0.25*1 for now, not considering leap years...
t = (0:(N_HOUR_IN_YEAR * N_YEARS)) # [hours]

# Harmonic Constituents of Tide come from https://tidesandcurrents.noaa.gov/harcon.html?unit=0&timezone=1&id=8723165&name=MIAMI%2C+BISCAYNE+BAY&state=FL
#  for Biscayne Bay, Miami, FL
# Also see: https://www.sciencedirect.com/topics/earth-and-planetary-sciences/tidal-constituent

# M2:
# S2: 
# N2: larger lunar eliptic semidiurnal constituent
# K1, O1 are effect of moon's declination

# X = sine_wave(t_vec, amplitude, period, phase_degrees)

# initial data frame:
tide_df <- data.frame(t = t) %>%
  mutate(t_days = t/24) %>%
  mutate(
    M2 = sine_wave(t, 0.322, 12.4206, 256.0),
    S2 = sine_wave(t, 0.057, 12,      282.7),
    O1 = sine_wave(t, 0.028, 25.8193, 214.3),
    K1 = sine_wave(t, 0.033, 23.9345, 184.4),
    N2 = sine_wave(t, 0.071, 12.6583, 242.2),
    M4 = sine_wave(t, 0.003,  6.2103, 33.2)
  ) %>%
  mutate(tide = M2 + S2 + O1 + K1 + N2 + M4)

plot_tides_simple2(tide_df, components = c('M2','S2','O1','K1','N2','M4'), n_days = 5)
plot_tides_simple2(tide_df, components = c('tide'), n_days = 365)


# #### Create Equilibrium Amplitude Fucntion 
# 
# # Other possible adds:
# # perigee = closest point of moon to earth
# # closest point of earth to sun
# # whatever contributes to the late summer/fall king tides in South Florida


# KING TIDES: -----------------------------------------------
# Identify the top ('king') tides, filtered by assuming 3-4 king tides per year
N_DAYS = 365*N_YEARS
top_tides <- tide_df %>% select(c(t_days, tide)) %>%
  filter(t_days < N_DAYS) %>%
  top_n(n = N_YEARS * 3.5, wt = tide)

# We can identify a height exceeded only by the top tides:
king_tide_threshold <- min(top_tides$tide)

plot_tides_simple2(tide_df, components = c('tide'), n_days = 365) +
  geom_hline(yintercept = king_tide_threshold, color = 'black', linetype = 'dashed')

# Where are these points over the first 3 years?

plot_tides_simple2(tide_df, components = c('tide'), n_days = 365*3) +
  geom_point(data = filter(top_tides, t_days < 365*3), aes(x = t_days, y = tide), color = 'black', shape = 1)


# We've gotten a king tide threshold defined by 3.5 (3-4) "King Tides" per year

# SLR Impact on "King Tide" Frequency --------------------------------------
# estimate for SLR?




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
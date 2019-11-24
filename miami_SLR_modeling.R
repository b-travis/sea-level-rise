#### TIDES & FLOODING, MIAMI FL ------------------------------------------------

N_YEARS <- 40
N_HOUR_IN_YEAR <- 24 * 365 #- 0.25*1 for now, not considering leap years...
# t = (0:(N_HOUR_IN_YEAR * N_YEARS)) # [hours]
t = seq(from = 0, to = N_HOUR_IN_YEAR * N_YEARS, by = 0.2) # [hours]

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
  mutate(t_years = t_days/365) %>%
  mutate(date_year = t_years + 2020) %>%
  mutate(
    M2 = sine_wave(t, 0.322, 12.4206, 256.0),
    S2 = sine_wave(t, 0.057, 12,      282.7),
    O1 = sine_wave(t, 0.028, 25.8193, 214.3),
    K1 = sine_wave(t, 0.033, 23.9345, 184.4),
    N2 = sine_wave(t, 0.071, 12.6583, 242.2),
    M4 = sine_wave(t, 0.003,  6.2103, 33.2),
    M6 = sine_wave(t, 0.011,  4.1401, 94.1),
    NU2 =sine_wave(t, 0.014, 12.6260, 229.0),
    SSA =sine_wave(t, 0.055, 4382.905,57.8),   # solar semiannual,
    SA = sine_wave(t, 0.083, 8765.8211,190.3), # solar annual
    L2 = sine_wave(t, 0.015, 12.1916, 232.1),
    K2 = sine_wave(t, 0.016, 11.9672, 279.4),
    P1 = sine_wave(t, 0.01, 24.0659, 182.6)
  ) %>%
  mutate(tide = M2 + S2 + O1 + K1 + N2 + M4 +M6+NU2+SSA+SA+L2+K2+P1)

plot_tides_simple2(tide_df, 
                   components = c('M2','S2','O1','K1','N2','M4','M6','NU2','SSA','SA','L2','K2','P1'), 
                   n_days = 25)
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
top_tides <- tide_df %>% select(c(t, t_days, t_years, date_year, tide)) %>%
  filter(t_days < N_DAYS) %>%
  top_n(n = N_YEARS * 4, wt = tide)



# We can identify a height exceeded only by the top tides:
# king_tide_threshold <- min(top_tides$tide)
king_tide_threshold <- 

plot_tides_simple2(tide_df, components = c('tide'), n_days = 365) +
  geom_hline(yintercept = king_tide_threshold, color = 'black', linetype = 'dashed')

# Where are these points over the first 3 years?

plot_tides_simple2(tide_df, components = c('tide'), n_days = 365*10) +
  geom_point(data = filter(top_tides, t_days < 365*10), aes(x = date_year, y = tide), color = 'black', shape = 1)


# We've gotten a king tide threshold defined by 3.5 (3-4) "King Tides" per year

# SLR Impact on "King Tide" Frequency --------------------------------------
# estimate for SLR?
# Sea Level Rise
# current rate:
SLR_RATE <- 0.00239 # [m/yr] source: NOAA https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8723170
tide_df$MSL <- tide_df$t_years * SLR_RATE
tide_df$tide_plus_SLR <- tide_df$MSL + tide_df$tide

plot_tides_simple2(tide_df, components = c('MSL'), n_days = N_DAYS)
plot_tides_simple2(tide_df, components = c('tide_plus_SLR'), n_days = 365*25) +
  geom_hline(yintercept = king_tide_threshold, color = 'black', linetype = 'dashed')

# Another set of rates:
# https://southeastfloridaclimatecompact.org/wp-content/uploads/2015/10/2015-Compact-Unified-Sea-Level-Rise-Projection.pdf
SLR_RATE <- 0.01016 # [m/yr] --> equivalent to 12 inches in 30 years
SLR_RATE <- 0.02032 # [m/yr] --> equivalent to 24 inches in 30 years

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



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
    M2 = sine_wave(t, 0.322, 12.4206, 256.0), # main lunar semidiurnal
    S2 = sine_wave(t, 0.057, 12,      282.7), # solar semidiurnal
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

p1 <- plot_tides_simple2(tide_df, 
                   components = c('M2','S2','K1','N2','SSA','SA'), 
                   n_days = 30)
save_plot(p1,'tide_constituents_30days.png')
p2 <- plot_tides_simple2(tide_df, components = c('tide'), n_days = 7)
save_plot(p2,'tide_7day.png')

# #### Create Equilibrium Amplitude Fucntion 
# 
# # Other possible adds:
# # perigee = closest point of moon to earth
# # closest point of earth to sun
# # whatever contributes to the late summer/fall king tides in South Florida


# KING TIDES: -----------------------------------------------
# Identify the top ('king') tides, filtered by assuming 3-4 king tides per year
N_DAYS = 365*N_YEARS

ident_top_tides <- tide_df %>%
  select(c(t_days, t_years, tide)) %>%
  mutate(t_days = ceiling(t_days),
         t_years = ceiling(t_years)) %>%
  group_by(t_days,t_years) %>%
  summarize(daily_max = max(tide)) %>%
  ungroup() %>%
  top_n(n = 5*N_YEARS, wt = daily_max)

king_tide_threshold <- min(ident_top_tides$daily_max)

top_tides <- tide_df %>% select(c(t_days, t_years, date_year, tide)) %>%
  filter(t_days < N_DAYS,
         tide >= king_tide_threshold)

plot_tides_simple2(tide_df, components = c('tide'), n_days = 365*5) +
  geom_hline(yintercept = king_tide_threshold, color = 'black', linetype = 'dashed')

# Where are these points over the first 5 years?

p_kings <- plot_tides_simple2(tide_df, components = c('tide'), n_days = 365*5) +
  geom_hline(yintercept = king_tide_threshold, color = 'black', linetype = 'dashed') +
  geom_point(data = filter(top_tides, t_days < 365*5), aes(x = date_year, y = tide), color = 'black', shape = 1)
save_plot(p_kings,'kings_5year.png')

# Histogram of count of king tide occurences
king_hist <- tide_df %>%
  select(c(t_days, t_years, date_year, tide)) %>%
  mutate(t_days = ceiling(t_days),
         t_years = ceiling(t_years),
         date_year = floor(date_year)) %>%
  group_by(t_days,t_years, date_year) %>%
  summarize(daily_max = max(tide)) %>%
  ungroup() %>%
  filter(daily_max >= king_tide_threshold) %>%
  group_by(date_year) %>%
  summarise(num_king_tides = n()) %>%
  ggplot() +
  geom_bar(aes(x = date_year, y = num_king_tides), stat = 'identity') +
  my_theme +
  ylab('Days Exceeding Flooding Threshold') +
  xlab('Year')
save_plot(king_hist, 'king_hist.png', size = c(6,6))

# We've gotten a king tide threshold defined by 3.5 (3-4) "King Tides" per year

# SLR Impact on "King Tide" Frequency --------------------------------------
# estimate for SLR?
# Sea Level Rise
# current rate:
SLR_RATE <- 0.00239 # [m/yr] source: NOAA https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8723170
tide_df$MSL <- tide_df$t_years * SLR_RATE
tide_df$tide_plus_SLR <- tide_df$MSL + tide_df$tide

plot_tides_simple2(tide_df %>% filter(date_year < 2021 | date_year > 2059), components = c('MSL'), n_days = N_DAYS)
tide_wSLR <- plot_tides_simple2(tide_df, components = c('tide_plus_SLR'), n_days = 365*25) +
  geom_hline(yintercept = king_tide_threshold, color = 'black', linetype = 'dashed') +
  geom_abline(slope = SLR_RATE, 
              intercept = -SLR_RATE*min(tide_df$date_year), 
              color = 'black')
# save_plot(tide_wSLR,'tide_wSLR_curr_5year.png')
save_plot(tide_wSLR,'tide_wSLR_12in30_5year.png')

# New histogram -- includes SLR
SLR_hist <- tide_df %>%
  mutate(tide = tide_plus_SLR) %>%
  select(c(t_days, t_years, date_year, tide)) %>%
  mutate(t_days = ceiling(t_days),
         t_years = ceiling(t_years),
         date_year = floor(date_year)) %>%
  group_by(t_days,t_years, date_year) %>%
  summarize(daily_max = max(tide)) %>%
  ungroup() %>%
  filter(daily_max >= king_tide_threshold) %>%
  group_by(date_year) %>%
  summarise(num_king_tides = n()) %>%
  ggplot() +
  geom_bar(aes(x = date_year, y = num_king_tides), stat = 'identity') +
  my_theme
save_plot(SLR_hist, 'hist_SLR_12in30.png', size = c(6,6))

# Another set of rates:
# https://southeastfloridaclimatecompact.org/wp-content/uploads/2015/10/2015-Compact-Unified-Sea-Level-Rise-Projection.pdf
SLR_RATE <- 0.01016 # [m/yr] --> equivalent to 12 inches in 30 years (or 10in/25yr)
SLR_RATE <- 0.02032 # [m/yr] --> equivalent to 24 inches in 30 years


# Histogram with specific SLR (6, 12, 18 inches)
m_per_in <- 0.0254

SLR_hist <- tide_df %>%
  mutate(tide = tide + 9*m_per_in) %>%
  select(c(t_days, t_years, date_year, tide)) %>%
  mutate(t_days = ceiling(t_days),
         t_years = ceiling(t_years),
         date_year = floor(date_year)) %>%
  group_by(t_days,t_years, date_year) %>%
  summarize(daily_max = max(tide)) %>%
  ungroup() %>%
  filter(daily_max >= king_tide_threshold) %>%
  group_by(date_year) %>%
  summarise(num_king_tides = n()) %>%
  ggplot() +
  geom_bar(aes(x = date_year, y = num_king_tides), stat = 'identity') +
  geom_hline(aes(yintercept = mean(num_king_tides))) +
  my_theme
save_plot(SLR_hist, 'hist_SLR_12in30.png', size = c(6,6))

# Days with High Tide Flooding, based on SLR scenarios:
SLR_scenarios_line <- tide_df %>%
  mutate(inch_0 = tide,
         inch_1 = tide + 1 * m_per_in,
         inch_2 = tide + 2 * m_per_in,
         inch_4 = tide + 4 * m_per_in,
         inch_6 = tide + 6 * m_per_in,
         inch_8 = tide + 8 * m_per_in,
         inch_10 = tide + 10 * m_per_in,
         inch_12 = tide + 12 * m_per_in,
         inch_14 = tide + 14 * m_per_in,
         inch_16 = tide + 16 * m_per_in,
         inch_18 = tide + 18 * m_per_in,
         inch_20 = tide + 20 * m_per_in) %>%
  pivot_longer(cols = starts_with('inch_'), names_to = 'SLR_scenario', values_to = 'sea_level') %>%
  mutate(SLR_scenario = gsub('inch_','',SLR_scenario)) %>%
  mutate(SLR_scenario = as.numeric(SLR_scenario)) %>%
  select(c(t_days, t_years, date_year, SLR_scenario, sea_level)) %>%
  mutate(t_days = ceiling(t_days),
         t_years = ceiling(t_years),
         date_year = floor(date_year)) %>%
  group_by(t_days,t_years, date_year, SLR_scenario) %>%
  summarize(daily_max = max(sea_level)) %>%
  ungroup() %>%
  filter(daily_max >= king_tide_threshold) %>%
  group_by(date_year, SLR_scenario) %>%
  summarise(num_king_tides = n()) %>%
  ungroup() %>%
  group_by(SLR_scenario) %>%
  summarise(mean_n_king_tide_days = mean(num_king_tides)) %>%
  ggplot() +
  # geom_bar(aes(x = SLR_scenario, y = mean_n_king_tide_days), stat = 'identity') +
  geom_line(aes(x = SLR_scenario, y = mean_n_king_tide_days)) +
  # facet_grid(cols = vars(SLR_scenario)) +
  # geom_hline(aes(yintercept = mean(num_king_tides))) +
  my_theme
save_plot(SLR_scenarios_line, 'SLR_scenarios_line.png', size = c(6,6))





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



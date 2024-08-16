#### Data analysis script
#### Fits augmented synthetic controls for sample polygon data

source("scripts/load.R")

### 1. Set parameters --------

# Data selection
COUNTRY <- "Cote d'Ivoire"  # Target country
START_YEAR <- 2016  # Simulated start year of protection project
MATCHING_PERIODS <- c(4,8,12,16,20,24)  # Length of pre-intervention period to use for matching (years)
POLY_SIZE <- 60000 # size of polygons in hectares
SIMULATIONS <- 1:6

# Simulations to run for RQ1 and RQ3 (RQ2 and RQ4 use distinct data)
# simulation_match_df <- tibble(
#   sim = c(SIMULATIONS, rep(5, length(MATCHING_PERIODS))),
#   match = c(rep(8, length(SIMULATIONS)), MATCHING_PERIODS)
# )

simulation_match_df <- tibble(
  sim = rep(SIMULATIONS, each = length(MATCHING_PERIODS)),
  match = rep(MATCHING_PERIODS, length(SIMULATIONS))
)

### 2. Load data
## To fix tomorrow - now have the geoms attached to grid_data, which is in wide format
## so need to extract them and then again pivot_longer and separate_wider_delim...
grid_data <- read_rds(paste0("data/processed/rds/", COUNTRY, "_", POLY_SIZE, "_", START_YEAR, "_data.rds"))

### 3. Prepare data and run analysis

sample_ids <- grid_data %>%
  st_drop_geometry() %>%
  select(ID, stratum) %>%
  filter(!is.na(stratum))

# Set up formula for analysis based on simulation type

# Loop through sample to perform analysis
sc_results <- map(sample_ids$ID, function(id) {
  
  # Add additional variables specific to treated unit
  grid_data_sample <- grid_data %>%
    set_treated(id) %>%
    add_dist_to_treated(id) %>%
    st_drop_geometry() %>%
    add_shared_eco_frac(id) %>%
    add_biome_match(id, drop = TRUE)
  
  # Prepare data to format required by augsynth
  
  grid_data_prepared <- grid_data_sample %>%
    wide_to_long() %>%
    filter(year > START_YEAR - MATCHING_PERIOD) %>%
    mutate(treated = treated * (year > START_YEAR))
  
  # Run synthetic controls for different simulations
  
  message("Fitting synthetic control: ID = ", id)
  sim_df <- simulation_match_df %>%
    mutate(ID = id,
           stratum = sample_ids$stratum[sample_ids$ID == id]
    ) %>%
    mutate(synth = map2(sim, match, run_synthetic_control, data = grid_data_prepared))

  sim_df
})


### 4. Combine results and summarise data

# Convert back to time series of observed and modelled forest loss for each unit

sc_df <- sc_results %>%
  bind_rows() %>%
  mutate(sc_results = map2(synth, ID, extract_synth)) %>%
  select(-synth) %>%
  unnest(sc_results)

set.seed(111)
viz_ids <- sample(sample_ids$ID, 36)

ts_plots <- sc_df %>%
  filter(ID %in% viz_ids) %>%
  filter(match == 8) %>%
  # filter(sim %in% c(1, 5)) %>%
  ggplot(aes(x = as.numeric(year))) +
  geom_line(aes(y = loss, colour = "Observed"), lwd = 0.9) +
  geom_line(aes(y = sc_loss, colour = as.factor(sim))) +
  geom_vline(xintercept = START_YEAR) +
  facet_wrap(~ID) +
  theme_bw() +
  labs(x = "Year", y = "Annual loss rate (area frac)", colour = NULL) +
  scale_colour_manual(
    labels = c(paste0("S", 1:6), "Observed"),
    values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33', "black"))

# ts_plots_post <- sc_df %>%
#   # filter(ID %in% viz_ids) %>%
#   filter(sim %in% c(1, 5)) %>%
#   filter(year >= START_YEAR) %>%
#   ggplot(aes(x = as.numeric(year))) +
#   geom_line(aes(y = loss)) +
#   geom_line(aes(y = sc_loss, colour = as.factor(sim))) +
#   geom_vline(xintercept = START_YEAR) +
#   facet_wrap(~ID) +
#   theme_bw() +
#   labs(x = "Year", y = "Annual loss rate (area frac)", colour = NULL) +
#   scale_colour_discrete(labels = c("Prior outcomes only", "All covariates"))

ggsave(
  paste0("results/figures/sc_plots/", COUNTRY, "_", START_YEAR, "_", POLY_SIZE, "_", MATCHING_PERIOD, "Y_ts_plots.jpg"),
  ts_plots,
  width = 24, height = 20, dpi = 300, units = "cm"
)

# Mean absolute error per simulation

sc_mae <- sc_df %>%
  mutate(test_period = (year > START_YEAR)) %>%
  mutate(abs_error = abs(sc_loss - loss)) %>%
  group_by(ID, sim, match, test_period) %>%
  summarise(mae = mean(abs_error))

test_period_lookup <- tibble(
  test_period = c(TRUE, FALSE),
  label = c("Test period", "Matching period")
)

mae_plot_sim <- sc_mae %>%
  filter(match == 8) %>%
  left_join(test_period_lookup) %>%
  ggplot(aes(x = as.factor(sim), y = mae, fill = as.factor(sim))) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~label) +
  theme_bw() +
  labs(x = "Simulation", y = "Mean absolute error\n(annual loss rate)")

ggsave(
  paste0("results/figures/sc_plots/", COUNTRY, "_", START_YEAR, "_", POLY_SIZE, "_", MATCHING_PERIOD, "Y_mae_plot_sim.jpg"),
  mae_plot_sim,
  width = 20, height = 16, dpi = 300, units = "cm"
)

# Mean absolute error by match period

mae_plot_match <- sc_mae %>%
  filter(sim == 5) %>%
  left_join(test_period_lookup) %>%
  ggplot(aes(x = as.factor(match), y = mae, fill = as.factor(match))) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Greens") +
  facet_wrap(~label) +
  theme_bw() +
  labs(x = "Matching period", y = "Mean absolute error\n(annual loss rate)")

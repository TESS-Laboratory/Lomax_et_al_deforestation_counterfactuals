#### Data analysis script
#### Fits augmented synthetic controls for sample polygon data

source("scripts/load.R")

### 1. Set parameters --------

# Data selection
COUNTRY <- "Cote d'Ivoire"  # Target country
START_YEAR <- 2016  # Simulated start year of protection project
MATCHING_PERIOD <- 16  # Length of pre-intervention period to use for matching (years)
POLY_SIZE <- 59000 # size of polygons in hectares

# Simulation run
SIMULATIONS <- 1:6

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
    add_biome_match(id)
  
  # Prepare data to format required by augsynth
  
  grid_data_prepared <- grid_data_sample %>%
    wide_to_long() %>%
    filter(year > START_YEAR - MATCHING_PERIOD) %>%
    mutate(treated = treated * (year > START_YEAR))
  
  # Run synthetic controls for different simulations
  
  message("Fitting synthetic control: ID = ", id)
  sim_df <- tibble(
    ID = id,
    stratum = sample_ids$stratum[sample_ids$ID == id],
    sim = SIMULATIONS) %>%
    mutate(synth = map(sim, run_synthetic_control, data = grid_data_prepared, pt = MATCHING_PERIOD))

  sim_df
})


### 4. Combine results and summarise data

# Convert back to time series of observed and modelled forest loss for each unit

tic()
sc_df <- sc_results %>%
  bind_rows() %>%
  mutate(sc_results = map2(synth, ID, extract_synth)) %>%
  select(-synth) %>%
  unnest(sc_results)
toc()

set.seed(111)
viz_ids <- sample(sample_ids$ID, 16)

ts_plots <- sc_df %>%
  filter(ID %in% viz_ids) %>%
  filter(sim %in% c(1, 5)) %>%
  ggplot(aes(x = as.numeric(year))) +
  geom_line(aes(y = loss)) +
  geom_line(aes(y = sc_loss, colour = as.factor(sim))) +
  geom_vline(xintercept = START_YEAR) +
  facet_wrap(~ID) +
  theme_bw() +
  labs(x = "Year", y = "Annual loss rate (area frac)", colour = NULL) +
  scale_colour_discrete(labels = c("Prior outcomes only", "All covariates"))

ts_plots_post <- sc_df %>%
  filter(ID %in% viz_ids) %>%
  filter(sim %in% c(1, 5)) %>%
  filter(year >= START_YEAR) %>%
  ggplot(aes(x = as.numeric(year))) +
  geom_line(aes(y = loss)) +
  geom_line(aes(y = sc_loss, colour = as.factor(sim))) +
  geom_vline(xintercept = START_YEAR) +
  facet_wrap(~ID) +
  theme_bw() +
  labs(x = "Year", y = "Annual loss rate (area frac)", colour = NULL) +
  scale_colour_discrete(labels = c("Prior outcomes only", "All covariates"))

ggsave(
  paste0("results/figures/sc_plots/", COUNTRY, "_", START_YEAR, "_", POLY_SIZE, "_", MATCHING_PERIOD, "Y_S", SIMULATION, "_ts_plots.jpg"),
  ts_plots,
  width = 24, height = 20, dpi = 300, units = "cm"
)

# Mean absolute error per simulation

sc_mae <- sc_df %>%
  mutate(test_period = (year > START_YEAR)) %>%
  mutate(abs_error = abs(sc_loss - loss)) %>%
  group_by(ID, sim, test_period) %>%
  summarise(mae = mean(abs_error))

test_period_lookup <- tibble(
  test_period = c(TRUE, FALSE),
  label = c("Test period", "Matching period")
)

mae_plot <- sc_mae %>%
  left_join(test_period_lookup) %>%
  ggplot(aes(x = as.factor(sim), y = mae, fill = as.factor(sim))) +
  geom_boxplot(alpha = 0.25, show.legend = FALSE) +
  facet_wrap(~label) +
  theme_bw() +
  labs(x = "Simulation", y = "Mean absolute error\n(annual loss rate)")



# observed_loss <- grid_data %>%
#   wide_to_long() %>%
#   filter(year > START_YEAR) %>%
#   group_by(ID) %>%
#   summarise(mean_loss = mean(loss))
# 
# sc_mae_frac <- sc_mae %>%
#   left_join(observed_loss) %>%
#   mutate(mae_frac = mae / mean_loss)
# 
# ggplot(sc_mae_frac, aes(x = mean_loss, y = mae, colour = as.factor(stratum))) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, colour = "grey20") +
#   theme_bw() +
#   labs(x = "Observed mean forest loss in test period",
#        y = "Mean absolute error of synthetic\ncontrol in test period",
#        colour = "Forest loss stratum")

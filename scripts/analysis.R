#### Data analysis script
#### Fits augmented synthetic controls for sample polygon data and writes the
#### results to disk as a CSV.

source("scripts/load.R")

### 1. Set parameters --------

# Data selection
COUNTRY <- "Brazil"  # Target country
START_YEAR <- 2016 # Simulated start year of protection project
MATCHING_PERIODS <- c(4,8,12,16,20,24)  # Length of pre-intervention period to use for matching (years)
POLY_SIZE <- 60000 # size of polygons in hectares
SIMULATIONS <- 1:5  # Simulations to run
ECON <- TRUE  # Economic data present for country
SEED <- 1471
MAX_POOL <- 1000  # Max number of potential donor polygons to constrain 
N_CORES <- 1

# Simulations to run for RQ1 and RQ3 (RQ2 and RQ4 use distinct data)
simulation_match_df <- tibble(
  sim = c(SIMULATIONS, rep(5, length(MATCHING_PERIODS))),
  match = c(rep(8, length(SIMULATIONS)), MATCHING_PERIODS)
) %>% unique

# Get parameters from command line if running from terminal
cmd_args <- commandArgs(TRUE)

if (length(cmd_args) == 4) {
  COUNTRY <- cmd_args[1]
  MAX_POOL <- as.numeric(cmd_args[2])
  N_CORES <- as.numeric(cmd_args[3])
  ECON <- as.logical(as.numeric(cmd_args[4]))
} else {
  print("Insufficient command line arguments given - using default values")
}

cat("Fitting synthetic controls for ", COUNTRY, "- Start Year: ", START_YEAR, ", Polygon Size: ", POLY_SIZE, " ha")

# Parallelisation
if (N_CORES > 1) {
  plan(multicore, workers = N_CORES)
}

### 2. Load data
## To fix tomorrow - now have the geoms attached to grid_data, which is in wide format
## so need to extract them and then again pivot_longer and separate_wider_delim...
grid_data <- read_rds(paste0("data/processed/rds/", COUNTRY, "_", POLY_SIZE, "_", START_YEAR, "_data.rds"))

### 3. Prepare data and run analysis

sample_ids <- grid_data %>%
  st_drop_geometry() %>%
  select(ID, stratum) %>%
  filter(!is.na(stratum))

# Reduce potential donors for large donor pools
if (nrow(grid_data) > MAX_POOL) {
  message("Donor pool too large; reducing to ", MAX_POOL)
  
  n_sample <- nrow(sample_ids)
  grid_data_sample <- filter(grid_data, ID %in% sample_ids$ID)
  
  set.seed <- SEED
  n_pool <- MAX_POOL - n_sample
  grid_data_pool <- grid_data %>%
    filter(!(ID %in% sample_ids$ID)) %>%
    slice_sample(n = n_pool)
  
  grid_data <- bind_rows(grid_data_sample, grid_data_pool)
}

# Set up formula for analysis based on simulation type

# Loop through sample to perform analysis
Sys.time()
tic()
sc_results <- future_map(sample_ids$ID, function(id) {
  
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
    mutate(treated = treated * (year > START_YEAR))
  
  # Run synthetic controls for different simulations
  
  message("Fitting synthetic control: ID = ", id)
  sim_df <- simulation_match_df %>%
    mutate(ID = id,
           stratum = sample_ids$stratum[sample_ids$ID == id]
    ) %>%
    mutate(synth = map2(sim, match, run_synthetic_control, data = grid_data_prepared, econ = ECON))

  sim_df
},
.options = furrr_options(seed = TRUE, packages = c("augsynth", "dplyr", "purrr", "sf")),
.progress = TRUE)
toc()

### 4. Combine results and export data

# Convert back to time series of observed and modelled forest loss for each unit

sc_df <- sc_results %>%
  bind_rows() %>%
  mutate(sc_results = map2(synth, ID, extract_synth)) %>%
  select(-synth) %>%
  unnest(sc_results)

output_filename <- paste0("sc_results_", COUNTRY, "_", START_YEAR, "_", POLY_SIZE, ".csv")

write_csv(sc_df, paste0("results/sc_results/", output_filename))

cat("Results written to ", output_filename)

pushover(message = paste0("SC analysis complete: ", COUNTRY, ". Start year: ", START_YEAR))


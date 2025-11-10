#### Data analysis script
#### Fits augmented synthetic controls for sample polygon data and writes the
#### results to disk as a CSV.

#### Guy Lomax
#### G.Lomax@exeter.ac.uk

# Instructions
## To run from terminal go to project directory and use syntax:

# "nohup Rscript scripts/analysis.R [COUNTRY] [POLY_SIZE] [START_YEAR] [MAX_POOL] [N_CORES] [ECON] [CUMULATIVE] &> [out_file] &"
# e.g.,
# "nohup Rscript scripts/analysis.R "Cote d'Ivoire" 60000 1000 2016 16 1 0 &> cotedivoire.out &"
# logicals should be entered as 1/0, not TRUE/FALSE or T/F

source("scripts/load.R")

### 1. Set parameters --------

# Data selection
COUNTRY <- "Bolivia"        # Target country
START_YEAR <- 2016          # Simulated start year of protection project
POLY_SIZE <- 60000          # size of polygons in hectares
SIMULATIONS <- 1:5          # Simulations to run
ECON <- TRUE                # Is economic data present for this country?
SEED <- 1471                # Random seed
MAX_POOL <- 1000            # Max number of potential donor polygons to constrain 
N_CORES <- 1                # Number of cores for parallelisation 
CUMULATIVE <- FALSE         # Fit to cumulative rather than annual loss?

# Simulations to run for different RQs
if (START_YEAR == 2016) {
  
  if (POLY_SIZE == 60000) {
    
    # RQ1 & RQ3
    simulation_match_df <- read_csv("data/raw/csv/simulation_list_60000.csv")
    
  } else {
    
    # RQ2
    simulation_match_df <- tibble(sim = 4, match = 8)
    
  }
} else if (START_YEAR == 1998) {
  
  # RQ4
  simulation_match_df <- tibble(sim = 4, match = 8)
  
}

simulation_match_df <- filter(simulation_match_df, sim %in% SIMULATIONS)

# Get parameters from command line if running from terminal
cmd_args <- commandArgs(TRUE)

if (length(cmd_args) == 7) {
  COUNTRY <- cmd_args[1]
  POLY_SIZE <- as.numeric(cmd_args[2])
  START_YEAR <- as.numeric(cmd_args[3])
  MAX_POOL <- as.numeric(cmd_args[4])
  N_CORES <- as.numeric(cmd_args[5])
  ECON <- as.logical(as.numeric(cmd_args[6]))
  CUMULATIVE <- as.logical(as.numeric(cmd_args[7]))
} else {
  print("Insufficient command line arguments given - using default values")
}

cat("Fitting synthetic controls for ", COUNTRY, "- Start Year: ", START_YEAR, ", Polygon Size: ", POLY_SIZE, " ha")

# Parallelisation
if (N_CORES > 1) {
  plan(multicore, workers = N_CORES)
}

### 2. Load data
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
  
  set.seed(SEED)
  n_pool <- MAX_POOL - n_sample
  grid_data_pool <- grid_data %>%
    filter(!(ID %in% sample_ids$ID)) %>%
    slice_sample(n = n_pool)
  
  grid_data <- bind_rows(grid_data_sample, grid_data_pool)
}

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
    add_biome_match(id, drop = TRUE) %>%
    select(-stratum, -cum_loss) %>%
    drop_na()  # Remove donors with NA values in key variables
  
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
    mutate(synth = map2(sim, match, run_synthetic_control, data = grid_data_prepared, econ = ECON, cumulative = CUMULATIVE))

  sim_df
},
.options = furrr_options(seed = TRUE, packages = c("augsynth", "dplyr", "purrr", "sf")),
.progress = TRUE)
toc()

### 4. Combine results and export data

# Convert back to time series of observed and modeled forest loss for each unit

write_rds(sc_results, paste0("data/processed/rds/", COUNTRY, "_", POLY_SIZE, "_", START_YEAR, "_sc_results.rds"))

sc_df <- sc_results %>%
  bind_rows() %>%
  filter(!is.na(synth)) %>%
  mutate(sc_results = map2(synth, ID, extract_synth, truth_data = grid_data, cumulative = CUMULATIVE)) %>%
  select(-synth) %>%
  unnest(sc_results)

# Save output
cumulative_flag <- ifelse(CUMULATIVE == TRUE, "_cumulative", "")

output_filename <- paste0("sc_results_", COUNTRY, "_", START_YEAR, "_", POLY_SIZE, cumulative_flag, ".csv")

write_csv(sc_df, paste0("results/sc_results/", output_filename))

cat("Results written to ", output_filename)

# Send notification
pushover(message = paste0("SC analysis complete: ", COUNTRY, ". Start year: ", START_YEAR))


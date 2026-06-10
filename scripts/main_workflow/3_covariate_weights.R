#### Data analysis script
#### Fits standard synthetic controls for sample polygon data (using MSCMT package)
#### and extracts assigned covariate weights.
#### NB: These results are reported in Supplementary Table 12 only because of the
#### risk of misinterpretation and do not form part of the core analysis.

# Instructions
## To run from terminal go to project directory and use syntax:

# "nohup Rscript scripts/3_covariate_weights.R [COUNTRY] [MAX_POOL] [N_CORES] [ECON] &> [out_file] &"
# e.g.,
# "nohup Rscript scripts/2_fit_synthetic_controls.R "Cote d'Ivoire" 1000 4 0 &> cotedivoire.out &"
# logicals should be entered as 1/0, not TRUE/FALSE or T/F

#### Guy Lomax
#### G.Lomax@exeter.ac.uk

source("scripts/load.R")

### 1. Set parameters --------

# Data selection
COUNTRY <- "Bolivia"  # Target country
START_YEAR <- 2016 # Simulated start year of protection project
MATCHING_PERIOD <- 8  # Length of pre-intervention period to use for matching (years)
POLY_SIZE <- 60000 # size of polygons in hectares
SIMULATIONS <- 5  # Simulations to run
ECON <- TRUE  # Economic data present for country
SEED <- 1471
MAX_POOL <- 100  # Max number of potential donor polygons to constrain 
N_CORES <- 4

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

# Loop through sample to perform analysis
set.seed(SEED)
sc_results <- future_map(sample_ids$ID, .options = furrr_options(seed = TRUE), .progress = TRUE, function(id) {
  
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
  sim_df <- tibble(
    ID = id,
    stratum = sample_ids$stratum[sample_ids$ID == id],
    match = MATCHING_PERIOD
  ) %>%
    mutate(synth = map(id, run_synthetic_control_mscmt, match = MATCHING_PERIOD, data = grid_data_prepared))
    
  sim_df
})

### 4. Combine results and summarise data

# Convert back to time series of observed and modelled forest loss for each unit

variable_weights_df <- sc_results %>%
  bind_rows() %>%
  mutate(sc_results = map(synth, extract_synth_weights)) %>%
  select(-synth) %>%
  unnest(sc_results)

write_csv(variable_weights_df,
          paste0("results/var_weights/", COUNTRY, "_weights.csv"))

message("Written to disk: ", COUNTRY, "_weights.csv")

# # Send notification
# pushover(paste0("Variable weights analysis completed: ", COUNTRY))

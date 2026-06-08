#### Data analysis script
#### Fits augmented synthetic controls for sample polygon data and writes the
#### results to disk as a CSV.

#### Guy Lomax
#### G.Lomax@exeter.ac.uk

# Instructions
## To run from terminal go to project directory and use syntax:

# "nohup Rscript scripts/analysis.R [COUNTRY] [POLY_SIZE] [START_YEAR] [MAX_POOL] [N_CORES] &> [out_file] &"
# e.g.,
# "nohup Rscript scripts/analysis.R "Cote d'Ivoire" 60000 1000 2016 16 &> cotedivoire.out &"
# logicals should be entered as 1/0, not TRUE/FALSE or T/F

source("scripts/load.R")

### 1. Set parameters --------

# Data selection
COUNTRY <- "Colombia"  # Target country
START_YEAR <- 2016 # Simulated start year of protection project
MATCHING_PERIODS <- 8  # Length of pre-intervention period to use for matching (years)
POLY_SIZE <- 60000 # size of polygons in hectares
SEED <- 1471
MAX_POOL <- 1000  # Max number of potential donor polygons to constrain
MIN_POOL <- 20  # Minimum donors to proceed with fitting
DONOR_FILTER <- FALSE  # Filter donor pool based on prior buffer deforestation rates?
FILTER_START <- 2009  # Start year for calculating prior buffer deforestation rates
FILTER_THRESHOLD <- 0.1  # fractional range to filter prior buffer deforestation rates
N_CORES <- 1
CUMULATIVE <- FALSE  # Use cumulative rather than annual deforestation to fit

# Get parameters from command line if running from terminal
cmd_args <- commandArgs(TRUE)

if (length(cmd_args) == 5) {
  COUNTRY <- cmd_args[1]
  POLY_SIZE <- as.numeric(cmd_args[2])
  START_YEAR <- as.numeric(cmd_args[3])
  MAX_POOL <- as.numeric(cmd_args[4])
  N_CORES <- as.numeric(cmd_args[5])
} else {
  print("Insufficient command line arguments given - using default values")
}

cat("Fitting synthetic controls for ", COUNTRY, "- Start Year: ", START_YEAR, ", Polygon Size: ", POLY_SIZE, " ha")

# Simulations to run by RQ
if (START_YEAR == 2016 & POLY_SIZE == 60000 & DONOR_FILTER == FALSE) {
  # RQ1 and RQ3
  simulation_match_df <- read_csv("data/raw/csv/simulation_list_60000.csv")
  
} else {
  # RQ2 or donor filter test
  simulation_match_df <- tibble(sim = 4, match = 8)
  
}

# Parallelisation
if (N_CORES > 1) {
  plan(multisession, workers = N_CORES)
}

### 2. Load data
grid_data <- read_rds(paste0("data/processed/rds/", COUNTRY, "_", POLY_SIZE, "_", START_YEAR, "_data.rds"))

# Test if econ data is present in dataset
econ <- max(str_detect(colnames(grid_data), "grp_pc_usd_2015|ag_grp_frac"))

### 3. Prepare data and run analysis

sample_ids <- grid_data %>%
  st_drop_geometry() %>%
  select(ID, stratum) %>%
  filter(!is.na(stratum)) %>%
  mutate(n_donors = NA, final_filter_range = NA)

# # Filter potential donors based on prior deforestation rates or MAX_POOL
# if (nrow(grid_data) > MAX_POOL) {
#   message("Donor pool too large; reducing to ", MAX_POOL)
# 
#   n_sample <- nrow(sample_ids)
#   grid_data_sample <- filter(grid_data, ID %in% sample_ids$ID)
# 
#   set.seed(SEED)
#   n_pool <- MAX_POOL - n_sample
#   grid_data_pool <- grid_data %>%
#     filter(!(ID %in% sample_ids$ID)) %>%
#     slice_sample(n = n_pool)
# 
#   grid_data <- bind_rows(grid_data_sample, grid_data_pool)
# }

# Loop through sample to perform analysis
Sys.time()
tic()
sc_results <- future_map(sample_ids$ID, function(id) {
  
  cat("ID = ", id, "\n")
  
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
  
  # Pre-filter based on prior buffer deforestation if DONOR_FILTER == TRUE
  
  if (DONOR_FILTER == TRUE) {
    buffer_prior_defor <- grid_data_prepared %>%
      filter(year >= FILTER_START & year <= START_YEAR) %>%
      group_by(ID) %>%
      summarise(prior_buffer_loss = sum(buffer_loss))
    
    # Treated unit deforestation
    buffer_prior_defor_treated <- buffer_prior_defor$prior_buffer_loss[buffer_prior_defor$ID == id]
    
    # Filter donors according to threshold; increase threshold if insufficient donors identified
    filter_range <- FILTER_THRESHOLD
    
    sufficient_donors <- FALSE
    
    donors <- filter(buffer_prior_defor, ID != id)
    
    while(sufficient_donors == FALSE) {
      message("Filtering donors on prior deforestation rate - buffer = ", filter_range * 100, "%\n")
      
      donors_filtered <- donors %>%
        filter(prior_buffer_loss >= buffer_prior_defor_treated * (1 - filter_range) &
              prior_buffer_loss <= buffer_prior_defor_treated * (1 + filter_range))
      
      n_donors <- nrow(donors_filtered)
      
      # Escape while loop if all donors are already included
      if (n_donors == nrow(buffer_prior_defor) - 1 | nrow(buffer_prior_defor) < MIN_POOL) {
        break
      }
      
      sufficient_donors <- n_donors >= MIN_POOL
      
      filter_range <- filter_range + FILTER_THRESHOLD
    }
    
    message("Donor filtering complete\n")
    
    grid_data_prepared <- filter(grid_data_prepared, ID %in% c(id, donors_filtered$ID))
    
  } else {
    n_donors <- length(unique(grid_data_prepared$ID)) - 1
  }
  
  
  # Run synthetic controls for different simulations
  
  message("Fitting synthetic control: ID = ", id)
  sim_df <- simulation_match_df %>%
    mutate(ID = id,
           stratum = sample_ids$stratum[sample_ids$ID == id],
           # n_donors = n_donors,
           # final_filter_range = filter_range - FILTER_THRESHOLD
    ) %>%
    mutate(synth = map2(sim, match, run_synthetic_control, data = grid_data_prepared, econ = econ, cumulative = CUMULATIVE))

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
donor_filter_flag <- ifelse(DONOR_FILTER == TRUE, "_filtered", "")

output_filename <- paste0("sc_results_", COUNTRY, "_", START_YEAR, "_", POLY_SIZE, cumulative_flag, donor_filter_flag, ".csv")

write_csv(sc_df, paste0("results/sc_results/", output_filename))

cat("Results written to ", output_filename)

# Send notification
pushover(message = paste0("SC analysis complete: ", COUNTRY, ". Start year: ", START_YEAR))


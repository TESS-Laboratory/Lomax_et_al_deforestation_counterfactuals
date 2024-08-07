#### Data analysis script
#### Fits augmented synthetic controls for sample polygon data

source("scripts/load.R")

### 1. Set parameters --------

# Data selection
COUNTRY <- "Cote d'Ivoire"  # Target country
START_YEAR <- 2016  # Simulated start year of protection project
POLY_SIZE <- 59000 # size of polygons in hectares

# Simulation run
SIMULATION <- 1

### 2. Load data
## To fix tomorrow - now have the geoms attached to grid_data, which is in wide format
## so need to extract them and then again pivot_longer and separate_wider_delim...
grid_data <- read_rds(paste0("data/processed/rds/", COUNTRY, "_", POLY_SIZE, "_", START_YEAR, "_data.rds"))

### 3. Run analysis

sample_ids <- grid_data$ID[!is.na(grid_data$stratum)]


for (i in sample_ids) {
  
  # Add additional variables specific to treated unit
  grid_data_sample <- grid_data %>%
    set_treated(i) %>%
    add_dist_to_treated(i) %>%
    st_drop_geometry() %>%
    add_shared_eco_frac(i) %>%
    add_biome_match(i)
  
  # Prepare data to format required by augsynth
  
  grid_data_long <- grid_data_sample %>%
    pivot_longer(cols = contains(".")) %>%
    separate_wider_delim(cols = "name", delim = ".", names = c("var", "year")) %>%
    pivot_wider(names_from = "var", values_from = "value")
  
  grid_data_synth <- augsynth::augsynth(
    
  )
  
}
## Script to generate and sample hexagonal or square grids of a specified area
## and minimum forest cover within a country polygon.

## Setup packages and functions --------
source("scripts/load.R")

## Set parameters --------

COUNTRY <- "Colombia"  # Target country
CRS <- "ESRI:54034"  # CRS to generate grid
START_YEAR <- 2016  # Simulated start year of protection project
COUNTRY_BUFFER <- 10 # buffer distance from country border to exclude when generating polygons (km)
POLY_SIZE <- 100000 # size of polygons in hectares
POLY_SHAPE <- "hex"  # square or hex polygon
POLY_BUFFER <- 10 # polygon buffer distance in km
SAMPLE_N <- 500  # number of polygons to sample
FC_THRESHOLD <- 20  # Minimum forest cover to include polygons in sample (%)
FOREST_EDGE_AREA <- 100 # Minimum continuous nonforest area to define as forest edge, hectares
OUTPUT_PATH <- "data/processed/vector/sample_polygons"  # Output directory
SEED <- 111  # Random number seed

AGG <- 5 # Factor to aggregate rasters to speed calculation/test pipeline

## Load datasets --------

country <- get_country(COUNTRY, crs = CRS)
fc <- get_raster(COUNTRY, folder = "gfc", layer = "treecover2000")
fc_loss <- get_raster(COUNTRY, folder = "gfc", layer = "lossyear")
# plantations <- get_tiled_raster(folder = "plantations", layer = 1, names = "plantation_year") %>%
#   crop(fc)
biomass <- get_stac_raster(COUNTRY, collection = "hgb", asset = "aboveground", crs = CRS)
dem <- get_stac_raster(COUNTRY, collection = "cop-dem-glo-90", asset = "data", folder = "dem", crs = CRS)
rivers <- get_vector("Lin2021_rivers", country_poly = country) %>%
  filter(strmOrder >= 4) %>%
  st_filter(country)


## Generate polygon grids --------

grid <- generate_polygons(
  country,
  buffer = COUNTRY_BUFFER,
  shape = POLY_SHAPE,
  area = POLY_SIZE
)

grid_buffer <- generate_buffers(grid, dist = POLY_BUFFER)

## Calculate forest cover and loss in polygon and buffer regions --------

# Convert forest cover percentage to binary layer using 50% threshold
# Remove plantations planted prior to 2000
fc_binary <- fc %>%
  aggregate(fact = AGG) %>%
  is_greater_than(50) %>%
  as.numeric()

# fc_mask <- fc_binary %>%
#   mask(plantations == 0, maskvalues = c(0, NA))

# NB: Currently removing all plantations (and with some reservations about
# the dataset completeness)

# Extract forest cover per polygon and filter to 
grid_fc <- grid %>%
  poly_extract(fc_binary) %>%
  filter(treecover2000 >= (FC_THRESHOLD) / 100)

grid_buffer_fc <- grid_buffer %>%
  filter(ID %in% grid_fc$ID) %>%
  poly_extract(fc_binary)

## Extract forest loss per polygon

grid_fc_loss <- poly_extract(grid_fc, fc_loss, fun = sum_by_value)
grid_buffer_fc_loss <- poly_extract(grid_buffer_fc, fc_loss, fun = sum_by_value) %>%
  rename(buffer_treecover2000 = treecover2000, buffer_area = area, buffer_area_frac = area_frac)

grid_fc_loss_all <- full_join(grid_fc_loss, st_drop_geometry(grid_buffer_fc_loss))

## Take random sample of polygons stratified by actual cumulative deforestation after start year --------

set.seed(SEED)
grid_sample <- grid_fc_loss_all %>%
  calc_cumulative_defor(START_YEAR, area_col = "area_frac", after = TRUE) %>%
  sample_polygons(n = SAMPLE_N, strata = "cum_defor")




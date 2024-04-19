## Script to generate and sample hexagonal or square grids of a specified area
## and minimum forest cover within a country polygon.

## Setup packages and functions --------
source("scripts/load.R")

## Set parameters --------

COUNTRY <- "Colombia"  # Target country
CRS <- "ESRI:54034"  # CRS to generate grid
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

## Load country polygon and project to equal area projection--------

country <- get_country(COUNTRY, crs = CRS)

## Generate polygon grids --------

tic()
grid <- generate_polygons(
  country,
  buffer = COUNTRY_BUFFER,
  shape = POLY_SHAPE,
  area = POLY_SIZE
)
toc()

grid_buffer <- generate_buffers(grid, dist = POLY_BUFFER)

## Calculate FC per polygon and filter to those with > FC_THRESHOLD --------

# Load forest cover raster and convert to binary forest/nonforest
fc <- get_raster(COUNTRY, folder = "gfc", layer = "treecover2000") %>%
  aggregate(fact = AGG) %>%
  is_greater_than(50) %>%
  as.numeric()

# Extract forest cover per polygon and filter to 
grid_fc <- grid %>%
  poly_extract(fc) %>%
  filter(treecover2000 > (FC_THRESHOLD / 100))

grid_buffer_fc <- grid_buffer %>%
  filter(ID %in% grid_fc$ID) %>%
  poly_extract(fc)

## Extract forest loss per polygon

fc_loss <- get_raster(COUNTRY, folder = "gfc", layer = "lossyear")

grid_fc_loss <- poly_extract(grid_fc[1:20,], fc_loss, fun = sum_by_value)
grid_buffer_fc_loss <- poly_extract(grid_buffer_fc[1:20,], fc_loss, fun = sum_by_value)

## Generate random sample polygons --------
set.seed(SAMPLE_SEED)

## Add distance from forest edge to polygon properties --------
# Detect non-forest patches

tic()
non_forest_patches <- landscapemetrics::get_patches(
  fc_threshold$Y2000,
  class = 0,
  to_disk = TRUE
)[[1]][[1]]
toc()

names(non_forest_patches) <- "patch"

# tic()
# patch_area <- fc_threshold$Y2000 %>%
#   lsm_p_area() %>%
#   filter(class == 0) %>%
#   select(id, value)
#   
# toc()
# readr::write_rds(patch_area, here("data", "processed", "patch_area_Colombia.rds"))

# Reclassify nonforest patch values to area
patch_area_large <- readr::read_rds(here("data", "processed", "patch_area_Colombia.rds")) %>%
  filter(value >= FOREST_EDGE_AREA)

tic()
non_forest_patch_area <- classify(non_forest_patches, rcl = patch_area_large, others = NA)
toc()

# Calculate distance to forest edge for all forest pixels
tic()
dist_to_edge <- terra::distance(non_forest_patch_area)
toc()

names(dist_to_edge) <- "dist_to_edge"

# Extract mean distance to edge for all sample polygons
tic()
grid_fc_edge <- extract_grid(grid_sample, dist_to_edge)
toc()

# Visualise

fc_agg_country <- mask(crop(fc_agg, country_ea), country_ea)
dist_to_edge_country_mask <- dist_to_edge %>%
  mask(country_ea) %>%
  mask(dist_to_edge > 0, maskvalues = c(0, NA))

tmap_mode("view")

tm_shape(fc_agg_country$Y2000) +
  tm_raster(col.scale = tm_scale_continuous(values = "Greens")) +
  tm_shape(dist_to_edge_country_mask) +
  tm_raster(col.scale = tm_scale_continuous(values = "viridis")) +
  tm_shape(grid_sample) +
  tm_borders("black", lwd = 2)

## Save to disk
output_name <- paste(COUNTRY, POLY_SHAPE, POLY_SIZE, sep = "_")

st_write(
  grid_fc_edge,
  paste0(OUTPUT_PATH, "/", output_name, ".geojson"),
  delete_dsn = TRUE
)


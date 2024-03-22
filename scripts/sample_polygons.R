## Script to generate and sample hexagonal or square grids of a specified area
## and minimum forest cover within a country polygon.

library(sf)
library(terra)
library(landscapemetrics)
library(here)
library(dplyr)
library(tmap)

source(here("scripts", "R", "generate_polygons.R"))

## Set parameters --------

COUNTRY <- "Colombia"
POLY_SIZE <- 10000 # hectares
POLY_SHAPE <- "hex"  # square or hex
SAMPLE_N <- 1000  # number of sample polygons
FC_THRESHOLD <- 40  # Minimum forest cover to include polygons in sample (%)
OUTPUT_PATH <- here("data", "processed", "vector", "sample_polygons")
SAMPLE_SEED <- 111

## Load country polygon --------

path <- here("data", "raw", "vector", "WB_countries_Admin0_10m", "WB_countries_Admin0_10m.shp")
country <- get_country(path, country_name = COUNTRY)

# Convert to equal area projection (Cylindrical equal area)
country_ea <- st_transform(country, crs = "ESRI:54034")

## Generate polygon grids --------

grid <- generate_polygons(country_ea, shape = POLY_SHAPE, area = POLY_SIZE)

## Calculate distance to forest edge per pixel --------

# Load forest cover raster
# Raster should be "closed", i.e., not contain small holes
fc <- rast(here("data", "raw", "raster", "gfc", "GFC_cover_Colombia_mosaic.tif"))

fc_agg <- fc %>%
  terra::aggregate(fact = 3, cores = 4) %>%
  project(crs(country_ea)) %>%
  mask(country_ea)

fc_threshold <- as.numeric(fc_agg > 30)

non_forest <- fc_threshold == 0

# Detect patches

tictoc::tic()
non_forest_patches <- landscapemetrics::get_patches(
  fc_threshold$Y2000,
  class = 0,
  to_disk = TRUE
)[[1]][[1]]
tictoc::toc()

patch_area <- lsm_p_area(non_forest_patches)

writeRaster(fc_agg, here("data", "processed", "raster", "fc_agg.tif"))

## Filter polygons to those with > FC_THRESHOLD forest cover --------

grid_fc <- extract_fc(grid, fc_agg$Y2000)

grid_fc_threshold <- dplyr::filter(grid_fc, forest_cover > FC_THRESHOLD)

## Generate random sample polygons --------
set.seed(SAMPLE_SEED)

grid_sample <- sample_polygons(grid_fc_threshold, n = SAMPLE_N)

# Visualise

fc_agg_country <- mask(crop(fc_agg, country_ea), country_ea)

tm_shape(country) +
  tm_fill("grey95") +
  tm_shape(fc_agg_country$Y2000) +
  tm_raster(col.scale = tm_scale_continuous(values = "Greens")) +
  tm_shape(grid_sample) +
  tm_borders("orange", lwd = 2)

## Save to disk
output_name <- paste(COUNTRY, POLY_SHAPE, FC_THRESHOLD, POLY_SIZE, sep = "_")

st_write(
  grid_sample,
  paste0(OUTPUT_PATH, "/", output_name),
  driver = "GeoJSON",
  delete_dsn = TRUE
)


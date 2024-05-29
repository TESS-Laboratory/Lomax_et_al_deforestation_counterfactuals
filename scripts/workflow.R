## Script to generate and sample hexagonal or square grids of a specified area
## and minimum forest cover within a country polygon.

## Setup packages and functions --------
source("scripts/load.R")

## Set parameters --------

# Spatial and temporal range
COUNTRY <- "Colombia"  # Target country
CRS <- "ESRI:54034"  # CRS to generate grid
START_YEAR <- 2016  # Simulated start year of protection project

# Polygon sampling
COUNTRY_BUFFER <- 10 # buffer distance from country border to exclude when generating polygons (km)
POLY_SIZE <- 100000 # size of polygons in hectares
POLY_SHAPE <- "hex"  # square or hex polygon
POLY_BUFFER_RATIO <- 1 # polygon buffer area as ratio of polygon area
SAMPLE_N <- 100  # number of polygons to sample

# Forest definitions
TREE_THRESHOLD <- 30  # Minimum % canopy cover to classify as forest
FC_THRESHOLD <- 20  # Minimum forest cover to include polygons in sample (%)
FOREST_EDGE_AREA <- 100 # Minimum continuous nonforest area to define as forest edge, hectares

# Data processing
OUTPUT_PATH <- "data/processed/vector/sample_polygons"  # Output directory
SEED <- 111  # Random number seed
AGG <- 5 # Factor to aggregate rasters to speed calculation/test pipeline

## Load datasets --------

# Country admin boundaries
country <- gadm(COUNTRY, level = 0, path = "data/raw/vector/gadm") %>% st_as_sf()
country_adm1 <- gadm(COUNTRY, level = 1, path = "data/raw/vector/gadm") %>% st_as_sf()

# Raster data
fc <- get_raster("data/raw/raster/gfc", COUNTRY, layer = "treecover2000")
fc_loss <- get_raster("data/raw/raster/gfc", COUNTRY, layer = "lossyear")
plantations <- get_tiled_raster(folder = "data/raw/raster/plantations", layer = 1)
biomass <- get_stac_raster(COUNTRY, collection = "hgb", asset = "aboveground", crs = CRS)
cropland <- get_tiled_raster("data/raw/raster/cropland")
dem <- get_stac_raster(COUNTRY, collection = "cop-dem-glo-90", asset = "data", folder = "dem", crs = CRS)
ppt <- get_raster("data/raw/raster/chirps", COUNTRY)
tMean <- get_raster("data/raw/raster/era5", COUNTRY)
travel_time_city <- get_raster("data/raw/raster/travel_time", "travel_time_to_cities_12")
travel_time_port <- get_raster("data/raw/raster/travel_time", "travel_time_to_ports_5")
population <- get_raster("data/raw/raster/population")

# Vector data

rivers <- get_vector("Lin2021_rivers", country_poly = country) %>%
  filter(strmOrder >= 4) %>%
  st_filter(country)
roads <- get_vector("GRIP_roads", country_poly = country)
protected_areas <- get_vector("protected_areas", country_name = COUNTRY, suffix = ".geojson")
redd_proj <- read_csv("data/REDD_database_no_meta.csv") %>%
  filter(!is.na(Longitude)) %>%
  filter(`country name` == COUNTRY) %>%
  filter(grepl("REDD", project_type)) %>%
  filter(Status_2022 %in% c("Ongoing", "Ended")) %>%
  mutate(area = as.numeric(area)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = "EPSG:4326")

## Generate polygon grids --------

grid <- generate_polygons(
  country,
  buffer = COUNTRY_BUFFER,
  shape = POLY_SHAPE,
  area = POLY_SIZE,
  crs = CRS
)

# Generate hexagonal buffers of POLY_BUFFER_RATIO * POLY_SIZE area

grid_buffer <- generate_buffers(grid, area_ratio = POLY_BUFFER_RATIO, buffer_only = FALSE)

# Generate REDD project polygons from centroids

redd_radius <- redd_proj %>%
  st_transform(CRS) %>%
  mutate(radius = sqrt(area * 0.01 / pi))  # km

redd_buffer <- generate_buffers(redd_radius, dist = redd_radius$radius)

# Remove polygons and buffers intersecting REDD projects
grid_redd <- grid %>%
  st_intersection(st_union(redd_buffer)) %>%
  mutate(area = st_area(x),
         area_frac = as.numeric(area) / (POLY_SIZE * 10000)) %>%
  filter(area_frac > 0.1)

grid_non_redd <- filter(grid, !(ID %in% grid_redd$ID))

## Calculate forest cover and loss in polygon and buffer regions --------

# Convert forest cover percentage to binary layer using 50% threshold
fc_binary <- fc %>%
  aggregate(fact = AGG) %>%
  is_greater_than(30) %>%
  as.numeric()

# fc_mask <- fc_binary %>%
#   mask(plantations == 0, maskvalues = c(0, NA))

# NB: Currently removing all plantations (and with some reservations about
# the dataset completeness)

# Extract forest cover per polygon and filter to threshold
grid_fc <- grid_non_redd %>%
  poly_extract(fc_binary) %>%
  filter(treecover2000 >= FC_THRESHOLD / 100)

grid_buffer_fc <- grid_buffer %>%
  filter(ID %in% grid_fc$ID) %>%
  poly_extract(fc_binary)

## Extract forest loss per polygon

grid_fc_loss <- poly_extract(grid_fc, fc_loss, fun = sum_by_value)
grid_buffer_fc_loss <- poly_extract(grid_buffer_fc, fc_loss, fun = sum_by_value) %>%
  rename(buffer_treecover2000 = treecover2000, buffer_area = area, buffer_area_frac = area_frac)

grid_fc_loss_all <- grid_fc_loss %>%
  full_join(st_drop_geometry(grid_buffer_fc_loss)) %>%
  replace_na(replace = list("area" = 0, "area_frac" = 0, "buffer_area" = 0, "buffer_area_frac = 0"))

## Take random sample of polygons stratified by actual cumulative deforestation up to start year --------

set.seed(SEED)
grid_sample <- grid_fc_loss_all %>%
  calc_cumulative_defor(START_YEAR, area_col = "area_frac", after = FALSE) %>%
  sample_polygons(n = SAMPLE_N, strata_col = "cum_defor", strata = 5) %>%
  left_join(grid_fc_loss_all, by = "ID") %>%
  st_as_sf()  # Re-attach geometry

grid_sample_


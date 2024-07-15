## Script to generate and sample hexagonal or square grids of a specified area
## and minimum forest cover within a country polygon.

source("scripts/load.R")

## 1. Set parameters --------

# Spatial and temporal range
COUNTRY <- "Colombia"  # Target country
CRS <- "ESRI:54034"  # CRS to generate grid
START_YEAR <- 2016  # Simulated start year of protection project

# Polygon sampling
COUNTRY_BUFFER <- 10 # buffer distance from country border to exclude when generating polygons (km)
POLY_SIZE <- 59000 # size of polygons in hectares
POLY_SHAPE <- "hex"  # square or hex polygon
POLY_BUFFER_RATIO <- 1 # polygon buffer area as ratio of polygon area
SAMPLE_N <- 100  # number of polygons to sample

# Forest definitions
FC_THRESHOLD <- 20  # Minimum forest cover to include polygons in sample (%)
FOREST_EDGE_AREA <- 100 # Minimum continuous nonforest area to define as forest edge, hectares

# Data processing
OUTPUT_PATH <- "data/processed/vector/sample_polygons"  # Output directory
SEED <- 111  # Random number seed
AGG <- 5 # Factor to aggregate rasters to speed calculation/test pipeline

## 2. Load datasets --------

# Lookup table for efficient data I/O
data_lookup <- read_csv("data/raw/csv/data_lookup.csv")  # TO COMPLETE

# Country admin boundaries and jurisdictional data
country <- gadm(COUNTRY, level = 0, path = "data/raw/vector/gadm") %>% st_as_sf()
country_adm1 <- gadm(COUNTRY, level = 1, path = "data/raw/vector/gadm") %>% st_as_sf()
econ_vars <- read_csv("data/raw/csv/DOSE_V2.csv") %>%
  filter(country == COUNTRY)

# Raster data
fc <- get_raster("data/processed/raster/tmf", match = COUNTRY)
fc_loss <- get_tiled_raster("data/raw/raster/tmf_loss/", match = COUNTRY)
biomass <- get_stac_raster(collection = "hgb", asset = "aboveground")
cropland <- get_tiled_raster("data/raw/raster/cropland")
dem <- get_stac_raster(collection = "cop-dem-glo-90", asset = "data", folder = "dem")
ppt <- get_raster("data/raw/raster/chirps", COUNTRY)
tMean <- get_raster("data/raw/raster/era5", COUNTRY)
ag_suitability <- rast("data/raw/raster/agricultural_suitability/1980-2009_hist_i/overall_suitability.bil")
travel_time_city <- get_raster("data/raw/raster/travel_time", "travel_time_to_cities_12")
travel_time_port <- get_raster("data/raw/raster/travel_time", "travel_time_to_ports_5")
population <- get_raster("data/raw/raster/population")

# Vector and csv data
ecoregions <- st_read("data/raw/vector/ecoregions2017/Ecoregions2017.shp") %>%
  st_transform(CRS)
rivers <- get_vector("data/raw/vector/Lin2021_rivers", match = COUNTRY, source = "rivers") %>%
  filter(strmOrder >= 5) %>%
  st_filter(country) %>%
  st_transform(CRS)
roads_grip <- get_vector("data/raw/vector/GRIP_roads", match = COUNTRY, source = "grip") %>%
  st_transform(CRS)
roads_osm <- get_osm("data/raw/vector/osm", match = COUNTRY) %>%
  st_transform(CRS)
protected_areas <- get_vector("data/raw/vector/protected_areas", match = COUNTRY, ext = ".geojson") %>%
  st_transform(CRS)
redd_projects <- st_read("data/processed/vector/redd_polys_renoster.gpkg") %>%
  filter(Country == COUNTRY) %>%
  st_transform(CRS)

## 3. Generate polygon grids and buffers --------

grid <- generate_polygons(
  country,
  buffer = COUNTRY_BUFFER,
  shape = POLY_SHAPE,
  area = POLY_SIZE,
  crs = CRS
)

# Generate hexagonal buffers of POLY_BUFFER_RATIO * POLY_SIZE area

grid_buffer <- generate_buffers(
  grid,
  area_ratio = POLY_BUFFER_RATIO,
  buffer_only = FALSE,
  joinStyle = "MITRE")

## 4. Filter to polygons meeting selection criteria --------

# Remove polygons intersecting REDD projects or protected areas designated since 1990.
protected_areas_new <- filter(protected_areas, STATUS_YR >= 1991)

grid_filtered <- grid %>%
  st_filter(st_union(redd_projects), .predicate = st_disjoint) %>%
  st_filter(st_union(protected_areas_new), .predicate = st_disjoint)

grid_buffer_filtered <- filter(grid_buffer, ID %in% grid_filtered$ID)

# Remove polygons with < 20% forest cover in 1990

fc1990 <- subset(fc, "Dec1990") %in% c(1, 2, 4)  # Undisturbed, degraded or regrowth forest

grid_threshold <- grid_filtered %>%
  poly_extract(fc1990) %>%
  filter(Dec1990 >= FC_THRESHOLD / 100)

grid_buffer_threshold <- grid_buffer %>%
  filter(ID %in% grid_threshold$ID)

## Prepare forest cover data
# (i) Annual forest cover loss
# (ii) Jurisdiction-level annual forest cover loss (polygons with 32 loss columns)
# (iii) Pixelwise distance to forest edge

## Prepare other data layers
# (i) Calculate slope from DEM
# (ii) Mask other raster layers to forest area in project start year
# (iii) Calculate forest fraction in protected areas

## Extract forest cover metrics

## Extract other variables

## Create panel data and drop geometries
## Create separate geometry object for calculation of proximity



## Extract annual forest loss per polygon in poly and buffer --------

# Convert annual forest classifications to binary forest loss
# tic()
# fc_loss <- app(fc, tmf_to_defor)
# toc()

loss_names <- paste0("fc_loss_", 1991:2022)
grid_fc_loss <- grid_threshold %>%
  poly_extract(fc_loss) %>%
  set_names(c("ID", "fc1990", loss_names, "x"))



grid_buffer_fc_loss <- poly_extract(grid_threshold_fc, fc_loss) %>%
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


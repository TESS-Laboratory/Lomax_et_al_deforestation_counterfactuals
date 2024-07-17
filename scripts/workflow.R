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
FOREST_EDGE_AREA <- 10 # Minimum continuous nonforest area to define as forest edge, hectares

# Data processing
OUTPUT_PATH <- "data/processed/vector/sample_polygons"  # Output directory
SEED <- 111  # Random number seed
AGG <- 5 # Factor to aggregate rasters to speed calculation/test pipeline

## 2. Load datasets --------

# Lookup table for efficient data I/O
data_lookup <- read_csv("data/raw/csv/data_lookup.csv")  # TO COMPLETE

# Country admin boundaries and jurisdictional data
country <- gadm(COUNTRY, level = 0, path = "data/raw/vector/gadm") %>%
  st_as_sf() %>%
  st_transform(CRS)
country_adm1 <- gadm(COUNTRY, level = 1, path = "data/raw/vector/gadm") %>%
  st_as_sf() %>%
  st_transform(CRS) %>%
  select(NAME_1, geometry)
econ_vars <- read_csv("data/raw/csv/DOSE_V2.csv") %>%
  filter(country == COUNTRY) %>%
  select(region, year, grp_pc_usd_2015, ag_grp_pc_usd_2015) %>%
  filter(year >= 1991 & year <= START_YEAR)

# Raster data
fc <- get_tiled_raster("data/raw/raster/tmf", match = COUNTRY, names = paste0("fc_", 1990:2022))
fc_loss <- get_tiled_raster("data/raw/raster/tmf_loss/", match = COUNTRY, names = paste0("loss_", 1991:2022))
biomass <- get_stac_raster(collection = "hgb", asset = "aboveground", names = "biomass")
cropland <- get_tiled_raster("data/raw/raster/cropland", names = "cropland")
dem <- get_stac_raster(
  collection = "cop-dem-glo-90",
  asset = "data",
  folder = "dem",
  names = "elevation")
ppt <- get_raster("data/raw/raster/chirps", COUNTRY)
tMean <- get_raster("data/raw/raster/era5", COUNTRY)
ag_suitability <- get_raster(
  "data/raw/raster/agricultural_suitability/1980-2009_hist_i/",
  match = "overall_suitability.bil",
  names = "ag_suitability")
travel_time_city <- get_raster("data/raw/raster/travel_time", "travel_time_to_cities_12")
travel_time_port <- get_raster("data/raw/raster/travel_time", "travel_time_to_ports_5")
population <- get_raster("data/raw/raster/population") %>%
  set.names(paste0("pop_density_", 1990:2016))

# Vector data
ecoregions <- st_read("data/raw/vector/ecoregions2017/Ecoregions2017.shp") %>%
  st_transform(CRS) %>%
  st_filter(country) %>%
  select(OBJECTID, ECO_ID, ECO_NAME, BIOME_NUM, BIOME_NAME, geometry)
# rivers <- get_vector("data/raw/vector/Lin2021_rivers", match = COUNTRY, source = "rivers") %>%
#   filter(strmOrder >= 5) %>%
#   st_filter(country) %>%
#   st_transform(CRS)
# roads_grip <- get_vector("data/raw/vector/GRIP_roads", match = COUNTRY, source = "grip") %>%
#   st_transform(CRS)
# roads_osm <- get_osm("data/raw/vector/osm", match = COUNTRY) %>%
#   st_transform(CRS)
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

fc_1990 <- subset(fc, "fc_1990") %in% c(1, 2, 4)  # Undisturbed, degraded or regrowth forest

grid_threshold <- grid_filtered %>%
  poly_extract(fc_1990) %>%
  rename(fc_1990 = weighted_mean) %>%
  filter(fc_1990 >= FC_THRESHOLD / 100)

grid_buffer_threshold <- grid_buffer %>%
  filter(ID %in% grid_threshold$ID)

## 5. Prepare additional data layers for extract --------
# (i) Forest cover in start year

fc_start <- subset(fc, str_which(names(fc), as.character(START_YEAR))) %in% c(1, 2, 4)

# (ii) Pixelwise distance to forest edge in start year

# Convert to equal area projection and aggregate
fc_start_agg <- aggregate(fc_start, 3, fun = "modal")
fc_start_ea <- project(fc_start_agg, CRS, method = "near")

non_forest_patches <- landscapemetrics::get_patches(
  fc_start_ea,
  class = 0
)[[1]][[1]]

names(non_forest_patches) <- "patch"

tic()
patch_area <- fc_start_ea %>%
  lsm_p_area() %>%
  filter(class == 0) %>%
  select(id, value)
toc()

# Reclassify nonforest patch values to area
patch_area_large <- filter(patch_area, value >= FOREST_EDGE_AREA)

tic()
non_forest_patch_area <- classify(non_forest_patches, rcl = patch_area_large, others = NA)
toc()

# Calculate distance to forest edge for all forest pixels
tic()
dist_to_edge <- terra::distance(non_forest_patch_area) %>%
  project(crs(fc_start), method = "bilinear")
toc()

names(dist_to_edge) <- "dist_to_edge"

# (iii) Calculate slope from DEM

slope <- terrain(dem)

# (iv) Mask relevant raster layers to forest area in project start year (~40min for Colombia)
tic()
forest_vars <- list(biomass, dem, slope, ag_suitability, dist_to_edge) %>%
  map(resample, y = fc_start, method = "bilinear", threads = TRUE) %>%
  map(crop, y = fc_start, mask = TRUE) %>%
  rast()
toc()

# (v) Jurisdiction-level annual forest cover loss (polygons with annual loss columns)
# and economic variables

fc_loss_agg <- aggregate(fc_loss, 3)

country_adm1_vars <- country_adm1 %>%
  poly_extract(fc_loss_agg, id_col = "NAME_1") %>%
  rename_with(~ gsub("weighted_mean.", "jurisdiction_", .x), starts_with("weighted_mean")) %>%
  poly_extract(fc_1990, id_col = "NAME_1") %>%
  mutate(across(starts_with("jurisdiction"), .fns = ~ .x / weighted_mean)) %>%
  select(-weighted_mean)

if(nrow(econ_vars) > 0) {
  econ_vars_wide <- econ_vars %>%
    mutate(ag_grp_frac = ag_grp_pc_usd_2015 / grp_pc_usd_2015) %>%
    select(-ag_grp_pc_usd_2015) %>%
    pivot_wider(names_from = year, values_from = contains("grp"))
  
  country_adm1_vars <- left_join(country_adm1_vars, econ_vars_wide, by = c("NAME_1" = "region"))
}

# (vi) Forest fraction in protected areas
##### NEED TO FINISH THIS - CURRENTLY ONLY HAVE FC IN 1990, BUT I NEED TO KNOW
##### WHAT FRAC OF FOREST AREA IN START YEAR IS PROTECTED

tic()
grid_pa_intersection <- grid_threshold %>%
  st_intersection(st_union(st_collection_extract(protected_areas))) %>%
  mutate(int_area = st_area(x))
toc()

# grid_pa_intersection_fc <- grid_pa_intersection %>%
#   poly_extract(fc_start) %>%
#   mutate(fc_protected = weighted_mean * int_area) %>%
#   mutate(frac_protected = fc_protected / wei)
#   select(-weighted_mean, int_area)

# (iv) Calculate fractional polygon overlap with jurisdictional and ecoregion boundaries
grid_adm1_intersection <- calc_intersection(grid_threshold, country_adm1_vars)

grid_ecoregion_intersection <- calc_intersection(grid_threshold, ecoregions)

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


#### Data preparation script
#### Generates a grid of a specified area and extracts key covariates into a dataframe

source("scripts/load.R")

### 1. Set parameters --------

# Spatial and temporal range
COUNTRY <- "Colombia"  # Target country
CRS <- "ESRI:54034"  # CRS to generate grid
START_YEAR <- 2016  # Simulated start year of protection project

# Polygon sampling
COUNTRY_BUFFER <- 10 # buffer distance from country border to exclude when generating polygons (km)
POLY_SIZE <- 59000 # size of polygons in hectares
POLY_SHAPE <- "hex"  # square or hex polygon
POLY_BUFFER_RATIO <- 1 # polygon buffer area as ratio of polygon area
SAMPLE_N <- 25  # number of polygons to sample per stratum
SAMPLE_STRATA <- 4  # number of forest loss strata

# Forest definitions
FC_THRESHOLD <- 20  # Minimum forest cover to include polygons in sample (%)
FOREST_EDGE_AREA <- 10 # Minimum continuous nonforest area to define as forest edge, hectares

# Data processing
OUTPUT_PATH <- "data/processed/vector/sample_polygons"  # Output directory
SEED <- 111  # Random number seed
AGG <- 3  # Factor to aggregate rasters to speed data processing

### 2. Load datasets --------

# Lookup table for efficient data I/O
data_lookup <- read_csv("data/raw/csv/data_lookup.csv")  # TO COMPLETE

# Country admin boundaries and jurisdictional data
country <- gadm(COUNTRY, level = 0, path = "data/raw/vector/gadm") %>%
  st_as_sf() %>%
  st_transform(CRS)
country_adm1 <- gadm(COUNTRY, level = 1, path = "data/raw/vector/gadm") %>%
  st_as_sf() %>%
  st_transform(CRS) %>%
  select(NAME_1, GID_1, geometry)
econ_vars <- read_csv("data/raw/csv/DOSE_V2.csv") %>%
  filter(country == COUNTRY) %>%
  select(region, year, grp_pc_usd_2015, ag_grp_pc_usd_2015) %>%
  filter(year >= 1991 & year <= START_YEAR)

# Raster data
fc <- get_tiled_raster("data/raw/raster/tmf", match = COUNTRY, names = paste0("fc.", 1990:2022))
fc_loss <- get_tiled_raster("data/raw/raster/tmf_loss/", match = COUNTRY, names = paste0("loss.", 1991:2022))
biomass <- get_stac_raster(collection = "hgb", asset = "aboveground", names = "biomass")
cropland <- get_tiled_raster("data/raw/raster/cropland", match = COUNTRY, names = "cropland")
dem <- get_stac_raster(
  collection = "cop-dem-glo-90",
  asset = "data",
  folder = "dem",
  names = "elevation")
ppt <- get_raster("data/raw/raster/chirps", COUNTRY)
tMean <- get_raster("data/raw/raster/era5", COUNTRY)
ag_suitability <- get_raster(
  "data/raw/raster/agricultural_suitability/1980-2009_hist_i",
  ext = ".bil",
  names = "ag_suitability")
travel_time <- get_raster("data/raw/raster/travel_time", names = c("time_to_city", "time_to_port"))
population <- get_raster("data/raw/raster/population", names = paste0("pop_density.", 1991:2016))

# Vector data
ecoregions <- st_read("data/raw/vector/ecoregions2017/Ecoregions2017.shp") %>%
  st_transform(CRS) %>%
  st_filter(country) %>%
  select(ECO_ID, ECO_NAME, BIOME_NUM, BIOME_NAME, geometry)
rivers <- get_vector("data/raw/vector/Lin2021_rivers", match = COUNTRY, source = "rivers", poly = country) %>%
  filter(strmOrder >= 5) %>%
  st_transform(CRS)
roads_grip <- get_vector("data/raw/vector/GRIP_roads", match = COUNTRY, source = "grip", poly = country) %>%
  st_transform(CRS)
roads_osm <- get_osm("data/processed/vector/osm", "data/raw/vector/osm", match = COUNTRY) %>%
  st_transform(CRS)
protected_areas <- get_vector("data/raw/vector/protected_areas", match = COUNTRY, ext = ".geojson") %>%
  st_transform(CRS)
redd_projects <- st_read("data/processed/vector/redd_polys_renoster.gpkg") %>%
  filter(Country == COUNTRY) %>%
  st_transform(CRS)

### 3. Generate polygon grids and buffers --------

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
  buffer_only = TRUE,
  joinStyle = "MITRE")

### 4. Filter to polygons meeting selection criteria --------

# Remove polygons intersecting REDD projects or protected areas designated since 1990.
protected_areas_new <- filter(protected_areas, STATUS_YR >= 1991)

grid_filtered <- grid %>%
  filter_disjoint(redd_projects) %>%
  filter_disjoint(protected_areas_new)

# Remove polygons with < 20% forest cover in 1990

fc_1990 <- subset(fc, "fc.1990") %in% c(1, 2, 4)  # Undisturbed, degraded or regrowth forest
names(fc_1990) <- "fc_1990"

grid_threshold <- grid_filtered %>%
  poly_extract(fc_1990) %>%
  filter(fc_1990 >= FC_THRESHOLD / 100) %>%
  select(-fc_1990)

grid_buffer_threshold <- grid_buffer %>%
  filter(ID %in% grid_threshold$ID)

### 5. Prepare additional data layers for extract --------
## (i) Forest cover in start year

tic()
fc_start <- subset(fc, str_which(names(fc), as.character(START_YEAR))) %in% c(1, 2, 4)
names(fc_start) <- "fc_start"
toc()

## (ii) Pixelwise distance to forest edge in start year

# Convert to equal area projection and aggregate
fc_start_agg <- aggregate(fc_start, fact = AGG, fun = "modal")
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

## (iii) Calculate slope from DEM

slope <- terrain(dem)

## (iv) Mask relevant raster layers to forest area in project start year (~40min for Colombia)
forest_vars <- list(biomass, dem, slope, ag_suitability, dist_to_edge) %>%
  mask_to_forest(fc_start_agg, combine = TRUE)

## (v) Jurisdiction-level annual forest cover loss (polygons with annual loss columns)
## and economic variables

fc_loss_agg <- aggregate(fc_loss, fact = AGG, fun = "mean")

country_adm1_vars <- country_adm1 %>%
  poly_extract(fc_loss_agg, id_col = "NAME_1", col_prefix = "jurisdiction_") %>%
  poly_extract(fc_1990, id_col = "NAME_1") %>%
  mutate(across(starts_with("jurisdiction"), .fns = ~ .x / fc_1990)) %>%
  select(-fc_1990)

if(nrow(econ_vars) > 0) {
  econ_vars_wide <- econ_vars %>%
    mutate(ag_grp_frac = ag_grp_pc_usd_2015 / grp_pc_usd_2015) %>%
    select(-ag_grp_pc_usd_2015) %>%
    pivot_wider(names_from = year, values_from = contains("grp"))

  country_adm1_vars <- left_join(country_adm1_vars, econ_vars_wide, by = c("NAME_1" = "region"))
}

grid_adm1_intersection <- grid_threshold %>%
  calc_intersection(country_adm1_vars, frac_col = "jurisdiction_frac") %>%
  group_by(ID) %>%
  summarise(across(.cols = starts_with("jurisdiction_loss"), .fns = ~ sum(.x * jurisdiction_frac)))

## (vi) Forest fraction in protected areas

# Find intersection of start year forest area in polygons with PAs
grid_pa_intersection <- grid_threshold %>%
  poly_extract(fc_start_agg) %>%
  mutate(poly_area = st_area(x)) %>%
  st_intersection(st_union(st_collection_extract(protected_areas))) %>%
  mutate(int_area = st_area(x))
toc()

# Calculate area of forest in PAs as frac of total forest in polygon
grid_pa_intersection_fc <- grid_pa_intersection %>%
  poly_extract(fc_start_agg, col_prefix = "int_") %>%
  st_drop_geometry() %>%
  mutate(fc_protected = int_fc_start * int_area) %>%
  mutate(protected_frac = drop_units(fc_protected / (fc_start * poly_area)))

# Restore dropped (non-intersecting) NA columns with 0 values
grid_pa_frac <- grid_pa_intersection_fc %>%
  right_join(st_drop_geometry(grid_threshold)) %>%
  select(ID, protected_frac) %>%
  replace_na(list("protected_frac" = 0))

## (vii) Calculate fractional polygon overlap with ecoregion boundaries

grid_ecoregion_intersection <- grid_threshold %>%
  calc_intersection(ecoregions, frac_col = "eco_frac") %>%
  group_by(ID) %>%
  nest(.key = "eco_frac")

## (viii) Calculate distance from nearest river and road

# Rasterize datasets
tic(); rivers_rast <- rasterize_lines(rivers, fc_start_ea); toc()
tic(); roads_rast_grip <- rasterize_lines(roads_grip, fc_start_ea); toc()
tic(); roads_rast_osm <- rasterize_lines(roads_osm, fc_start_ea); toc()

roads_rast_all <- max(c(roads_rast_grip, roads_rast_osm))

# Calculate distance rasters
tic()
dist_to_river <- distance(rivers_rast)
names(dist_to_river) <- "dist_to_river"
toc()
dist_to_road <- distance(roads_rast_all)
names(dist_to_road) <- "dist_to_road"
toc()

### 6. Extract variables for grid and buffer polygons --------

## Extract raster variables for polygons and buffers
raster_vars <- list(fc_start, fc_loss, forest_vars, ppt, tMean, dist_to_river,
                    dist_to_road, travel_time, cropland, population)
buffer_raster_vars <- list(fc_loss_agg, cropland)

grid_fc_raster_vars <- extract_from_list(grid_threshold, raster_vars)

grid_fc_buffer_vars <- grid_buffer_threshold %>% 
  extract_from_list(buffer_raster_vars, col_prefix = "buffer_") %>%
  st_drop_geometry()

## Join with vector variables and create merged wide-format data frame 
##### !! Issue - where a polygon intersects multiple adm1 or ecoregions, its row is duplicated
##### Need a better way to summarise and retain data where multiple intersections exist before merging.
all_vars <- list(grid_fc_raster_vars, grid_fc_buffer_vars, grid_adm1_intersection, grid_ecoregion_intersection, grid_pa_frac)

grid_fc_all_vars <- all_vars %>%
  reduce(full_join) %>%
  mutate(cropland = (cropland + POLY_BUFFER_RATIO * buffer_cropland) / (1 + POLY_BUFFER_RATIO)) %>%
  select(-buffer_cropland)

## Pull geometry into separate object and convert to long-format df

# grid_geoms <- select(grid_fc_all_vars, ID, x)
# grid_vars_long <- grid_fc_all_vars %>%
#   st_drop_geometry() %>%
#   pivot_longer(cols = contains(".")) %>%
#   separate_wider_delim(cols = "name", delim = ".", names = c("var", "year")) %>%
#   pivot_wider(names_from = "var", values_from = "value")

### 7. Take stratified sample of polygons by actual cumulative deforestation after start year --------
# TO DO: REWRITE OR REPACKAGE IN FUNCTION

grid_loss_strata <- grid_fc_all_vars %>%
  st_drop_geometry() %>%
  select(ID, starts_with("loss")) %>%
  pivot_longer(cols = starts_with("loss")) %>%
  separate_wider_delim(cols = "name", delim = ".", names = c("var", "year")) %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  group_by(ID) %>%
  filter(year > START_YEAR) %>%
  summarise(cum_loss = sum(loss)) %>%
  mutate(stratum = cut_interval(cum_loss, n = SAMPLE_STRATA, labels = FALSE))

# Sample either 25 per group or the smallest available group size

# sample_n <- min(c(25, count(grid_loss_strata, stratum)$n))

set.seed(SEED)
grid_sample_ids <- slice_sample(grid_loss_strata, n = SAMPLE_N, by = stratum)

grid_vars_sample <- left_join(grid_fc_all_vars, grid_sample_ids)

### 8. Save data

write_rds(grid_vars_sample, paste0("data/processed/rds/", COUNTRY, "_", POLY_SIZE, "_", START_YEAR, "_data.rds"))


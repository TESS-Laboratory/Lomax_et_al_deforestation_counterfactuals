## Script to generate hexagonal or square grids of a specified diameter covering
## a country polygon.

library(sf)
library(terra)
library(here)
library(dplyr)
library(tmap)

## Set parameters --------

COUNTRY <- "Colombia"
POLY_SIZE <- 10000 # hectares
POLY_SHAPE <- "hex"  # square or hex
SAMPLE_N <- 1000  # Number of sample polygons

## Load country polygon --------

path <- here("data", "raw", "vector", "WB_countries_Admin0_10m", "WB_countries_Admin0_10m.shp")

get_country <- function(path, country_name) {
  countries <- st_read(path)
  country <- dplyr::filter(countries, NAME_EN == country_name)
  
  country
}

country <- get_country(path, country_name = COUNTRY)

# Convert to equal area projection (Cylindrical equal area)
country_ea <- st_transform(country, crs = "ESRI:54034")
## NEED TO ADD MERIDIANS, EASTINGS, NORTHINGS ETC

## Generate polygon grids --------

generate_polygons <- function(geometry, shape, area) {
  # Set arg to TRUE if shape == "square", FALSE if shape == "hex", else NA
  square_arg = ifelse(
    tolower(shape) == "square", TRUE, ifelse(
      tolower(shape) == "hex", FALSE, NA))
  
  # Set area
  area_ha <- units::set_units(area, "hectares")
  area_km2 <- units::set_units(area_ha, "km ^ 2")
  
  # Generate grid
  grid <- st_make_grid(
    x = geometry,
    cellsize = area_km2,
    what = "polygons",
    square = square_arg
  )
  
  # Filter to those contained by geometry
  
  grid_contained <- grid %>%
    st_as_sf() %>%
    st_filter(geometry, .predicate = st_within)
  
  grid_contained
}

# Filter polygons to those contained by country boundaries

grid <- generate_polygons(country_ea, shape = POLY_SHAPE, area = POLY_SIZE)

tm_shape(country) + tm_fill("grey95") +
  tm_shape(grid) + tm_borders("forestgreen")

# ## Import forest/nonforest layer from MPC --------
# library(rstac)
# library(rsi)
# 
# stac_source <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")
# 
# stac_search(
#   q = stac_source,
#   collections = "alos-fnf-mosaic",
#   datetime = "2015-01-01/2020-01-01",
#   bbox = st_bbox(country) %>% as.vector(),
#   limit = 999
# ) %>%
#   get_request()
# 
# 
# nc_fnf <- get_stac_data(
#   # Spatial AOI:
#   aoi = country_ea,
#   # Temporal AOI:
#   start_date = "2020-01-01",
#   end_date = "2021-12-31",
#   # Which asset do we want, from which collection, from which API:
#   asset_names = "C",
#   stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1",
#   collection = "alos-fnf-mosaic",
#   # Where to save the file:
#   output_filename = here("data", "raw", "raster", "palsar_colombia_2020.tif")
# )
# 
# nc_fnf_rast <- rast(nc_fnf)

## Filter polygons to those with > 40% forest cover --------

# Load forest cover raster
fc <- rast(here("data", "raw", "raster", "gfc", "GFC_cover_Colombia_mosaic.tif"))

fc_agg <- fc %>%
  terra::aggregate(fact = 10, cores = 4) %>%
  project(crs(grid))

extract_fc <- function(grid, fc_layer, fc_threshold = NULL, fc_values = NULL) {
  
  # Convert input layer to binary forest/nonforest
  if (!is.null(fc_values)) {
    fc_layer <- fc_layer %in% fc_values
  } else if (!is.null(fc_values)) {
    fc_layer <- (fc_layer >= fc_threshold)
  }
  
  # Extract mean cover per polygon
  # NB: Not weighted by overlapping pixel area (fine for small pixel size)
  grid <- vect(grid)
  grid_fc <- terra::extract(fc_layer, grid, fun = mean, bind = TRUE)
  
  st_as_sf(grid_fc) %>%
    rename(forest_cover = 1)
}

grid_fc <- extract_fc(grid, fc_agg$Y2000) %>%
  rename("forest_cover" = 1)

grid_fc_threshold <- dplyr::filter(grid_fc, forest_cover > 40)

## Generate random sample polygons --------
set.seed(111)

sample_polygons <- function(x, n) {
  if (n >= nrow(x)) {
    warning("Requested n exceeds number of polygons in x")
  }
  slice_sample(x, n = n)
}

grid_sample <- sample_polygons(grid_fc_threshold, SAMPLE_N)

# Visualise

fc_agg_country <- mask(crop(fc_agg, country_ea), country_ea)

tm_shape(country) +
  tm_fill("grey95") +
  tm_shape(fc_agg_country$Y2000) +
  tm_raster(col.scale = tm_scale_continuous(values = "Greens")) +
  tm_shape(grid_sample) +
  tm_borders("red", lwd = 2)

hist(grid_sample$forest_cover, breaks = 100)

## 


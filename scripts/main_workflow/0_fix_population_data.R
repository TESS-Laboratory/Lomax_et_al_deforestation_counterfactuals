# Script to impute missing population data in GlobPop raster using WorldPop
# Guy Lomax

source("scripts/load.R")

COUNTRY <- "Cote d'Ivoire"

# Load
data_lookup <- read_csv("data/raw/csv/data_lookup.csv")
country <- geodata::gadm(COUNTRY, level = 0, path = "data/raw/vector/gadm") %>%
  st_as_sf()
country_adm1 <- gadm(COUNTRY, level = 1, path = "data/raw/vector/gadm") %>%
  st_as_sf() %>%
  select(NAME_1, GID_1, geometry)
overlap_years <- 2000:2020
globpop <- get_raster("data/raw/raster/population/globpop", names = paste0("pop_density.", 1991:2023))
worldpop <- get_raster("data/raw/raster/population/worldpop", match = COUNTRY, names = paste0("pop_density.", 2000:2020), source = "worldpop")


# Fill gaps by scaling WorldPop values to GlobPop by their respective means

globpop_filled <- map(overlap_years, function(y) {
  
  # Subset both datasets to one year
  message("Year: ", y)
  globpop_year <- subset(globpop, str_which(names(globpop), as.character(y)))
  worldpop_year <- subset(worldpop, str_which(names(worldpop), as.character(y)))
  
  # Crop, mask and resample to the same resolution and extent
  globpop_crop <- crop(globpop_year, worldpop_year)
  
  worldpop_resample <- resample(worldpop_year, globpop_crop)
  
  worldpop_mask <- mask(worldpop_resample, globpop_crop)
  
  globpop_mask <- mask(globpop_crop, worldpop_mask)
  
  # Calculate overall scale factor as the ratio between GlobPop and WorldPop in their shared area
  scale_factor <- global(globpop_mask, na.rm = TRUE) / global(worldpop_mask, na.rm = TRUE)
  
  # Scale WorldPop by scale_factor to "match" GlobPop
  globpop_pred <- scale_factor$mean[1] * worldpop_resample
  
  # Gapfill GlobPop with Worldpop
  globpop_out <- merge(globpop_year, globpop_pred)
  
  globpop_out
}) %>% rast()

# Extrapolation of gapfilled areas outside WorldPop data period (i.e., 1991-1999 and 2021-2023)

raster_scale <- function(year, base_rast) {
  message("Processing year: ", year)
  
  # Extract raw GlobPop raster for given year
  y_rast <- globpop %>%
    subset(str_which(names(globpop), as.character(year)))
  
  # Mask gapfilled raster for base year to extent of raw GlobPop raster (i.e., remove gaps)
  base_masked <- mask(base_rast, y_rast)
  
  # Calculate scale factor (ratio of raw to base total population estimate)
  scale_factor <- global(y_rast, na.rm = TRUE) / global(base_rast, na.rm = TRUE)
  
  # Scale gapfilled areas by scale factor
  y_pred <- scale_factor$mean[1] * base_rast
  
  # Combine
  y_out <- merge(y_rast, y_pred)
  
  y_out
}

# Apply function to fill gaps for 1991-1999
globpop_early <- map(1991:1999, raster_scale, base_rast = globpop_filled$pop_density.2000) %>%
  rast()

# Apply function to fill gaps for 2021-2023
globpop_late <- map(2021:2023, raster_scale, base_rast = globpop_filled$pop_density.2020) %>%
  rast()

# Merge and write to disk
globpop_all <- c(globpop_early, globpop_filled, globpop_late)

writeRaster(globpop_all, paste0("data/processed/raster/population/", COUNTRY, "_population_gapfilled.tif"))

pushover(message = "Population raster processing complete: ", COUNTRY)
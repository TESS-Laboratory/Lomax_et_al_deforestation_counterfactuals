# Script to impute missing population data in GlobPop raster using WorldPop
# Guy Lomax

source("scripts/load.R")

COUNTRY <- "Cote d'Ivoire"
AGG <- 5  # Aggregation factor to reduce noise.

# Load
country <- geodata::gadm(COUNTRY, level = 0, path = "data/raw/vector/gadm") %>%
  st_as_sf()
country_adm1 <- gadm(COUNTRY, level = 1, path = "data/raw/vector/gadm") %>%
  st_as_sf() %>%
  select(NAME_1, GID_1, geometry)
overlap_years <- 2000:2020
globpop <- get_raster("data/raw/raster/population", names = paste0("pop_density.", 1991:2023))
worldpop <- get_raster("data/raw/raster/population/worldpop", match = COUNTRY, names = paste0("pop_density.", 2000:2020), source = "worldpop")


# Fill gaps by scaling WorldPop values to GlobPop by their respective means

globpop_filled <- map(overlap_years, function(y) {
  message("Year: ", y)
  globpop_year <- subset(globpop, str_which(names(globpop), as.character(y)))
  worldpop_year <- subset(worldpop, str_which(names(worldpop), as.character(y)))
  
  globpop_crop <- crop(globpop_year, worldpop_year)
  
  worldpop_resample <- resample(worldpop_year, globpop_crop)
  
  worldpop_mask <- mask(worldpop_resample, globpop_crop)
  
  globpop_mask <- mask(globpop_crop, worldpop_mask)
  
  scale_factor <- global(globpop_mask, na.rm = TRUE) / global(worldpop_mask, na.rm = TRUE)
  
  globpop_pred <- scale_factor$mean[1] * worldpop_resample
  
  globpop_out <- merge(globpop_year, globpop_pred)
  
  globpop_out
}) %>% rast()

raster_scale <- function(year, base_rast) {
  message("Processing year: ", year)
  
  y_rast <- globpop %>%
    subset(str_which(names(globpop), as.character(year)))
  
  base_masked <- mask(base_rast, y_rast)
  
  scale_factor <- global(y_rast, na.rm = TRUE) / global(base_rast, na.rm = TRUE)
  
  y_pred <- scale_factor$mean[1] * base_rast
  
  y_out <- merge(y_rast, y_pred)
  
  y_out
}

globpop_early <- map(1991:1999, raster_scale, base_rast = globpop_filled$pop_density.2000) %>%
  rast()

globpop_late <- map(2021:2023, raster_scale, base_rast = globpop_filled$pop_density.2020) %>%
  rast()

# Merge and write to disk
globpop_all <- c(globpop_early, globpop_filled, globpop_late)

writeRaster(globpop_all, paste0("data/processed/raster/population/", COUNTRY, "_population_gapfilled.tif"))

pushover(message = "Population raster processing complete: ", COUNTRY)

# Scale missing data in previous and subsequent years by mean of GlobPop temporal layers
# 
# # Predict GlobPop values for prior years
# 
# df_2000 <- globpop_filled$pop_density.2000 %>%
#   as.data.frame(cells = TRUE)
# 
# extrapolate_raster <- function(year, base_raster) {
#   message("Filing gaps for year ", year)
#   
#   base_log <- log(base_raster + 0.01)
#   
#   df_base <- as.data.frame(base_log, cells = TRUE) %>%
#     rename(base = 2)
#   
#   globpop_y <- globpop %>%
#     subset(str_which(names(globpop), as.character(year))) %>%
#     crop(globpop_filled) %>%
#     add(0.01) %>%
#     log()
#   
#   df_y <- globpop_y %>%
#     as.data.frame(cells = TRUE) %>%
#     rename(y = 2)
#   
#   df_joined <- left_join(df_base, df_y)
#   
#   mod <- lm(y ~ base, data = df_joined)
#   
#   globpop_pred <- coef(mod)[1] + coef(mod)[2] * base_raster
#   
#   globpop_out <- merge(globpop_y, globpop_pred) %>%
#     exp() %>%
#     subtract(0.01)
#   
#   globpop_out
# }
# 
# 
# globpop_previous <- map(1991:1999, extrapolate_raster, base_raster = globpop_filled$pop_density.2000) %>%
#   rast()
# 
# globpop_next <- map(2021:2023, extrapolate_raster, base_raster = globpop_filled$pop_density.2020) %>%
#   rast()
# 
# globpop_combined <- c()

# Script to impute missing population data in GlobPop raster using WorldPop

source("scripts/load.R")

COUNTRY <- "Democratic Republic of the Congo"

# Load
country <- geodata::gadm(COUNTRY, level = 0, path = "data/raw/vector/gadm") %>%
  st_as_sf()
country_adm1 <- gadm(COUNTRY, level = 1, path = "data/raw/vector/gadm") %>%
  st_as_sf() %>%
  select(NAME_1, GID_1, geometry)
overlap_years <- 2000:2020
globpop <- get_raster("data/raw/raster/population", names = paste0("pop_density.", 1991:2023))
worldpop <- get_raster("data/raw/raster/population/worldpop", names = paste0("pop_density.", 2000:2020))

# Fit linear models between non-missing pixel values in each dataset and layer

globpop_filled <- map(overlap_years, function(y) {
  message("Fitting model for year ", y)
  globpop_year <- subset(globpop, str_which(names(globpop), as.character(y)))
  worldpop_year <- subset(worldpop, str_which(names(worldpop), as.character(y)))
  
  globpop_crop <- crop(globpop_year, worldpop_year) %>% aggregate(5)
  
  worldpop_resample <- resample(worldpop_year, globpop_crop)
  
  pop_df <- c(globpop_crop, worldpop_resample) %>%
    as.data.frame() %>%
    rename("globpop" = 1, "worldpop" = 2)
  
  mod <- lm(globpop ~ worldpop, data = pop_df)
  
  globpop_pred <- coef(mod)[1] + coef(mod)[2] * worldpop_resample
  
  globpop_out <- merge(globpop_crop, globpop_pred)
  
}) %>% rast()

# Predict GlobPop values for 

# Script to mosaic tiled rasters

library(terra)
library(here)


# Load

folder_path <- here("data", "raw", "raster", "gfc")
file_prefix <- "GFC_cover_Colombia"
files <- Sys.glob(paste0(folder_path, "/", file_prefix, "*.tif")) %>%
  lapply(rast)

# Merge and write
mosaic <- do.call(mosaic, files)

writeRaster(mosaic, paste0(folder_path, "/", file_prefix, "_mosaic.tif"))

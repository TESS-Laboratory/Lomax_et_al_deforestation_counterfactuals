# Script to mosaic tiled rasters

library(terra)
library(sf)

# Load

folder_path <- "data/raw/raster/tmf"
file_prefix <- "TMFAnnualClasses_Colombia"
files <- Sys.glob(paste0(folder_path, "/", file_prefix, "*.tif"))

# Warp and write merged file
tictoc::tic()
gdal_utils(
  util = "warp",
  source = files,
  destination = paste0(folder_path, "/", file_prefix, "_merged.tif")
)
tictoc::toc()

writeRaster(mosaic, paste0(folder_path, "/", file_prefix, ".tif"))

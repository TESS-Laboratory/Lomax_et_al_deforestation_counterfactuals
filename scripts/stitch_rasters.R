# Script to mosaic tiled rasters

library(sf)

COUNTRY <- "Colombia"

# Load
folder_path <- "data/raw/raster/tmf"
files <- Sys.glob(paste0(folder_path, "/*", COUNTRY, "*.tif"))
folder_out <- "data/processed/raster/tmf"

# Warp and write merged file
tictoc::tic()
gdal_utils(
  util = "warp",
  source = files,
  destination = paste0(folder_out, "/tmf_", COUNTRY, "_merged.tif")
)
tictoc::toc()
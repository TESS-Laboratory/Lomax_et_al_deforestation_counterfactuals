# Script to mosaic tiled rasters

library(terra)

# Load

folder_path <- "data/raw/raster/gfc"
file_prefix <- "gfc_Colombia"
files <- Sys.glob(paste0(folder_path, "/", file_prefix, "*.tif")) |>
  lapply(rast)

# Merge, reproject and write
tictoc::tic()
mosaic <- purrr::reduce(files, mosaic)
tictoc::toc()

writeRaster(mosaic, paste0(folder_path, "/", file_prefix, ".tif"))

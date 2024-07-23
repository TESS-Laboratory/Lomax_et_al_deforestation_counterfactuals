## Functions for preparing and processing raster data

#' @title Convert TMF layers to binary forest loss
#' @description
#' Converts the annual class layers in TMF (undisturbed, degraded, deforested etc.)
#' to a binary map of annual deforestation
#' 
#' @usage tmf_to_defor(tmf)
#' 
#' @param tmf a SpatRaster object containing the transition codes
#' 

tmf_to_defor <- function(tmf) {
  
  # Merge annual codes with lagged annual codes to encode 2-digit transition codes
  
  transitions <- 10 * tmf[1:(length(tmf) - 1)] + tmf[2:length(tmf)]
  
    defor_codes <- c(
    13,  # Undisturbed to deforested
    23,  # Degraded to deforested
    43   # Regrowth to deforested
  )
  
  defor <- transitions %in% defor_codes
  
  defor
}

#' @title Fast distance
#' @description 
#' Takes an efficient approach to implementing terra::distance() using
#' for only those cells within target polygons
#' 
#' @usage fast_distance(raster, target_polys)
#' 
#' @param raster SpatRaster containing values for which to calculate distance
#' @param target_polys sf object. Polygons to limit the calculation to.

fast_distance <- function(raster, target_polys) {
  
  if(crs(raster) != crs(target_polys)) {
    message("Transforming polygons to raster CRS.")
    target_polys <- st_transform(target_polys, crs(raster))
  }
  
  message("Masking cells outside target polygons.")
  raster_masked <- mask(raster, target_polys, updatevalue = -1)
  raster_combined <- max(raster, raster_masked, na.rm = TRUE)
  
  message("Calculating distance.")
  dist <- distance(raster_combined, exclude = -1)

  dist
  
}

#' @title Mask to forest
#' @description Combines a list of rasters and crops/masks them to a raster layer
#' of forest cover
#' 
#' @usage mask_to_forest(list, forest, combine = TRUE)
#' 
#' @param list A list of SpatRasters (which can have different extents, resolutions
#' or CRS)
#' @param forest A binary single-layer SpatRaster in which forest cover is
#' represented by a value of 1. Nonforest can be 0 or NA.
#' @param combine logical. Should resulting rasters be combined into a single
#' multilayer SpatRaster?

mask_to_forest <- function(list, forest, combine = TRUE) {
  
  resample_verbose <- function(x, y, ...) {
    name <- names(x)
    message("Resampling layer: ", name)
    tic()
    z <- resample(x, y, ...)
    toc()
    z
  }
  
  crop_verbose <- function(x, y, ...) {
    name <- names(x)
    message("Cropping layer: ", name)
    tic()
    z <- crop(x, y, ...)
    toc()
    z
  }
  
  message("Resampling rasters to forest cover CRS and grid")
  list_resampled <- map(list, resample_verbose, y = forest, method = "bilinear", threads = TRUE)
  list_resampled
  
  message("Cropping and masking rasters to forest cover area")
  list_masked <- map(list_resampled, crop_verbose, y = forest, mask = TRUE)

  if (combine == TRUE) {
    message("Combining raster list")
    tic()
    combined <- rast(list_masked)
    toc()
    combined
  } else {
    list_masked
  }

}

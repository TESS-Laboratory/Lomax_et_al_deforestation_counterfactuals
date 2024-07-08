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

tmf_to_defor <- function(tmf, degradation = FALSE) {
  
  # Merge annual codes with lagged annual codes to encode 2-digit transition codes
  
  transitions <- 10 * tmf[[1:(nlyr(tmf) - 1)]] + tmf[[2:nlyr(tmf)]]
  
  defor_codes <- c(
    13,  # Undisturbed to deforested
    23,  # Degraded to deforested
    43   # Regrowth to deforested
  )
  
  defor <- transitions %in% defor_codes
  
  defor
}

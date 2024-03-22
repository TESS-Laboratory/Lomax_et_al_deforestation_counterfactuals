#' @title Generate polygon grid
#' @description
#' Generates a square or hexagonal grid of a specified size within a
#' target polygon object (e.g., country borders)
#' 
#' @usage generate_polygons(geometry, shape = "hex", area, unit = "hectares")
#' 
#' @param geometry The polygon object within which to generate the grid
#' @param shape The desired grid shape, either "hex" or "square"
#' @param area A number representing the desired area of grid cells
#' @param unit A string representing the areal unit, e.g., "hectares"
#' 
#' @export
#' 

generate_polygons <- function(geometry, shape = "hex", area, unit = "hectares") {
  # Set arg to TRUE if shape == "square", FALSE if shape == "hex", else NA
  square_arg = ifelse(
    tolower(shape) == "square", TRUE, ifelse(
      tolower(shape) == "hex", FALSE, NA))
  
  # Set area
  area_units <- units::set_units(area, unit, mode = "standard")
  area_km2 <- units::set_units(area_units, "km ^ 2")

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

#' @title Get country
#' @description
#' A helper function to extract a country polygon from a global dataset
#' given a country name and dataset file path
#' 
#' @usage get_country(country_name, path)
#' 
#' @param country_name The name of the target country
#' @param path The filepath to the vector file containing country boundaries
#' 

get_country <- function(country_name, path) {
  countries <- st_read(path)
  country <- dplyr::filter(countries, NAME_EN == country_name)
  
  country
}

#' @title Extract forest cover
#' @description
#' Extracts forest cover % for each member of a grid from an underlying raster.
#' The raster can contain a percentage or fractional forest cover value,
#' a binary forest/nonforest or a categorical set of values representing forest.
#' 
#' @usage extract_fc(grid, fc_layer, fc_threshold = NULL, fc_values = NULL)
#' 
#' @param grid An sf object containing a grid or other polygons
#'   for which to extract values
#' @param fc_layer A SpatRaster layer containing forest cover information
#' @param fc_threshold (Optional) A number. If `fc_layer` represents fractional
#'   cover, defines a threshold to distinguish forest and nonforest pixels.
#'   If not specified, the function will take a mean of the continuous values.
#' @param fc_values (Optional) A vector of values. If `fc_layer` is categorical,
#'   defines the values that will be considered forest.
#' 

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

#' @title Sample polygons
#' @description
#' Randomly samples polygons from a grid, with a warning if the desired sample
#' size exceeds the total number of polygons
#' 
#' @usage sample_polygons(x, n)
#' 
#' @param x An sf polygon object
#' @param n The number of polygons to sample
#' 

sample_polygons <- function(x, n) {
  if (n >= nrow(x)) {
    warning("Requested n exceeds number of polygons in x")
  }
  slice_sample(x, n = n)
}
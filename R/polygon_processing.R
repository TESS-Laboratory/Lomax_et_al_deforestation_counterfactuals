## Functions for generating, sampling and processing data with unit polygons

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

generate_polygons <- function(geometry, buffer = 0, shape = "hex", area, unit = "hectares") {
  # Set arg to TRUE if shape == "square", FALSE if shape == "hex", else NA
  square_arg = ifelse(
    tolower(shape) == "square", TRUE, ifelse(
      tolower(shape) == "hex", FALSE, NA))
  
  # Shrink polygon by buffer distance
  message("Calculating buffer")
  geometry_buffer <- st_buffer(geometry, dist = set_units(-1 * buffer, "km"))
  
  # Set area
  area_units <- set_units(area, unit, mode = "standard")
  area_km2 <- set_units(area_units, "km ^ 2")

  # Generate grid
  message("Generating grid")
  grid <- st_make_grid(
    x = geometry_buffer,
    cellsize = area_km2,
    what = "polygons",
    square = square_arg
  )

  # Filter to those contained by geometry

  grid_contained <- grid %>%
    st_as_sf() %>%
    st_filter(geometry_buffer, .predicate = st_within)
  
  # Assign polygon ID
  
  grid_contained$ID <- seq_len(nrow(grid_contained))

  grid_contained
}

## Functions for generating, sampling and processing data with unit polygons

#' @title Generate polygon buffers
#' @description
#' Generates buffers of a specified radius from an sf MULTIPOLYGON object
#' 
#' @usage generate_buffers(geometry, dist)
#' 
#' @param geometry The sf multipolygon object within which to generate the grid
#' @param dist numeric. The buffer distance
#' 

generate_buffers <- function(geometry, dist) {
  geometry_buffered <- st_buffer(geometry, dist = set_units(dist, "km"))
  
  buffer_only <- map2(geometry_buffered$x, geometry$x, st_difference) %>%
    st_as_sfc() %>%
    st_set_crs(crs(geometry)) %>%
    st_as_sf() %>%
    bind_cols(st_drop_geometry(geometry))
  
  buffer_only
}


#' @title Extract forest cover
#' @description
#' Extracts forest cover % for each member of a grid from an underlying raster.
#' The raster can contain a percentage or fractional forest cover value,
#' a binary forest/nonforest or a categorical set of values representing forest.
#' 
#' @usage extract_grid(grid, layer)
#' 
#' @param grid An sf object containing a grid or other polygons
#'   for which to extract values
#' @param layer A SpatRaster containing values
#' 

poly_extract <- function(grid, layer, fun = "mean", id_col = "ID", ...) {
  
  if(is.character(fun)) {
    
    # exact_extract setup if fun is a string (built-in summary function)
    
    extract <- 
      exact_extract(
        layer, 
        grid, 
        fun = fun, 
        force_df = TRUE,
        full_colnames = TRUE,
        coverage_area = TRUE,
        append_cols = id_col
      ) %>%
      rename_with(.cols = starts_with(fun), ~ gsub(paste0(fun, "."), "", .x))
    
    output <- grid %>%
      full_join(extract)
    
  } else if (is_function(fun)) {
    
    # exact_extract setup if fun is a user-defined function taking a data.frame
    
   extract <-
     exact_extract(
       layer,
       grid,
       fun = fun,
       ...,
       force_df = TRUE,
       coverage_area = TRUE,
       summarize_df = TRUE,
       append_cols = "ID"
     )
   
   output <- grid %>%
     full_join(extract)
   
  } else {
    
    stop("'fun' is not a valid function")
    
  }
  
  output
}

#' @title Sum by value
#' @description 
#' A function only for use with grid_extract that returns the total area covered
#' by each value in a categorical raster in a wide format data frame
#' 
#' @usage sum_by_value(df, wide = FALSE)
#' 
#' @param df a data.frame of columns "value" and "coverage_area" returned by
#' exactextractr::exact_extract
#' @param wide logical. Should the returned dataframe be wide format (one column per
#' pixel value) or long format?
#' 

sum_by_value <- function(df, wide = FALSE) {
  
  if (nrow(df) == 0) {
    
    NA
    
  } else {
    
    output <- df %>%
      group_by(value) %>%
      summarise(area = sum(coverage_area)) %>%
      arrange(value) %>%
      rename(cell_value = value, )
    
    if (wide == TRUE) {
      
      output <- pivot_wider(output, names_from = "cell_value", values_from = "area")
      
    }
    
    output
    
  }
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
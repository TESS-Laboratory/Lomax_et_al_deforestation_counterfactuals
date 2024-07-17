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

generate_polygons <- function(geometry, buffer = 0, shape = "hex", area, unit = "hectares", crs = NULL) {
  # Set arg to TRUE if shape == "square", FALSE if shape == "hex", else NA
  square_arg = ifelse(
    tolower(shape) == "square", TRUE, ifelse(
      tolower(shape) == "hex", FALSE, NA))
  
  if (!is.null(crs)) {
    geometry <- st_transform(geometry, crs)
  }
  
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
  grid_contained$poly_area <- area_km2

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

generate_buffers <- function(
    geometry,
    dist = NULL,
    area_ratio = NULL,
    shape = "hex",
    crs = NULL,
    buffer_only = FALSE,
    ...) {
  
  # Transform if needed
  if(!is.null(crs)) {
      geometry <- st_transform(geometry)
  }
  
  # Calculate radius if area provided
  if(is.null(dist)) {
    radius <- ifelse(
      shape == "hex",
      sqrt(st_area(st_geometry(geometry)) / (2 * sqrt(3))),
      sqrt(st_area_geometry / 2)
    )
    
    dist <- radius * (sqrt(1 + area_ratio) - 1)
  } else {
    dist <- set_units(dist, "km")
  }
  
  geometry_buffered <- st_buffer(geometry, dist = dist, ...)

  if (buffer_only == TRUE) {
    geometry_buffered <- map2(st_geometry(geometry_buffered), st_geometry(geometry), st_difference) %>%
      st_as_sfc() %>%
      st_set_crs(crs(geometry)) %>%
      st_as_sf() %>%
      bind_cols(st_drop_geometry(geometry))
  }

  geometry_buffered
}


#' @title Polygon extract
#' @description
#' Extracts values from cells in a SpatRaster that are covered by defined
#' polygons using an inbuilt or user-defined summary function. A wrapper around
#' exact_extract from the exactextractr package.
#' 
#' @usage poly_extract(polys, layer)
#' 
#' @param polys An sf object containing a grid or other polygons
#'   for which to extract values
#' @param layer A SpatRaster containing values
#' @param fun A user-defined function or a string representing an inbuilt function
#' @param id_col character. The name of the polygon ID column in the poly object
#' @param ... additional arguments to pass to exactextractr::exact_extract()
#' 

poly_extract <- function(poly, layer, fun = "weighted_mean", id_col = "ID", ...) {
  
  if(is.character(fun)) {
    
    # exact_extract setup if fun is a string (built-in summary function)
    
    extract <- 
      exact_extract(
        layer, 
        grid, 
        fun = fun, 
        weights = "area",
        ...,
        force_df = TRUE,
        coverage_area = TRUE,
        append_cols = id_col,
        max_cells_in_memory = 1e+09
      )
    
  } else if (is_function(fun)) {
    
    # exact_extract setup if fun is a user-defined function taking a data.frame
    
   extract <-
     exact_extract(
       layer,
       grid,
       fun = fun,
       weights = "area",
       ...,
       force_df = TRUE,
       coverage_area = TRUE,
       summarize_df = TRUE,
       append_cols = id_col,
       colname_fun = function(values) values,
       max_cells_in_memory = 1e+09
     )
   
  } else {
    
    stop("'fun' is not a valid function")
    
  }
  
  output <- grid %>%
    full_join(extract)
  
  output
}

#' @title Calculate intersection
#' @description Calculates the fraction of grid polygons intersecting with each
#' of another set of polygons
#' 
#' @usage calc_intersection(x, y, area_col = NULL, drop_geom = TRUE)
#' 
#' @param x sf object. The target polygon
#' @param y sf object. The polygons for which to calculate fractional cover
#' @param area_col character. An optional column name in x containing the total
#' area of each polygon. If NULL, this will be calculated with st_area().
#' @param drop_geom logical. Should the intersected geometries be dropped,
#' leaving a non-sf data frame? Defaults to TRUE.

calc_intersection <- function(x, y, area_col = NULL, drop_geom = TRUE) {
  
  if (is.null(area_col)) {
    x$poly_area.x <- st_area(x)
  } else {
    x$poly_area.x <- x[[area_col]]
  }
  
  intersection_area <- x %>%
    st_intersection(y) %>%
    mutate(area_frac = st_area(.) / poly_area.x) %>%
    select(-poly_area.x)
  
  if (drop_geom == TRUE) {
    intersection_area <- st_drop_geometry(intersection_area)
  }
  
  intersection_area

}

#' @title Calculate cumulative deforestation
#' @description
#' Calculates cumulative deforestation in a set of polygons from
#' 2000 to a given start year, returning a data frame
#' 
#' @usage calc_cumulative_defor <- function(x, start, area_col, after)
#' 
#' @param x an sf object or data frame generated by poly_extract
#' @param start numeric. The start year before which to count deforestation
#' @param area_col object name. The name of the column containing annual area (not a string)
#' @param after logical. Should cumulative deforestation be calculated from the
#' start year to the end of the dataset, rather than up to the start year?


calc_cumulative_defor <- function(x, start, area_col, after = FALSE) {
  
  start_val <- start - 2000
  
  if (after == TRUE) {
    frac_defor <- x %>%
      group_by(ID) %>%
      filter(cell_value != 0) %>%
      summarise(cum_defor = sum(.data[[area_col]] * (cell_value <= start_val), na.rm = T))
    
  } else {
    frac_defor <- x %>%
      group_by(ID) %>%
      summarise(cum_defor = sum(.data[[area_col]] * (cell_value > start_val), na.rm = T))
  }
  
  if (any(class(x) == "sf")) {
    frac_defor <- st_drop_geometry(frac_defor)
  }
  
  frac_defor
}

#' @title Sample polygons
#' @description
#' Takes a stratified sample of polygons from a grid, with a warning if the
#' desired sample size exceeds the total number of polygons
#' 
#' @usage sample_polygons(x, n, strata)
#' 
#' @param x An sf polygon object
#' @param n integer. The number of polygons to sample
#' @param strata character. The column on which to stratify
#' 

sample_polygons <- function(x, n, strata_col = NULL, strata = 5) {
  if (n >= nrow(x)) {
    warning("Requested n exceeds number of polygons in x")
  }
  
  if (is.null(strata_col)) {
    
    slice_sample(x, n = n)
    
  } else {
    
    x %>%
    mutate(quantile = cut_number(.data[[strata_col]], n = strata, labels = FALSE)) %>%
      slice_sample(n = SAMPLE_N / strata, by = quantile)
    
  }
  
}

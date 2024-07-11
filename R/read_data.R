## Functions for efficiently reading in data to the workflow

#' @title Get vector object
#' @description
#' A function to load and filter vector objects to those intersecting
#' a defined polygon
#' 
#' @usage get_vector(folder, match, poly, ext)
#' 
#' @param folder character. The name of the dataset folder.
#' @param match character. A string to match in target filename(s)
#' @param poly sf object. One or more polygons to filter the dataset.
#' @param ext character. The target file extension
#' 

get_vector <- function(folder, match = NULL, poly = NULL, ext = ".shp") {
  file_paths <- Sys.glob(paste0(folder, "/*", ext))
  
  if (!is.null(match)) {
    files <- file_paths[grepl(match, file_paths)]
  } else {
    files <- file_paths
  }
  
  sf_list <- map(files, st_read)
  
  if(!is.null(poly)) {
    
    poly_transform <- st_transform(poly, crs = st_crs(sf_list[[1]]))
    
    intersects <- sf_list %>%
      map(st_bbox) %>%
      map(st_as_sfc) %>%
      map(st_intersects, y = poly_transform, sparse = FALSE) %>%
      unlist()
    
    sf_list <- sf_list[intersects]
  }
  
  sf_combined <- bind_rows(sf_list)

  sf_combined
    
}

#' @title Read OSM road data
#' @description 
#' Reads in lines from an OSM PBF file and filters by default to "highways" objects
#' 
#' @usage get_osm(folder, match, type)
#' 
#' @param folder character. The name of the dataset folder
#' @param match character. A string (e.g., country name) to match in the target file(s)
#' @param layer character. The key to filter resulting data on

get_osm <- function(folder, match = NULL, type = "highway") {
  
  file_path <- Sys.glob(paste0(folder, "/*", match, "*.pbf"))
  
  data <- suppressWarnings(st_read(file_path, layer = "lines")) %>%
    filter(!is.na(.data[[type]]))

  data
}


#' @title Get prepared raster layer defined by a given match string
#' @description 
#' A function to load a complete raster with a country name from a filepath
#' 
#' @usage get_raster(folder, match, layer)
#' 
#' @param folder character. The name of the dataset folder
#' @param match character. A string (e.g., country name) to match in the target
#' @param layer character or numeric. A vector of names or layer numbers to
#' extract from the target raster

get_raster <- function(folder, match = NULL, layer = NULL) {
  
  file_paths <- Sys.glob(paste0(folder, "/*", match, "*.tif*"))
  
  raster <- rast(file_paths)
  
  if (!is.null(layer)) {
    raster <- raster[[layer]]
  }
  
  raster
}

#' @title Get raster from tiles
#' @description 
#' A function to find and load a Virtual Raster Dataset (VRT) from a collection
#' of tile-based rasters. Thank you, Robert Hijmans!
#' 
#' @usage get_tiled_raster(folder, layer = NULL)
#' 
#' @param folder character. The name of the dataset folder.
#' @param layer character or numeric. A vector of names or layer numbers to
#' extract from the target rasters.

get_tiled_raster <- function(folder, match = "", layer = NULL, names = NULL, crop = NULL) {
  
  # Find raster tiles
  file_paths <- Sys.glob(paste0(folder, "/*.tif"))
  file_paths <- file_paths[grepl(match, file_paths)]
  
  # Create virtual raster
  vrt <- vrt(file_paths)
  
  # Subset and/or rename layers
  if(!is.null(layer)) {
    vrt <- vrt[[layer]]
  }
  
  if (!is.null(names)) {
    names(vrt) <- names
  }
  
  if (!is.null(crop)) {
    vrt <- crop(vrt, country)
  }
  
  # Return virtual raster dataset
  vrt
}

#' @title Get raster from STAC
#' @description
#' A function to download data from the MPC STAC that checks whether a file is
#' already present in the relevant folder before downloading.
#' 
#' @usage get_stac_raster(country, collection, asset, folder)
#' 
#' @param country character. Country name.
#' @param collection character. A STAC collection ID.
#' @param asset character. The asset or band name(s) to retrieve.
#' @param folder character. Then name of the output folder in "data/raw/raster/"
#' 

get_stac_raster <- function(poly, collection, asset, folder = NULL, crs = NULL) {
  
  if(is.null(folder)) {
    folder <- collection
  }
  
  filepath <- paste0("data/raw/raster/", folder, "/", country, ".tif")

  if (!file.exists(filepath)) {
    message("File not detected. Downloading from STAC.")
    filepath <- rsi::get_stac_data(
      aoi = poly,
      start_date = "2000-01-01",  end_date = "2024-01-01",
      asset_names = asset,
      collection = collection,
      stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
      output_filename = filepath,
    )
  } else {
    message("File detected. Loading from disc.")
  }
  
  rast(filepath)
}

#' @title Read Renoster polygons
#' @description
#' A function to read in, clean and fix geometries of REDD project boundaries
#' held in .gpkg format in the Renoster dataset (Karnik et al., pp)
#' 
#' @usage read_renoster(path, skip = NULL)
#' 
#' @param path string. A filepath to the data file.
#' @param skip (optional) string. A vector of project codes to skip (e.g., if they contain
#' unfixable geometries). Codes should be in the format "XXX123", e.g., "VCS381".

read_renoster <- function(filepath, id_list) {
  st_read(filepath) %>%
    select(ProjectID, geom) %>%
    filter(ProjectID %in% id_list)
}

#' @title Read KML files
#' @description
#' A function to read in, clean and fix geometries of project boundaries stored
#' in .kml formats (e.g., the Verra dataset). Files with no layers or other
#' errors will be silently skipped. Files with multiple layers will have all
#' layers read in and combined into a single sf object.
#' 
#' @usage read_kml(path)
#' 
#' @param path string. A filepath to the data file.

read_kml <- function(filepath) {
  
  layer_names <- possibly(st_layers)(filepath)$name
  
  st_read2 <- possibly(st_read)
  
  if(!is.null(layer_names)) {
    layers <- map(layer_names, function(name) st_read2(filepath, layer = name, quiet = TRUE))
    layers <- bind_rows(layers)
    
    layers
  }
}
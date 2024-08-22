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

get_vector <- function(folder, match = NULL, poly = NULL, source = "gee", ext = ".shp") {
  file_paths <- Sys.glob(paste0(folder, "/*", ext))
  
  if (!is.null(match)) {
    match <- data_lookup[data_lookup$country == match,][[source]]
    
    files <- file_paths[str_detect(file_paths, match)]
  } else {
    files <- file_paths
  }
  
  sf_combined <- files %>%
    map(st_read) %>%
    bind_rows()
  
  if(!is.null(poly)) {
    message("Filtering to those that intersect with polygon.")
    poly_transform <- st_transform(poly, crs = st_crs(sf_combined))
    
    sf_combined <- st_filter(sf_combined, poly_transform)
  }
  
  if(sum(st_is(st_geometry(sf_combined), "GEOMETRYCOLLECTION")) > 0) {
    message("Unpacking geometry collections")
    sf_combined <- st_collection_extract(sf_combined)
  }

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

get_osm <- function(proc_folder, raw_folder, match, source = "osm", type = "highway") {
  
  match <- data_lookup[data_lookup$country == match,][[source]]
  
  proc_files <- Sys.glob(paste0(proc_folder, "/*", match, "*.gpkg"))
  
  if (length(proc_files) == 0) {
    
    message("Processed GPKG not found. Converting from OSM PBF.")
    raw_filepath <- Sys.glob(paste0(raw_folder, "/*", match, "*.pbf"))
    dest_filepath <- paste0(proc_folder, "/osm_", match, ".gpkg")
    gdal_utils("vectortranslate", raw_filepath, dest_filepath)
    
    lines <- st_read(dest_filepath, layer = "lines")
    
  } else {
    
    lines <- st_read(proc_files[1], layer = "lines")
    
  }
  
  output <- filter(lines, !is.na(.data[[type]]))
    
  output
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

get_raster <- function(folder, match = NULL, names = NULL, ext = ".tif", source = "gee") {
  
  if (!is.null(match)) {
    match <- data_lookup[data_lookup$country == match,][[source]]
    file_paths <- Sys.glob(paste0(folder, "/*", match, "*", ext, "*"))
    
  } else {
    file_paths <- Sys.glob(paste0(folder, "/*", ext, "*"))
  }
  
  raster <- rast(file_paths)
  
  if (!is.null(names)) {
    names(raster) <- names
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

get_tiled_raster <- function(folder, match = NULL, layer = NULL, names = NULL, ext = ".tif", source = "gee") {
  
  if (!is.null(match)) {
    match <- data_lookup[data_lookup$country == match,][[source]]
    file_paths <- Sys.glob(paste0(folder, "/*", match, "*", ext, "*"))
  } else {
    file_paths <- Sys.glob(paste0(folder, "/*", ext, "*"))
  }
  
  # Create virtual raster
  vrt <- vrt(file_paths)
  
  # Subset and/or rename layers
  if(!is.null(layer)) {
    vrt <- vrt[[layer]]
  }
  
  if (!is.null(names)) {
    names(vrt) <- names
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
#' @param collection character. A STAC collection ID.
#' @param asset character. The asset or band name(s) to retrieve.
#' @param aoi sf object. Polygon(s) representing AOI.
#' @param filename character. Filename to save to disk.
#' @param folder character. Then name of the output folder in "data/raw/raster/"
#' 

get_stac_raster <- function(aoi = country, filename = COUNTRY, collection, asset, folder = NULL, names = NULL, tile = 1) {
  
  if(is.null(folder)) {
    folder <- collection
  }
  
  if (tile > 1) {
    aoi_grid <- aoi %>%
      st_make_grid(n = tile) %>%
      map(st_sfc, crs = st_crs(aoi))
    
    aoi <- aoi_grid[which(unlist(map(aoi_grid, st_intersects, y = aoi, sparse = FALSE)))]
    
    filename <- paste0(filename, "_", 1:length(aoi))
  
    filepaths <- paste0("data/raw/raster/", folder, "/", filename, ".tif")
  
    if (!all(file.exists(filepaths))) {
      message("Files not detected. Downloading from STAC...")
      
      get_stac_mappable <- function(aoi, output_filename, ...) {
        message("Getting data: ", output_filename)
        get_stac_data(aoi = aoi, output_filename = output_filename, ...)
      }
      
      filepaths <- map2(
        aoi, 
        filepaths, 
        get_stac_mappable,
        start_date = "2000-01-01",  end_date = "2024-01-01",
        asset_names = asset,
        collection = collection,
        stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/"
      ) %>% unlist()
    } else {
      
    message("All files detected. Loading from disk.")
    
    }
    
    raster <- vrt(filepaths)
    
  } else {
    filepath <- paste0("data/raw/raster/", folder, "/", filename, ".tif")
    
    if (!file.exists(filepath)) {
      message("File not detected. Downloading from STAC...")
      filepath <- get_stac_data(
        aoi = country,
        start_date = "2000-01-01", end_date = "2024-01-01",
        asset_names = asset,
        collection = collection,
        stac_source = "https://planetarycomputer.microsoft.com/api/stac/v1/",
        output_filename = filepath
      )
    } else {
      
      message("File detected. Loading from disk.")
    
    }
    
    raster <- rast(filepath)
    
  }
  
  if (!is.null(names)) {
    names(raster) <- names
  }  
  
  raster

}

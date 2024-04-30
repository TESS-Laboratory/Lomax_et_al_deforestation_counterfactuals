#' @title Get country
#' @description
#' A helper function to extract country boundary polygons from a global dataset
#' 
#' @usage get_country(country_name, crs, path)
#' 
#' @param country_name the name of the target country
#' @param crs (optional) a target CRS to transform the polygon
#' @param path
#' (optional) a custom filepath to the vector file containing country boundaries
#' 

get_country <- function(country_name, crs = NULL, path = NULL) {
  if(is.null(path)) {
    path <- "data/raw/vector/WB_countries/WB_countries_Admin0_10m.shp"
  }
  countries <- st_read(path)
  country <- dplyr::filter(countries, NAME_EN == country_name)
  
  if(!is.null(crs)) {
    country <- st_transform(country, crs = crs)
    message("Reprojected to ", crs)
  }
  
  country
}

#' @title Get vector object
#' @description
#' A function to load and filter vector objects for a country polygon
#' 
#' @usage get_vector(country_name, folder)
#' 
#' @param country_name character.
#' @param folder character. The name of the dataset folder.
#' 

## TOO SLOW FOR VERY LARGE FILES - NEED TO FIND A WAY TO FILTER THEM BEFORE
## READING IN! MAYBE A LOOKUP TABLE?

get_vector <- function(folder, country_name = NULL, country_poly = NULL) {
  dir_path <- common::dir.find("data", folder, up = 0, down = 5)
  dir_files <- Sys.glob(paste0(dir_path, "/*.shp"))
  
  if (!is.null(country_name)) {
    files <- dir_files[grepl(country_name, dir_files)]
  } else {
    files <- dir_files
  }
  
  sf_list <- map(files, st_read)
  
  if(!is.null(country_poly)) {
    
    poly_transform <- st_transform(country_poly, crs = st_crs(sf_list[[1]]))
    
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

#' @title Get prepared raster layer labeled by country 
#' @description 
#' A function to load a complete raster with a country name from a filepath
#' 
#' @usage get_raster(country_name, folder, crs, path, agg, layer, type)
#' 
#' @param country_name character. The name of the target country
#' @param folder character. The name of the dataset folder
#' @param layer character or numeric. A vector of names or layer numbers to
#' extract from the target raster

get_raster <- function(country_name, folder, layer = NULL) {
  dir_path <- common::dir.find("data", folder, up = 0, down = 5)
  dir_files <- list.files(dir_path)
  country_file <- dir_files[grepl(country_name, dir_files)][1]
  
  raster <- rast(paste0(dir_path, "/", country_file))
  
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

get_tiled_raster <- function(folder, layer = NULL, names = NULL) {
  # Find raster tiles
  dir_path <- common::dir.find("data", folder, up = 0, down = 5)
  tif_files <- Sys.glob(paste0(dir_path, "/*.tif"))
  
  # Create virtual raster
  vrt <- vrt(tif_files)
  
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
#' @param country character. Country name.
#' @param collection character. A STAC collection ID.
#' @param asset character. The asset or band name(s) to retrieve.
#' @param folder character. Then name of the output folder in "data/raw/raster/"
#' 

get_stac_raster <- function(country, collection, asset, folder = NULL, crs = NULL) {
  
  if(is.null(folder)) {
    folder <- collection
  }
  
  filepath <- paste0("data/raw/raster/", folder, "/", country, ".tif")

  if (!file.exists(filepath)) {
    message("File not detected. Downloading from STAC.")
    country_sf <- get_country(country, crs = crs)
    filepath <- rsi::get_stac_data(
      aoi = country_sf,
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


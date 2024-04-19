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

#' @title Get raster layer
#' @description 
#' A helper function to load a raster from a filepath
#' 
#' @usage get_raster(country_name, folder, crs, path, agg, layer, type)
#' 
#' @param country_name character. The name of the target country
#' @param folder character. The name of the dataset folder to find data
#' @param crs character. An optional target CRS to reproject the raster
#' @param agg positive integer. An optional factor by which to aggregate data to coarser resolution
#' @param layer numeric or character vector defining layers of the raster to retain
#' @param cat logical. Are the data layers categorical? If `TRUE`, reprojection
#' and aggregation will use nearest neighbour and mode, respectively, rather than
#' bilinear interpolation and mean.
#' 

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


#' @title Convert forest loss layer to annual layers
#' @description 
#' A function that takes a forest loss year layer as input (with pixel values
#' equal to the year of loss) and converts it into a series of binary raster
#' layers equal to the year of loss
# Script to combine and evaluate REDD project polygon data

library(readxl)
library(dplyr)
library(readr)
library(sf)
library(tmap)
library(purrr)

source("R/read_data.R")

sf_use_s2(FALSE)

verra <- read_xlsx("data/raw/csv/verra_afolu.xlsx")
renoster <- read_csv("data/raw/csv/carbon_projects_database_index.csv")

verra_redd_active <- verra %>%
  filter(projectStatus == "Registered") %>%
  filter(`KML File Exists?` == TRUE) %>%
  filter(grepl("REDD", afoluActivity))

renoster_ad <- filter(renoster, `Project Type` == "AD")

# Simplify and join into single table

cols_keep_verra <- c("projectNumber", "projectName", "country")
cols_keep_renoster <- c("ProjectID", "Project Name", "Country", "Continent")


# Load, clean and combine Renoster polygon data
africa_polys <- read_renoster("data/raw/vector/redd_polygons/africa.gpkg")
asia_polys <- read_renoster("data/raw/vector/redd_polygons/asia.gpkg")
south_america_polys <- read_renoster(
    "data/raw/vector/redd_polygons/south_america.gpkg",
    skip = c("VCS796", "VCS1496"))  # Unfixable geometries

renoster_polys <- bind_rows(africa_polys, asia_polys, south_america_polys) %>%
  inner_join(renoster_ad, by = "ProjectID")

st_write(renoster_polys, "data/processed/vector/renoster_polys.gpkg", delete_dsn = TRUE)

# Load, clean and combine Verra polygon data
verra_files <- Sys.glob("data/raw/vector/redd_polygons/kml_files/*.kml")
verra_ids <- stringr::str_extract(verra_files, "\\d+") %>% as.numeric()

read_kml <- function(filepath) {
  
  layer_names <- possibly(st_layers)(filepath)$name
  
  st_read2 <- possibly(st_read)
  
  if(!is.null(layer_names)) {
    layers <- map(layer_names, function(name) st_read2(filepath, layer = name, quiet = TRUE))
    layers <- bind_rows(layers)
  
    layers
  }
}

verra_geoms <- tibble(filename = verra_files, projectID = verra_ids) %>%
  mutate(sf = map(filename, read_kml)) %>%
  tidyr::unnest(sf) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  select(projectID, Name, geometry) %>%
  mutate(projectID = paste0("VCS", projectID)) %>%
  
  st_cast("MULTIPOLYGON")


verra_geoms <- map(verra_files, possibly(st_read, otherwise = NULL)) %>% bind_rows()

st_collection_extract(st_geometry(africa_polys)[2], "POLYGON")

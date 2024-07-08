# Script to combine and evaluate REDD project polygon data

library(readxl)
library(dplyr)
library(readr)
library(sf)
library(tmap)
library(purrr)

source("R/read_data.R")

sf_use_s2(FALSE)

renoster <- read_csv("data/raw/csv/carbon_projects_database_index.csv")

cols_keep_renoster <- c("ProjectID", "Project Name", "Country", "Continent")

renoster_ad <- renoster %>%
  filter(`Project Type` == "AD") %>%
  select(all_of(cols_keep_renoster))

# Load, clean and combine Renoster polygon data
africa_polys <- read_renoster(
  "data/raw/vector/redd_polygons/africa.gpkg",
  id_list = renoster_ad$ProjectID)
asia_polys <- read_renoster(
  "data/raw/vector/redd_polygons/asia.gpkg",
  id_list = renoster_ad$ProjectID)
south_america_polys <- read_renoster(
  "data/raw/vector/redd_polygons/south_america.gpkg",
  id_list = renoster_ad$ProjectID
)

renoster_polys <- bind_rows(africa_polys, asia_polys, south_america_polys) %>%
  inner_join(renoster_ad, by = "ProjectID") %>%
  st_make_valid()

st_write(renoster_polys, "data/processed/vector/renoster_polys.gpkg", delete_dsn = TRUE)
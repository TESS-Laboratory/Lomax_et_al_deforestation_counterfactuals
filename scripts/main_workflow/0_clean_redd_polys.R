# Script to combine and clean REDD project polygon data from Karnik et al.
# (2025) dataset (referred to below as the "renoster" dataset)

library(dplyr)
library(readr)
library(sf)

sf_use_s2(FALSE)

renoster <- read_csv("data/raw/csv/redd_projects/carbon_projects_database_index.csv")

cols_keep_renoster <- c("ProjectID", "Project Name", "Country", "Continent")

renoster_ad <- renoster %>%
  filter(`Project Type` == "AD") %>%
  select(all_of(cols_keep_renoster))

# Load, combine and clean Renoster polygon data
africa_polys <- st_read("data/raw/vector/redd_polygons/africa.gpkg") %>%
  filter(ProjectID %in% renoster_ad$ProjectID)
asia_polys <- st_read("data/raw/vector/redd_polygons/asia.gpkg") %>%
  filter(ProjectID %in% renoster_ad$ProjectID)
south_america_polys <- st_read("data/raw/vector/redd_polygons/south_america.gpkg") %>%
  filter(ProjectID %in% renoster_ad$ProjectID)

renoster_polys <- bind_rows(africa_polys, asia_polys, south_america_polys) %>%
  inner_join(renoster_ad, by = "ProjectID") %>%
  mutate(geom = purrr::map(geom, st_make_valid) %>%
           st_as_sfc(crs = st_crs(africa_polys)))

st_write(renoster_polys, "data/processed/vector/renoster_polys.gpkg", delete_dsn = TRUE)

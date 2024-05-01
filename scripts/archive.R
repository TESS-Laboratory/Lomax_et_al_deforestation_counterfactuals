## DO NOT RUN
## This is a file to store code snippets currently not used in the main workflow
## script.

#### Calculating distance from forest edge for each grid cell (2024-05-01)

## Add distance from forest edge to polygon properties --------
# Detect non-forest patches

tic()
non_forest_patches <- landscapemetrics::get_patches(
  fc_threshold$Y2000,
  class = 0,
  to_disk = TRUE
)[[1]][[1]]
toc()

names(non_forest_patches) <- "patch"

# tic()
# patch_area <- fc_threshold$Y2000 %>%
#   lsm_p_area() %>%
#   filter(class == 0) %>%
#   select(id, value)
#   
# toc()
# readr::write_rds(patch_area, here("data", "processed", "patch_area_Colombia.rds"))

# Reclassify nonforest patch values to area
patch_area_large <- readr::read_rds(here("data", "processed", "patch_area_Colombia.rds")) %>%
  filter(value >= FOREST_EDGE_AREA)

tic()
non_forest_patch_area <- classify(non_forest_patches, rcl = patch_area_large, others = NA)
toc()

# Calculate distance to forest edge for all forest pixels
tic()
dist_to_edge <- terra::distance(non_forest_patch_area)
toc()

names(dist_to_edge) <- "dist_to_edge"

# Extract mean distance to edge for all sample polygons
tic()
grid_fc_edge <- extract_grid(grid_sample, dist_to_edge)
toc()

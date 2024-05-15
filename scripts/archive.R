## DO NOT RUN
## This is a file to store code snippets currently not used in the main workflow
## script.

#### Calculating distance from forest edge for each grid cell (2024-05-01)

## Calculate mean distance from forest edge for each polygon --------
# Detect non-forest patches

tic()
non_forest_patches <- landscapemetrics::get_patches(
  fc_threshold$Y2000,  # Binary forest/non-forest raster
  class = 0,  # Non-forest class
  to_disk = TRUE  # Saves memory
)[[1]][[1]]
toc()

names(non_forest_patches) <- "patch"

tic()
patch_area <- fc_threshold$Y2000 %>%
  lsm_p_area() %>%
  filter(class == 0) %>%
  select(id, value)

toc()

# Reclassify nonforest patch values to area
patch_area_large <- filter(patch_area, value >= FOREST_EDGE_AREA)

tic()
non_forest_patch_area <- classify(non_forest_patches, rcl = patch_area_large, others = NA)
toc()

# Calculate distance to forest edge for all forest pixels
tic()
dist_to_edge <- terra::distance(non_forest_patch_area)
toc()

names(dist_to_edge) <- "dist_to_edge"

plot(dist_to_edge)

# Extract mean distance to edge for all sample polygons
tic()
grid_fc_edge <- extract_grid(grid_sample, dist_to_edge)
toc()

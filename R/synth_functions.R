## Functions for conducting synthetic control analysis on prepared data

#' @title Set treated
#' @description Adds a column to a data frame denoting the polygon ID that 
#' represents the "treated" unit.
#' 
#' @usage set_treated(df, treated_id)
#' 
#' @param df a data.frame containing polygon data
#' @param treated_id the ID of the treated unit

set_treated <- function(df, treated_id) {
  df$treated <- 0
  df$treated[df$ID == treated_id] <- 1
  
  df
}

#' @title Add distance to treated
#' @description Calculates the Euclidean distance between the centroid
#' of the treated polygon and that of all other polygons, then adds as a
#' new column in the data frame
#' 
#' @usage add_dist_to_treated(sf, treated_id)
#' 
#' @param sf sf object containing IDs and geoms
#' @param treated_id the ID of the treated unit

add_dist_to_treated <- function(sf, treated_id) {
  centroids <- st_centroid(sf)
  
  centroids_treated <- filter(centroids, ID == treated_id)
  
  distances <- st_distance(centroids, centroids_treated) %>%
    set_units("km") %>%
    drop_units
  
  df_with_dist <- sf %>%
    mutate(dist_to_treated = distances) %>%
    st_drop_geometry()
  
  df_with_dist
}

#' @title Add shared ecoregion fraction
#' @description Calculates the total shared ecoregion fraction between each
#' polygon and the target unit, then adds as a new column in the data frame.
#' Also adds a binary reflecting whether a polygon shares a biome with the
#' treated unit.
#' 
#' @usage add_shared_eco_frac(df, treated_id)
#' 
#' @param df a data.frame containing polygon data
#' @param treated_id the ID of the treated unit

add_shared_eco_frac <- function(df, treated_id) {
  eco_frac_treated <- df %>%
    filter(ID == treated_id) %>%
    unnest(eco_frac) %>%
    select(ECO_ID, eco_frac) %>%
    rename(eco_frac_treated = eco_frac)
  
  df_full <- unnest(df, eco_frac)
  
  eco_shared <- df_full %>%
    left_join(eco_frac_treated) %>%
    replace_na(list("eco_frac_treated" = 0)) %>%
    mutate(eco_frac_shared = pmin(eco_frac, eco_frac_treated)) %>%
    group_by(ID) %>%
    summarise(eco_frac_shared = sum(eco_frac_shared))
  
  df_eco_shared <- left_join(df, eco_shared)
  
  df_eco_shared
}


#' @title Add biome match
#' @description Adds a binary variable reflecting whether the unit shares a
#' biome with the treated/target unit.
#' 
#' @usage add_biome_match(df, treated_id)
#' 
#' @param df a data.frame containing polygon data
#' @param treated_id the ID of the treated unit

add_biome_match <- function(df, treated_id) {
  
  biomes_treated <- df %>%
    filter(ID == treated_id) %>%
    unnest(eco_frac) %>%
    select(BIOME_NUM) %>%
    unlist()
  
  biome_shared <- df %>%
    unnest(eco_frac) %>%
    mutate(shared_biome = BIOME_NUM %in% biomes_treated) %>%
    group_by(ID) %>%
    summarise(shared_biome = max(shared_biome))
  
  df_biome_shared <- left_join(df, biome_shared)
  
  df_biomed_shared
}

## Functions for preparing data and conducting synthetic control analysis

#' @title Wide to long
#' @description Converts wide-form data to long-form by year
#' 
#' @usage wide_to_long(df)
#' 
#' @param df wide-format data frame

wide_to_long <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = contains(".")) %>%
    separate_wider_delim(cols = "name", delim = ".", names = c("var", "year")) %>%
    mutate(year = as.numeric(year)) %>%
    pivot_wider(names_from = "var", values_from = "value")
  
  df_long
}


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
    drop_units() %>%
    as.vector()
  
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
#' @param drop logical. Should the "eco_frac" column be dropped?

add_shared_eco_frac <- function(df, treated_id, drop = FALSE) {
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
  
  if (drop == TRUE) {
    df_eco_shared <- select(df_eco_shared, -eco_frac)
  }
  
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
#' @param drop logical. Should the "eco_frac" column be dropped?

add_biome_match <- function(df, treated_id, drop = FALSE) {
  
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
  
  if (drop == TRUE) {
    df_biome_shared <- select(df_biome_shared, -eco_frac)
  }
  
  df_biome_shared
}

#' @title Get formula
#' @description Retrieves appropriate simulation formula for a numeric simulation
#' number (1-5)
#' 
#' @usage get_formula(sim)
#' 
#' @param sim numeric. The simulation number

get_formula <- function(sim, econ = TRUE) {
  if (sim == 1) {
    augsynth_formula <- "loss ~ treated"
  } else if (sim %in% c(2, 3)) {
    augsynth_formula <- "loss ~ treated | fc_start + buffer_loss + biomass + jurisdiction_loss +
                            precipitation + temperature_2m + elevation + slope + ag_suitability +
                            dist_to_road + dist_to_river + time_to_city + time_to_port + cropland +
                            protected_frac + pop_density + dist_to_edge"
  } else if (sim == 4) {
    augsynth_formula <- "loss ~ treated | fc_start + buffer_loss + biomass + jurisdiction_loss +
                            precipitation + temperature_2m + elevation + slope + ag_suitability +
                            dist_to_road + dist_to_river + time_to_city + time_to_port + cropland +
                            protected_frac + pop_density + dist_to_edge +
                            dist_to_treated"
  } else if (sim %in% c(5, 6)) {
    augsynth_formula <- "loss ~ treated | fc_start + buffer_loss + biomass + jurisdiction_loss +
                            precipitation + temperature_2m + elevation + slope + ag_suitability +
                            dist_to_road + dist_to_river + time_to_city + time_to_port + cropland +
                            protected_frac + pop_density + dist_to_edge +
                            dist_to_treated +
                            eco_frac_shared"
  } else {
    stop("Not a valid simulation. Must be an integer between 1 and 6.")
  }
  
  if (econ == TRUE & (sim != 1)) {
    augsynth_formula <- paste0(augsynth_formula, "+ grp_pc_usd_2015 + ag_grp_frac")
  }
  
  formula(augsynth_formula)
}

#' @title Prepare outcome data
#' @description Adjusts the pre-intervention outcome data according to the simulation
#' selected. For simulations that do not use the full set of pre-intervention outcomes,
#' these will be replaced by the mean value (for S2, S4 and S5) or by zeros (S3).
#' 
#' @usage prepare_outcomes(sim, outcome_ts, pt)
#' 
#' @param sim integer. The desired simulation number
#' @param outcome_ts numeric. A vector or column name containing the outcomes
#' @param pt numeric. The number of pre-treatment periods before project start

prepare_outcomes <- function(sim, outcome_ts, pt, cumulative) {
  
  if (cumulative == TRUE) {
    outcome_ts <- cumsum(outcome_ts)
  }
  
  if (sim == 3) {
    outcome_ts[1:pt] <- 0
  } else if (sim %in% c(2, 4, 5)) {
    outcome_ts[pt] <- ifelse(cumulative == TRUE, last(outcome_ts[1:pt]), mean(outcome_ts[1:pt]))
    outcome_ts[1:(pt-1)] <- 0
  }
  
  outcome_ts
}


#' @title Run synthetic control
#' @description Helper function to run augmented synthetic control algorithm
#' with specified default settings based on the simulation number provided
#' 
#' @usage run_synthetic_control(sim, data)
#' 
#' @param sim integer. The desired simulation number
#' @param data a data.frame containing input data
#' @param out_model character. The outcome model to use (see `augsynth` function)
#' @param econ logical. Does the dataset include economic variables?
#' @param cumulative logical. Fit synthetic control to cumulative (rather than annual)
#' forest loss outcomes?
#' @param out_model outcome model to impute control outcomes
#' @param ... Other arguments to pass to prepare_outcomes()

run_synthetic_control <- function(sim, match, data, econ = TRUE, cumulative = FALSE, out_model = "Ridge") {
  
  formula <- get_formula(sim, econ = econ)
  
  if (sim %in% c(5,6)) {
    data <- filter(data, shared_biome == 1)
    
    shared_eco_list <- filter(data, eco_frac_shared != 0)
    
    if (length(unique(shared_eco_list$ID)) == 1) {
      return(NA)
    }
  }
  
  data_prepared <- data %>%
    filter(year > (START_YEAR - match)) %>%
    group_by(ID) %>%
    mutate(loss = prepare_outcomes(sim, loss, pt = match, cumulative = cumulative)) %>%
    ungroup()
  
  augsynth_safe <- possibly(augsynth, otherwise = NA, quiet = FALSE)
  
  synth <- augsynth_safe(
    form = formula,
    unit = ID,
    time = year,
    data = data_prepared,
    progfunc = out_model,
    scm = TRUE
  )
  
  synth
  
}

#' @title Run synthetic control - MSCMT
#' @description Helper function to run augmented synthetic control algorithm
#' with specified default settings based on the simulation number provided
#' 
#' @usage run_synthetic_control_mscmt(sim, data)
#' 
#' @param sim integer. The desired simulation number
#' @param data a data.frame containing input data
#' @param out_model character. The outcome model to use (see `augsynth` function)
#' @param ... Other arguments to pass to prepare_outcomes()

run_synthetic_control_mscmt <- function(id, match, data) {
  
  match2 <- match - 1
  
  data_prepared <- data %>%
    filter(year > (START_YEAR - match)) %>%
    mutate(name = paste0("ID", as.character(ID))) %>%
    as.data.frame()
  
  data_list <- data_prepared %>%
    listFromLong(unit.variable = "ID", time.variable = "year", unit.names.variable = "name")
  
  controls_ids <- unique(data_prepared$name)[unique(data_prepared$ID) != id] %>% as.character()
  times_dep <- cbind("loss" = c(START_YEAR - match2, START_YEAR))
  times_pred <- cbind(
    "fc_start" = c(START_YEAR, START_YEAR),
    "buffer_loss" = c(START_YEAR - match2, START_YEAR),
    "biomass" = c(START_YEAR, START_YEAR),
    "jurisdiction_loss" = c(START_YEAR - match2, START_YEAR),
    "precipitation" = c(START_YEAR, START_YEAR),
    "temperature_2m" = c(START_YEAR, START_YEAR),
    "elevation" = c(START_YEAR, START_YEAR),
    "slope" = c(START_YEAR, START_YEAR),
    "ag_suitability" = c(START_YEAR, START_YEAR),
    "dist_to_road" = c(START_YEAR, START_YEAR),
    "dist_to_river" = c(START_YEAR, START_YEAR),
    "time_to_city" = c(START_YEAR, START_YEAR),
    "time_to_port" = c(START_YEAR, START_YEAR),
    "cropland" = c(START_YEAR, START_YEAR),
    "protected_frac" = c(START_YEAR, START_YEAR),
    "pop_density" = c(START_YEAR - match2, START_YEAR),
    "dist_to_edge" = c(START_YEAR, START_YEAR),
    "dist_to_treated" = c(START_YEAR, START_YEAR),
    "eco_frac_shared" = c(START_YEAR, START_YEAR)
  )
  
  if ("grp_pc_usd_2015" %in% colnames(data)) {
    times_pred <- cbind(times_pred,
                        "grp_pc_usd_2015" = c(START_YEAR - match2, START_YEAR),
                        "ag_grp_frac" = c(START_YEAR - match2, START_YEAR))
  }
  
  mscmt_safe <- possibly(mscmt, otherwise = NA, quiet = FALSE)
  
  synth <- mscmt_safe(
    data = data_list,
    treatment.identifier = paste0("ID", as.character(id)),
    controls.identifier = controls_ids,
    times.dep = times_dep,
    times.pred = times_pred,
    agg.fns = rep("mean", ncol(times_pred))
  )
  
  synth
  
}

#' @title Extract synth error
#' @description Extracts a time series or average value of the absolute error
#' (equivalent to the absolute average treatment effect on the treated) from
#' an augsynth object.
#' 
#' @usage extract_synth_error(synth, ts = FALSE)
#' 
#' @param synth an augsynth object
#' @param ts logical. Should the function return a time series of annual error
#' values rather than the default single mean absolute error in the
#' post-intervention period?

extract_synth_error <- function(synth, ts = FALSE) {
  
  if (is.na(synth)) return(NA)
  
  att_df <- summary(synth)$att
  
  intervention_year <- synth$t_int
  
  att_post_intervention <- filter(att_df, Time >= intervention_year)
  
  att_abs <- abs(att_post_intervention$Estimate)
  
  if (ts == TRUE) {
    att_abs
  } else {
    mean(att_abs)
  }
}

#' @title Extract synth
#' @description Unpacks a synth object into a data frame of observed and predicted
#' loss values
#' 
#' @usage extract_synth(synth, id)
#' 
#' @param synth an augsynth object
#' @param id numeric. The polygon ID of the treated unit for a given synth

extract_synth <- function(synth, id, truth_data, cumulative = FALSE) {
  
  start_year <- synth$data$time %>%
    as.numeric() %>%
    min()
  
  results_df <- grid_data %>%
    st_drop_geometry() %>%
    wide_to_long() %>%
    filter(ID == id & year >= start_year) %>%
    select(year, loss) %>%
    mutate(sc_loss = predict(synth))
  
  if (cumulative == TRUE) {
    results_df <- results_df %>%
      group_by()
      mutate(loss = cumsum(loss))
  }
  
  results_df
}

#' @title Extract synth variable importance
#' @description Extracts a data frame of variable importance scores for
#' an object created by the MSCMT::mscmt() function. 
#' 
#' @usage extract_synth_importance(synth, id)
#' 
#' @param synth an augsynth object
#' @param id numeric. The polygon ID of the treated unit for a given synth

extract_synth_importance <- function(synth) {
  
  if (all(is.na(synth))) return(NA)
   else {
     v <- synth$v %>%
       as.data.frame() %>%
       tibble::rownames_to_column("variable") %>%
       mutate(variable = str_extract(variable, "[^.]+"))
     v
   }
}


#' @title Join results csvs
#' @description Combines results csv files for each country into a single df
#' 
#' @usage join_results(country, folder, match)
#' 
#' @param folder character. The folder path
#' @param match character. The unique string identifying SC results

join_results <- function(country, folder) {

  filepaths <- paste0(folder, "/sc_results_", country, "_2016_60000",
                      c("", "_extended", "_S6"), ".csv")
  
  df_joined <- filepaths %>%
    map(read_csv) %>%
    bind_rows()
  
  write_csv(df_joined, paste0(folder, "/joined/sc_results_", country, "_2016_60000.csv"))
 
}


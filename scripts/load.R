reload <- function() {
  # load all the packages you need for the project
  message("Loading packages...")
  suppressPackageStartupMessages(
    source("scripts/packages.R")
  )
  # make the data dir if it doesn't exist
  if (!file.exists("data")) {
    message("Creating data directory...")
    dir.create("data")
  }
  # read the functions from R directory
  message("Loading functions...")
  function_files <- list.files("R", pattern = ".R$", full.names = TRUE)
  walk(function_files, source)
  message("Complete")
  
  # set global options
  terra::terraOptions(
    memfrac = 0.8,
    memmax = NA,
    tempdir = "data/processed/tmp"
  )
  
  options(scipen = 999)
  
  # Load data lookup table for efficient data I/O
  assign("data_lookup", read_csv("data/raw/csv/data_lookup.csv"), pos = ".GlobalEnv") 
}

reload()

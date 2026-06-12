# Synthetic controls underestimate deforestation in high-loss tropical forests

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R version](https://img.shields.io/badge/R-4.5.2-blue.svg)](https://www.r-project.org/)

**Authors:** Guy Lomax, Andrew Cunliffe, Hugh Graham, Christopher Philipson, Ted Feldpausch, Emily Doyle, Jessica Thomas, Edward Mitchard, and David Burslem.

**Paper DOI:** 

## Overview

This repository contains the code for a global assessment of the performance of the synthetic control method for estimating counterfactuals for avoided tropical deforestation REDD+ projects. We use a placebo approach to assess 2,141 simulated forest protection projects across nine tropical forest countries, comparing modelled counterfactuals with observed deforestation trends using the augmented synthetic control approach. Across all countries, we find a consistent negative bias in the synthetic control estimates in areas experiencing higher forest loss. This pattern is robust across covariate specifications, matching periods, spatial scales, and prior filtering of the donor pool, and is unpredictable using standard pre-intervention diagnostics. 

The experimental design for this study was pre-registered with the Open Science Framework (https://osf.io/w57a2). 

The codebase used for processing, analysing and visualising these data will archived with Zenodo at DOI:10.5281/zenodo.18671478. and also at https://github.com/TESS-Laboratory/Lomax_et_al_deforestation_counterfactuals. 

All input datasets used in this study are publicly available from the cited sources. Key output datasets generated during the analysis are also provided on Zenodo (https://doi.org/10.5281/zenodo.20641864) to facilitate reproduction of results and figures.

---

## Getting started

To get a local copy of the project up and running, follow the steps below.

1. **Clone the repository from GitHub**

*From RStudio Projects*
- In RStudio, click File > New Project
- Click "Version Control > Git"
- In the Repository URL box, paste `https://github.com/TESS-Laboratory/Lomax_et_al_deforestation_counterfactuals`
- Edit the folder location if desired.
- Click "Create Project".

*From the terminal*

- In an open terminal, navigate to the folder in which you would like to clone the project.
- Run the following command:
`git clone https://github.com/TESS-Laboratory/Lomax_et_al_deforestation_counterfactuals`

2. **Set up project environment using renv**
- Install the renv package using `install.packages("renv")`
- Active renv using `renv::activate()`
- Load the project environment from the lockfile with `renv::restore()`

3. **Create data directory structure**
- The structure of the data directory is shown in /data/data_directory_structure.txt

4. **Download and extract input datasets**
- The following datasets must be manually downloaded:
  - DOSE (economic covariates) - https://doi.org/10.5281/zenodo.7573249
  - Agricultural suitability - https://doi.org/10.5281/zenodo.5982577
  - Travel time to cities and ports - https://doi.org/10.6084/m9.figshare.7638134.v3
  - Population density - https://doi.org/10.5281/zenodo.11179644 (GlobPOP) with Cote d'Ivoire and DRC requiring additional gapfilling with WorldPop (https://hub.worldpop.org/geodata/listing?id=29)
  - Ecoregions - https://ecoregions.appspot.com/
  - Distance to rivers - https://doi.org/10.6084/m9.figshare.c.5052635
  - Distance to roads - GRIP (https://www.globio.info/download-grip-dataset) and country-level OSM (https://download.geofabrik.de/)
  - Existing REDD+ projects - global project details (https://www.reddprojectsdatabase.org/) and polygon boundaries for South America, Africa and Asia (https://doi.org/10.5281/zenodo.11459391)
- The following datasets can be retrieved and exported using the included Google Earth Engine script (https://code.earthengine.google.com/?scriptPath=users%2Fguylomax01%2FLomax_deforestation_counterfactuals%3Acountry_data_export):
  - Forest cover and loss (Tropical Moist Forests dataset)
  - Precipitation (CHIRPS)
  - Mean air temperature (ERA5-Land)
  - Global cropland extent (Potapov et al., 2021)
  - World database on protected areas
- Remaining required data is retrieved programmatically within the script _1_prepare_data.R_.

5. **Run the scripts in numeric order**
- To run the main analysis for a selected country, run the scripts in the folder scripts/main_workflow:
  - _0_clean_redd_polys.R_ - This must be run once to combine and fix geometries for existing REDD+ polygons.
  - _0_fix_population_data.R_ - This must be run for Cote d'Ivoire and Democratic Republic of the Congo to fill missing data in GlobPOP population density rasters.
  - _1_prepare_data.R_ - This script imports and processes covariate data and exports a vector of polygons with associated covariates for synthetic control modelling. This must be run once for each country and simulated project start year (1998 and 2016).
  - _2_fit_synthetic_controls.R_ - This script fits synthetic controls to each placebo project polygon in a sample dataset using the augsynth method. It is the main analysis script and must be run once for each country, simulated start year and polygon size.
  - _2x_covariate_weights.R_ - This fits standard (i.e., non-augsynth) synthetic controls to the baseline analysis case to extract covariate weights for each project, which are not retrievable from augsynth objects. It is not required for the core analysis but generates the weights in Supplementary Table 12.
  - _3_generate_plots.R_ - This generates output plots and CSV tables used in the manuscript, extended data and supplementary information.

6. **To run only the synthetic control analysis or generate plots, download and extract the processed analysis data at https://zenodo.org/records/20641865**
- _country_polygon_data.zip_ - Extract to data/processed/rds/country_polygon_data. This provides all polygon covariate data for running core analysis in _2_fit_synthetic_controls.R_.
- _synthetic_control_results.zip_ - Extract to results/sc_results. This contains CSVs of primary results for generating the majority of plots in _3_generate_plots.R_.
- _sc_fits.zip_ - Extract to data/processed/rds/sc_fits. This provides original fitted augsynth objects for extracting donor weights and generating Extended Data Figure 1 in _3_generate_plots.R_.



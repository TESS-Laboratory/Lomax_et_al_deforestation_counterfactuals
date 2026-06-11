# Script to extract 10th, 50th and 90th percentiles of REDD Project areas

library(readxl)
library(dplyr)
library(stringr)

# Read in spreadsheet

# redd <- read_xlsx("data/REDD_database_colname_fixed.xlsx", sheet = "01_Projects", na = "ND")
redd <- readxl::read_xlsx("data/raw/csv/redd_projects/ID-RECCO V5.0_20231201_final data_project.xlsx",
                               sheet = "01_Projects",
                               skip = 1,
                               na = "ND")[-1,] %>%
  mutate(across(everything(), parse_guess))

# Convert area to numeric (after removing whitespace and comma separators)
redd$area <- redd$area %>%
  str_trim() %>%
  str_replace_all(",", "") %>%
  as.numeric()  

head(redd)

# Define current target countries
countries <- c(
  "Colombia", "Brazil", "Bolivia",
  "Congo, the Democratic Republic of the", "Madagascar", "Côte d'Ivoire",
  "Indonesia", "Myanmar", "Malaysia"
)

# Filter dataset to REDD projects of interest
redd_focus <- redd %>%
  filter(Status_2022 != "TBC") %>%
  filter(str_detect(tolower(project_type), "redd")) %>%
  filter(str_detect(tolower(project_type), "jurisdictional", negate = TRUE)) %>%
  filter(`Multiple locations` %in% c("No", "Non")) %>%
  filter(`country name` %in% countries) %>%
  mutate(area = as.numeric(area))

# Calculate quantiles
quantile(redd_focus$area, c(0.1, 0.5, 0.9), na.rm = T)

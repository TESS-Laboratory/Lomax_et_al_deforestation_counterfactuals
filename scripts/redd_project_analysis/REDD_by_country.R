###### Script to create a table of REDD+ projects by country #####
#### set up environment ####

library(readr)
library(dplyr)
library(countrycode)

####load data ####
redd_proj <- readxl::read_xlsx("data/raw/csv/redd_projects/ID-RECCO V5.0_20231201_final data_project.xlsx",
                               sheet = "01_Projects",
                               skip = 1,
                               na = "ND")[-1,] %>%
  mutate(across(everything(), parse_guess))

# Convert area to numeric (after removing whitespace and comma separators)
redd_proj$area <- redd_proj$area %>%
  str_trim() %>%
  str_replace_all(",", "") %>%
  as.numeric()  
redd_proj <- redd_proj %>% 
  rename(country_name = "country name")

##### filter for relevant projects #####
filt_redd_proj <- redd_proj %>%
  filter(!(project_type %in% c("jurisdictional"))) %>%
           filter(grepl("REDD", project_type) & (Status_2022 %in% c("Ended", "Ongoing")))

##### summarise #######
# Summarize the data
summary_REDD <- filt_redd_proj %>%
  group_by(country_name) %>%
  summarize(
    total_area_ha = sum(area, na.rm = TRUE),
    number_of_projects = n()
  )

##### add country codes ####
## correct panama ##
summary_REDD$country_name <- gsub("Panamá", "Panama", summary_REDD$country_name)
## add UN country codes ###
summary_REDD$un.country.code<- countrycode(summary_REDD$country_name, origin = "country.name", destination = "un")
## add region from 7 Regions as defined in the World Bank Development Indicators ##
summary_REDD$region<- countrycode(summary_REDD$un.country.code, origin = "un", destination = "region")

#### simplify tables for document ####
write_csv(summary_REDD, "data/processed/csv/REDD_proj_by_country.csv")

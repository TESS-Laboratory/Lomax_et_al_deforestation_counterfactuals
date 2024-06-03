###### Script to create a table of REDD+ projects by country #####
#### set up environment ####
install.packages("tidyverse")
install.packages("countrycode")
library(tidyverse)
library(countrycode)

####load data ####
redd_proj<- read.csv("data/REDD_database_no_meta.csv", header = TRUE)
redd_proj$area<- as.numeric(redd_proj$area)  

##### filter for relevant projects #####
filt_redd_proj <- redd_proj %>%
  filter(!(project_type %in% c("jurisdictional"))) %>%
           filter(grepl("REDD", project_type) & (Status_2022 %in% c("Ended", "Ongoing")))


##### summarise #######
# Summarize the data
summary_REDD <- filt_redd_proj %>%
  group_by(country.name) %>%
  summarize(
    total_area_ha = sum(area, na.rm = TRUE),
    number_of_projects = n()
  )

##### add country codes ####
## correct panama ##
summary_REDD$country.name <- gsub("Panamá", "Panama", summary_REDD$country.name)
## add UN country codes ###
summary_REDD$un.country.code<- countrycode(summary_REDD$country.name, origin = "country.name", destination = "un")
## add region from 7 Regions as defined in the World Bank Development Indicators ##
summary_REDD$region<- countrycode(summary_REDD$un.country.code, origin = "un", destination = "region")

#### simplify tables for document ####
write.csv(summary_REDD, "data/REDD_proj_by_country.csv", row.names = FALSE)

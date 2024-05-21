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
           filter((project_type %in% c("REDD")) & (Status_2022 %in% c("Ended", "Ongoing")))


##### summarise #######
# Summarize the data
summary_REDD <- filt_redd_proj %>%
  group_by(country.name) %>%
  summarize(
    total_area = sum(area),
    #sum_B = sum(B),
    number_of_projects = n()
  )

##### add country codes ####
## correct panama ##
summary_REDD$country.name <- gsub("Panamá", "Panama", summary_REDD$country.name)
summary_REDD$country.code<- countrycode(summary_REDD$country.name, origin = "country.name", destination = "un")
summary_REDD$region<- countrycode(summary_REDD$country.code, origin = "un", destination = "region")

###### Script to create a table of REDD+ projects by country #####
#### set up environment ####
install.packages("tidyverse")
library(tidyverse)

####load data ####
redd_proj<- read.csv("data/REDD_database_no_meta.csv", header = TRUE)

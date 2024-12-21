## TB-Free Chuuk R code
## Screened Villages Map

#PACKAGES
library(tmap)
library(sf)
library(tmaptools)
library(tidyverse)
library(readxl)

# Load the shapefile
chuuk_lagoon_shapefile <- st_read("Map Files/5-08_Census_population/chk_census_population_2010.shp")

# Load the screened village data
sites <- read_excel("Map Files/villages_lat_long.xlsx") %>%
  rename(name = village, lon = village_longitude, lat = village_lat)

# Create a spatial points data frame
coordinates(sites) <- ~lon+lat

# Create the map with the background shapefile
tm_basemap("Esri.WorldStreetMap") +
  tm_shape(chuuk_lagoon_shapefile)  +
  tm_polygons() +
  tm_shape(sites) +
  tm_dots(col = "blue", size = 0.1, popup.vars = "name")
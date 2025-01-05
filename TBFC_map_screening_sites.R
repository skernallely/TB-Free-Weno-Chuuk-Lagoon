## TB-Free Chuuk R code
## Screened Villages Map

#PACKAGES
library(tmap)
library(sp)
library(sf)
library(tmaptools)
library(tidyverse)
library(readxl)

#set WD
setwd("~/PIHOA/TBFC/R Analysis/Weno_Chuuk_Lagoon")

#change default tmap display to View
tmap_mode("view")

#remove double checking of polygons
tmap_options(check.and.fix = TRUE)

# Load the shapefile
chuuk_lagoon_shapefile <- st_read("Map Files/5-08_Census_population/chk_census_population_2010.shp")

# Load the screened village data
sites <- read_excel("Map Files/villages_lat_long.xlsx") %>%
  rename(name = village, lon = village_longitude, lat = village_lat)

# Create a spatial points data frame
coordinates(sites) <- ~lon+lat

##make new box
bbox_existing <- st_bbox(chuuk_lagoon_shapefile) # current bounding box
bbox_new <- c(340000, 803000,
              380000, 829000)

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

# Create the map with the background shapefile
screening_map <- tm_basemap("Esri.WorldStreetMap") +
  tm_shape(chuuk_lagoon_shapefile, bbox=bbox_new)  +
  tm_polygons() +
  tm_shape(sites) +
  tm_dots(col = "blue", size = 0.1, popup.vars = "name")

# Save the map
tmap_save(screening_map,"Figures/Map_of_screening_sites_test.png")

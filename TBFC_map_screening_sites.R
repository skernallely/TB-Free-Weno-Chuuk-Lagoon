## TB-Free Chuuk R code
## Screened Villages Map

#PACKAGES
library(sp)
library(sf)
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggpubr)
library(knitr)

#set WD
setwd("~/PIHOA/TBFC/R Analysis/Weno_Chuuk_Lagoon")

# Load the shapefile
chuuk_lagoon_shapefile <- st_read(
  "Map Files/5-08_Census_population/chk_census_population_2010.shp") 

#set the CRS for the Chuuk file to EPSG 32656
chuuk_lagoon_shapefile  <- chuuk_lagoon_shapefile %>% st_set_crs(32656)

#reproject the shapefile with the added CRS
chuuk_lagoon_shapefile  <- chuuk_lagoon_shapefile %>% st_transform(32656)

# Load the screened village data
sites <- read_excel("Map Files/villages_lat_long.xlsx") %>%
  rename(name = village, lon = village_longitude, lat = village_lat)

#turn sites into a spatial object using the epsf coords
sites <- st_as_sf(sites, coords = c("x_epsg_32656", "y_epsg_32656"), 
                  crs = "32656", agr = "constant") %>% 
  st_set_crs(32656)

#make data frame for island labels with x and y locations and nine_yr_tb_rate
island_labels <- tribble(~name,~polygon_name,~x,~y,~nine_yr_tb_rate,
  "Weno","Weno Choniro", 373213.646333, 826836.802366, 229,
  "Tonoas","Tonoas", 375616.784445, 818323.576945, 118,
  "Fefan","Fefan", 369211.689383, 810848.182887, 1582,
  "Uman","Uman Fonuweisom", 377783.415214, 809536.754466,252,
  "Udot","Udot Fonuweisom", 358106.554831, 814163.841611, 119,
  "Tol","Tolensom", 350305.267296, 816201.203085, 332, 
  "Onei","Oneisom", 346096.959500, 818764.935701, 122,
  "Paata","Paata Tupunion", 340746.319681, 816617.134015, 251,
  "Polle","Pwene", 341689.996794, 809506.997999, 445)

tb_case_data <- merge(chuuk_lagoon_shapefile %>%
                            select(Name,geometry) %>%
                            rename(polygon_name=Name), 
                      island_labels, by = "polygon_name") %>%
  st_as_sf()
#add a nice base layer?
# map<-basemap_gglayer(chuuk_lagoon_shapefile, 
#         map_service = "maptiler", 
#         map_type = "streets",
#         map_token = "HiAUGHcoZzET0QlDlw2U")

#make the map
screening_sites_w_cases <-
  ggplot(data = chuuk_lagoon_shapefile) +
  geom_sf() +
  geom_sf(data = tb_case_data, aes(fill = nine_yr_tb_rate)) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       limits = c(0, 2000)) +
  geom_sf(data = sites, size = 2, 
          fill = "black", aes(color = "High tuberculosis incidence villages")) +
  scale_color_manual(values = c("High tuberculosis incidence villages" = "black")) +
  geom_text(data = island_labels, aes(x, y, label = name), size = 5) +
  coord_sf(xlim = c(332000,383000), ylim = c(802000, 833000), expand = FALSE) +
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Markers",
    fill = "Cases of tuberculosis per 100,000 people (2013-2021)",
    caption = "Geometries: Island Atlas of Micronesia, iRei, WERI, DECEM; Case data: Chuuk Department of Health and Social Affairs"
  )  +
  guides(
    colour = guide_legend(position = "inside"),
    fill   = guide_colourbar(position = "inside")
  )  +
  theme_map(12) +
  theme(
    legend.position.inside = c(0, .53),
    legend.background = element_blank(),
    plot.background=element_rect(fill="white"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )

#future map file path
map_file <- "Figures/Figure 1 - Map of screening sites with nine year case rate.png"

#Save map
ggsave(plot=screening_sites_w_cases,
       map_file,
       width = 1280, height = 1020, units = "px", scale = 2, dpi=300)

#crop the aspect ratio lines
# plot_crop(map_file)


##TMAP Version
library(tmap)
library(tmaptools)

#change default tmap display to View
# tmap_mode("plot")

#remove double checking of polygons
tmap_options(check.and.fix = TRUE)

# Load the shapefile
chuuk_lagoon_shapefile <- st_read(
  "Map Files/5-08_Census_population/chk_census_population_2010.shp") 

#set the CRS for the Chuuk file to EPSG 32656
chuuk_lagoon_shapefile  <- chuuk_lagoon_shapefile %>% st_set_crs(32656)

#reproject the shapefile with the added CRS
chuuk_lagoon_shapefile  <- chuuk_lagoon_shapefile %>% st_transform(32656)

# Load the screened village data
sites <- read_excel("Map Files/villages_lat_long.xlsx") %>%
  rename(name = village, lon = village_longitude, lat = village_lat)

# Create a spatial points data frame
coordinates(sites) <- ~lon+lat

##make new box
bbox_new <- st_bbox(c(xmin = 340000, xmax = 380000, ymax = 829000, ymin = 803000), 
                    crs = st_crs(32656))

#make the new bbox into an sf polygon
bbox_new <- bbox_new %>%  
  st_as_sfc() 

# Create the map with the background shapefile
tm_basemap("Esri.WorldStreetMap") +
  tm_shape(chuuk_lagoon_shapefile, bbox=bbox_new)  +
  tm_polygons() +
  tm_shape(sites) +
  tm_dots(col = "blue", size = 0.1, popup.vars = "name") +
  # Save the map
  tmap_save(screening_map,"Figures/Map_of_screening_sites_test.png")

#work on adding an insert for where Chuuk is
library(cowplot)



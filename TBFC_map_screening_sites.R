## TB-Free Chuuk R code
## Screened Villages Map

#PACKAGES
library(here)
library(sp)
library(sf)
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggpubr)
library(knitr)

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
  "Polle","Pwene", 341689.996794, 809506.997999, 445,
  "Eot", "Eot", NA, NA, 42,
  "Fonoton", "Fonoton", NA, NA, 29,
  "Fanapanges", "Fanapanges", NA, NA, 50,
  "Parem", "Parem", NA, NA,  97,
  "Piis-Panewu", "Piis-Panewu", NA, NA, 89,
  "Ramanum", "Ramanum", NA, NA, 128,
  "Siis", "Siis", NA, NA, 32,
  NA, NA, NA, NA, 0
  )

tb_case_data <- merge(chuuk_lagoon_shapefile %>%
                            select(Name,geometry) %>%
                            rename(polygon_name=Name), 
                      island_labels, by = "polygon_name") %>%
  mutate(rate_labels = case_when(nine_yr_tb_rate < 150 ~ "29 to <150",
                                 nine_yr_tb_rate < 300 ~ "150 to <300",
                                 nine_yr_tb_rate < 450 ~ "300 to <450",
                                 nine_yr_tb_rate >= 450 ~ "≥450"),
         rate_labels = factor(rate_labels, levels=c("29 to <150",
                                                    "150 to <300","300 to <450",
                                                    "≥450"))) %>%
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
  geom_sf(data = tb_case_data, aes(fill = rate_labels)) + # add island case data
  scale_fill_manual(limits = levels(tb_case_data$rate_labels),
                        values = c("#dae8ff", "#b6d3ff",
                                   "#6e85b7", "#273871")
    ) + 
  geom_sf(data = sites, size = 2, shape = 21, #add villages
          fill="black", aes(colour = "Villages with high incidence of tuberculosis screened in TB-Free Chuuk")) +
  scale_color_manual(values = c("Villages with high incidence of tuberculosis screened in TB-Free Chuuk" = "white")) +
  geom_text(data = island_labels, aes(x, y, label = name), size = 4) + #add island labels
  coord_sf(xlim = c(332000,383000), ylim = c(803000, 833000), expand = FALSE) + #get map size
  labs( #add basic labels
    x = "Longitude",
    y = "Latitude",
    color = "Markers",
    fill = "Cases of tuberculosis per 100,000 persons (2013–2021)",
    caption = paste0(
    "Abbreviations: TB - tuberculosis; Geometries: Chuuk Municipalities, Digital Atlas of Micronesia, 2020;",
    "\n",
    "Case data: Chuuk State Department of Health Services; Population data: Micronesia Population and Housing Census 2010")
  )  +
  guides( #arrange legends together
    colour = guide_legend(position = "inside", order = 1),
    fill   = guide_legend(position = "inside", ncol = 1, order = 2)
  )  +
  theme_map(10) + #change all map text size
  theme(
    plot.caption = element_text(hjust = 0), #move caption to left
    plot.background=element_rect(fill="white",linewidth = 0), #add white background
    plot.margin=grid::unit(c(0,0,0,0), "mm"), #remove padding

    legend.position.inside = c(0, .65), #move legends to inside 
    legend.background = element_blank(), #remove white background on legend
    legend.spacing.y = unit(0, "pt"), #make spacing between two legends smaller
    legend.key.size = unit(4, 'mm'), #adjust size of legends
    
    panel.background = element_blank(), #remove background of graph
    panel.border = element_blank(), #remove plot border
    panel.grid.major = element_blank(), #remove gridlines
    panel.grid.minor = element_blank(), #remove gridlines
  ) +
  labs(x = NULL, y = NULL)


#future map file path
map_file <- "Figures/Figure 1 - Map of screening sites with nine year case rate"

#Save map
ggsave(plot=screening_sites_w_cases,
       paste0(map_file,".tiff"),
       width = 90, height = 58, units = "mm", scale = 2, dpi=300)

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



# Libraries----
library(devtools)
library(dplyr)
library(data.table)
library(tidycensus) 
library(tigris)
library(lubridate)
library(sf)
library(rgdal)
library(shapefiles)
library(ggplot2)
library(ggmap) # for theme_nothing()
library(proj4) # for ptransform()
library(readr)


rm(list = ls());cat('\f');gc()

# Functions----

# Load Projection MODULE
devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/mapping/main/modules/MODULE_mapping_project_LonLat2LCC.R?raw=TRUE")

# Vars----
census.year   <- 2021
wd.shapefiles <- "C:/Users/TimBender/Documents/R/ncceh/mapping/shapefiles" # directory of shapefiles

# Setup----
setwd(wd.shapefiles)

# Data----
interim.cd            <- shapefiles::read.shapefile("Interim Congressional")
census.cd             <- tigris::congressional_districts(state = "NC", cb = T, year = census.year)
census.state          <- tigris::states(cb = T, year = census.year) %>% .[.$STUSPS == "NC",]
census.counties       <- tigris::counties(state = "NC", cb = T, year = census.year)
crosswalk_co_reg_dist <- read_csv("https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/dev/crosswalks/county_district_region_crosswalk.csv")

# county tidying----
census.counties2       <- right_join(census.counties, 
                                     crosswalk_co_reg_dist,
                                     by = c("NAME" = "County"))


couty_coc.regions <- census.counties2 %>%
  group_by(`Coc/Region`) %>%
  summarise()

county_districts <- census.counties2 %>%
  group_by(District) %>%
  summarise()

# Interim CD tidying----
new_interim.cd <- NULL
for(i in 1:length(interim.cd[["shp"]][["shp"]])){
  # check for multi-part CD errors
  if(interim.cd[["shp"]][["shp"]][[i]]$num.parts != 1){
    stop("update code to account for shape having multiple parts")
  }
  # get cd number
  temp.cd_number <- interim.cd[["shp"]][["shp"]][[i]]$record
  temp.cd_coords <- interim.cd[["shp"]][["shp"]][[i]][["points"]]
  
  # append output data
  new_interim.cd <- rbind(new_interim.cd, 
                          data.frame(I = i, 
                                     cd_number = temp.cd_number, 
                                     X = temp.cd_coords$X, 
                                     Y = temp.cd_coords$Y)) %>%
    as_tibble()
  # cleanup 
  rm(temp.cd_number, temp.cd_coords)
}

#  project interim CD to lonlat----
new_interim_cd.lonlat <- proj_LCC2LonLat(df_LCC = new_interim.cd[,c("X", "Y")])
new_interim.cd <- cbind(new_interim.cd, 
                        new_interim_cd.lonlat) %>% as_tibble()


# define_bbox for each layer ----
list.bbox                       <- list()
list.bbox[["state_bbox"]]       <- sf::st_bbox(census.state)
list.bbox[["counties_bbox"]]    <- NA
list.bbox[["census.counties2"]] <- NA


# Plot map----
# basemap <- ggplot() + 
#   # geom_sf(data = census.state, 
#   #         color = NA, fill = "white") + 
#   # geom_sf(data = county_districts, 
#   #         aes(fill = District)) +
#   # geom_sf(data = census.counties, 
#   #         color = "black", fill = NA)+
#   # geom_polygon(data = new_interim.cd, 
#   #              linewidth = 1,
#   #              color = "black", fill = NA,
#   #              aes(x = x, y = y, 
#   #                  group = factor(cd_number))) +
#   #theme_nothing()+
#   theme(legend.position = "bottom", 
#         legend.direction = "vertical", 
#         axis.text = element_blank(), 
#         axis.ticks = element_blank())+
#   scale_color_discrete(name = "Congressional District Boundaries")+
#   scale_fill_discrete(name = "Congressional District Boundaries")+
#   scale_x_continuous(name = NULL)+
#   scale_y_continuous(name = NULL)+
#   labs(title = "<title>", 
#        subtitle = "<subtitle>")


new_interim.cd_10 <- left_join(new_interim.cd, 
                               data.frame(cd_number2= c(13,1,3,4,5,6,7,8,9), 
                                          cd_number = c(5,1,8,9,10,11,12,13,14)))

for(i in unique(crosswalk_co_reg_dist$District)){
  
  #dnum <- sample(unique(new_interim.cd$cd_number), size = 1)
  
  plot <-  ggplot() + 
    geom_sf(data = county_districts[county_districts$District == i,],
            aes(fill = District)) +
    geom_sf(data = census.counties2[census.counties2$District == i,],
            color = "black", fill = NA)+
    # geom_polygon(data = new_interim.cd_10[new_interim.cd_10$cd_number2 == dnum,], 
    #              color = "black", fill = NA, linewidth = 1, 
    #              aes(x = x, y = y))+
    geom_polygon(data = new_interim.cd_10[paste("District",
                                                new_interim.cd_10$cd_number2,
                                                sep = " ") == i,],
                 linewidth = 1,
                 color = "black", fill = NA,
                 aes(x = x, y = y,
                     group = factor(cd_number)))+
    theme(legend.position = "bottom", 
          legend.direction = "vertical", 
          axis.text = element_blank(), 
          axis.ticks = element_blank())+
    scale_color_discrete(name = "Congressional District Boundaries")+
    scale_fill_discrete(name = "Congressional District Boundaries")+
    scale_x_continuous(name = NULL)+
    scale_y_continuous(name = NULL)+
    labs(title = "<title>", 
         subtitle = "<subtitle>")
  
  print(plot)
  #Sys.sleep(3)
}




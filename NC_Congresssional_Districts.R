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
get_bbox <- function(x1, y1){
  c(xmin = min(x1), 
    ymin = min(y1), 
    xmax = max(x1), 
    ymax = max(y1))
}
merge2bbox <- function(bb1, bb2){
  data.frame(new_xmin = min(c(bb1[["xmin"]], 
                              bb2[["xmin"]])), 
             new_ymin = min(c(bb1[["ymin"]], 
                              bb2[["ymin"]])), 
             new_xmax = max(c(bb1[["xmax"]],
                              bb2[["xmax"]])), 
             new_ymax = max(c(bb1[["ymax"]], 
                              bb2[["ymax"]])))
}

# Load Projection MODULE
devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/mapping/main/modules/MODULE_mapping_project_LonLat2LCC.R?raw=TRUE")

# Vars----
census.year   <- 2021
wd.shapefiles <- "C:/Users/TimBender/Documents/R/ncceh/mapping/shapefiles" # directory of shapefiles

# Setup----
setwd(wd.shapefiles)

# Data----
ncleg_interim.cd      <- shapefiles::read.shapefile("Interim Congressional")
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

ncceh.county_districts <- census.counties2 %>%
  group_by(District) %>%
  summarise()

# Interim CD tidying----
new_ncleg_interim.cd <- NULL
for(i in 1:length(ncleg_interim.cd[["shp"]][["shp"]])){
  # check for multi-part CD errors
  if(ncleg_interim.cd[["shp"]][["shp"]][[i]]$num.parts != 1){
    stop("update code to account for shape having multiple parts")
  }
  # get cd number
  temp.cd_number <- ncleg_interim.cd[["shp"]][["shp"]][[i]]$record
  temp.cd_coords <- ncleg_interim.cd[["shp"]][["shp"]][[i]][["points"]]
  
  # append output data
  new_ncleg_interim.cd <- rbind(new_ncleg_interim.cd, 
                                data.frame(I = i, 
                                           cd_number = temp.cd_number, 
                                           X = temp.cd_coords$X, 
                                           Y = temp.cd_coords$Y)) %>%
    as_tibble()
  # cleanup 
  rm(temp.cd_number, temp.cd_coords)
}

#  project interim CD to lonlat----
new_ncleg_interim_cd.lonlat <- proj_LCC2LonLat(df_LCC = new_ncleg_interim.cd[,c("X", "Y")])
new_ncleg_interim.cd <- cbind(new_ncleg_interim.cd, 
                              new_ncleg_interim_cd.lonlat) %>% as_tibble()

# manual assignment of district names to polygons 
new_interim.cd_10 <- left_join(new_ncleg_interim.cd, 
                               data.frame(cd_number2= c(13,1,3,4,5,6,7,8,9), 
                                          cd_number = c(5,1,8,9,10,11,12,13,14)))

# define_bbox for each layer ----
list.bbox                       <- list()
list.bbox[["census.state"]]     <- sf::st_bbox(census.state)
list.bbox[["census.counties2"]] <- sf::st_bbox(census.counties2)

list.bbox[["CD.ncleg.D01"]]     <- get_bbox(x1 = new_interim.cd_10$x[new_interim.cd_10$cd_number2 == 1 & !is.na(new_interim.cd_10$cd_number2)], 
                                            y1 = new_interim.cd_10$y[new_interim.cd_10$cd_number2 == 1 & !is.na(new_interim.cd_10$cd_number2)])
list.bbox[["CD.ncleg.D03"]]     <- get_bbox(x1 = new_interim.cd_10$x[new_interim.cd_10$cd_number2 == "District 3"], 
                                            y1 = new_interim.cd_10$y[new_interim.cd_10$cd_number2 == "District 3"])
list.bbox[["CD.ncleg.D04"]]     <- get_bbox(x1 = new_interim.cd_10$x[new_interim.cd_10$cd_number2 == "District 4"], 
                                            y1 = new_interim.cd_10$y[new_interim.cd_10$cd_number2 == "District 4"])
list.bbox[["CD.ncleg.D05"]]     <- get_bbox(x1 = new_interim.cd_10$x[new_interim.cd_10$cd_number2 == "District 5"], 
                                            y1 = new_interim.cd_10$y[new_interim.cd_10$cd_number2 == "District 5"])
list.bbox[["CD.ncleg.D06"]]     <- get_bbox(x1 = new_interim.cd_10$x[new_interim.cd_10$cd_number2 == "District 6"], 
                                            y1 = new_interim.cd_10$y[new_interim.cd_10$cd_number2 == "District 6"])
list.bbox[["CD.ncleg.D07"]]     <- get_bbox(x1 = new_interim.cd_10$x[new_interim.cd_10$cd_number2 == "District 7"], 
                                            y1 = new_interim.cd_10$y[new_interim.cd_10$cd_number2 == "District 7"])
list.bbox[["CD.ncleg.D08"]]     <- get_bbox(x1 = new_interim.cd_10$x[new_interim.cd_10$cd_number2 == "District 8"], 
                                            y1 = new_interim.cd_10$y[new_interim.cd_10$cd_number2 == "District 8"])
list.bbox[["CD.ncleg.D09"]]     <- get_bbox(x1 = new_interim.cd_10$x[new_interim.cd_10$cd_number2 == "District 9"], 
                                            y1 = new_interim.cd_10$y[new_interim.cd_10$cd_number2 == "District 9"])
list.bbox[["CD.ncleg.D10"]]     <- get_bbox(x1 = new_interim.cd_10$x[new_interim.cd_10$cd_number2 == "District 10"], 
                                            y1 = new_interim.cd_10$y[new_interim.cd_10$cd_number2 == "District 10"])
list.bbox[["CD.ncleg.D11"]]     <- get_bbox(x1 = new_interim.cd_10$x[new_interim.cd_10$cd_number2 == "District 11"], 
                                            y1 = new_interim.cd_10$y[new_interim.cd_10$cd_number2 == "District 11"])
list.bbox[["CD.ncleg.D13"]]     <- get_bbox(x1 = new_interim.cd_10$x[new_interim.cd_10$cd_number2 == "District 13"], 
                                            y1 = new_interim.cd_10$y[new_interim.cd_10$cd_number2 == "District 13"])

list.bbox[["CD.ncceh.D01"]]     <- sf::st_bbox(ncceh.county_districts[ncceh.county_districts$District == "District 1",])  
list.bbox[["CD.ncceh.D03"]]     <- sf::st_bbox(ncceh.county_districts[ncceh.county_districts$District == "District 3",])
list.bbox[["CD.ncceh.D04"]]     <- sf::st_bbox(ncceh.county_districts[ncceh.county_districts$District == "District 4",])
list.bbox[["CD.ncceh.D05"]]     <- sf::st_bbox(ncceh.county_districts[ncceh.county_districts$District == "District 5",])
list.bbox[["CD.ncceh.D06"]]     <- sf::st_bbox(ncceh.county_districts[ncceh.county_districts$District == "District 6",])
list.bbox[["CD.ncceh.D07"]]     <- sf::st_bbox(ncceh.county_districts[ncceh.county_districts$District == "District 7",])
list.bbox[["CD.ncceh.D08"]]     <- sf::st_bbox(ncceh.county_districts[ncceh.county_districts$District == "District 8",])
list.bbox[["CD.ncceh.D09"]]     <- sf::st_bbox(ncceh.county_districts[ncceh.county_districts$District == "District 9",])
list.bbox[["CD.ncceh.D10"]]     <- sf::st_bbox(ncceh.county_districts[ncceh.county_districts$District == "District 10",])
list.bbox[["CD.ncceh.D11"]]     <- sf::st_bbox(ncceh.county_districts[ncceh.county_districts$District == "District 11",])
list.bbox[["CD.ncceh.D13"]]     <- sf::st_bbox(ncceh.county_districts[ncceh.county_districts$District == "District 13",])

list.bbox

list.bbox[["CD01_final"]]




merge2bbox(bb1 = list.bbox[["CD.ncceh.D01"]], 
           bb2 = list.bbox[["CD.ncleg.D01"]])

ggplot() + 
  geom_polygon(data = list.bbox[["CD.ncceh.D01"]])


  # Plot map----

for(i in unique(crosswalk_co_reg_dist$District)){
   
  plot <-  ggplot() + 
    geom_sf(data = ncceh.county_districts[ncceh.county_districts$District == i,],
            aes(fill = District)) +
    geom_sf(data = census.counties2[census.counties2$District == i,],
            color = "black", fill = NA)+
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




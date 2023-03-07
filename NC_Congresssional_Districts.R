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
#http://web.archive.org/web/20070828201450/http://www.remotesensing.org/geotiff/proj_list/lambert_conic_conformal_2sp.html
#https://github.com/benda18/R-Examples/blob/master/nearestneighbor.R
 
rm(list=ls());cat('\f');gc()
setwd("C:/Users/TimBender/Documents/R/ncceh/mapping/shapefiles")

LongLatToUTM<-function(x,y,zone){
  require(sp)
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
#https://www.ncleg.gov/Redistricting

nc.cos <- tigris::counties("NC", year = 2020)
nc.cgd_2022 <- shapefiles::read.shapefile("Interim Congressional")
nc.plc.2020 <- tigris::places(state = "NC") %>%
  sf::st_centroid()
nc.cgd_2020 <- tigris::congressional_districts("NC", year = 2020)
nc.bnd_2020 <- tigris::states()
nc.bnd_2020 <- nc.bnd_2020[nc.bnd_2020$NAME == "North Carolina",]

nc.plc.2020$geometry %>% class()

sf::st_centroid(x = nc.plc.2020)

nccd22 <- NULL
for(i in 1:length(nc.cgd_2022[["shp"]][["shp"]])){
  nccd22 <- rbind(nccd22, 
                  data.frame(lon = nc.cgd_2022[["shp"]][["shp"]][[i]]$points$X, 
                             lat = nc.cgd_2022[["shp"]][["shp"]][[i]]$points$Y, 
                             district = nc.cgd_2022[["shp"]][["shp"]][[i]]$record))
  
  
  
}

# major cities
major.cities <- nc.plc.2020 %>%
  sf::as_Spatial()

major.cities

major.cities2 <- major.cities 

#create a list of xy coordinates from hfa2 to transform
trans_coords <- major.cities2[,c("INTPTLAT", "INTPTLON")] %>%
  as.data.frame() %>%
  .[,c(2,1)]
trans_coords$INTPTLAT <- trans_coords$INTPTLAT %>% as.numeric()
trans_coords$INTPTLON <- trans_coords$INTPTLON %>% as.numeric()

#coordinate transformation via projection
pc <- ptransform(trans_coords/180*pi,
                 '+proj=latlong +ellps=sphere',
                 '+proj=lcc +lat_1=34.33333333333334 +lat_2=36.16666666666666 +lat_0=33.75 +lon_0=-79.0 +x_0=609601.22 +y_0=0')

major.cities2$x2 <- pc$x
major.cities2$y2 <- pc$y

major.cities2 <- as.data.frame(major.cities2)

major.cities2 %>%
  group_by(LSAD) %>%
  summarise(median(ALAND), 
            sd(ALAND))

# convert state boundary to utm
state.lonlat <- nc.bnd_2020 %>%
  sf::as_Spatial()

state.lonlat2 <- NULL
for(i in 1:length(state.lonlat@polygons)){
  temp.lonlat <- state.lonlat@polygons[[i]]@Polygons[[1]]@coords %>% 
    as.data.frame()
  colnames(temp.lonlat) <- c("x", "y")
  temp.lonlat$group <- i
  
  state.lonlat2 <- rbind(state.lonlat2, 
                            temp.lonlat)
  rm(temp.lonlat)
}

#create a list of xy coordinates from hfa2 to transform
trans_coords <- state.lonlat2[,1:2]

#coordinate transformation via projection
pc <- ptransform(trans_coords/180*pi, 
                 '+proj=latlong +ellps=sphere',
                 '+proj=lcc +lat_1=34.33333333333334 +lat_2=36.16666666666666 +lat_0=33.75 +lon_0=-79.0 +x_0=609601.22 +y_0=0')

state.lonlat2$x2 <- pc$x
state.lonlat2$y2 <- pc$y


# convert counties to UTM
counties.all <- nc.cos %>% .$geometry %>% sf::as_Spatial()
 
counties.lonlat <- nc.cos %>%
  .[.$NAME %in% c("Alexander", "Catawba",
                  "Iredell", "Rutherford", "Burke", 
                  "Lincoln", "Cleveland", "Gaston", "Caldwell"),] %>%
  .$geometry %>% sf::as_Spatial()

counties.all2 <- NULL
for(i in 1:length(counties.all@polygons)){
  temp.lonlat <- counties.all@polygons[[i]]@Polygons[[1]]@coords %>% 
    as.data.frame()
  colnames(temp.lonlat) <- c("x", "y")
  temp.lonlat$group <- i
  
  counties.all2 <- rbind(counties.all2, 
                            temp.lonlat)
  rm(temp.lonlat)
}
#create a list of xy coordinates from hfa2 to transform
trans_coords <- counties.all2[,1:2]

#coordinate transformation via projection
pc <- ptransform(trans_coords/180*pi, 
                 '+proj=latlong +ellps=sphere',
                 '+proj=lcc +lat_1=34.33333333333334 +lat_2=36.16666666666666 +lat_0=33.75 +lon_0=-79.0 +x_0=609601.22 +y_0=0')

counties.all2$x2 <- pc$x
counties.all2$y2 <- pc$y



counties.lonlat2 <- NULL
for(i in 1:length(counties.lonlat@polygons)){
  temp.lonlat <- counties.lonlat@polygons[[i]]@Polygons[[1]]@coords %>% 
    as.data.frame()
  colnames(temp.lonlat) <- c("x", "y")
  temp.lonlat$group <- i
  
  counties.lonlat2 <- rbind(counties.lonlat2, 
                            temp.lonlat)
  rm(temp.lonlat)
}


#create a list of xy coordinates from hfa2 to transform
trans_coords <- counties.lonlat2[,1:2]

#coordinate transformation via projection
pc <- ptransform(trans_coords/180*pi, 
                 '+proj=latlong +ellps=sphere',
                 '+proj=lcc +lat_1=34.33333333333334 +lat_2=36.16666666666666 +lat_0=33.75 +lon_0=-79.0 +x_0=609601.22 +y_0=0')

counties.lonlat2$x2 <- pc$x
counties.lonlat2$y2 <- pc$y

counties.lonlat2$County <- NA
counties.lonlat2$County[counties.lonlat2$group == 1] <- "Lincoln"
counties.lonlat2$County[counties.lonlat2$group == 2] <- "Caldwell"
counties.lonlat2$County[counties.lonlat2$group == 6] <- "Irdell"
counties.lonlat2$County[counties.lonlat2$group == 5] <- "Rutherford"
counties.lonlat2$County[counties.lonlat2$group == 3] <- "Alexander"
counties.lonlat2$County[counties.lonlat2$group == 4] <- "Cleveland"
counties.lonlat2$County[counties.lonlat2$group == 7] <- "Catawba"
counties.lonlat2$County[counties.lonlat2$group == 9] <- "Burke"
counties.lonlat2$County[counties.lonlat2$group == 8] <- "Gaston"

plot3 <- ggplot() + 
  coord_fixed()+
  # geom_path(data = counties.all2, 
  #           aes(x = x2, y = y2, group = group), 
  #           color = "grey") +
  geom_path(data = state.lonlat2, 
            aes(x = x2, y = y2, group = group), 
            size = 1, 
            color = "#2F747E")+
  geom_polygon(data = nccd22[nccd22$district %in% 2,], 
               aes(x = lon, y = lat, group = district), 
               fill = "#2F747E", color = NA, 
               alpha = 0.5)+
  geom_path(data = counties.lonlat2[!counties.lonlat2$County %in% c("Gaston", "Lincoln", 
                                                                    "Cleveland", "Caldwell"),], 
            aes(x = x2, y = y2, group = group), size = 0.7, 
            color = "#2F747E")+
  theme(legend.position = "bottom")+
  theme_nothing()+
  geom_point(data = major.cities2[major.cities2$NAME %in% 
                                    c("Durham", "Raleigh", "Chapel Hill", "Greensboro", "Charlotte", 
                                      "Fayetteville", "Asheville", 
                                      "Winston-Salem", "High Point", 
                                      "Wilmington", "Goldsboro", 
                                      "New Bern", "Fayetteville", 
                                      "Rocky Mount", "Rockingham", "Salisbury", 
                                      "Statesville", "Hickory", 
                                      "Shelby", "Hendersonville", "Morganton"),], 
             aes(x = x2, y = y2)) 


plot3



ggsave(filename = "NC_CongressionalDist_2022.eps", 
       plot = plot3, device = "eps")

ggsave(filename = "NC_CongressionalDist_2022.pdf", 
       plot = plot3, device = "pdf")

ggsave(filename = "NC_CongressionalDist_2022.ps", 
       plot = plot3, device = "ps")

# I("PROJCS[\"NAD_1983_StatePlane_North_Carolina_FIPS_3200\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Lambert_Conformal_Conic\"],PARAMETER[\"False_Easting\",609601.22],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-79.0],PARAMETER[\"Standard_Parallel_1\",34.33333333333334],PARAMETER[\"Standard_Parallel_2\",36.16666666666666],PARAMETER[\"Latitude_Of_Origin\",33.75],UNIT[\"Meter\",1.0]]") %>%
#   strsplit(.,",")
# 
# 
# ggplot()+
#   geom_sf(data = nc.bnd_2020[nc.bnd_2020$NAME == "North Carolina",])+
#   geom_sf(data = nc.cgd_2020[nc.cgd_2020$CD116FP == "10",], fill = "cyan") + 
#   geom_sf(data = nc.cos[nc.cos$NAME %in% 
#                           c("Alexander", "Catawba",
#                             "Iredell", "Rutherford", "Burke"),], 
#           fill = NA, color = "red", size = 1) 
# 
# # older----
# nccd22 <- NULL
# for(i in 1:length(nc.cgd_2022[["shp"]][["shp"]])){
#   nccd22 <- rbind(nccd22, 
#         data.frame(lon = nc.cgd_2022[["shp"]][["shp"]][[i]]$points$X, 
#              lat = nc.cgd_2022[["shp"]][["shp"]][[i]]$points$Y, 
#              district = nc.cgd_2022[["shp"]][["shp"]][[i]]$record))
#   
#   
#   
# }
# 
# 
# 
# #project(nc.cgd_2022, "+proj=NAD83")
# 
# 
# 
# 
# library(ggplot2) # for plotting
# library(cowplot) # for ggsave
# 
# 
# #plot1 <- 
# plot1 <- ggplot() + 
#   geom_polygon(data = nccd22, 
#                aes(x = lon, y = lat, group = district), 
#                fill = "white", color = "black")+
#   coord_quickmap() +
#   theme_nothing() +
#   geom_polygon(data = LongLatToUTM(x = as.data.frame(nc.cos$geometry[[1]][[1]][[1]])$V1, 
#                                    y = as.data.frame(nc.cos$geometry[[1]][[1]][[1]])$V2, 
#                                    zone = 13), 
#                aes(x = X, y = Y))
# plot1
# ggsave(filename = "NC_CongressionalDist_2022.pdf", plot = plot1, 
#        device = "pdf")
# 
# ?ggsave
# temp.chart <- data.frame(age = c("0-5", "6-17", "18-24", 
#                   "25-54", "55-64", "65+"), 
#            experiencing_homelessness = c(105,117,95,618,140,53)) 
# 
# temp.chart$age_f <- factor(temp.chart$age, 
#                            levels = c("0-5", "6-17", "18-24", 
#                                       "25-54", "55-64", "65+"))
# 
# ggplot() + 
#   geom_col(data = temp.chart, 
#            aes(x = experiencing_homelessness, y = age_f))

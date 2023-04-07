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

rm(list=ls());cat('\f');gc()

# Vars----
counties.filter.vector <- NA 

census.year <- 2021
wd.shapefiles <- "C:/Users/TimBender/Documents/R/ncceh/mapping/shapefiles"
wd.map_outputs <- NA
urls.df <- data.frame(short_name = c("Coordinate Projection definitions for proj4 package", 
                                     "GitHub Mapping Example in R", 
                                     "Official NC Legislative Redistricting Website", 
                                     "GitHub link for this Script"), 
                      long_name  = c("how to project to lon/lat using proj4 library in R, specifically coordinate projection type Lambert Conic Conformal (2SP)", 
                                     "Nearest Neighbor exercise from Tim's personal github", 
                                     "Most up-to-date information from the State of NC on current legal challenges and current congressional districting boundaries", 
                                     "From Tim's NCCEH github"), 
                      url        = c("http://web.archive.org/web/20070828201450/http://www.remotesensing.org/geotiff/proj_list/lambert_conic_conformal_2sp.html",
                                     "https://github.com/benda18/R-Examples/blob/master/nearestneighbor.R", 
                                     "https://www.ncleg.gov/Redistricting", 
                                     "https://github.com/timbender-ncceh/mapping/blob/main/NC_Congresssional_Districts.R")) %>% as_tibble()



# set wds----
setwd(wd.shapefiles)

# Functions----
LongLatToUTM<-function(x,y,zone){
  require(sp)
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

# Data: Congressional District features----
nc.cgd_interim <- shapefiles::read.shapefile("Interim Congressional")
nc.cgd_census  <- tigris::congressional_districts(state = "NC", cb = T, year = census.year)

# Data: Base Mapping features-----
nc.counties <- tigris::counties(state = "NC", 
                                cb = T, 
                                year = census.year)

nc.places   <- tigris::places(state = "NC", 
                              cb = F, 
                              year = census.year) %>% sf::st_centroid()

nc.state    <- tigris::states(cb = T, 
                              year = census.year) %>%
  .[.$NAME == "North Carolina",]


# MVP----
ggplot() + 
  geom_sf(data = nc.state)+
  geom_sf(data = nc.counties)+
  geom_sf(data = nc.places)+
  labs(title = "MVP_map")




# TIDY ----
# Congressional Districts----
nc.cgd_interim_df <- NULL
for(i in 1:length(nc.cgd_interim[["shp"]][["shp"]])){
  nc.cgd_interim_df <- rbind(nc.cgd_interim_df, 
                  data.frame(point_order = as.numeric(rownames(nc.cgd_interim[["shp"]][["shp"]][[i]]$points)), 
                             X           = nc.cgd_interim[["shp"]][["shp"]][[i]]$points$X, 
                             Y           = nc.cgd_interim[["shp"]][["shp"]][[i]]$points$Y, 
                             district    = nc.cgd_interim[["shp"]][["shp"]][[i]]$record))
  
  
  
}

nc.cgd_interim_df <- as_tibble(nc.cgd_interim_df)
rm(nc.cgd_interim)

ggplot() + 
  geom_polygon(data = nc.cgd_interim_df, 
               aes(x = X, y = Y, fill = factor(district))) +
  coord_quickmap()


# Places (major cities)----
major.cities <- nc.places %>%
  sf::as_Spatial()
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

# State----
# convert state boundary to utm
state.lonlat <- nc.state %>%
  sf::as_Spatial()

rm(nc.state)

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
rm(state.lonlat)

#create a list of xy coordinates from hfa2 to transform
trans_coords <- state.lonlat2[,1:2]

#coordinate transformation via projection
pc <- ptransform(trans_coords/180*pi, 
                 '+proj=latlong +ellps=sphere',
                 '+proj=lcc +lat_1=34.33333333333334 +lat_2=36.16666666666666 +lat_0=33.75 +lon_0=-79.0 +x_0=609601.22 +y_0=0')

state.lonlat2$x2 <- pc$x
state.lonlat2$y2 <- pc$y

state.lonlat2 <- state.lonlat2 %>% as_tibble()

# Counties-----
# convert counties to UTM
# counties.all       <- nc.counties %>% .$geometry %>% sf::as_Spatial()
# counties.all$NAME <- nc.counties$NAME
counties.all <- nc.counties %>% sf::as_Spatial()

# Filter counties if applicable
counties.lonlat <- nc.counties

# if(!is.na(counties.filter.vector)){
#   counties.lonlat <- counties.lonlat %>%
#     .[.$NAME %in% counties.filter.vector,]
#   temp <- counties.lonlat
#   counties.lonlat <- counties.lonlat %>%
#     .$geometry %>% sf::as_Spatial()
#   counties.lonlat$NAME <- temp$NAME
#   rm(temp)
# }else{
#   counties.lonlat <- counties.lonlat 
#   temp <- counties.lonlat
#   counties.lonlat <- counties.lonlat %>%
#     .$geometry %>% sf::as_Spatial()
#   counties.lonlat$NAME <- temp$NAME
#   rm(temp)
# }

if(!is.na(counties.filter.vector)){
  counties.lonlat <- counties.lonlat[counties.lonlat$NAME %in% counties.filter.vector,]
}

counties.lonlat

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

# counties.lonlat2 <- NULL
# for(i in 1:length(counties.lonlat@polygons)){
#   sf::st_geometry(counties.lonlat)
#   counties.lonlat$geometry[[1]]
#   temp.lonlat <- counties.lonlat@polygons[[i]]@Polygons[[1]]@coords %>% 
#     as.data.frame()
#   colnames(temp.lonlat) <- c("x", "y")
#   temp.lonlat$group <- i
#   temp.lonlat$NAME <- counties.lonlat$NAME[i]
#   
#   counties.lonlat2 <- rbind(counties.lonlat2, 
#                             temp.lonlat)
#   rm(temp.lonlat)
# }


#create a list of xy coordinates from hfa2 to transform
trans_coords <- counties.lonlat2[,1:2]

#coordinate transformation via projection
pc <- ptransform(trans_coords/180*pi, 
                 '+proj=latlong +ellps=sphere',
                 '+proj=lcc +lat_1=34.33333333333334 +lat_2=36.16666666666666 +lat_0=33.75 +lon_0=-79.0 +x_0=609601.22 +y_0=0')

counties.lonlat2$x2 <- pc$x
counties.lonlat2$y2 <- pc$y


# counties.lonlat2$County[counties.lonlat2$group == 1] <- "Lincoln"
# counties.lonlat2$County[counties.lonlat2$group == 2] <- "Caldwell"
# counties.lonlat2$County[counties.lonlat2$group == 6] <- "Irdell"
# counties.lonlat2$County[counties.lonlat2$group == 5] <- "Rutherford"
# counties.lonlat2$County[counties.lonlat2$group == 3] <- "Alexander"
# counties.lonlat2$County[counties.lonlat2$group == 4] <- "Cleveland"
# counties.lonlat2$County[counties.lonlat2$group == 7] <- "Catawba"
# counties.lonlat2$County[counties.lonlat2$group == 9] <- "Burke"
# counties.lonlat2$County[counties.lonlat2$group == 8] <- "Gaston"

plot3 <- ggplot() + 
  coord_fixed()+
  geom_path(data = counties.all2,
            aes(x = x2, y = y2, group = group, color = "Counties"),
            #color = "grey") +
  ) +
  geom_path(data = state.lonlat2, 
            aes(x = x2, y = y2, group = group), 
            size = 1, 
            color = "#2F747E")+
  geom_polygon(data = nc.cgd_interim_df[nc.cgd_interim_df$district %in% 2,], 
               aes(x = X, y = Y, group = district, fill = "Congressional\nDistricts"), 
               #fill = "#2F747E", 
               color = NA, 
               alpha = 0.5)+
  geom_path(data = counties.lonlat2[!counties.lonlat2$County %in% c("Gaston", "Lincoln", 
                                                                    "Cleveland", "Caldwell"),], 
            aes(x = x2, y = y2, group = group, 
                color = "Selected Counties"),
            size = 0.7, 
            #color = "#2F747E")+
  )+
  theme(legend.position = "bottom", 
        legend.direction = "vertical")+
  scale_color_discrete(name = "Boundaries")+
  scale_fill_discrete(name = "Boundaries")+
  #theme_nothing()+
  geom_point(data = major.cities2[major.cities2$NAME %in% 
                                    c("Durham", "Raleigh", "Chapel Hill", "Greensboro", "Charlotte", 
                                      "Fayetteville", "Asheville", 
                                      "Winston-Salem", "High Point", 
                                      "Wilmington", "Goldsboro", 
                                      "New Bern", "Fayetteville", 
                                      "Rocky Mount", "Rockingham", "Salisbury", 
                                      "Statesville", "Hickory", 
                                      "Shelby", "Hendersonville", "Morganton"),], 
             aes(x = x2, y = y2, 
                 color = "Select Cities")) 


plot3 +
  labs(title = "Rough Template - Congressional Districts Map", 
       subtitle = "Showing what data is available")



ggsave(filename = "NC_CongressionalDist_2022.eps", 
       plot = plot3, device = "eps")

ggsave(filename = "NC_CongressionalDist_2022.pdf", 
       plot = plot3, device = "pdf")

ggsave(filename = "NC_CongressionalDist_2022.ps", 
       plot = plot3, device = "ps")


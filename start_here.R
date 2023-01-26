# SAMPLE MAP TEMPLATE

# This template map shows how to create a map of North Carolina's 100 counties and 
# their Census population figures for 


# Pre-Requisite Software----
# 1) Download R (the programming language) from https://cran.r-project.org/bin/windows/  (click on 'install R for the first time')
# 2) Download RStudio (the Integrated Developer Environment [IDE] from https://download1.rstudio.org/desktop/windows/RStudio-2022.07.2-576.exe)



# Initiate Libraries----

# Note:  The first time you use any library, you will almost certainly have to
# install it from the 'Console' (command line) below in RStudio with the
# following function:  install.packages("PutNameOfPackageHereInQuotes")  Note2:
# the terms "package" and "library" are used interchangeably in R

library(ggplot2)
library(tigris)
library(tidycensus)
library(dplyr)
library(glue)

# Following is a quick line of code I always include at the beginning of every
# script that clears out old variables from the global environment, clears
# memory and clears the console screen. It's a good way to just start from
# scratch so you know that nothing from your last script accidentally gets
# carried over and used without you knowing about it.
rm(list = ls());gc();cat('\f')  

# Set Variables----
var_state        <- "North Carolina"  # you can change this to any US state (and possibly territory)
demographic.info <- "population"      # you can also change this to 'housing'
var_year         <- 2020              # year of census data to pull.  i think 2019 is most recent available due to pandemic

# Download All US State Boundary Geographies----
boundary_all.states <- tigris::states(year = var_year)

# this line of code filters down to the state you set in 'var_state' from the
# top of the page
boundary_var.state <- boundary_all.states[boundary_all.states$NAME %in% var_state,]

# plot a map of var_state
ggplot() + 
  geom_sf(data = boundary_var.state)+
  labs(title = "This is the minimum you need for a basic map in R",
          subtitle = "Below we'll talk about changing the map theme adding more features",
          caption = "There's so much more we can do though")

# you san set any map to memory as a variable for future use as follows.  This
# is useful when you need to make multiple maps of the same area: you can create
# a base map with all of the shared features like geographic boundaries, major
# roadways, etc., and then later on add new features onto that map without
# having to re-writing the same code to produce another basemap.
plot_basemap <- ggplot() + 
  geom_sf(data = boundary_var.state)

# to plot your basemap, simply run it alone: 
plot_basemap

# now would be a good time to make some tweaks to the map theme.  we're going to
# use the theme() function to remove the longitude and latitude coordinates from
# this map since there's nothing that we'll be adding to this map that the
# viewer will need to know that level of geographic precison to comprehend, plus
# it's ugly.  Any of the values you see in the code below in green (character
# values, need to be enclosed with quotation marks) or in red (numbers) can be
# modified to reflect your preferences

# if you need help understanding how something in R works, run it in the console
# below with a question mark appended before it.  that will bring up the help
# file to the right ---->
?theme()  # for example

plot_basemap <- plot_basemap +
  theme(axis.text         = element_blank(),                  # removes the longitude/latitude labels
        axis.ticks        = element_blank(),                  # removes the '-' (tick) marks from the axis
        panel.background  = element_rect(fill = "light blue", # color name, code, etc 
                                         color = "blue"),     # color name, code, etc 
        plot.background   = element_rect(fill = "pink",       # color name, code, etc 
                                         color = "red"),      # color name, code, etc 
        plot.title        = element_text(face = "bold",       # ("plain", "italic", "bold", "bold.italic")
                                         size = 18,           # text size in pts
                                         color = NULL),       # color name, code, etc 
        plot.subtitle     = element_text(face = NULL,         # ("plain", "italic", "bold", "bold.italic")
                                         size = 16,           # text size in pts
                                         color = NULL),       # color name, code, etc 
        plot.caption      = element_text(face = "italic",     # ("plain", "italic", "bold", "bold.italic")
                                         size = NULL,         # text size in pts
                                         color = NULL),       # color name, code, etc 
        legend.background = element_rect(fill = "light green",# color name, code, etc 
                                         color = "green")) +  # color name, code, etc 
  labs(title    = "[Title - Update for each final map]",      # Set the Map title here 
       subtitle = glue("{var_state}, USA, {var_year}"),       # -No need to update; I've used the 
                                                              # 'glue()' function along with 'var_state' 
                                                              # and 'var_year' variables to auto-generate 
                                                              # this title so that when these variables 
                                                              # are changed, this text will update automatically
       caption  = "Source: US Census Bureau, Accessed 2023")  # Set the Map caption here 

# notice that we saved the basemap with the new theme and 'labs' elements
# overwriting 'plot_basemap'.  now when we plot the basemap, we have a more
# complete and finished-looking map
plot_basemap

# HOW TO ADD ADDITIONAL GEOGRAPHIC FEATURES TO YOUR MAP----

# The US Census Bureau has an open database of cartographic boundary shapefiles
# that known as TIGER.  The R package 'tigris' allows easy access to this
# library in R.  The basemap produced so far has used state boundaries pulled
# from tigris().  But let's explore some of the other things we can add to our
# basemap to help the map's audience better orient themselves to their location
# in the state by lownloading major road and water features.


county_names <- tigris::counties(state = var_state, cb = T, year = var_year)$NAME
gc()

temp.counties <- c("Durham", "Orange", "Chatham", sample(county_names, size = 10, replace = F)) %>%
  unique()
rm_geometry   <- T


state_water.lines <- NULL
for(i in county_names[county_names %in% 
                      temp.counties]){
  temp.wl <- tigris::area_water(state = var_state, 
                                county = i) %>%
    mutate(., 
           county_name = i)
  
  if(rm_geometry){
    temp.wl <- sf::st_drop_geometry(temp.wl)
  }
  
  state_water.lines <- rbind(state_water.lines, 
                             temp.wl)
  rm(temp.wl)
}

gc()

state_water.lines %>%
  .[!is.na(.$FULLNAME),] %>%
  group_by(FULLNAME) %>%
  summarise(n = n(), 
            n_co = n_distinct(county_name), 
            n_hydroid = n_distinct(HYDROID))

slice_max(group_by(state_water.lines[!is.na(state_water.lines$FULLNAME),], county_name), 
          order_by = AWATER, n = 3)



ggplot() + 
  geom_sf(data = tigris::counties(var_state,cb=T,year=var_year)[counties(var_state,cb=T,year=var_year)$NAME %in% 
                                                                  temp.counties,], 
          fill = "white", color = "grey")+
  geom_sf(data = state_water.lines[!is.na(state_water.lines$FULLNAME),],#[grepl("Eno|Jord|Falls|Johns", state_water.lines$FULLNAME, ignore.case = F),], 
          #fill = "black", color = "black")
          aes(fill = MTFCC, color = MTFCC))

state_water.lines$MTFCC %>% table(., useNA = "always")

state_water.lines[grepl("Eno|Jord|Falls", 
                        state_water.lines$FULLNAME, 
                        ignore.case = F),]$FULLNAME %>% unique()




# HOW TO ADD CENSUS DATA TO YOUR MAP----
nc_county_population <- tidycensus::get_estimates(geography = "county", 
                                                  product = "population", 
                                                  state = var_state, 
                                                  geometry = T)


# add to our county map
map.nc + 
  geom_sf(data = nc_county_population)

# filter out variable to "POP" (removes 'density', which we don't want)
nc_county_population <- nc_county_population[nc_county_population$variable == "POP",]


# fill (color) counties based on population size variable
pop.nc <- map.nc + 
  geom_sf(data = nc_county_population, 
          aes(fill = value))

pop.nc

# Changing the Legend, Colors, Grid, Axis, and other Thematic Items----

pop.nc + 
  theme(axis.text = element_blank(),            # removes the lon/lat labels from the axes
        axis.ticks = element_blank())+          # remoes the tick marks from the axes 
  scale_fill_viridis_c(option = "C",            # changes the color pattern to a preset that I like
                       labels = scales::comma,  # adds commas to the legend values
                       name = "County Population")+ # allows you to custom-name the legend
  labs(title = "[map title goes here]", 
       subtitle = "[map subtitle goes here]", 
       caption = "[i use this argument for data source references]")


# Improved with library(glue)
pop.nc + 
  theme(axis.text = element_blank(),            # removes the lon/lat labels from the axes
        axis.ticks = element_blank())+          # remoes the tick marks from the axes 
  scale_fill_viridis_c(option = "C",            # changes the color pattern to a preset that I like
                       labels = scales::comma,  # adds commas to the legend values
                       name = glue("{demographic.info}"))+ # allows you to custom-name the legend
  labs(title = glue("{state.filter} {demographic.info} Information by County"), 
       subtitle = glue("Census Year: {census.year}"), 
       caption = glue("Source: US Census Bureau"))

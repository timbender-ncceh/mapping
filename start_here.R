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

# Following is a quick line of code I always include at the beginning of every
# script that clears out old variables from the global environment, clears
# memory and clears the console screen. It's a good way to just start from
# scratch so you know that nothing from your last script accidentally gets
# carried over and used without you knowing about it.
rm(list = ls());gc();cat()  

# Variables----
state.filter     <- "North Carolina"  # you can change this to any US state (and possibly territory)
demographic.info <- "population"      # you can also change this to 'housing'
census.year      <- 2019              # year of census data to pull.  i think 2019 is most recent available due to pandemic


# Download State Boundary Geographies----
all_states <- tigris::states(year = census.year)

# Plot 
ggplot() + 
  geom_sf(data = all_states)

# filter down to just North carolina with our variable from the top of the page
nc_state <- all_states[all_states$NAME == state.filter,]

# plot
ggplot() + 
  geom_sf(data = nc_state)

# save this map as a variable called 'map.nc'.  We will be adding onto it
map.nc <- ggplot() + 
  geom_sf(data = nc_state)

# download some county data from the US Census bureau
nc_county_population <- tidycensus::get_estimates(geography = "county", 
                                                  product = "population", 
                                                  state = state.filter, 
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

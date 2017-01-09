#### Load Libraries and Data ####
library(plyr)
library(dplyr)
library(leaflet)
library(tigris)
library(shiny)
library(shinythemes)
library(acs)
####

cny.shp <- tracts(state = 36, county = c("Onondaga", "Oswego", "Cortland", "Madison", "Cayuga"), cb = TRUE)
cny.df <- as.data.frame(cny.shp)



dat <- read.csv("data/cny.csv")

dat$AFFGEOID <- dat$Id

merged <- inner_join(cny.df, dat)

df_merged <- geo_join(cny.shp, dat, "AFFGEOID", "AFFGEOID")

popup <- paste0("Census Tract: ", df_merged$NAME, "<br>", "Children enrolled in IL: ", df_merged$kids)
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = df_merged$kids
)
####

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = df_merged, 
              fillColor = ~pal(kids), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.5, 
              weight = 2, 
              smoothFactor = 0.1,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = df_merged$kids, 
            position = "bottomright", 
            title = "Kids Enrolled",
            labFormat = labelFormat(suffix = " Kids")) 
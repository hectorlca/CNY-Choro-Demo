
#### Load Libraries and Data and Wrangle Shapefile ####
library(plyr)
library(dplyr)
library(tigris)
library(maptools)
library(sp)
library(ggplot2)
library(ggmap)
library(PBSmapping)
library(shiny)
library(leaflet)
library(shinythemes)
#### Shapefile Wrangle ####

dat <- read.csv("data/cny.csv", stringsAsFactors = FALSE)
dat <- na.omit(dat)
dat$pop.inc <- format(dat$mean.income,big.mark=",",scientific=FALSE)
dat$pop.inc <-
dat$pop.inc <- as.character( paste0( "<b>", dat$pop.inc, ".</b>") )
dat$poptract <- as.character( paste0( "<h5><b>", dat$tract, ".</h5></b>") )
dat$popcounty <- as.character(paste0("<b>", dat$county, "</b>"))
cny.shp <- tracts(state = 36, county = c("Onondaga", "Oswego", "Cortland", "Madison", "Cayuga"), cb = TRUE)


dat$error.incpercent <- as.numeric(dat$error.incpercent)
dat$mean.income <- as.numeric(dat$mean.income)

df_merged <- geo_join(cny.shp, dat, "GEOID", "GEOID")
df_merged <- na.omit(df_merged)
cny.df <- as.data.frame(df_merged)

####

# Define UI for application that draws a histogram
ui <- 
  navbarPage(
    "Campaign for Grade Level Reading",
    theme = shinytheme("flatly"),
    tabPanel("Average Income",
    fluidRow(
    column(width = 12,
          leafletOutput("incmap",
                         height = 900)))
  ),
  
  tabPanel("SNAP",
    fluidRow(
      column(width = 12,
             leafletOutput("snapmap",
                           height = 900))
    )
  )
  )





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$incmap <- renderLeaflet({
    
    popup <- paste0( df_merged$poptract, "<b>County: </b>", df_merged$popcounty, "</br>", 
                     "<b>Average Household Income: $</b>", df_merged$pop.inc, "</br>",
                     "Error Margin: ", df_merged$error.incpercent, "%")
    
    pal <- colorNumeric(
      palette = "GnBu",
      domain = as.numeric(df_merged$mean.income))

    leaflet() %>%
      addProviderTiles("Stamen.Toner") %>%
      addPolygons(data = df_merged, 
                  fillColor = ~pal(mean.income), 
                  color = "black", # you need to use hex colors
                  fillOpacity = 0.9, 
                  weight = 0.5, 
                  smoothFactor = 0.5,
                  popup = popup) %>%
      addLegend(pal = pal, 
                values = df_merged$mean.income, 
                position = "bottomright", 
                title = "Average Household Income",
                labFormat = labelFormat(prefix = "$")
                ) %>%
      setView(lng=-76.13, lat=43.03, zoom=9)
    
    
    
    
  })
  
  output$snapmap <- renderLeaflet({
    
    popup <- paste0( df_merged$poptract, "<b>County: </b>", df_merged$popcounty, "</br>", 
                     "<b>PErcent Households in SNAP: </b>", df_merged$percent.snap, "</br>",
                     "Error Margin: ", df_merged$error.snap, "%")
    
    pal <- colorNumeric(
      palette = "Purples",
      domain = as.numeric(df_merged$percent.snap))
    
    leaflet() %>%
      addProviderTiles("Stamen.Toner") %>%
      addPolygons(data = df_merged, 
                  fillColor = ~pal(percent.snap), 
                  color = "black", # you need to use hex colors
                  fillOpacity = 0.9, 
                  weight = 0.5, 
                  smoothFactor = 0.5,
                  popup = popup) %>%
      addLegend(pal = pal, 
                values = df_merged$percent.snap, 
                position = "bottomright", 
                title = "Percent Households in SNAP",
                labFormat = labelFormat(suffix = "%")
      ) %>%
      setView(lng=-76.13, lat=43.03, zoom=9)
    
    
    
    
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
















####

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

# Shapefile Wrangle #

dat <- read.csv("data/cny.csv", stringsAsFactors = FALSE)

dat$AFFGEOID <- dat$Id

merged <- inner_join(cny.df, dat)


df_merged <- geo_join(cny.shp, dat, "AFFGEOID", "AFFGEOID")
df_merged$avg.hhincome <- as.numeric(df_merged$avg.hhincome)



# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    fluidRow(
      column(width = 12,
             leafletOutput("povmap",
                           height = 900)))
  )





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$povmap <- renderLeaflet({
    
    popup <- paste0("Geography: ", df_merged$Geography, "<br>", 
                    "<b>Poverty Rate: </b>", df_merged$avg.hhincome, ".",  "<br>",
                    "Unemployment Rate: ", df_merged$unemployment.rate, "<br>"
    )
    
    pal <- colorNumeric(
      palette = "Reds",
      domain = df_merged$avg.hhincome
    )
    ####
    
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addPolygons(data = df_merged, 
                  fillColor = ~pal(avg.hhincome), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = .5, 
                  weight = 2, 
                  smoothFactor = 0.5,
                  popup = popup) %>%
      addLegend(pal = pal, 
                values = df_merged$avg.hhincome, 
                position = "bottomright", 
                title = "Poverty",
                labFormat = labelFormat(suffix = "%"),
                bins = 7) %>%
      setView(lng=-76.13, lat=43.03, zoom=9)
    
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

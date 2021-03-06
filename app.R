#### Load Libraries and  Shapefile ####
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
library(dygraphs)
library(shinydashboard)
library(ggmap)

# Shapefile for CNY

cny.shp <- tracts(
  state = 36, county = c("Onondaga", 
                         "Oswego", 
                         "Cortland", 
                         "Madison", 
                         "Cayuga"), cb = TRUE)

schools <- read.csv("data/geoschools.csv") # for school dot map.

lead <- read.csv("data/final_lead.csv") #for bubble dot map
lead$percent.good <- round((lead$outletsOver15/lead$num.outlets) *100, 1)
lead$percent.bad <- 100-lead$percent.good




#### Schools Wrangle ####

schools <- select(schools, LEGAL.NAME, 
                  GRADE.ORGANIZATION.DESCRIPTION,
                  CEO.FIRST.NAME, CEO.LAST.NAME,
                  CEO.EMAIL,
                  lon, lat)

schoolIcon <- makeIcon(
  iconUrl = "http://image.flaticon.com/icons/svg/124/124804.svg",
  iconWidth = 30, iconHeight = 40,
  iconAnchorX = 22, iconAnchorY = 94)





#### Filter Dropdowns ####

both.attendance <- read.csv ("data/weekly_attendance.csv")
both.attendance$IEP <- as.character(both.attendance$IEP)
wattendance.sub <- both.attendance

wiep.dropdown <- 
  list(p(strong(h5("Choose IEP Status"))), 
       tags$div(align = 'left', 
                class = 'dropdown',
                selectInput("watt.IEP", 
                            "", 
                            c("All",unique(wattendance.sub$IEP)))))

wethnicity.dropdown <-
  list(p(strong(h5("Choose Ethnicity"))), 
       tags$div(align = 'left', 
                class = 'dropdown', 
                selectInput(inputId  = 'watt.ethnicity',
                            "",
                            choices  = c("All", unique (as.character(wattendance.sub$Ethnicity))),
                            selected = "All")))

wtier.dropdown <- 
  list(p(strong(h5("Choose School Type"))), 
       tags$div(align = 'left', 
                class = 'dropdown',
                selectInput("watt.tier", 
                            "", 
                            c("All",unique(as.character(wattendance.sub$tier))))))

wgender.dropdown <- 
  list(p(strong(h5("Choose Gender"))), 
       tags$div(align = 'left', 
                class = 'dropdown',
                selectInput("watt.gender", 
                            "", 
                            c("All",unique(as.character(wattendance.sub$Gender))))))


####
 
dat <- read.csv("data/cny.csv", stringsAsFactors = FALSE)
dat <- na.omit(dat)
dat$pop.inc <- format(dat$mean.income,big.mark=",",scientific=FALSE)
dat$pop.inc <- as.character( paste0( "<b>", dat$pop.inc, ".</b>") )
dat$poptract <- as.character( paste0( "<h5><b>", dat$tract, ".</h5></b>") )
dat$popcounty <- as.character(paste0("<b>", dat$county, "</b>"))



dat$error.incpercent <- as.numeric(dat$error.incpercent)
dat$mean.income <- as.numeric(dat$mean.income)

df_merged <- geo_join(cny.shp, dat, "GEOID", "GEOID")
df_merged <- na.omit(df_merged)
cny.df <- as.data.frame(df_merged)







#### UI Design ####

ui <- 
  navbarPage(
    "Central New York",
    theme = shinytheme("journal"),
    tabPanel("Average Income",
             fluidRow(
               column(width = 12,
                      leafletOutput("incmap",
                                    height = 800)))
    ),
    
    tabPanel("SNAP",
             fluidRow(
               column(width = 12,
                      leafletOutput("snapmap",
                                    height = 800))
             )
    ),
    
    tabPanel("Schools",

             fluidRow(
               column(width = 12,
                      leafletOutput("schoolmap",
                                    height = 800))
             )
    ),

    tabPanel("Weekly Attendance",
             fluidRow(
              valueBoxOutput("attendpercent"),
              valueBoxOutput("attendchange")
             ),
             fluidRow(
               box(width=12,
                   column(3, wiep.dropdown),
                   column(3, wethnicity.dropdown),
                   column(3, wtier.dropdown),
                   column(3, wgender.dropdown)
               )),
             fluidRow(
               box(width = 12,
                   dygraphOutput ("weekly.attendance", height = 600)))
             
    )
  )





#### Create Server and Outputs ####


server <- function(input, output) {
  
  output$incmap <- renderLeaflet({
    
    popup <- paste0( df_merged$poptract, "<b>County: </b>", df_merged$popcounty, "</br>", 
                     "<b>Average Household Income: $</b>", df_merged$pop.inc, "</br>",
                     "Error Margin: ", df_merged$error.incpercent, "%")
    
    pal <- colorNumeric(
      palette = "GnBu",
      domain = as.numeric(df_merged$mean.income))
    
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addPolygons(data = df_merged, 
                  fillColor = ~pal(mean.income), 
                  color = "black", # you need to use hex colors
                  fillOpacity = 0.7, 
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
                     "<b>Percent Households in SNAP: </b>", df_merged$percent.snap, "</br>",
                     "Error Margin: ", df_merged$error.snap, "%")
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = as.numeric(df_merged$percent.snap))
    
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addPolygons(data = df_merged, 
                  fillColor = ~pal(percent.snap), 
                  color = "black", # you need to use hex colors
                  fillOpacity = 0.6, 
                  weight = 0.6, 
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
  
  output$weekly.attendance <- renderDygraph({ 
    
    wattendance.sub <- both.attendance
    
    if (input$watt.IEP != "All") {
     wattendance.sub <- wattendance.sub[wattendance.sub$IEP == input$watt.IEP,]
      
    }
    
    if (input$watt.ethnicity != "All") {
      wattendance.sub <- wattendance.sub[wattendance.sub$Ethnicity == input$watt.ethnicity,]
    }
    
    if (input$watt.tier != "All") {
      wattendance.sub <- wattendance.sub[wattendance.sub$tier == input$watt.tier,]
    }
    
    if (input$watt.gender != "All") {
      wattendance.sub <- wattendance.sub[wattendance.sub$Gender == input$watt.gender,]
    }
    
    wattendance.group <- group_by (wattendance.sub, sw)
    wattendance.sum <- summarise (wattendance.group, present15 = sum(presences15), absent15 = sum(absences15), present16 = sum(presences16, na.rm=T), absent16 = sum(absences16, na.rm=T))
    wattendance.sum <- mutate(wattendance.sum, enrollment15 = (present15+absent15), enrollment16 = (absent16+present16))
    wattendance.sum <- mutate(wattendance.sum, attendance15_rate = (present15/enrollment15), attendance16_rate = (present16/enrollment16))
    wattendance.sum <- select(wattendance.sum, sw, attendance15_rate, attendance16_rate)
    
    
    dygraph(wattendance.sum, main = "Weekly Attendance Rate Annual Comparison") %>%
      
      dySeries(name = "attendance15_rate", label = "2015 Attendance Rate", 
               color = "green", fillGraph = FALSE, strokeWidth = 3)%>%
      dySeries(name = "attendance16_rate", label = "2016 Attendance Rate", 
               color = "blue", fillGraph = FALSE, strokeWidth = 3)%>%
      dyLegend(show = "follow", width = 250, labelsSeparateLines = TRUE, hideOnMouseOut = FALSE)%>%
      dyRangeSelector(retainDateWindow = TRUE ) %>%
      dyAxis("y", label = "Attendance Rate", valueRange = c(0.6, 1.1)) %>%
      dyAxis("x", drawGrid = TRUE) %>%
      dyRangeSelector() %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = FALSE)%>%
      dyEvent("10", "Start of 2nd Quarter", labelLoc = "bottom") %>%
      dyEvent("20", "Start of 3rd Quarter", labelLoc = "bottom") %>%
      dyEvent("30", "Start of 4th Quarter", labelLoc = "bottom")
    
    
  })
  
  output$schoolmap <- renderLeaflet({
    
    lead$pop.school <- as.character( paste0( "<h5>", lead$School, ".</h5>") )
    lead$pop.bad <- as.character( paste0( "<b>", lead$percent.bad, "</b>") )
    lead$pop.dist <- as.character(paste0("<b>", lead$District, "</b>"))
    
    lead <- na.omit(lead)
    lead$percent.bad <- round(lead$percent.bad, 1)

    popup <- paste0( lead$pop.school, 
                     "<b>Percent Outlets > 15 ppb: </b>", lead$pop.bad, "%", "</br>",
                     "<b>School District: </b>", lead$pop.dist)
    
    leaflet() %>%
      setView(lng=-76.13, lat=43.03, zoom=10) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addCircles(
        lat = lead$lat, lng = lead$lon,
        #radius = (lead$dotsize*1000),
        radius = (lead$percent.bad*20),
        color = "#de2d26",
        popup = popup,
        stroke = FALSE, 
        fillOpacity = 0.7)
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


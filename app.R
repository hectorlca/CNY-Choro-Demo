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
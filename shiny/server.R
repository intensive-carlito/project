function(input, output) {
  
  output$nb_appart <- renderValueBox({
    # Nb d'apaprtement
    valueBox(
      value = nrow(airbnb),
      subtitle = "Nombre d'appartements parisiens",
      icon = icon("fas fa-building"),
      color = "yellow" 
    )
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = airbnb[1:200,],~longitude, ~latitude, popup = ~as.character(price), label = ~as.character(name)) %>%
      addPolygons(data= arrondissement,stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.01) 
  })
  
}
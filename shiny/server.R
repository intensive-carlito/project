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
    
    tmp <- leaflet() %>%
      addProviderTiles(providers$Esri,options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = airbnb,~longitude, ~latitude, popup = ~as.character(price), label = ~as.character(price),clusterOptions = markerClusterOptions()) %>%
      addPolygons(data = arrondissement, stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.01)
    if (input$heatMapCheckBox) {
        tmp %>% addHeatmap(data = airbnb, intensity=~price, lng=~longitude, lat=~latitude, min=~input$heatMapSlider[1], max=~input$heatMapSlider[2], blur = 60)
    }
    else {
      tmp
    }
  })
  
  Table_normalisation <- reactive({
    normalise <- GET(paste0("https://api-adresse.data.gouv.fr/search/?q=",chartr(",_/ ","+++++",input$inputAd)))
    ad=as.data.frame(content(normalise)$features[[1]])
    if (dim(ad)[2] >= 2) {
      colnames(ad)[c(1,2)] <- c("longitude","latitude")
      temp=dplyr::select(sample_frac(airbnb,0.1), latitude, longitude, neighbourhood_cleansed,l_qu, id, price,picture_url)
      mat2 <- as.data.frame(distm(setDT(temp)[,.(longitude,latitude)], setDT(ad)[,.(longitude,latitude)], fun=distVincentyEllipsoid))%>%
        cbind(dplyr::select(temp,neighbourhood_cleansed,l_qu, id, price,picture_url)) %>% arrange(V1) %>% head(3) %>% dplyr::select(neighbourhood_cleansed,l_qu, id, price,picture_url)
      ad = cbind(ad,mat2)
    }
    else {
      ad <- data.frame(longitude=2.3488, latitude=48.8534, properties.label="", properties.type="", neighbourhood_cleansed="")
    }
    ad
  })
  
  # Table_plus_proche <- reactive({
  #   mat2 <- as.data.frame(distm(setDT(Table_normalisation())[,.(longitude,latitude)], setDT(ad)[,.(longitude,latitude)], fun=distVincentyEllipsoid))%>%
  #       cbind(dplyr::select(temp,neighbourhood_cleansed,l_qu)) %>% arrange(V1) %>% head(1) %>% dplyr::select(neighbourhood_cleansed,l_qu)
  #     ad = cbind(ad,mat2)
  # })
  
  output$table <- renderTable(dplyr::select(head(Table_normalisation(),1),
                                            Adresse_normalisee=properties.label,
                                            precision_de_la_normalisation=properties.type,
                                            longitude,latitude,
                                            Quartier_bdd_Airbnb=neighbourhood_cleansed,
                                            sous_quartier=l_qu))
  
  output$table_3_plus_proche <- DT::renderDataTable(DT::datatable(dplyr::select(mutate(Table_normalisation(),
                                                                                       lien=paste0("<a href='https://www.airbnb.com/rooms/", as.character(id),"' target='_blank'>",id,"</a>"),
                                                                                       picture_url=paste0('<img src=',picture_url,' height="52"></img>')),
                                                                                lien, price,picture_url),escape = FALSE))

  output$mymap_norm <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$Esri,options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = Table_normalisation(),~longitude, ~latitude, label = ~as.character(properties.label)) %>%
      addPolygons(data= arrondissement,stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.01) #%>%
      #addHeatmap(data = airbnb, intensity=~price, lng=~longitude, lat=~latitude, max=.6, blur = 60)
    
  })
  
  
  price_ml <- reactive({
    test2 <- (data.frame(l_qu=head(Table_normalisation(),1)$l_qu,
                      zipcode=as.character(head(Table_normalisation(),1)$properties.postcode),
                      bedrooms=input$bedrooms,
                      bathrooms=input$bathrooms,
                      summary_l=length(input$summary),
                      delai_inscription=difftime(as.Date("2019-01-01"),input$delai_inscription,tz="Europe/Paris","days"),
                      host_total_listings_count=0L, stringsAsFactors = F)) %>%
      mutate(l_qu=factor(l_qu,levels=levels(airbnb$l_qu)),
             zipcode=factor(zipcode,levels=levels(airbnb$zipcode)))
    model <- input$modele
    if (model == "modelRF") {
      predicted <- round(predict(modelRF, test2), 0)
    } else {
      #my_data <- P09_modele[0,]
      #for (c in colnames(test2)) {
      #  my_data[1, c] <- test2[1, c]
      #}
      if (!"host_since" %in% colnames(test2)) {
        names <- setdiff(setdiff(colnames(P09_modele), "id"), colnames(test2))
        test2[,names] <- NA
        test2 <- mutate(test2, host_since=as.Date(host_since))
      }
      predicted <- round(predict(modelXGB, Matrix(test2, sparse = TRUE), type="raw"))
    }
  })
  
  output$price <- renderText(price_ml())
}

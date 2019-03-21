# shiny
# saveRDS(select(airbnb,id,latitude,longitude, name, price, guests_included,neighbourhood_cleansed),"./shiny/R_data/airbnb.RDS")
# airbnb=readRDS("./shiny/R_data/airbnb.RDS")

if (!require('plotly')) install.packages('plotly'); library('plotly')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('data.table')) install.packages('data.table'); library('data.table')
if (!require('bcp')) install.packages('bcp'); library('bcp')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('DT')) install.packages('DT'); library('DT')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
#if (!require('geojsonio')) install.packages('geojsonio'); library('geojsonio')
if (!require('leaflet.extras')) install.packages('leaflet.extras'); library('leaflet.extras')
library("geosphere")
library(httr)



dashboardPage(skin = "red",
              dashboardHeader(
                title = "AirBNB formation_ENSAE : David, Charles, Mathieu"
              ),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Visualisation / Carto", tabName = "carto"),
                  menuItem("Prédiction", tabName = "Autre"),
                  menuItem("Méthodologie", tabName = "methodo")
                )
              ),
              
              dashboardBody(
                tabItems(
                  tabItem("carto",
                          fluidRow(
                            valueBoxOutput("nb_appart"),
                            box(checkboxInput("heatMapCheckBox", "Prix par zone"),
                                conditionalPanel( condition = "input.heatMapCheckBox==true",
                                                  sliderInput("heatMapSlider", label = "Slider Range", min = 0, max = 1, value = c(1), step = 0.02))
                                ,leafletOutput("mymap")
                            )
                          )
                  ),
                  tabItem("Autre",
                          fluidRow(
                            # selectInput("modele", "Modèle :",
                            #             c("Régression linéaire" = "model_RStep_2",
                            #               "Régression linéaire Poisson" = "model_RLogStep_2")),
                            box(
                              title = "Adresse de l appartement", status = "primary", solidHeader = TRUE,
                              collapsible = TRUE,
                              textInput("inputAd", "Emplacement de l'appartement : ", value = "10 rue de naravin, 75009, paris", width = NULL, placeholder = NULL),
                              tableOutput('table'),
                              leafletOutput("mymap_norm"),
                              tableOutput("table_3_plus_proche")
                            ),
                            box(                 
                              title = "Description", status = "primary", solidHeader = TRUE,
                              collapsible = TRUE,
                              sliderInput("accommodates", "Nombre de personnes accueillies dans l'appartement : ",min = 1, max = 20, value = c(1), step = 1),
                              sliderInput("bedrooms", "Nombre de lit dans l'appartement : ",min = 0, max = 10, value = c(1), step = 1),
                              sliderInput("bathrooms", "Nombre de salle de bain dans l'appartement : ",min = 0, max = 10, value = c(1), step = 1),
                              textInput("summary", label="Faites une description de l'appartement", value = " "),
                              multiInput("amenities_", label = 'indiquez votre équipement', choices=amenities)
                            ),
                            checkboxInput("AlreadyCheckBox", "Avez vous déjà un compte AirBnb ?"),
                            conditionalPanel( condition = "input.AlreadyCheckBox==true",
                                              box(                 
                                                title = "Votre historique", status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                                dateInput("delai_inscription", "depuis quand êtes vous inscrits : ",value = as.character(Sys.Date()),
                                                          min = as.Date("2015-01-01"), max = Sys.Date() ,format = "dd/mm/yy",
                                                          startview = 'year'),
                                                numericInput("host_total_listings_count", label="Combien avez vous eu de visiteurs ?", value = 1,min=1,step	=1)
                                              )),
                            column(4,
                                   includeHTML("www/methodo.html")),
                            column(4, offset = 1,
                                   h1(textOutput("price")))
                          )
                  ), 
                  tabItem("methodo",
                          a("test", href="notes.html", target='blank'))
                )
              )
)

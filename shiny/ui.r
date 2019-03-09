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



dashboardPage(
  dashboardHeader(
    title = "AirBNB formation_ENSAE : David, Charles, Mathieu"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualisation / Carto", tabName = "carto"),
      menuItem("Autre", tabName = "Autre")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("carto",
              fluidRow(
                valueBoxOutput("nb_appart"),
                checkboxInput("heatMapCheckBox", "Prix par zone"),
                sliderInput("heatMapSlider", label = "Slider Range", min = 0, max = 1, value = c(0, 1), step = 0.02),
                leafletOutput("mymap")
              )
      ),
      tabItem("Autre",
              fluidRow(
                textInput("inputAd", "Emplacement de l'appartement : ", value = "10 rue de naravin, 75009, paris", width = NULL, placeholder = NULL),
                tableOutput('table'),
                includeHTML("www/methodo.html"),
                leafletOutput("mymap_norm")
              )
      )
    )
    )
)

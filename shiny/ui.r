# shiny
# saveRDS(select(airbnb,id,latitude,longitude, name, price, guests_included),"./shiny/R_data/airbnb.RDS")
# airbnb=readRDS("./shiny/R_data/airbnb.RDS")

if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('plotly')) install.packages('plotly'); library('plotly')
if (!require('leaflet')) install.packages('leaflet'); library('leaflet')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('data.table')) install.packages('data.table'); library('data.table')
if (!require('bcp')) install.packages('bcp'); library('bcp')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('DT')) install.packages('DT'); library('DT')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
if (!require('geojsonio')) install.packages('geojsonio'); library('geojsonio')




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
                leafletOutput("mymap", height=1000)
                )
       ),
      tabItem("Autre",
              fluidRow(
                valueBoxOutput("nb_pal2")
              )
            )
    )
    )
)


########## Librairies à lire  #############
# install.packages('bit64') 
library(shiny)
library(dplyr)
library(tidyverse)
# library(readxl)
# library(lobstr)
library(data.table)
library(stringr)
library(plotly)
library(leaflet)

################ Importation/ Traitement données ################ 

pathR="./R_script"
source(paste(pathR,"P00_Projet_desc_App.R",sep = "/"),local = TRUE)

pathRSh="./R_script/App_sheets"
source(paste(pathRSh,"App_sheet1.R",sep = "/"),local = TRUE)

################ Fin Importation Traitement données ############


################ Application ##################

ui <- navbarPage("airbnb",
                tabPanel("Description des données",
                          # Titre
                          titlePanel("Caractéristiques du bien"),

                          # Sidebar
                          sidebarLayout(
                            sidebarPanel(
                                selectInput("Quart", "Quartiers de Paris",list(`Quartiers` =Quartier)),
                                sliderInput("NLits", "Nombre de lits", 1, min = 0, max = 10,value = c(2, 4)),
                                sliderInput("NChamb", "Nombre de chambres", 1, min = 0, max = 10,value = c(1, 3)),
                                sliderInput("NBath", "Nombre de salles de bain", 1, min = 0, max = 5,value = c(1, 3))
                            ),
                            mainPanel(
                              plotOutput("Dist"),
                              tableOutput("Stat"),
                              leafletOutput("Map")
                            )
                          )
                 )
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
                  # Sheet 1 -----------------------
                  pathRSh="./R_script/App_sheets"
                  source(paste(pathRSh,"App_sheet1.R",sep = "/"),local = TRUE)
                  
                  output$Dist <- renderPlot({
                    Sheet1Fun("Dist")
                  })
                  output$Stat = renderTable({
                    Sheet1Fun("Stat")
                  })
                  output$Map <- renderLeaflet({
                    Sheet1Fun("Map")
                  })
                  
}

# Run the application 
shinyApp(ui = ui, server = server)


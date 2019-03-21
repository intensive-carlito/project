library(dplyr)
library(shinyWidgets)
library(randomForest)
library(xgboost)
airbnb=readRDS("../shiny/R_data/airbnb_shiny.RDS") %>% head(1000)
arrondissement <- readRDS("../shiny/R_data/quartiers.rds")
model_RF1 <- readRDS("../shiny/R_data/model_RF1.rds")
model_RLogStep_2=readRDS("../shiny/R_data/model_RLogStep_2.rds")
model_GB3 <- readRDS("../shiny/R_data/model_GB3.rds")
amenities=readRDS("../shiny/R_data/amenities.rds")

# saveRDS(arrondissement,"D:/Formation Data Scientist Ensae/04 - Projet/project-ds/input/arrondissements.rds")

# modelXGB <- readRDS("../shiny/R_data/model_xgb_10perc.rds")

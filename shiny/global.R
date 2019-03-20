library(dplyr)
library(randomForest)
library(xgboost)
airbnb=readRDS("../shiny/R_data/airbnb_shiny.RDS") %>% head(1000)
arrondissement <- readRDS("../shiny/R_data/quartiers.rds")
modelRF <- readRDS("../shiny/R_data/model_.rds")

# saveRDS(arrondissement,"D:/Formation Data Scientist Ensae/04 - Projet/project-ds/input/arrondissements.rds")

# modelXGB <- readRDS("../shiny/R_data/model_xgb_10perc.rds")

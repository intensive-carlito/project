library(dplyr)
airbnb=readRDS("../shiny/R_data/airbnb_shiny.RDS") %>% head(1000)
arrondissement <- readRDS("../shiny/R_data/quartiers.rds")

# saveRDS(arrondissement,"D:/Formation Data Scientist Ensae/04 - Projet/project-ds/input/arrondissements.rds")

library(dplyr)
airbnb=readRDS("./R_data/airbnb.RDS") %>% head(5000)
arrondissement <- readRDS("../input/arrondissements.rds")

# saveRDS(arrondissement,"D:/Formation Data Scientist Ensae/04 - Projet/project-ds/input/arrondissements.rds")

library(data.table)
library(plotly)
library(dplyr)
library(foreign)
# install.packages("geosphere")
library("geosphere")
# install.packages("Imap")
library("Imap")
library("tidyverse")


# Airbnb ------------------------------------------------------------------------------------------
airbnb=fread("./input/listings.csv",stringsAsFactors=FALSE, encoding = 'UTF-8') %>%
  mutate(price=as.numeric(gsub(",","",as.character(substr(price,2,100)))))

View(head(airbnb))

toto=as.data.frame(table(airbnb$first_review)) %>% filter(Var1!="")
plot_ly(data=toto, x=~Var1, y=~Freq, type="scatter", mode = 'lines')


toto2=as.data.frame(table(airbnb$price))
median(airbnb$price, na.rm=T)

plot_ly(data=toto2, x=~Var1, y=~Freq, type="scatter", mode = 'lines')
chelou=filter(airbnb,price==16)

write.csv(head(airbnb,100),"./input/airbnb_quartier.csv")

# données cadastrale ------------------------------------------------------------------------------------------
# cadastre=fread("C:/temp/projet/PARCELLE_CADASTRALE.csv",stringsAsFactors=FALSE)
# cadastre_dbf=read.dbf("C:/temp/projet/PARCELLE_CADASTRALE/PARCELLE_CADASTRALE.dbf", as.is = FALSE)
#  non on fera de l'intersection avec la qgis

# monuments ------------------------------------------------------------------------------------------
monuments=fread("C:/temp/projet/monuments/merimee-MH-valid.csv",stringsAsFactors=FALSE, encoding = 'UTF-8') %>% filter(DPT=="75")

# 
# saveRDS(airbnb,"./R_data/airbnb.RDS")
# # airbnb=readRDS("./R_data/airbnb.RDS")
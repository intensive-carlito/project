library(data.table)
library(plotly)
library(dplyr)
library(foreign)
install.packages("geosphere")
library("geosphere")
install.packages("Imap")
library("Imap")


# Airbnb ------------------------------------------------------------------------------------------
airbnb=fread("C:/temp/projet/listings.csv",stringsAsFactors=FALSE, encoding = 'UTF-8') %>%
  mutate(price=as.numeric(gsub(",","",as.character(substr(price,2,100)))))

View(head(airbnb))

toto=as.data.frame(table(airbnb$first_review)) %>% filter(Var1!="")
plot_ly(data=toto, x=~Var1, y=~Freq, type="scatter", mode = 'lines')


toto2=as.data.frame(table(airbnb$price))

median(airbnb$price, na.rm=T)

plot_ly(data=toto2, x=~Var1, y=~Freq, type="scatter", mode = 'lines')

chelou=filter(airbnb,price==16)


# données cadastrale ------------------------------------------------------------------------------------------
# cadastre=fread("C:/temp/projet/PARCELLE_CADASTRALE.csv",stringsAsFactors=FALSE)
# cadastre_dbf=read.dbf("C:/temp/projet/PARCELLE_CADASTRALE/PARCELLE_CADASTRALE.dbf", as.is = FALSE)
#  non on fera de l'intersection avec la qgis

# monuments ------------------------------------------------------------------------------------------
monuments=fread("C:/temp/projet/monuments/merimee-MH-valid.csv",stringsAsFactors=FALSE, encoding = 'UTF-8') %>% filter(DPT=="75")


# RATP ------------------------------------------------------------------------------------------
ratp=fread("C:/temp/projet/accessibilite-des-gares-et-stations-metro-et-rer-ratp.csv",stringsAsFactors=FALSE, encoding = 'UTF-8') %>%
  filter(floor(CodeINSEE/1000)==75) %>% select(nomptar,coord) %>% unique()
ratp = ratp %>% separate(coord, c("longitude", "latitude"), sep=",") %>% 
  mutate(longitude= as.numeric(longitude),
         latitude= as.numeric(latitude)) %>%
  cbind(variable=paste0("V",c(1:nrow(ratp)))) %>% 
  mutate(variable=as.character(variable))
mat2 <- as.data.frame(distm(setDT(head(airbnb,1000))[,.(longitude,latitude)], setDT(ratp)[,.(longitude,latitude)], fun=distVincentyEllipsoid)) %>%
  cbind(select(head(airbnb,1000),id))

mat3=melt(mat2, id="id") %>% mutate(variable=as.character(variable)) %>% left_join(ratp, by="variable")

min(mat3$value)




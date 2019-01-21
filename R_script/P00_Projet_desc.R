library(dplyr)
library(data.table)
library(plotly)
library(foreign)
# install.packages("geosphere")
library("geosphere")
# install.packages("colorspace")
library("ggplot2")
library("tidyverse")


# Airbnb ------------------------------------------------------------------------------------------
airbnb=fread("./input/listings.csv",stringsAsFactors=FALSE, encoding = 'UTF-8') %>%
  dplyr::filter(substr(zipcode,1,2)=="75") %>%
  mutate(price=as.numeric(gsub(",","",as.character(substr(price,2,100)))),
         security_deposit=as.numeric(gsub(",","",as.character(substr(security_deposit,2,100)))),
         zipcode=substr(zipcode,1,5)
  ) 
airbnb$host_since=as.Date(airbnb$host_since,"%Y-%m-%d")
airbnb$price_cut<-cut(airbnb$price, seq(0,1000,100))

airbnb = mutate(airbnb, price_cut = as.factor(ifelse(price<=200,as.character(cut(price, seq(0,200,25))),
                                                     ifelse(price<=500,as.character(cut(price, seq(200,500,25))),
                                                            ifelse(price<=1000,as.character(cut(price, seq(500,1000,100))),"]1000,infini[")))))

toto=as.data.frame(table(airbnb$first_review)) %>% filter(Var1!="")
plot_ly(data=toto, x=~Var1, y=~Freq, type="scatter", mode = 'lines')

median(airbnb$price, na.rm=T)


write.csv(head(airbnb,100),"./input/airbnb_quartier.csv")

filter(airbnb,accommodates==2) %>% group_by(neighbourhood_cleansed) %>%
  summarise(price_mean=mean(price),
            price_median=median(price),
            n=n())
table(airbnb$accommodates)

airbnb2=filter(airbnb,price<400)
boxplot(price~neighbourhood_cleansed,data=airbnb2)


# données cadastrale ------------------------------------------------------------------------------------------
# cadastre=fread("C:/temp/projet/PARCELLE_CADASTRALE.csv",stringsAsFactors=FALSE)
# cadastre_dbf=read.dbf("C:/temp/projet/PARCELLE_CADASTRALE/PARCELLE_CADASTRALE.dbf", as.is = FALSE)
#  non on fera de l'intersection avec la qgis

# monuments ------------------------------------------------------------------------------------------
monuments=fread("./input/merimee-MH-valid.csv",stringsAsFactors=FALSE, encoding = 'UTF-8') %>% filter(DPT=="75")

# 
# saveRDS(airbnb,"./R_data/airbnb.RDS")
# # airbnb=readRDS("./R_data/airbnb.RDS")
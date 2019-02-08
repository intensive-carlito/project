#setwd("C:/DOAAT/certificat/project-ds-master")

library("dplyr")
library(httr)
library(XML)
library(RCurl)
library(jsonlite)
library(rvest)
library(tidyr)

#auth_proxy_edf("pcy", Sys.getenv("USERNAME"), Sys.getenv("MDP_SESAME"))

# site 1 : http://monumentsdeparis.net/
library("rvest")
url <- "http://monumentsdeparis.net/"
liens <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="shortcut-content"]/ul/li[*]/a') %>%
  xml_attr("href") %>% as.data.frame()

colnames(liens)=c("lien")
liens = liens %>% mutate(lien=ifelse(!grepl(".*timeout.*", liens$lien),paste0("http://monumentsdeparis.net/",lien), as.character(lien)))

# boucles sur tous les monuments 

liens2=data.frame()
for (i in 1:nrow(liens))
{
  tryCatch({
    print(i)
    url <- liens[i,]
    temp <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="Carte"]/p[1]') %>% 
      html_text() %>%
      as.data.frame()

    liens2=rbind(liens2,temp)
  }, error=function(e){})
}
colnames(liens2)=c("ad")

# Suppression de 'Adresse : '
liens3 = liens2 %>% mutate(ad=substr(ad, 10, 1000))

# Suppression des virgules derrière les numéros
#tmp = liens3 %>% mutate(ad=gsub("([^,]*),(.*),\\s*([0-9]+) ([^,]*)$", "\\1;\\2;\\3;\\4", x = ad, perl=TRUE))


#liens3=tmp %>% separate(ad, c("libelle","ad", "cp_ville","autre"),";")

write.csv(liens3,"./input/monuments.csv")
# qq modifs manuelles, le fichiers d'adresse n'est pas assez propre

#liens4=read.csv("./input/monuments.csv",sep=",") %>% mutate(code_postal=as.character(code_postal))
# tentative de normalisation !

res <- POST("http://api-adresse.data.gouv.fr/search/csv/", 
            timeout(600), 
            body = list(data = upload_file("./input/monuments.csv")))

#write.csv2(liens4, t3 <- tempfile(fileext = ".csv"),fileEncoding = "UTF-8")
#res <- POST("http://api-adresse.data.gouv.fr/search/csv/", 
#            timeout(600), 
#            body = list(data = upload_file(tf),postcode="code_postal"))
res$status_code
res_content=content(res, encoding = "UTF-8")
#colnames(res_content)=c("ad")
select(res_content, latitude, longitude, result_label);
#Adresse_norm=res_content %>% separate(ad, 
#                                      c("N","libelle","ad","code_postal","latitude","longitude","result_label","result_score","result_type","result_id","result_housenumber","result_name","result_street","result_postcode","result_city","result_context","result_citycode"),
#                                      ";") %>% select(libelle,latitude,longitude)

monuments = res_content %>%
  select (latitude, longitude, result_label) %>%
  mutate (longitude = as.numeric(longitude), latitude = as.numeric(latitude)) %>%
  cbind(variable=paste0("V",c(1:nrow(res_content)))) %>%
  mutate(variable=as.character(variable))

mat2 <- as.data.frame(distm(setDT(head(airbnb,1000))[,.(longitude,latitude)], setDT(monuments)[,.(longitude,latitude)], fun=distVincentyEllipsoid)) %>%
  cbind(select(head(airbnb,1000),id))

mat3=melt(mat2, id="id") %>% mutate(variable=as.character(variable)) %>% left_join(monuments, by="variable")

# vérification de la méthode
# filter(mat3,value==min(mat3$value))
# filter(airbnb,id==9359)$listing_url


P04_dist_monuments=filter(mat3, value<=100) %>% group_by(id) %>% summarise(mon_100=n()) %>%
  full_join(filter(mat3, value<=200) %>% group_by(id) %>% summarise(mon_200=n()), by="id") %>%
  full_join(filter(mat3, value<=500) %>% group_by(id) %>% summarise(mon_500=n()), by="id") %>%
  full_join(filter(mat3, value<=1000) %>% group_by(id) %>% summarise(mon_1000=n()), by="id")
hist(P04_dist_monuments$mon_500)


saveRDS(P04_dist_monuments,"./R_data/P04_dist_monuments.RDS")
# P04_dist_monuments=readRDS("./R_data/P04_dist_monuments.RDS")
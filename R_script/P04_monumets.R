setwd("C:/DOAAT/certificat/project-ds-master")

library("dplyr")
library(httr)
library(EDFProxy) # package de la R&D dispo sur Veol pour le proxy EDF
library(XML)
library(RCurl)
library(jsonlite)
library(rvest)

auth_proxy_edf("pcy", Sys.getenv("USERNAME"), Sys.getenv("MDP_SESAME"))

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

    liens2=rbind.fill(liens2,temp)
  }, error=function(e){})
}
colnames(liens2)=c("ad")
liens3 = liens2 %>% mutate(ad=substr(ad, 10, 1000))
liens3=liens3 %>% separate(ad, c("libelle","ad", "cp_ville","autre"),",")

write.csv(liens3,"./input/monuments.csv")
# qq modifs manuelles, le fichiers d'adresse n'est pas assez propre

liens4=read.csv("./input/monuments.csv",sep=";") %>% mutate(code_postal=as.character(code_postal))
# tentative de normalisation !
auth_proxy_edf("pcy", Sys.getenv("USERNAME"), Sys.getenv("MDP_SESAME"))

write.csv2(liens4, tf <- tempfile(fileext = ".csv"),fileEncoding = "UTF-8")
res <- POST("http://api-adresse.data.gouv.fr/search/csv/", 
            timeout(600), 
            body = list(data = upload_file(tf),postcode="code_postal"))
res$status_code
res_content=content(res, encoding = "UTF-8")
colnames(res_content)=c("ad")
Adresse_norm=res_content %>% separate(ad, 
                                      c("N","libelle","ad","code_postal","latitude","longitude","result_label","result_score","result_type","result_id","result_housenumber","result_name","result_street","result_postcode","result_city","result_context","result_citycode"),
                                      ";") %>% select(libelle,latitude,longitude)




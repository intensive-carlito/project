library("dplyr")
library(httr)
library(EDFProxy) # package de la R&D dispo sur Veol pour le proxy EDF
library(XML)
library(RCurl)
library(jsonlite)
library(rvest)

auth_proxy_edf("pcy", Sys.getenv("USERNAME"), Sys.getenv("MDP_SESAME"))

library("rvest")
url <- "https://www.timeout.fr/paris/bar/100-meilleurs-bars"
liens <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="content"]/article/div/div[2]/div[1]/div[2]/div[2]/*/div[2]/header/h3/a') %>%
  xml_attr("href") %>% as.data.frame()

colnames(liens)=c("lien")
liens = liens %>% mutate(lien=ifelse(!grepl(".*timeout.*", liens$lien),paste0("https://www.timeout.fr",lien), as.character(lien)))

# boucles sur tous les bars 

liens2=data.frame()
for (i in 1:nrow(liens))
{
  tryCatch({
    print(i)
    url <- liens[i,]
    temp <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="tab___content_2"]/div/div/div') %>%
      xml_attr("data-params") %>% fromJSON %>%as.data.frame()
    liens2=rbind.fill(liens2,temp)
  }, error=function(e){})
}




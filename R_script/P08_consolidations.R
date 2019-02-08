P01_dist_RATP=readRDS("./R_data/P01_dist_RATP.RDS")
P04_dist_monuments=readRDS("./R_data/P04_dist_monuments.RDS")
quartiers=readRDS("./R_data/quartiers.rds")
airbnb=readRDS("./R_data/airbnb.RDS")

# contruction de la table avant la modélisation
# > filtre des valeurs anormales ( prix > quantile 98%)
# > traitement sur les NAs
# > ajout des valeurs externes (distances des métros, monuments et quartiers plus précis) 
# > creation de variables (longueur de la descritpion)

P08_airbnb_shiny = dplyr::select(id, longitude, latitude, )
P08_airbnb=dplyr::select(airbnb,
                           id,
                           summary, 
                           host_since,
                           host_response_time,
                           host_response_rate,
                           host_is_superhost,
                           host_total_listings_count,
                           zipcode,
                           property_type,
                           room_type,
                           accommodates,
                           bathrooms,
                           bedrooms,
                           bed_type,
                           #security_deposit,
                           guests_included,
                           minimum_nights,
                           number_of_reviews,
                           review_scores_rating,
                           cancellation_policy,
                           beds,
                           price) %>%
  mutate(delai_inscription=as.Date("2019-01-01")-host_since,
         summary_l=nchar(summary),
         zipcode=as.factor(zipcode),
         bed_type=as.factor(bed_type),
         host_response_time=as.factor(host_response_time),
         host_response_rate=floor(as.numeric(gsub('%','',host_response_rate))/10) ,  # transformation en note de 0 à 10,
         host_response_rate=ifelse(is.na(host_response_rate),10,host_response_rate),
         host_is_superhost=as.factor(host_is_superhost),
         property_type=as.factor(property_type),
         room_type=as.factor(room_type),
         #security_deposit=ceiling(security_deposit/100), # transforme en 100aine d'euros,
         minimum_nights=ifelse(minimum_nights>10, 10, minimum_nights),  # je "cape" le nb de nuits minimums
         review_scores_rating=ceiling(review_scores_rating/10),
         review_scores_rating=ifelse(is.na(review_scores_rating),10,review_scores_rating),
         cancellation_policy=as.factor(cancellation_policy),
         beds=ifelse(is.na(beds),1,beds),
         bedrooms=ifelse(is.na(bedrooms),1,bedrooms),
         bathrooms=ifelse(is.na(bathrooms),1,bathrooms)
  ) %>%
  dplyr::select(-summary) %>%
  dplyr::filter(!is.na(host_since)) %>%

  left_join(quartiers, by="id") %>%
  left_join(P01_dist_RATP, by="id") %>%
  left_join(select(P04_dist_monuments,id,mon_100=n_100,mon_200=n_200,mon_500=n_500,mon_1000=n_1000), by="id") %>%
  arrange(id) %>% filter(!is.na(l_qu))
  
# Retrait données extremes
Quant98 <- quantile(P08_airbnb$price,0.98)
P08_airbnb <- P08_airbnb %>% filter(price<=Quant98)  # temporaire pour test rapide de modele

library(naniar)
vis_miss(sample_frac(P08_airbnb,0.1))
sapply(P08_airbnb, function(y) sum(length(which(is.na(y)))))

P08_airbnb[is.na(P08_airbnb)] <- 0
saveRDS(P08_airbnb, "./R_data/P08_airbnb.rds")

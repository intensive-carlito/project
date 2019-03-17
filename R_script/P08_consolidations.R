P01_dist_RATP=readRDS("./R_data/P01_dist_RATP.RDS")
P04_dist_monuments=readRDS("./R_data/P04_dist_monuments.RDS")
quartiers=readRDS("./R_data/quartiers.rds")
airbnb=readRDS("./R_data/airbnb.RDS")

# contruction de la table avant la modélisation
# > filtre des valeurs anormales ( prix > quantile 98%)
# > traitement sur les NAs
# > ajout des valeurs externes (distances des métros, monuments et quartiers plus précis) 
# > creation de variables (longueur de la descritpion)

# P08_airbnb_shiny = dplyr::select(id, longitude, latitude, )


# dist_centre <- function(lo, la) {
#   centre_paris <- c(2.3522219, 48.856614)
#   distm(c(lo, la), centre_paris, fun=distVincentyEllipsoid)
# }
# 
# centre_paris <- c(2.3522219, 48.856614)
# airbnb <- mutate(airbnb, dist_centre=dist_centre(longitude, latitude))
# 
# abnb <- as.data.table(airbnb)
# abnb[, centre_paris:=distm(c(3, 4), c, fun=distVincentyEllipsoid)]

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
                           beds,amenities,
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
         # dist_centre=dist_centre(longitude,latitude),
         bathrooms=ifelse(is.na(bathrooms),1,bathrooms)
  ) %>%
  dplyr::select(-summary) %>%
  dplyr::filter(!is.na(host_since)) %>%

  left_join(quartiers, by="id") %>%
  left_join(P01_dist_RATP, by="id") %>%
  left_join(dplyr::select(P04_dist_monuments,id,mon_100=n_100,mon_200=n_200,mon_500=n_500,mon_1000=n_1000), by="id") %>%
  arrange(id) %>% filter(!is.na(l_qu)) %>% 
  mutate(l_qu= ifelse(l_qu %in% c("Gaillon","Vivienne", "Place-VendÃ´me","Palais-Royal"),"Gaillon-Vivienne",l_qu),
         l_qu= ifelse(l_qu %in% c("Mail"),"Bonne-nouvelle",l_qu),
         l_qu= ifelse(l_qu %in% c("Odeon","Saint-Germain-des-PrÃ©s","Monnaie","Saint-Thomas-d'Aquin","St-Germain-l'Auxerrois"),"Odéon",l_qu),
         l_qu= ifelse(l_qu %in% c("ChaussÃ©e-d'Antin","Europe","Plaine de Monceaux"),"Saint-Georges",l_qu),
         l_qu= ifelse(l_qu %in% c("Ecole-Militaire","Invalides","Madeleine","Champs-ElysÃ©es","Faubourg-du-Roule"),"Champs-ElysÃ©es",l_qu),
         l_qu= ifelse(l_qu %in% c("Bercy","Bel-Air","Arsenal"),"Picpus",l_qu),
         l_qu= ifelse(l_qu %in% c("Faubourg-Montmartre", "La Chapelle","Pont-de-Flandre"),"Villette",l_qu),
         l_qu= ifelse(l_qu %in% c("Croulebarbe", "Montparnasse","Val-de-Grace"),"Montparnasse",l_qu),
         l_qu= ifelse(l_qu %in% c("Parc-de-Montsouris"),"Maison-Blanche",l_qu),
         l_qu= ifelse(l_qu %in% c("Sorbonne","Saint-Victor","Notre-Dame","Jardin-des-Plantes","Val-de-Grace","SalpÃªtriÃ¨re"),"Jardin-des-Plantes",l_qu)) %>%
  mutate(l_qu=as.factor(l_qu)) %>%
  mutate(amenities=gsub("\\{|\\}","",amenities)) %>% mutate(nb_amen=str_count(amenities, ',')) 


# amen= as.data.frame(stringr::str_split_fixed(toto$amenities, ',',max(str_count(toto$amenities, ',')))) 
# amen= cbind(amen,select(airbnb, id))
# amen_d = reshape2::melt(amen, id.vars="id") %>% filter(value != "")
# amen_d_f=as.data.frame(table(amen_d$value))
# amen_d_f = arrange(amen_d_f,-Freq) %>% head(60)
  
# Retrait données extremes
Quant98 <- quantile(P08_airbnb$price,0.98)
P08_airbnb <- P08_airbnb %>% filter(price<=Quant98)  # temporaire pour test rapide de modele

library(naniar)
vis_miss(sample_frac(P08_airbnb,0.1))
sapply(P08_airbnb, function(y) sum(length(which(is.na(y)))))

P08_airbnb[is.na(P08_airbnb)] <- 0
saveRDS(P08_airbnb, "./R_data/P08_airbnb.rds")
airbnb_shiny=select(airbnb,id,longitude,latitude,neighbourhood_cleansed,price, picture_url) %>% inner_join(select(P09_modele, id, l_qu), by="id")
saveRDS(airbnb_shiny, "./R_data/airbnb_shiny.rds")
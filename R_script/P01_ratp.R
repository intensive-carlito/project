# airbnb

# RATP ------------------------------------------------------------------------------------------
ratp=fread("C:/temp/projet/accessibilite-des-gares-et-stations-metro-et-rer-ratp.csv",stringsAsFactors=FALSE, encoding = 'UTF-8') %>%
  filter(floor(CodeINSEE/1000)==75) %>% select(nomptar,coord) %>% unique()
ratp = ratp %>% separate(coord, c("latitude", "longitude"), sep=",") %>% 
  mutate(longitude= as.numeric(longitude),
         latitude= as.numeric(latitude)) %>%
  cbind(variable=paste0("V",c(1:nrow(ratp)))) %>% 
  mutate(variable=as.character(variable))
mat2 <- as.data.frame(distm(setDT(head(airbnb,1000))[,.(longitude,latitude)], setDT(ratp)[,.(longitude,latitude)], fun=distVincentyEllipsoid)) %>%
  cbind(select(head(airbnb,1000),id))

mat3=melt(mat2, id="id") %>% mutate(variable=as.character(variable)) %>% left_join(ratp, by="variable")

# vérification de la méthode
# filter(mat3,value==min(mat3$value))
# filter(airbnb,id==9359)$listing_url


P01_dist_RATP=filter(mat3, value<=100) %>% group_by(id) %>% summarise(n_100=n()) %>%
  full_join(filter(mat3, value<=200) %>% group_by(id) %>% summarise(n_200=n()), by="id") %>%
  full_join(filter(mat3, value<=500) %>% group_by(id) %>% summarise(n_500=n()), by="id") %>%
  full_join(filter(mat3, value<=1000) %>% group_by(id) %>% summarise(n_1000=n()), by="id")
hist(P01_dist_RATP$n_100)


saveRDS(P01_dist_RATP,"./R_data/P01_dist_RATP.RDS")
# P01_dist_RATP=readRDS("./R_data/P01_dist_RATP.RDS")
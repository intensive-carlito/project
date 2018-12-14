# # airbnb=readRDS("./R_data/airbnb.RDS")
# P01_dist_RATP=readRDS("./R_data/P01_dist_RATP.RDS")
airbnb2=head(airbnb,100)

P09_modele = select(airbnb,summary, host_name, host_since, host_location) %>% 
  mutate(host_location_ind=(host_location %like% "Paris"))
table(P09_modele$host_location_ind)

View(filter(airbnb, id==36490))
filter(airbnb, id==36490)$picture_url
toto=as.data.frame(table(airbnb$host_location))
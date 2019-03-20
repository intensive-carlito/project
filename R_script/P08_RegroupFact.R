############################# Analyse des données factorielles ##########################
# Objectif: rassembler les facteurs qui ont un nombre de donnée trop faible

# Liste variables qualitatives:
var_qual <- c("host_response_time",
              "property_type",
              "room_type",
              "bed_type",
              "cancellation_policy",
              "l_qu"
              )

### host_response_time ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
N_HRT <- P08_airbnb %>% select(host_response_time) %>%
  group_by(host_response_time) %>%
  summarise (Ndata =n() ) %>%
  arrange(Ndata)
# Pas de facteur à faible nombre: min à 1052

### property_type '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
N_ProT <- P08_airbnb %>% select(property_type,price) %>%
  group_by(property_type) %>%
  summarise (Ndata =n(),Min=min(price),Median=median(price),Max=max(price)) %>%
  arrange(Ndata)

# Facteurs à faible nombre: rassembler ceux qui sont dans des gammes similaires:
P08_airbnb$property_type <- gsub("Atypique","Other",P08_airbnb$property_type)
P08_airbnb$property_type <- gsub("Barn","Other",P08_airbnb$property_type) # une grange
P08_airbnb$property_type <- gsub("Cabin","Other",P08_airbnb$property_type) # cabine 
P08_airbnb$property_type <- gsub("Cottage","Hotel",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Dome house","Other",P08_airbnb$property_type) # une sorte d'igloo
P08_airbnb$property_type <- gsub("Dorm","Other",P08_airbnb$property_type) # Dortoir
P08_airbnb$property_type <- gsub("Nature lodge","Other",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Resort","Hotel",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Tipi","Other",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Treehouse","House",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Bungalow","Other",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Cave","Other",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Earth house","Other",P08_airbnb$property_type) # maison sous terre 
P08_airbnb$property_type <- gsub("Casa particular (Cuba)","House",P08_airbnb$property_type,fixed=TRUE) 
P08_airbnb$property_type <- gsub("Aparthotel","Serviced apartment",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Houseboat","House",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Villa","House",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Boat","Other",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Tiny house","House",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Guest suite","Other",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Guesthouse","Other",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Hostel","Hotel",P08_airbnb$property_type) 
P08_airbnb$property_type <- gsub("Townhouse","House",P08_airbnb$property_type) 
P08_airbnb$property_type <- as.factor(P08_airbnb$property_type)
# P08_airbnb = P08_airbnb %>% mutate(property_type = ifelse(property_type %in% c("Villa",""),"House", property_type))

### room_type '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
N_RoomT <- P08_airbnb %>% select(room_type) %>%
  group_by(room_type) %>%
  summarise (Ndata =n() ) %>%
  arrange(Ndata)
# minim à 443 (shared room) -> acceptable

### bed_type' ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
N_BedT <- P08_airbnb %>% select(bed_type) %>%
  group_by(bed_type) %>%
  summarise (Ndata =n() ) %>%
  arrange(Ndata)
# Facteurs à faible nombre
P08_airbnb$bed_type <- gsub("Airbed","Not bed",P08_airbnb$bed_type) # matelas gonflable
P08_airbnb$bed_type <- gsub("Futon","Not bed",P08_airbnb$bed_type)  
P08_airbnb$bed_type <- gsub("Couch","Not bed",P08_airbnb$bed_type) # canape (pas canape lit) 
P08_airbnb$bed_type <- as.factor(P08_airbnb$bed_type)

### cancellation_policy ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
N_CanP <- P08_airbnb %>% select(cancellation_policy) %>%
  group_by(cancellation_policy) %>%
  summarise (Ndata =n() ) %>%
  arrange(Ndata)
# Facteurs à faible nombre => rassembler tous les strictes
P08_airbnb$cancellation_policy <- gsub("super_strict_30","strict",P08_airbnb$cancellation_policy) 
P08_airbnb$cancellation_policy <- gsub("super_strict_60","strict",P08_airbnb$cancellation_policy) 
P08_airbnb$cancellation_policy <- gsub("strict_14_with_grace_period","strict",P08_airbnb$cancellation_policy) 
P08_airbnb$cancellation_policy <- as.factor(P08_airbnb$cancellation_policy)

### l_qu ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
N_l_qu <- P08_airbnb %>% select(l_qu) %>%
  group_by(l_qu) %>%
  summarise (Ndata =n() ) %>%
  arrange(Ndata)
# minim à 412 (Porte-Dauphine) -> acceptable

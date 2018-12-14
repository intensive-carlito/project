library("randomForest")
# # airbnb=readRDS("./R_data/airbnb.RDS")
# P01_dist_RATP=readRDS("./R_data/P01_dist_RATP.RDS")
airbnb2=head(airbnb,100)

P09_modele = select(airbnb,
                    id,
                    summary, 
                    # host_name, 
                    host_since, 
                    # host_location, 
                    host_response_time, 
                    host_response_rate,
                    host_is_superhost,
                    host_total_listings_count,
                    neighbourhood_cleansed,
                    zipcode,
                    property_type,
                    room_type,
                    accommodates,
                    bathrooms,
                    bedrooms,
                    # beds,
                    bed_type,
                    #square_feet,
                    security_deposit,
                    guests_included,
                    minimum_nights,
                    number_of_reviews,
                    review_scores_rating,
                    cancellation_policy,
                    price) %>% 
  # mutate(host_location_ind=(host_location %like% "Paris"))
  mutate(host_location_ind=grepl("\\b(?i)paris\\b",host_location),
         bedrooms=ifelse(is.na(bedrooms),0,bedrooms),
         security_deposit=ifelse(is.na(security_deposit),0,security_deposit),
         bathrooms=ifelse(is.na(bathrooms),1,bathrooms),
         host_total_listings_count=ifelse(is.na(host_total_listings_count),1,host_total_listings_count),
         review_scores_rating=ifelse(is.na(review_scores_rating),90,review_scores_rating))


train <- P09_modele %>% sample_frac(0.8)
test <- anti_join(P09_modele, train,by="id")

model <- randomForest(price ~ summary                  +host_name                +host_since               +host_location            +
                        host_response_time       +host_response_rate       +host_is_superhost        +host_total_listings_count+
                        neighbourhood_cleansed   +zipcode                  +property_type            +room_type                +
                        accommodates             +bathrooms                +bedrooms                 +bed_type                 +
                        security_deposit         +guests_included          +minimum_nights           +number_of_reviews        +
                        review_scores_rating     +cancellation_policy      +price                    +host_location_ind, 
                      data = train, ntree = 500, na.action = na.omit)
sapply(train, function(y) sum(length(which(is.nan(y)))))



mean(airbnb$review_scores_rating,na.rm=T)
View(filter(airbnb, id==36490))
filter(airbnb, id==36490)$picture_url
View(table(airbnb$bed_type))
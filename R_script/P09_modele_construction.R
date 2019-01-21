library("randomForest")
P09_modele = select(airbnb,
                    id,
                    summary, 
                    host_since,
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
                    bed_type,
                    security_deposit,
                    guests_included,
                    minimum_nights,
                    number_of_reviews,
                    review_scores_rating,
                    cancellation_policy,
                    price_cut) %>%
  mutate(delai_inscription=as.Date("2019-01-01")-host_since,
         summary_l=nchar(summary),
         zipcode=as.factor(zipcode),
         bed_type=as.factor(bed_type),
         neighbourhood_cleansed=as.factor(neighbourhood_cleansed),
         host_response_time=as.factor(host_response_time),
         host_response_rate=floor(as.numeric(gsub('%','',host_response_rate))/10) ,  # transformation en note de 0 à 10,
         host_response_rate=ifelse(is.na(host_response_rate),10,host_response_rate),
         host_is_superhost=as.factor(host_is_superhost),
         property_type=as.factor(property_type),
         room_type=as.factor(room_type),
         security_deposit=ceiling(security_deposit/100), # transforme en 100aine d'euros,
         minimum_nights=ifelse(minimum_nights>10, 10, minimum_nights),  # je "cape" le nb de nuits minimums
         review_scores_rating=ceiling(review_scores_rating/10),
         review_scores_rating=ifelse(is.na(review_scores_rating),10,review_scores_rating),
         cancellation_policy=as.factor(cancellation_policy)
  ) %>%
  select(-summary)
table(P09_modele$cancellation_policy, useNA='ifany')


P09_modele <- P09_modele %>% sample_frac(0.1)  # temporaire pour test rapide de modele

train <- P09_modele %>% sample_frac(0.8)
test <- anti_join(P09_modele, train,by="id")

model <- randomForest(price_cut ~ summary_l  +delai_inscription  +zipcode  +neighbourhood_cleansed  +accommodates  +bathrooms     + bedrooms     +bed_type 
                      +host_response_time  +host_is_superhost + host_total_listings_count + property_type + room_type + security_deposit + guests_included 
                      + minimum_nights +number_of_reviews + review_scores_rating + cancellation_policy, 
                      data = train, ntree = 100, na.action = na.omit)
varImpPlot(model)
# model
# sapply(train, function(y) sum(length(which(is.nan(y)))))

# test$predicted <- predict(model, test)
# table(test$price_cut ,test$predicted )

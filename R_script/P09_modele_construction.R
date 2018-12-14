P09_modele = select(airbnb,
                    id,
                    summary, 
                    host_since,
                    zipcode,
                    neighbourhood_cleansed,
                    accommodates,
                    bathrooms,
                    bedrooms,
                    bed_type,
                    price_cut) %>%
  mutate(delai_inscription=as.Date("2019-01-01")-host_since,
         summary_l=nchar(summary),
         zipcode=as.factor(zipcode),
         bed_type=as.factor(bed_type),
         neighbourhood_cleansed=as.factor(neighbourhood_cleansed)) %>% 
  select(-summary)


train <- P09_modele %>% sample_frac(0.8)
test <- anti_join(P09_modele, train,by="id")

model <- randomForest(price_cut ~ summary_l  +delai_inscription  +zipcode  +neighbourhood_cleansed  +accommodates  +bathrooms     + bedrooms     +bed_type   , 
                      data = train, ntree = 100, na.action = na.omit)
# sapply(train, function(y) sum(length(which(is.nan(y)))))
varImpPlot(model)
model

test$predicted <- predict(model, test)

table(test$price_cut ,test$predicted )

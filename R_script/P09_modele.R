library("randomForest")
library("caret")
###### Importation data ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
P09_modele<-readRDS("./R_data/P08_airbnb.rds")
sapply(P09_modele, function(y) sum(length(which(is.nan(y)))))

set.seed(1234)
trainIndex <- createDataPartition(P09_modele$id, p = .5,list = FALSE,times = 1)
train <- P09_modele[trainIndex,]
test <- anti_join(P09_modele, train,by="id")

model <- randomForest(price ~ bedrooms + delai_inscription + summary_l + zipcode + l_qu + bathrooms + host_total_listings_count , 
                      data = train, ntree = 100, na.action = na.omit, mtry=6)
# bedrooms + delai_inscription + summary_l + zipcode + l_qu + bathrooms + host_total_listings_count

varImpPlot(model)

test$predicted <- predict(model, test)
Metrics::rmse(test$price,test$predicted)

saveRDS(model,"./R_data/model_.rds")
test2=(data.frame(l_qu="Saint-Georges",
                  zipcode="75009",
                  bedrooms=1,
                  bathrooms=1,
                  summary_l=100L,
                  delai_inscription=difftime(Sys.time(),Sys.time(),tz="Europe/Paris","days"),
                  host_total_listings_count=0L, stringsAsFactors = F)) %>%
  mutate(l_qu=factor(l_qu,levels=levels(P09_modele$l_qu)),
         zipcode=factor(zipcode,levels=levels(P09_modele$zipcode)))
test2$predicted <- predict(model, test2)

str(select(test,l_qu,zipcode,bedrooms,bathrooms,summary_l,delai_inscription,host_total_listings_count))
str(test2)

# write.csv(P09_modele,"C:/temp/P09_modele.csv", quote = F, row.names = F)
control <- trainControl(method="repeatedcv", number=5, repeats=1)
rf_grid <- expand.grid(mtry = c(2, 3, 4, 5),
                       splitrule = c("gini"),
                       min.node.size = c(1, 3, 5))
rf_grid

rf_fit <- train(price ~ bedrooms + delai_inscription + summary_l + zipcode + l_qu + bathrooms + host_total_listings_count , 
                data = train, trControl=control, 
                method = "rf",                
                tuneGrid = rf_grid)
rf_fit

control <- trainControl(method="repeatedcv", number=5, repeats=1)
metric <- "RMSE"
set.seed(7)
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=6)
rf_default <- train(price~bedrooms + delai_inscription + summary_l + zipcode + l_qu + bathrooms + host_total_listings_count , 
                    data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)
# Random Forest 
# 
# 5740 samples
# 7 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold, repeated 1 times) 
# Summary of sample sizes: 4591, 4592, 4592, 4594, 4591 
# Resampling results:
#   
#   RMSE      Rsquared   MAE     
# 44.62674  0.5027252  30.42737
# 
# Tuning parameter 'mtry' was held constant at a value of 6
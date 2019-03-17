library("randomForest")
library("caret")
###### Importation data ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
P09_modele<-readRDS("./R_data/P08_airbnb.rds")
sapply(P09_modele, function(y) sum(length(which(is.nan(y)))))

set.seed(1234)
trainIndex <- createDataPartition(P09_modele$id, p = .8,list = FALSE,times = 1)
train <- P09_modele[trainIndex,]
test <- anti_join(P09_modele, train,by="id")

model <- randomForest(price ~ bedrooms + delai_inscription + summary_l + zipcode + l_qu + bathrooms + host_total_listings_count , 
                      data = train, ntree = 100, na.action = na.omit)
# bedrooms + delai_inscription + summary_l + zipcode + l_qu + bathrooms + host_total_listings_count

varImpPlot(model)

test$predicted <- predict(model, test)
Metrics::rmse(test$price,test$predicted)



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
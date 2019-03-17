library("randomForest")
library(mlbench)

###### Importation data ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
P09_modele<-readRDS("./R_data/P08_airbnb.rds")
sapply(P09_modele, function(y) sum(length(which(is.nan(y)))))


# Features selection
# https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/

# calculate correlation matrix
P09_modeleMatrix <- cor(select_if(P09_modele, is.numeric))


# summarize the correlation matrix
# print(P09_modeleMatrix)
# toto=as.data.frame(P09_modeleMatrix)
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(P09_modeleMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)

# Rank Features By Importance
control <- trainControl(method="repeatedcv", number=5, repeats=2)
# train the model
P09_modele_mini=sample_frac(P09_modele,0.1)
model <- train(price~., data=P09_modele_mini, method="rf", preProcess="scale", trControl=control,importance=T)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)

# > print(importance)
# rf variable importance
# 
# only 20 most important variables shown (out of 146)
# 
# Overall
# host_total_listings_count     57.45
# bedrooms                      45.73
# accommodates                  36.20
# room_typePrivate room         33.70
# n_1000                        23.07
# zipcode75006                  23.06
# property_typeBoutique hotel   22.88
# id                            20.99
# beds                          19.86
# l_quChamps-ElysÃ©es           18.85
# zipcode75004                  17.41
# host_since                    17.07
# bathrooms                     16.68
# l_quOdéon                     15.94
# delai_inscription             15.69
# room_typeShared room          14.62
# host_response_timeN/A         14.34
# zipcode75008                  13.68
# n_500                         12.72
# zipcode75016                  12.15


#python
#[(1, 'bedrooms'), (1, 'delai_inscription'), (1, 'host_since_nb'), (1, 'summary_l'), 
#    (1, 'zipcode'), (2, 'bathrooms'), (3, 'accommodates'), (4, 'number_of_reviews'), 
#    (5, 'host_total_listings_count'), (6, 'n_1000'), (7, 'l_qu')

# >> on sélectionne :
#   bedrooms + delai_inscription + host_since_nb + summary_l + zipcode + l_qu + bathrooms + host_total_listings_count 



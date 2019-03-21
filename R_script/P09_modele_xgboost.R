############################# Modele Boosting ##########################
#library(dplyr)
#library(ggplot2)
library(xgboost)
library(Matrix)
#library(plotly)

###### Importation data ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
modele_train <- readRDS("./R_data/P08_airbnb_train.rds")
modele_test <- readRDS("./R_data/P08_airbnb_test.rds")

sparse_train <- sparse.model.matrix(price ~ .-id, data = modele_train)[,-1]
sparse_test <- sparse.model.matrix(price ~ .-id, data = modele_test)[,-1]

dtrain <- xgb.DMatrix(sparse_train, label = modele_train$price)
cv <- xgb.cv(data = dtrain, nrounds = 900, nthread = 4, nfold = 5, metrics = list("rmse"),
             max_depth = 7, eta = 0.1, objective = "reg:linear")
print(cv)
print(cv, verbose=TRUE)


###### Essai basic d'entraînement XGBoost avec la librairie caret ########
library("caret")

# Paramétrage des paramètres de contrôle pour caret::train
# Nous réalisons 10 groupes de cross-validation, en répétant deux fois et avec une recherche aléatoire du réglage des hyper-paramètres.
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2, search = "random")

# Entraînement d'un modèle xgbTree avec caret::Train
P09_modele_mini <- sample_frac(P09_modele, 0.1)
model <- train(price~.-id, data = P09_modele_mini, method = "xgbTree", trControl = fitControl)
saveRDS(object = model, file = "model_xgb_10perc.rds")

# estimate variable importance
importance <- varImp(model, scale=FALSE)

print(importance)

# Instead of tree for our boosters, you can also fit a linear regression or logistic regression model using xgbLinear
# model <- train(factor(Improved)~., data = df, method = "xgbLinear", trControl = fitControl)

# See model results
print(model)

###### Selection variables ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

#' P09_modele_xgb = dplyr::select(airbnb2,
#'                            id,
#'                            summary, 
#'                            host_since,
#'                            host_response_time,
#'                            host_response_rate,
#'                            host_is_superhost,
#'                            host_total_listings_count,
#'                            zipcode,
#'                            property_type,
#'                            room_type,
#'                            accommodates,
#'                            bathrooms,
#'                            bedrooms,
#'                            bed_type,
#'                            #security_deposit,
#'                            guests_included,
#'                            minimum_nights,
#'                            number_of_reviews,
#'                            review_scores_rating,
#'                            cancellation_policy,
#'                            beds,
#'                            price) %>%
#'   mutate(delai_inscription=as.Date("2019-01-01")-host_since,
#'          summary_l=nchar(summary),
#'          zipcode=as.factor(zipcode),
#'          bed_type=as.factor(bed_type),
#'          host_response_time=as.factor(host_response_time),
#'          host_response_rate=floor(as.numeric(gsub('%','',host_response_rate))/10) ,  # transformation en note de 0 à 10,
#'          host_response_rate=ifelse(is.na(host_response_rate),10,host_response_rate),
#'          host_is_superhost=as.factor(host_is_superhost),
#'          property_type=as.factor(property_type),
#'          room_type=as.factor(room_type),
#'          #security_deposit=ceiling(security_deposit/100), # transforme en 100aine d'euros,
#'          minimum_nights=ifelse(minimum_nights>10, 10, minimum_nights),  # je "cape" le nb de nuits minimums
#'          review_scores_rating=ceiling(review_scores_rating/10),
#'          review_scores_rating=ifelse(is.na(review_scores_rating),10,review_scores_rating),
#'          cancellation_policy=as.factor(cancellation_policy),
#'          beds=ifelse(is.na(beds),1,beds),
#'          bedrooms=ifelse(is.na(bedrooms),1,bedrooms),
#'          bathrooms=ifelse(is.na(bathrooms),1,bathrooms)
#'   ) %>%
#'   dplyr::select(-summary) %>%
#'   dplyr::filter(!is.na(host_since))
#' 
#' #''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' 
#' train <- sample_frac(P09_modele_xgb, 0.8)
#' test <- anti_join(P09_modele_xgb, train, by="id")
#' sparse_matrix <- sparse.model.matrix(price ~ .-id, data = train)[,-1]
#' head(sparse_matrix)
#' price_vector <- train$price
#' 
#' bst <- xgboost(data = sparse_matrix, label = price_vector, max_depth = 4,
#'                eta = 1, nthread = 4, nrounds = 10,objective = "reg:linear")
#' 
#' # Importance des variables
#' importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
#' head(importance)
#' xgb.plot.importance(importance_matrix = importance)
#' 
#' sparse_test <- sparse.model.matrix(price ~ .-id, data = test)[,-1]
#' res <- predict(bst, sparse_test)
#' 
#' anova(model_RL2, test="Chisq")
#' diff <- res - test$price
#' plot_ly(x=~diff, type='histogram')
#' summary(diff)
#' 
#' # P09_modele = P09_modele %>% mutate(log_price=log(price))
#' # Retrait données extremes
#' Quant98 <- quantile(P09_modele$price,0.98)
#' P09_modele <- P09_modele %>% filter(price<=Quant98)%>% sample_frac(0.1)  # temporaire pour test rapide de modele



############################# Modele Gradient boosting (arbre de regression) ##########################
library(gbm)

###### Importation data ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
train<-readRDS("./R_data/P08_airbnb_train.rds")
test<-readRDS("./R_data/P08_airbnb_test.rds")
train <- train %>% dplyr::select(-id)
test <- test %>% dplyr::select(-id)

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

###### Indicateurs de comparaison des modeles ''''''''''''''''''''''''''''''''''''''''''''''
DiffMod <- read.csv("./R_script/Resultats/Diff_Model.csv", sep = ",")
DiffMod <- DiffMod  %>% select(-X)

###### Gradient boosting arbres de regression ''''''''''''''''''''''''''''''''''''''''''''''
## model_GB1 (shrinkage à 0.01)
T1<-Sys.time()
model_GB1 <- gbm(price ~.,data=train, distribution="gaussian",shrinkage = 0.01,n.trees = 100000,cv.folds = 5,n.cores=3)
T2<-Sys.time()
TdiffGB1= difftime(T2, T1)

## model_GB2 (shrinkage à 0.1)
T1<-Sys.time()
model_GB2 <- gbm(price ~.,data=train, distribution="gaussian",shrinkage = 0.1,n.trees = 100000,cv.folds = 5,n.cores=3)
T2<-Sys.time()
TdiffGB2= difftime(T2, T1)

## model_GB3 (shrinkage à 0.01,)
T1<-Sys.time()
model_GB3 <- gbm(price ~.,data=train, distribution="gaussian",shrinkage = 0.01,n.trees = 100000,cv.folds = 5,n.cores=4,interaction.depth = 2)
T2<-Sys.time()
TdiffGB3= difftime(T2, T1)

# temps de calcul
DiffMod[6,7]<- TdiffGB1/60
DiffMod[6,8]<- TdiffGB2/60
DiffMod[6,9]<- TdiffGB3

###### Analyse resultat '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## model_GB1
# Nombre d'iteration optimal
N_it_GB1 <- gbm.perf(model_GB1,method = "cv")
#Erreur modele apprentissage
train_pred_GB1 <- predict(model_GB1,train,n.trees=N_it_GB1)
# plot_ly(x=~(train_pred_GB1-train$price), type='histogram')
DiffMod[1,7]<- mean(train_pred_GB1-train$price)
DiffMod[2,7]<- var(train_pred_GB1-train$price)^(0.5)
DiffMod[3,7]<- 1-var(train_pred_GB1-train$price)/var(train$price)

#Erreur modele OOB
Res_GB1<- predict(model_GB1,test,n.trees=N_it_GB1)
Diff_GB1<-Res_GB1-test$price
DiffMod[4,7]<- mean(Diff_GB1)
DiffMod[5,7]<- var(Diff_GB1)^(0.5)

## model_GB2
# Nombre d'iteration optimal
N_it_GB2 <- gbm.perf(model_GB2,method = "cv")
#Erreur modele apprentissage
train_pred_GB2 <- predict(model_GB2,train,n.trees=N_it_GB2)
# plot_ly(x=~(train_pred_GB2-train$price), type='histogram')
DiffMod[1,8]<- mean(train_pred_GB2-train$price)
DiffMod[2,8]<- var(train_pred_GB2-train$price)^(0.5)
DiffMod[3,8]<- 1-var(train_pred_GB2-train$price)/var(train$price)

#Erreur modele OOB
Res_GB2<- predict(model_GB2,test,n.trees=N_it_GB2)
Diff_GB2<-Res_GB2-test$price
DiffMod[4,8]<- mean(Diff_GB2)
DiffMod[5,8]<- var(Diff_GB2)^(0.5)

## model_GB3
# Nombre d'iteration optimal
N_it_GB3 <- gbm.perf(model_GB3,method = "cv")
#Erreur modele apprentissage
train_pred_GB3 <- predict(model_GB3,train,n.trees=N_it_GB3)
# plot_ly(x=~(train_pred_GB3-train$price), type='histogram')
DiffMod[1,9]<- mean(train_pred_GB3-train$price)
DiffMod[2,9]<- var(train_pred_GB3-train$price)^(0.5)
DiffMod[3,9]<- 1-var(train_pred_GB3-train$price)/var(train$price)

#Erreur modele OOB
Res_GB3<- predict(model_GB3,test,n.trees=N_it_GB3)
Diff_GB3<-Res_GB3-test$price
DiffMod[4,9]<- mean(Diff_GB3)
DiffMod[5,9]<- var(Diff_GB3)^(0.5)


###### Exportation résultats  ###### '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
write.csv(DiffMod, file = "./R_script/Resultats/Diff_Model.csv",row.names=TRUE, na="")
saveRDS(model_GB1,"./shiny/R_data/model_GB1.rds")
saveRDS(model_GB2,"./shiny/R_data/model_GB2.rds")
saveRDS(model_GB3,"./shiny/R_data/model_GB3.rds")

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''



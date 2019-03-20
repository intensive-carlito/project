############################# Modele Gradient boosting (arbre de regression) ##########################
library(gbm)

###### Importation data ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
P09_modele<-readRDS("./R_data/P08_airbnb.rds")

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

###### Retrait de certaines variables (pour espace) '''''''''''''''''''''''''''''''''''''''
P09_modele <- P09_modele %>% dplyr::select(-amenities,-id)
# pour que gbm fonctionne :il faut des variables numerics ou facteur)
P09_modele$host_since <- as.numeric(P09_modele$host_since) 
P09_modele$delai_inscription <- as.numeric(P09_modele$delai_inscription)

###### Decomposition en echantillons  ###### '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# train <- P09_modele %>% sample_frac(0.8)
# test <- anti_join(P09_modele, train,by="id")
# Pour Obtenir une distribution des prix similaire pour l'apprentisage et le test:
set.seed(1234)
Id_train <- createDataPartition(P09_modele$price,1,p=0.8,list=FALSE)
train <- P09_modele[Id_train,]
test <- P09_modele[-Id_train,]

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
model_GB3 <- gbm(price ~.,data=train, distribution="gaussian",shrinkage = 0.01,n.trees = 100000,cv.folds = 5,n.cores=3,interaction.depth = 3)
T2<-Sys.time()
TdiffGB3= difftime(T2, T1)

# temps de calcul
DiffMod[6,7]<- TdiffGB1
DiffMod[6,8]<- TdiffGB2

###### Analyse resultat '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## model_GB1
# Nombre d'iteration optimal
N_it_GB1 <- gbm.perf(model_GB1,method = "cv")
#Erreur modele apprentissage
train_pred_GB1 <- predict(model_GB1,train,n.trees=N_it_GB1)
plot_ly(x=~(train_pred_GB1-train$price), type='histogram')
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
plot_ly(x=~(train_pred_GB2-train$price), type='histogram')
DiffMod[1,8]<- mean(train_pred_GB2-train$price)
DiffMod[2,8]<- var(train_pred_GB2-train$price)^(0.5)
DiffMod[3,8]<- 1-var(train_pred_GB2-train$price)/var(train$price)

#Erreur modele OOB
Res_GB2<- predict(model_GB2,test,n.trees=N_it_GB2)
Diff_GB2<-Res_GB2-test$price
DiffMod[4,8]<- mean(Diff_GB2)
DiffMod[5,8]<- var(Diff_GB2)^(0.5)

###### Exportation résultats  ###### '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
write.csv(DiffMod, file = "./R_script/Resultats/Diff_Model.csv",row.names=TRUE, na="")

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

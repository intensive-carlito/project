############################# Modele Random Forest ##########################
library("randomForest")
library("caret")
library("olsrr") # Package permettant de visualiser les resultats d'une regression
#https://cran.r-project.org/web/packages/olsrr/vignettes/intro.html
library("ggplot2")
library("MASS")
library("fitdistrplus")
library("broom")
library("dplyr")
library("plotly")
library("stringr")
library("parallel")
library("doParallel")
library("foreach")

###### Importation data ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
P09_modele<-readRDS("./R_data/P08_airbnb.rds")

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

###### Retrait de certaines variables (pour espace) ''''''''''''''''''''''
P09_modele <- P09_modele %>% dplyr::select(-amenities,-id)

###### Decomposition en echantillons  ###### '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# train <- P09_modele %>% sample_frac(0.8)
# test <- anti_join(P09_modele, train,by="id")
# Pour Obtenir une distribution des prix similaire pour l'apprentisage et le test:
set.seed(1234)
Id_train <- createDataPartition(P09_modele$price,1,p=0.8,list=FALSE)
train <- P09_modele[Id_train,]
test <- P09_modele[-Id_train,]

###### Indicateurs de comparaison des modeles ''''''''''''''''''''''''''''''
DiffMod <- read.csv("./R_script/Resultats/Diff_Model.csv", sep = ",")
DiffMod <- DiffMod  %>% select(-X)

###### calcul parallel 1 ''''''''''''''''''''''''''''''''''''''''''''''''''''
# mty = 10 (par defaut)
T1<-Sys.time()
cl <- makeCluster(detectCores())  # Detection nombre de CPU
registerDoParallel(cl)            # Initiation paralle
model_RF1 <- foreach(ntree=rep(125, detectCores()), .combine= combine, .packages='randomForest') %dopar% {
  randomForest(price ~.,
               data=train,
               ntree=ntree, na.action = na.omit,do.trace = TRUE,importance = TRUE)
}
stopCluster(cl)                   # Fin paralle
T2<-Sys.time()
Tdiff= difftime(T2, T1)
###### Fin calcul parallel 1 ''''''''''''''''''''''''''''''''''''''''''''''''''''

###### calcul 1 CPU  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''
T1<-Sys.time()
model_RF2 <- randomForest(price ~.,data = train, ntree = 500, na.action = na.omit,do.trace = TRUE,importance = TRUE)
T2<-Sys.time()
Tdiff2= difftime(T2, T1)
###### Fin calcul 1 CPU  ''''''''''''''''''''''''''''''''''''''''''''''''''''

###### calcul parallel 2 ''''''''''''''''''''''''''''''''''''''''''''''''''''
# mty = 5 au lieu de 10
T1<-Sys.time()
cl <- makeCluster(detectCores())  # Detection nombre de CPU
registerDoParallel(cl)            # Initiation paralle
model_RF3 <- foreach(ntree=rep(125, detectCores()), .combine= combine, .packages='randomForest') %dopar% {
  randomForest(price ~.,
               data=train,
               ntree=ntree, na.action = na.omit,do.trace = TRUE,importance = TRUE,mtry=5)
}
stopCluster(cl)                   # Fin paralle
T2<-Sys.time()
Tdiff_RF3= difftime(T2, T1)
###### Fin calcul parallel 2 ''''''''''''''''''''''''''''''''''''''''''''''''''''

  
# Difference entre paralle et non parallel : RF1 et RF2
Res_RF1<-predict(model_RF1,test)
Res_RF2<-predict(model_RF2,test)
Diff_RF1<-Res_RF1-test$price
mean(Diff_RF1)
var(Diff_RF1)
Diff_RF2<-Res_RF2-test$price
mean(Diff_RF2)
var(Diff_RF2)
Diff_RF1_RF2 <- Res_RF1 - Res_RF2
mean(Diff_RF1_RF2)
var(Diff_RF1_RF2)
plot_ly(x=~(Diff_RF1_RF2), type='histogram')


###### Analyse resultat '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
## RF1
#Erreur modele apprentissage
plot_ly(x=~(model_RF1$predicted-train$price), type='histogram')
DiffMod[1,4]<- mean(model_RF1$predicted-train$price)
DiffMod[2,4]<- var(model_RF1$predicted-train$price)^(0.5)
DiffMod[3,4]<- 1-sum((model_RF1$predicted-train$price)^2)/sum((train$price-mean(train$price))^2)
1-var(model_RF1$predicted-train$price)/var(train$price)

#Erreur modele OOB
Res_RF1<- predict(model_RF1,test)
Diff_RF1<-Res_RF1-test$price
DiffMod[4,4]<- mean(Diff_RF1)
DiffMod[5,4]<- var(Diff_RF1)^(0.5)

## RF2
#Erreur modele apprentissage
plot_ly(x=~(model_RF2$predicted-train$price), type='histogram')
DiffMod[1,5]<- mean(model_RF2$predicted-train$price)
DiffMod[2,5]<- var(model_RF2$predicted-train$price)^(0.5)
DiffMod[3,5]<- 1-sum((model_RF2$predicted-train$price)^2)/sum((train$price-mean(train$price))^2)
sum(model_RF2$predicted-mean(train$price))^2/sum(train$price-mean(train$price))^2

#Erreur modele OOB
Res_RF2<- predict(model_RF2,test)
Diff_RF2<-Res_RF2-test$price
DiffMod[4,5]<- mean(Diff_RF2)
DiffMod[5,5]<- var(Diff_RF2)^(0.5)

## RF3
#Erreur modele apprentissage
plot_ly(x=~(model_RF3$predicted-train$price), type='histogram')
DiffMod[1,6]<- mean(model_RF3$predicted-train$price)
DiffMod[2,6]<- var(model_RF3$predicted-train$price)^(0.5)
DiffMod[3,6]<- 1-sum((model_RF3$predicted-train$price)^2)/sum((train$price-mean(train$price))^2)
sum(model_RF3$predicted-mean(train$price))^2/sum(train$price-mean(train$price))^2

#Erreur modele OOB
Res_RF3<- predict(model_RF3,test)
Diff_RF3<-Res_RF3-test$price
DiffMod[4,6]<- mean(Diff_RF3)
DiffMod[5,6]<- var(Diff_RF3)^(0.5)

# temps de calcul
DiffMod[6,4]<- Tdiff
DiffMod[6,5]<- Tdiff2
DiffMod[6,6]<- Tdiff_RF3

####Importance des variables  
## RF 1
SignVar_RF1 <- as.data.frame(cbind(model_RF1$importance,row.names(model_RF1$importance)))
SignVar_RF1$IncNodePurity <- as.numeric(as.character(SignVar_RF1$IncNodePurity))
SignVar_RF1 <- SignVar_RF1 %>% arrange(-IncNodePurity)
# Critere mean decrease GINI
Mean_De_GINI1 <-ggplot(data=SignVar_RF1, aes(x=V2,y=IncNodePurity)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle=80))+
  scale_x_discrete(limits=SignVar_RF1$V2)
ggsave("./R_script/Resultats/Mean_De_GINI1",plot=Mean_De_GINI2,device= "png")

## RF 2
SignVar_RF2 <- as.data.frame(cbind(model_RF2$importance,row.names(model_RF2$importance)))
SignVar_RF2$IncNodePurity <- as.numeric(as.character(SignVar_RF2$IncNodePurity))
SignVar_RF2$`%IncMSE` <- as.numeric(as.character(SignVar_RF2$`%IncMSE`))
# Critere mean decrease GINI
SignVar_RF2 <- SignVar_RF2 %>% arrange(-IncNodePurity)
Mean_De_GINI2 <- ggplot(data=SignVar_RF2, aes(x=V3,y=IncNodePurity)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle=80))+
  scale_x_discrete(limits=SignVar_RF2$V3)
ggsave("./R_script/Resultats/Mean_De_GINI2",plot=Mean_De_GINI2,device= "png")
# Critere mean decrease Accuracy (pas dispo pour arbre 1)
SignVar_RF2 <- SignVar_RF2 %>% arrange(-`%IncMSE`)
Mean_De_ACC2 <-ggplot(data=SignVar_RF2, aes(x=V3,y=`%IncMSE`)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle=80))+
  scale_x_discrete(limits=SignVar_RF2$V3)
ggsave("./R_script/Resultats/Mean_De_ACC2",plot=Mean_De_ACC2,device= "png")

###### Exportation résultats  ###### '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
write.csv(DiffMod, file = "./R_script/Resultats/Diff_Model.csv",row.names=TRUE, na="")

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

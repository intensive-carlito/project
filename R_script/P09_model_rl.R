############################# Modele Regression linaire ##########################
# install.packages("olsrr")
# install.packages("fitdistrplus")
# install.packages("broom") # Pour ranger les outputs du model avec export CSV

library("caret")
library("olsrr") # Package permettant de visualiser les resultats d'une regression
#https://cran.r-project.org/web/packages/olsrr/vignettes/intro.html
library("ggplot2")
library("MASS")
library("fitdistrplus")
library("broom")
library("dplyr")
library("plotly")

###### Importation data ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
train<-readRDS("./R_data/P08_airbnb_train.rds")
test<-readRDS("./R_data/P08_airbnb_test.rds")
train <- train %>% dplyr::select(-id)
test <- test %>% dplyr::select(-id)

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

###### Visualisation data ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' # summary(P09_modele)
#' # str(P09_modele)
#' nrow(P09_modele)
#' ggplot (P09_modele,aes(x=price))+
#'   geom_density(color="darkblue", fill="lightblue") # s'apparente à une loi log normal => passage au log
#' ggplot (P09_modele,aes(x=log(price)))+
#'   geom_density(color="darkblue", fill="lightblue") 
#' 
#' #''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# 
# ###### Fitting d'une loi ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# PriceV<- P09_modele$price
# PriceV[PriceV==0] <- 1 # Prix minim de 1 dollard pour loi log normal
# # Loi normal
# fit_Norm  <- fitdist(PriceV, "norm")
# # Loi log normal => il faut que les Prix soit >0 
# fit_LogNorm  <- fitdist(PriceV, "lnorm")
# # Loi gamma 
# fit_Gam  <- fitdist(PriceV, "gamma")
# # Loi weibull 
# fit_Wei  <- fitdist(PriceV, "weibull")
# 
# # Erreur de fitting
# fit_LogNorm$
# 
# par(mfrow=c(2,2))
# plot.legend <- c("Normal", "lognormal", "gamma","Weibull")
# denscomp(list(fit_Norm,fit_LogNorm , fit_Gam, fit_Wei), legendtext = plot.legend)
# cdfcomp (list(fit_Norm,fit_LogNorm , fit_Gam, fit_Wei), legendtext = plot.legend)
# qqcomp  (list(fit_Norm,fit_LogNorm , fit_Gam, fit_Wei), legendtext = plot.legend)
# ppcomp  (list(fit_Norm,fit_LogNorm , fit_Gam, fit_Wei), legendtext = plot.legend)

# => La loi lognormal semble la loi la plus appropriée suivant le les densités, mais surtout le QQPlot et PPplot
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''



###### Decomposition en echantillons  ###### '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


# Indicateurs de comparaison des modeles
DiffMod<-matrix(0,nrow=6,ncol=9)
DiffMod[,1]<-c("Average IB error","Sigma IB error (RMSE)","R-squared","Average OB error","Sigma OB error (RMSE)","temps calcul (h)")
colnames(DiffMod)<-c("Indicateurs","ReLM","ReGLM","Random_Forest1","Random_Forest2","Random_Forest3","GB_RegTree1","GB_RegTree2","GB_RegTree3")

##### Modele non stepwise #####
# # Modele 1
# model_RL1 <- glm(price ~ zipcode, data = train, family='poisson')
# Resultat1<- predict(model_RL1,test)
# anova(model_RL1)
# Diff1<-exp(Resultat1)-test$price
# plot_ly(x=~Diff1, type='histogram')
# summary(Diff1)
# 
# # Modele 2
# model_RL2 <- glm(price ~ zipcode+bathrooms+ bedrooms, data = train, family='poisson')
# Resultat2<- predict(model_RL2,test)
# anova(model_RL2)
# Diff2<-exp(Resultat2)-test$price
# plot_ly(x=~Diff2, type='histogram')
# summary(Diff2)
# 
# # Modele 3
# model_RL3 <- glm(price ~ zipcode+bathrooms+ bedrooms+ review_scores_rating , data = train, family='poisson')
# Resultat3<- predict(model_RL3,test)
# anova(model_RL3)
# Diff3<-exp(Resultat3)-test$price
# plot_ly(x=~Diff3, type='histogram')
# summary(Diff3)

##### Modele stepwise #####
library(MASS)
### Modele Normal ###
# Fit the full model 
model_RStep <- lm(price ~., data = train)
T1<-Sys.time()

# Pour récupérer les résultats de la première régression.
#DiffMod[1,2]<- mean(model_RStep$residuals)
#DiffMod[2,2]<- summary(model_RStep)$sigma
#DiffMod[3,2]<- summary(model_RStep)$adj.r.squared
#Res_norm<- predict(model_RStep,test)
#Diff_norm<-Res_norm-test$price
#DiffMod[4,2]<- mean(Diff_norm)
#DiffMod[5,2]<- sqrt(var(Diff_norm))
#DiffMod[6,2]<- TdiffLM/3600
#rs1 = rstudent(model_RStep)
#length(rs1[rs1>=-2 & rs1<=2])/length(rs1)
#plot(rs1)
#abline(h=c(-2,0,2), col="red")

model_RStep_2 <- stepAIC(model_RStep, direction = "both", 
                            trace = FALSE)# Summary of the model
T2<-Sys.time()
TdiffLM= difftime(T2, T1)
summary(model_RStep_2)
#Erreur modele
# ols_plot_resid_fit(model_RStep_2) # Visualisation erreurs
# plot_ly(x=~model_RStep_2$residuals, type='histogram')
# plotdist(model_RStep_2$residuals,distr="norm", 
#          para=list(mean(model_RStep_2$residuals),summary(model_RStep_2)$sigma) )

DiffMod[1,2]<- mean(model_RStep_2$residuals)
DiffMod[2,2]<- summary(model_RStep_2)$sigma
DiffMod[3,2]<- summary(model_RStep_2)$adj.r.squared
# Avec metric RMSE()
# train_pred <- predict(model_RStep_2,train)
# RMSE(train_pred,train$price)

# Erreur Out of bag
Res_norm<- predict(model_RStep_2,test)
Diff_norm<-Res_norm-test$price
# plot_ly(x=~Diff_norm, type='histogram')
# summary(Diff_norm)
DiffMod[4,2]<- mean(Diff_norm)
DiffMod[5,2]<- var(Diff_norm)^(0.5)

# temps de calcul
DiffMod[6,2]<- TdiffLM/3600

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


###### Modele LogNormal Stepwise  ###### '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Model regression 
model_RLogStep <- glm(price ~., data = train, family='poisson')
# Model step_wise regression
T1<-Sys.time()
#DiffMod[1,3]<- mean(exp(model_RLogStep$residuals))
#DiffMod[2,3]<- var(exp(model_RLogStep$residuals))^(0.5)
#R2<-(1-var(exp(model_RLogStep$residuals))/var(train$price))
#R2adj<- 1-(1-R2)*(length(train$price)-1)/(length(train$price)-length(coef(summary(model_RLogStep)))-1-1)
#DiffMod[3,3]<- R2adj
# Erreur Out of bag
#Res_Lognorm<- predict(model_RLogStep,test)
#Diff_Lognorm<-exp(Res_Lognorm)-test$price
# plot_ly(x=~Diff_Lognorm, type='histogram')
# summary(Diff_Lognorm)
#DiffMod[4,3]<- mean(Diff_Lognorm)
#DiffMod[5,3]<- var(Diff_Lognorm)^(0.5)


model_RLogStep_2 <- stepAIC(model_RLogStep, direction = "both", 
                      trace = FALSE)# Summary of the model
T2<-Sys.time()
TdiffLog= difftime(T2, T1)
# summary(model_RLogStep_2)
# Erreur modele
# ols_plot_resid_fit(model_RLogStep_2) # Visualisation erreurs
# plot_ly(x=~model_RLogStep_2 $residuals, type='histogram')
# plotdist(model_RLogStep_2 $residuals,distr="norm", 
#          para=list(mean(model_RLogStep_2 $residuals),summary(model_RLogStep_2 )$sigma) )

DiffMod[1,3]<- mean(exp(model_RLogStep_2$residuals))
DiffMod[2,3]<- var(exp(model_RLogStep_2$residuals))^(0.5)
R2<-(1-var(exp(model_RLogStep_2$residuals))/var(train$price))
R2adj<- 1-(1-R2)*(length(train$price)-1)/(length(train$price)-length(coef(summary(model_RLogStep_2)))-1-1)
DiffMod[3,3]<- R2adj
# (1-summary(model_RLogStep_2 )$deviance /summary(model_RLogStep_2 )$null.deviance) 

# Erreur Out of bag
Res_Lognorm<- predict(model_RLogStep_2,test)
Diff_Lognorm<-exp(Res_Lognorm)-test$price
# plot_ly(x=~Diff_Lognorm, type='histogram')
# summary(Diff_Lognorm)
DiffMod[4,3]<- mean(Diff_Lognorm)
DiffMod[5,3]<- var(Diff_Lognorm)^(0.5)
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# temps de calcul
DiffMod[6,3]<- TdiffLog/60


###### Exportation résultats  ###### '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
write.csv(DiffMod, file = "./R_script/Resultats/Diff_Model.csv",row.names=TRUE, na="")
saveRDS(model_RLogStep_2,"./shiny/R_data/model_RLogStep_2.rds")
saveRDS(model_RStep_2,"./shiny/R_data/model_RStep_2.rds")
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''



# ### Distribution des erreurs ###
# Diff4<- as.data.frame(Diff_norm)
# colnames(Diff4)<-c("ReLM")
# Diff5<- as.data.frame(Diff_Lognorm)
# colnames(Diff5)<-c("ReGLM")
# Error<- bind_cols(Diff4,Diff5)
# nrow(Error)
# ncol(Error)
# 
# ggplot (data=Error,aes(x=ReLM))+
#   geom_density(color="darkblue")+
#   geom_density(data=Error,aes(x=ReGLM),color="red")


###### Test avec package leaps  ###### '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# # Model regression 
# library(leaps)
# model_RStep_2bis <- regsubsets(price ~., data = train,nbest=1,nvmax=NULL)



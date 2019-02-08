############################# Modele Regression linaire ##########################
library("caret")

###### Selection variables ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


table(P09_modele$beds, useNA='ifany')

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# P09_modele = P09_modele %>% mutate(log_price=log(price))

ggplot (P09_modele %>% filter(price<=Quant98),aes(x=price))+
  geom_density(color="darkblue", fill="lightblue") 

# install.packages("naniar")


###### Decomposition echantillon i ###### '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


train <- P09_modele %>% sample_frac(0.8)
test <- anti_join(P09_modele, train,by="id")
DiffMod<-matrix(0,nrow=5,ncol=2)
row.names(DiffMod)<-c("Average IB error","Variance IB error","AIC","Average IB error","Variance OB error")
colnames(DiffMod)<-c("ReLM","ReGLM")

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
model_RStep <- lm(price ~., data = P09_modele)
model_RStep_2 <- stepAIC(model_RStep, direction = "both", 
                            trace = FALSE)# Summary of the model
summary(model_RStep_2)
#Erreur modele
plot_ly(x=~model_RStep_2$residuals, type='histogram')
DiffMod[1,1]<- mean(model_RStep_2$residuals)
DiffMod[2,1]<- var(model_RStep_2$residuals)
DiffMod[3,1]<- AIC(model_RStep_2)


# Erreur Out of bag
Resultat4<- predict(model_RStep_2,test)
Diff4<-Resultat4-test$price
plot_ly(x=~Diff4, type='histogram')
summary(Diff4)
DiffMod[4,1]<- mean(Diff4)
DiffMod[5,1]<- var(Diff4)

### Modele Log Normal ###
# Fit the full model 
model_RLogStep <- glm(price ~., data = P09_modele, family='poisson')
model_RLogStep_2 <- stepAIC(model_RLogStep, direction = "both", 
                      trace = FALSE)# Summary of the model
summary(model_RLogStep_2)
#Erreur modele
plot_ly(x=~model_RLogStep_2$residuals, type='histogram')
DiffMod[1,2]<- mean(model_RLogStep_2$residuals)
DiffMod[2,2]<- var(model_RLogStep_2$residuals)
DiffMod[3,2]<- model_RLogStep_2$aic

# Erreur Out of bag
Resultat4<- predict(model_RLogStep_2,test)
Diff4<-Resultat4-test$price
plot_ly(x=~Diff4, type='histogram')
summary(Diff4)
DiffMod[4,2]<- mean(Diff4)
DiffMod[5,2]<- var(Diff4)


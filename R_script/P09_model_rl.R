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


# Modele 1
model_RL1 <- glm(price ~ zipcode, data = train, family='poisson')
Resultat1<- predict(model_RL1,test)
anova(model_RL1)
Diff1<-exp(Resultat1)-test$price
plot_ly(x=~Diff1, type='histogram')
summary(Diff1)

# Modele 2
model_RL2 <- glm(price ~ zipcode+bathrooms+ bedrooms, data = train, family='poisson')
Resultat2<- predict(model_RL2,test)
anova(model_RL2)
Diff2<-exp(Resultat2)-test$price
plot_ly(x=~Diff2, type='histogram')
summary(Diff2)

# Modele 3
model_RL3 <- glm(price ~ zipcode+bathrooms+ bedrooms+ review_scores_rating , data = train, family='poisson')
Resultat3<- predict(model_RL3,test)
anova(model_RL3)
Diff3<-exp(Resultat3)-test$price
plot_ly(x=~Diff3, type='histogram')
summary(Diff3)

##### Modele stepwise #####
library(MASS)
### Modele Log Normal ###
# Fit the full model 
model_RLogStep <- glm(price ~., data = P09_modele, family='poisson')
# Stepwise regression model
model_RLogStep_2 <- stepAIC(model_RLStep, direction = "both", 
                      trace = FALSE)# Summary of the model
summary(model_RLogStep_2)
#Erreur modele
plot_ly(model_RLogStep_2$residuals)

# Out of bag
Resultat4<- predict(model_RLogStep_2,test)
Diff4<-Resultat4-test$price
plot_ly(x=~Diff4, type='histogram')
summary(Diff4)



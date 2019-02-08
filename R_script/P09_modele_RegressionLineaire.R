############################# Modele Regression linaire ##########################
library("caret")

###### Selection variables ###### ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

P09_modele = dplyr::select(airbnb,
                    id,
                    summary, 
                    host_since,
                    host_response_time,
                    host_response_rate,
                    host_is_superhost,
                    host_total_listings_count,
                    zipcode,
                    property_type,
                    room_type,
                    accommodates,
                    bathrooms,
                    bedrooms,
                    bed_type,
                    #security_deposit,
                    guests_included,
                    minimum_nights,
                    number_of_reviews,
                    review_scores_rating,
                    cancellation_policy,
                    beds,
                    price) %>%
  mutate(delai_inscription=as.Date("2019-01-01")-host_since,
         summary_l=nchar(summary),
         zipcode=as.factor(zipcode),
         bed_type=as.factor(bed_type),
         host_response_time=as.factor(host_response_time),
         host_response_rate=floor(as.numeric(gsub('%','',host_response_rate))/10) ,  # transformation en note de 0 à 10,
         host_response_rate=ifelse(is.na(host_response_rate),10,host_response_rate),
         host_is_superhost=as.factor(host_is_superhost),
         property_type=as.factor(property_type),
         room_type=as.factor(room_type),
         #security_deposit=ceiling(security_deposit/100), # transforme en 100aine d'euros,
         minimum_nights=ifelse(minimum_nights>10, 10, minimum_nights),  # je "cape" le nb de nuits minimums
         review_scores_rating=ceiling(review_scores_rating/10),
         review_scores_rating=ifelse(is.na(review_scores_rating),10,review_scores_rating),
         cancellation_policy=as.factor(cancellation_policy),
         beds=ifelse(is.na(beds),1,beds),
         bedrooms=ifelse(is.na(bedrooms),1,bedrooms),
         bathrooms=ifelse(is.na(bathrooms),1,bathrooms)
  ) %>%
  dplyr::select(-summary) %>%
  dplyr::filter(!is.na(host_since))
table(P09_modele$beds, useNA='ifany')

#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# P09_modele = P09_modele %>% mutate(log_price=log(price))
# Retrait données extremes
Quant98 <- quantile(P09_modele$price,0.98)
P09_modele <- P09_modele %>% filter(price<=Quant98)%>% sample_frac(0.1)  # temporaire pour test rapide de modele

library(naniar)
vis_miss(P09_modele)
sapply(P09_modele, function(y) sum(length(which(is.na(y)))))

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



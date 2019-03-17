# -*- coding: utf-8 -*-
"""
Created on Thu Jun 14 09:25:51 2018

@author: B60456
"""

import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.feature_selection import RFE
from sklearn.preprocessing import LabelEncoder
from datetime import timedelta
from datetime import datetime
import numpy as np

p04_RF=pd.read_csv("c:/temp/P09_modele.csv", sep=",")
p04_RF["host_since_nb"]=(pd.to_datetime(p04_RF["host_since"], format="%Y-%m-%d") - datetime.now())
p04_RF["host_since_nb"]=p04_RF.host_since_nb/ np.timedelta64(1, 'D')

le = LabelEncoder()
p04_RF['host_is_superhost'] = le.fit_transform(p04_RF.host_is_superhost)
p04_RF['property_type'] = le.fit_transform(p04_RF.property_type)
p04_RF['room_type'] = le.fit_transform(p04_RF.room_type)
p04_RF['bed_type'] = le.fit_transform(p04_RF.bed_type)
p04_RF['cancellation_policy'] = le.fit_transform(p04_RF.cancellation_policy)
p04_RF['l_qu'] = le.fit_transform(p04_RF.l_qu)

#host_response_time
List_f=["host_since_nb","host_response_rate","host_is_superhost",
"host_total_listings_count",
"zipcode","property_type","room_type",
"accommodates","bathrooms","bedrooms","bed_type",
"guests_included","minimum_nights","number_of_reviews",
"review_scores_rating","cancellation_policy","beds",
"delai_inscription","summary_l","l_qu",
"n_100","n_200","n_500","n_1000",
"mon_100","mon_200","mon_500","mon_1000"]


#♦ RPT é RPD -----------------------------------------------------------------------------------

X_train=p04_RF.loc[:, List_f]
y_train=p04_RF["price"]

# RFE ------------ 
# Tour 1 -> RFR
rfr = RandomForestRegressor(max_depth=None,n_estimators=50,min_samples_split=4)
rfe = RFE(rfr, n_features_to_select=5)
rfe.fit(X_train,y_train)

#rank all features, i.e continue the elimination until the last one
print("Features sorted by their rank:")
print(sorted(zip(map(lambda x: round(x, 4), rfe.ranking_), List_f)))

#Features sorted by their rank:
#[(1, 'bedrooms'), (1, 'delai_inscription'), (1, 'host_since_nb'), (1, 'summary_l'), 
#    (1, 'zipcode'), (2, 'bathrooms'), (3, 'accommodates'), (4, 'number_of_reviews'), 
#    (5, 'host_total_listings_count'), (6, 'n_1000'), (7, 'l_qu'), 
#    (8, 'n_500'), (9, 'minimum_nights'), (10, 'cancellation_policy'), 
#    (11, 'beds'), (12, 'property_type'), (13, 'guests_included'), (14, 'review_scores_rating'), 
#    (15, 'room_type'), (16, 'host_response_rate'), (17, 'n_200'), 
#    (18, 'host_is_superhost'), (19, 'n_100'), (20, 'bed_type'), 
#    (21, 'mon_1000'), (22, 'mon_500'), (23, 'mon_200'), (24, 'mon_100')]


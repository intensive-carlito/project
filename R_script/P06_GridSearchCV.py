# -*- coding: utf-8 -*-
"""
Created on Tue Jun 26 09:44:34 2018

@author: B60456
"""
import xgboost as xgb
from sklearn.grid_search import GridSearchCV
from sklearn.ensemble import RandomForestRegressor
import numpy as np
import pandas as pd

def mean_absolute_percentage_error(y_true, y_pred): 
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100

List_f_RPD_RPT=["bedrooms" , "delai_inscription" , "summary_l" , "zipcode" , "l_qu" , "bathrooms" , "host_total_listings_count"]


p04_RF=pd.read_csv("c:/temp/P09_modele.csv", sep=",")
le = LabelEncoder()
p04_RF['l_qu'] = le.fit_transform(p04_RF.l_qu)


# XGB > GridSearch -------------------------------------------------------
cv_params = {'max_depth': [3,4,5,6,7], 'min_child_weight': [1,3,5]}
ind_params = {'learning_rate': 0.1, 'n_estimators': 500, 'seed':0, 'subsample': 0.8, 'colsample_bytree': 0.8, 
             'objective': 'reg:linear'}
optimized_GBM = GridSearchCV(xgb.XGBRegressor(**ind_params), 
                            cv_params, 
                             scoring = 'neg_mean_absolute_error', cv = 5, n_jobs = -1) 

optimized_GBM.fit(p04_RF[List_f_RPD_RPT], p04_RF["price"].values.ravel())
optimized_GBM.grid_scores_
optimized_GBM.best_params_
#Out[43]: {'max_depth': 7, 'min_child_weight': 5}
#20190128 Out[64]: {'max_depth': 5, 'min_child_weight': 1}

max_depth_=4
min_child_weight_=1

cv_params = {'learning_rate': [0.1, 0.01], 'subsample': [0.8,0.9, 1]}

ind_params = {'n_estimators': 1000, 'seed':0, 'colsample_bytree': 0.8, 
             'objective': 'reg:linear', 'max_depth': max_depth_, 'min_child_weight': min_child_weight_}
optimized_GBM = GridSearchCV(xgb.XGBRegressor(**ind_params), 
                            cv_params, 
                             scoring = 'neg_mean_absolute_error', cv = 5, n_jobs = -1)
optimized_GBM.fit(p04_RF[List_f_RPD_RPT], p04_RF["price"].values.ravel())
optimized_GBM.grid_scores_
optimized_GBM.best_params_
#Out[2]: {'learning_rate': 0.01, 'subsample': 0.8}
#20190128  {'learning_rate': 0.01, 'subsample': 0.9}
learning_rate_=0.1
subsample_=0.9

#Early stopping round
xgdmat = xgb.DMatrix(p04_RF[List_f_RPD_RPT], p04_RF["price"].values.ravel())
our_params = {'eta': 0.1, 'seed':0, 'subsample': subsample_, 'colsample_bytree': 0.8, 
             'objective': 'reg:linear', 'max_depth':max_depth_, 'min_child_weight':min_child_weight_, 'learning_rate': learning_rate_} 
# Grid Search CV optimized settings

cv_xgb = xgb.cv(params = our_params, dtrain = xgdmat, num_boost_round = 10000, nfold = 5,
                metrics = ['rmse'], # Make sure you enter metrics inside a list or you may encounter issues!
                early_stopping_rounds = 5) # Look for early stopping that minimizes error
cv_xgb.plot(y="test-rmse-mean")
#370







# RFR -------------------------------------------------------
cv_params = {'n_estimators': [100,300,500,1000], 'max_depth': [None], 'min_samples_split': [2,4]}
optimized_RFR = GridSearchCV(RandomForestRegressor(), 
                            cv_params, 
                             scoring = 'neg_mean_absolute_error', cv = 5, n_jobs = -1) 
optimized_RFR.fit(p04_RF[List_f_RPD_RPT], p04_RF["price"].values.ravel())
optimized_RFR.grid_scores_
optimized_RFR.best_params_
#{'max_depth': None, 'min_samples_split': 4, 'n_estimators': 1500}



















import pandas as pd
from datetime import datetime, timedelta
import xgboost as xgb
import numpy as np
from sklearn.model_selection import KFold # import KFold
from plotly.offline import plot
from plotly.graph_objs import Scatter
import pickle
from xgboost import plot_importance
from matplotlib import pyplot
from pygam import LinearGAM
from sklearn.ensemble import RandomForestRegressor

def rmse(predictions, targets):
    return np.sqrt(((predictions - targets) ** 2).mean())

P04_RF=pd.read_csv("c:/temp/P09_modele.csv", sep=",")
le = LabelEncoder()
P04_RF['l_qu'] = le.fit_transform(p04_RF.l_qu)

kf = KFold(n_splits=5, random_state=None, shuffle=False) # Define the split - into 2 folds 

List_f_RPD_RPT=["bedrooms" , "delai_inscription" , "summary_l" , "zipcode" , "l_qu" , "bathrooms" , "host_total_listings_count"]

boucle_param=pd.DataFrame()
i=0
for train_index, test_index in kf.split(P04_RF):
    i += 1
    print(str(i)+" / "+str(kf.n_splits))
    y_test=pd.DataFrame()
    X_train, X_test = P04_RF.loc[train_index][List_f_RPD_RPT], P04_RF.loc[test_index][List_f_RPD_RPT]
    y_train, y_test = P04_RF.loc[train_index]["price"], pd.DataFrame(P04_RF.loc[test_index]["price"])
    
    
    """ XGboost ----------------------------------------------- """
    xgbm = xgb.XGBRegressor(n_estimators=370, max_depth=4, min_child_weight=1, learning_rate=0.1, subsample=0.9)
    xgbm_fit=xgbm.fit(X_train,y_train)
    y_test["pred_xgbm"]=xgbm_fit.predict(X_test)
    
   
    regr_rf = RandomForestRegressor(n_estimators=100, max_depth=None,random_state=2)
    regr_rf.fit(X_train, y_train)
    y_test["pred_rf"]=regr_rf.predict(X_test)

    gam = LinearGAM(n_splines=10).fit(X_train.as_matrix(),y_train)
    y_test["pred_gam"]=gam.predict(X_test.as_matrix())    
    
    y_test["price"]=P04_RF.loc[test_index]["price"]
    y_test["ensemble"]=(y_test["pred_xgbm"]+y_test["pred_rf"]+y_test["pred_gam"])/3
        
    boucle_param=boucle_param.append(pd.DataFrame([{'KFOLD': i,
                                                    'resultat_xgbm': rmse(y_test["price"], y_test["pred_xgbm"]),                                            
                                                    'resultat_rf': rmse(y_test["price"], y_test["pred_rf"]),
                                                    'resultat_gam': rmse(y_test["price"], y_test["pred_gam"]),
                                                    'resultat_ensemble': rmse(y_test["price"], y_test["ensemble"])}]))
    print(boucle_param.tail(1))

boucle_param.mean(0)


trace1 = Scatter( x = boucle_param["KFOLD"], y = boucle_param["resultat_xgbm"],
    mode = 'lines', name = 'Prévision xgbm'  )
trace2 = Scatter( x = boucle_param["KFOLD"], y = boucle_param["resultat_ensemble"],
    mode = 'lines', name = 'Prévision resultat_ensemble'  )
trace3 = Scatter( x = boucle_param["KFOLD"], y = boucle_param["resultat_rf"],
    mode = 'lines', name = 'Prévision rf'  )
trace4 = Scatter( x = boucle_param["KFOLD"], y = boucle_param["resultat_gam"],
    mode = 'lines', name = 'Prévision gam'  )

data = [trace1,trace2,trace3,trace4]
plot(data, filename='line-mode.html')

liste = [10,50,100,150,200,250]
for i in liste:
    print(i)
    regr_rf = RandomForestRegressor(n_estimators=i, max_depth=None,random_state=2)
    regr_rf.fit(X_train, y_train)
    y_test["pred_rf"]=regr_rf.predict(X_test)
    rmse(y_test["price"], y_test["pred_rf"])
# -*- coding: utf-8 -*-
"""
Created on Sat May 22 09:28:17 2021

@author: Maxime
"""
import pandas as pd
from statsmodels.tsa.stattools import adfuller
import hurst as hu



#Augmented Dickey-Fuller test (p<0.05, data is stationary)


def ADF (data):
    ADF_p = []
    for i in range (1,6):
        ADF = adfuller(data.iloc[:,i])
        p_val = ADF[1]
        ADF_p.append(p_val)
    
    ADF_p = pd.DataFrame(ADF_p).T
    ADF_p.columns = data.columns[1:]
    return ADF_p

#Testing long memory with Hurst exponent, 0<H<0.5 (anti-persistent behavior), H=0.5 (Brownian motion), 0.5<H<1 (persistent behavior)


def Hurst(data):
    Hurst_value = []
    for i in range (1,6):
        Hurst = hu.compute_Hc(data.iloc[:,i], kind = 'change', simplified=False)
        Hurst_val = Hurst[0]
        Hurst_value.append(Hurst_val)
    
    Hurst_value = pd.DataFrame(Hurst_value).T
    Hurst_value.columns = data.columns[1:]
    return Hurst_value
from statsmodels.stats.stattools import jarque_bera
from statsmodels.tsa.stattools import adfuller
import statsmodels.api as sm
from scipy import stats
import pandas as pd
import os
import numpy as np
import warnings
warnings.filterwarnings("ignore")

def jarque_bera_test(df):
    """
    | Computes the Jarque-Bera test for each asset within the dataframe.
    | The dataframe should consists of the assets' returns where each column 
    represents a different asset.
    :param df: initil DataFrame of returns
    :return test_results: a DataFrame containing the test results for each individual asset. 
    """
    assets = df.columns.values
    test_results = pd.DataFrame(index=['jb_value', 'pvalue', 'skew', 'ex-kurtosis'], columns=assets)
    for asset in assets:
        test_results.loc[:,asset] = jarque_bera(df[asset].values)
    test_results.loc['ex-kurtosis', :] = test_results.loc['ex-kurtosis', :] - 3
    return test_results


def stationarity_test(df):
    """
    | Computes the ADF test for stationarity.
    :param df: initial DataFrame of returns
    :return test_results: a DataFrame containing the test results for each individual asset. 
    """
    assets = df.columns.values
    test_results = pd.DataFrame(index=['test statistic', 'pvalue'], columns=assets)
    for asset in assets:
        test_results.loc[:,asset] = adfuller(df.loc[:,asset])[:2]
    return test_results


def gamma(x,h):
    n, h = len(x), np.abs(h)
    x = x - x.mean()
    return np.multiply(x[:n-h], x[h:]).sum() / n

def rho(x, h):
    return gamma(x,h) / gamma(x, 0)

def asympt_gamma_matrix(x, h):
    n, h = len(x), np.abs(h)
    x = x - x.mean()
    x2 = np.square(x)
    gamma_m = np.zeros((h,h))
    for i in range(h):
        for j in range(i,h):
            gamma_m[i,j] = np.multiply(np.multiply(x[j-i:n-i-1], x[0:n-j-1]), x2[j+1:n]).sum() / n
            gamma_m[j,i] = gamma_m[i,j]
    rho_m = gamma_m / gamma(x, 0) ** 2
    return gamma_m, rho_m


def corrected_LB_test(x, h):
    n, h = len(x), np.abs(h)
    rho_m = asympt_gamma_matrix(x, h)[1]
    def compute_rho(h):
        return rho(x,h)
    acf = np.vectorize(compute_rho)(np.linspace(1,h,h).astype(int))
    test_statistic = n * np.dot(np.dot(acf.reshape((1,h)), np.linalg.inv(rho_m)), acf.reshape((h,1)))[0,0]
    pvalue = 1 - stats.chi2.cdf(test_statistic, h)
    return test_statistic, pvalue


def adjusted_Box_test(df, h):
    etfs = [x for x in df.columns]
    test_results = pd.DataFrame(index=['test statistic', 'pvalue'], columns=etfs)
    for asset in etfs:
        test_results.loc[:,asset] = corrected_LB_test(df.loc[:,asset].values, h)
    return test_results


def LM_test(x,h):
    n = len(x)
    x_2 = x**2-np.mean(x**2)
    dat = np.empty((n-h, h+1))
    dat[:] = np.nan
    for i in range(0, h+1):
        dat[:,i] = np.asarray(x_2[(h-i):(n-i)]).flatten()
    model = sm.OLS(dat[:,0], sm.add_constant(dat[:,1:(h+1)]))
    results = model.fit()
    r2 = results.rsquared
    return r2*n, (1 - stats.chi2.cdf(r2*n, h))

def LM_test_df(df, h):
    etfs = [x for x in df.columns]
    test_results = pd.DataFrame(index=['test statistic', 'pvalue'], columns=etfs)
    for asset in etfs:
        test_results.loc[:,asset] = LM_test(df.loc[:,asset].values, h)
    return test_results


def RS(x):
    n = len(x)
    vals = [np.sum(np.square(x[:k]) - np.mean(np.square(x))) for k in range(1, n)]
    return ((max(vals) - min(vals)) / np.var(x)) / np.sqrt(n)

def Long_Memory_test(df):
    etfs = [x for x in df.columns]
    test_results = pd.DataFrame(index=['test statistic'], columns=etfs)
    for asset in etfs:
        test_results.loc[:,asset] = RS(df.loc[:,asset].values)
    return test_results


def run_regression(y,x):
    model = sm.OLS(y, x)
    results = model.fit()
    res = results.summary().tables[1].as_html()
    res_df = pd.read_html(res, header=0, index_col=0)[0]
    return res_df.iloc[1,0], res_df.iloc[1,3]


def run_asym_tests(x,h):
    x = x - x.mean()
    x2 = np.square(x)
    sign = lambda e: 1 if e<0 else 0 
    sign_bias = sm.add_constant([sign(e) for e in x])
    pos_bias = sm.add_constant([max(e,0) for e in x])
    neg_bias = sm.add_constant([min(e,0) for e in x])
    
    sign_coeff, sign_pval = run_regression(x2[h:],sign_bias[:-h,:])
    neg_coeff, neg_pval = run_regression(x2[h:],neg_bias[:-h,:])
    pos_coeff, pos_pval = run_regression(x2[h:],pos_bias[:-h,:])
    test_results = pd.DataFrame(index=['coeff', 'pvalue'], columns=['sign test', 'neg test', 'pos test'])
    test_results.loc[:,'sign test'] = [sign_coeff, sign_pval]
    test_results.loc[:,'neg test'] = [neg_coeff, neg_pval]
    test_results.loc[:,'pos test'] = [pos_coeff, pos_pval]
    return test_results
    
    
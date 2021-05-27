import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib.dates import DateFormatter
import matplotlib.pylab as pylab
import numpy as np
import datetime
import os
from statsmodels.graphics.gofplots import qqplot
import scipy.stats as stats
from scipy.stats import norm
from statsmodels.tsa.stattools import pacf
import statsmodels.api as sm

params = {
         'axes.labelsize': 'x-large',
         'axes.titlesize':'x-large',
         'xtick.labelsize':'x-large',
         'ytick.labelsize':'x-large'}
pylab.rcParams.update(params)


def plot_df(df, results_path):
    num_col = np.shape(df)[1]
    etfs = [x for x in df.columns if x != 'DT']
    dates = [date[:10] for date in df.index.values]
    x_values = [datetime.datetime.strptime(d,"%Y-%m-%d").date() for d in dates]
    fig, axs = plt.subplots(3, 1, figsize=(20, 13))
    for i in range(len(etfs)):
        axs[i].plot(df[etfs[i]].values)
        axs[i].set_xticklabels(np.unique(x_values)[[0,22, 42, 64, 86, 106, 129, 149]])
        fmt_month = mdates.MonthLocator()
        axs[i].xaxis.set_minor_locator(fmt_month)
    
    plt.savefig(os.path.join(results_path, 'returns_timeseries.png'), dpi=400, facecolor='aliceblue',edgecolor='k',bbox_inches='tight')
    plt.show()


def qq_plot(df, results_path):
    etfs = [x for x in df.columns]
    fig, axs = plt.subplots(3,1,figsize=(8,10))
    for i in range(len(etfs)):
        pp = sm.ProbPlot(df[etfs[i]].values, fit=True)
        qq = pp.qqplot(markerfacecolor='royalblue', markeredgecolor='#1f77b4', alpha=1, ax=axs[i])
    sm.qqline(qq.axes[0], line='45', fmt='r--')
    sm.qqline(qq.axes[1], line='45', fmt='r--')
    sm.qqline(qq.axes[2], line='45', fmt='r--')
    plt.xlim([-5, 5])
    fig.tight_layout()
    plt.savefig(os.path.join(results_path, 'qq_plots.png'), dpi=400, facecolor='aliceblue',edgecolor='k',bbox_inches='tight')
    plt.show()
    
    
    
def gamma(x,h):
    n, h = len(x), np.abs(h)
    x = x - x.mean()
    return np.multiply(x[:n-h], x[h:]).sum() / n

def rho(x, h):
    return gamma(x,h) / gamma(x, 0)

def get_bands(x, nlag, alpha):
    n = len(x)
    def compute_rho(h):
        return rho(x,h)
    acf = np.vectorize(compute_rho)(np.linspace(1,nlag,nlag).astype(int))
    x2 = np.square(x)
    var = 1+np.vectorize(lambda h: gamma(x2,h))(np.linspace(1,nlag,nlag).astype(int)) / gamma(x, 0)**2  # why is the professor adding 1 here??
    band = np.sqrt(var / n)
    return acf, band

def plot_adjusted_acf_pacf(df, nlag, alpha, results_path, adjusted=True):
    etfs = [x for x in df.columns]
    fig, axs = plt.subplots(3, 2, figsize=(10,7))
    for i in range(len(etfs)):
        n = len(df)
        acf, band = get_bands(df[etfs[i]].values, nlag, alpha)
        if adjusted:
            axs[i,0].plot(np.linspace(1,nlag,nlag).astype(int), -norm.ppf(1 - alpha/2)*band, color='r')
            axs[i,0].plot(np.linspace(1,nlag,nlag).astype(int),norm.ppf(1 - alpha/2)*band, color='r')
        axs[i,0].plot(np.linspace(1,nlag,nlag).astype(int), acf, 'ko')
        axs[i,0].fill_between(np.linspace(1,nlag,nlag).astype(int), -norm.ppf(1 - alpha/2)/np.sqrt(n), norm.ppf(1 - alpha/2)/np.sqrt(n),  facecolor='aliceblue', interpolate=True)
        axs[i,0].plot(np.linspace(1,nlag,nlag).astype(int), [-norm.ppf(1 - alpha/2)/np.sqrt(n)] * nlag, color='#1f77b4', ls='--')
        axs[i,0].plot(np.linspace(1,nlag,nlag).astype(int), [norm.ppf(1 - alpha/2)/np.sqrt(n)] * nlag,color='#1f77b4', ls='--')
        for j in np.linspace(1,nlag,nlag).astype(int):
            axs[i,0].vlines(x=j, ymin=0, ymax=acf[j-1], colors='k', lw=2)
        axs[i,0].hlines(y=0, xmin=1, xmax=nlag, colors='k', lw=1)
        
        #pacf part
        #plot_pacf(df[etfs[i]].values, lags=nlag, ax=axs[i,1], zero=False, alpha=alpha, title=False)
        pacf_data = pacf(df[etfs[i]].values, nlags=nlag, alpha=alpha)[0][1:]
        axs[i,1].plot(np.linspace(1,nlag,nlag).astype(int), pacf_data, 'ko')
        axs[i,1].fill_between(np.linspace(1,nlag,nlag).astype(int), -norm.ppf(1 - alpha/2)/np.sqrt(n), norm.ppf(1 - alpha/2)/np.sqrt(n),  facecolor='aliceblue', interpolate=True)
        axs[i,1].plot(np.linspace(1,nlag,nlag).astype(int), [-norm.ppf(1 - alpha/2)/np.sqrt(n)] * nlag, color='#1f77b4', ls='--')
        axs[i,1].plot(np.linspace(1,nlag,nlag).astype(int), [norm.ppf(1 - alpha/2)/np.sqrt(n)] * nlag,color='#1f77b4', ls='--')
        for j in np.linspace(1,nlag,nlag).astype(int):
            axs[i,1].vlines(x=j, ymin=0, ymax=pacf_data[j-1], colors='k', lw=2)
        axs[i,1].hlines(y=0, xmin=1, xmax=nlag, colors='k', lw=1)
        min_val = 1.2*np.min([-norm.ppf(1 - alpha/2)/np.sqrt(n)]+list(acf)+list(-norm.ppf(1 - alpha/2) * band))
        max_val = 1.2*np.max([norm.ppf(1 - alpha/2)/np.sqrt(n)]+list(acf)+list(norm.ppf(1 - alpha/2) * band))
        axs[i,0].set_ylim([min_val,max_val])
        axs[i,1].set_ylim([min_val,max_val])
        
    fig.tight_layout()
    if adjusted:
        plt.savefig(os.path.join(results_path, 'returns_acf_pacf.png'), dpi=400, facecolor='aliceblue',edgecolor='k',bbox_inches='tight')
    else:
        plt.savefig(os.path.join(results_path, 'returns2_acf_pacf.png'), dpi=400, facecolor='aliceblue',edgecolor='k',bbox_inches='tight')
    plt.show()
    
    

library(arrow)
library(rugarch)
library(rmgarch)

#unlink(pkgFile)

path <- "C:/Users/Lazar/Desktop/Financial Volatility/Assignment/data.feather"
data_full <- arrow::read_feather(path)
data_full <- data.frame(data_full)
data <- data_full[1:which(data_full$DT == '2019-10-31 17:00:00'),]
data_val <- data_full[which(data_full$DT == '2019-11-01 11:00:00'):which(data_full$DT == '2019-11-29 17:00:00'),]
data_pred <- data_full[which(data_full$DT == '2019-12-02 11:00:00'):which(data_full$DT == '2019-12-31 17:00:00'),]

uspec <- ugarchspec(variance.model = list(model = 'sGARCH'))
# DCC 1,1 3 Stocks
number_ticks <- function(n) {function(limits) pretty(limits, n)}
DCC11spec <- dccspec(multispec(c(uspec, uspec, uspec)), distribution = 'mvt', model = 'DCC')
dcc11fit <- dccfit(DCC11spec, data = data[,2:4])
varcovDCC11 <- rcov(dcc11fit)
cormatDCC11 <- rcor(dcc11fit)

library(ggplot2)
library(xtable)
# Model Summary
summaryDCC11 <- show(dcc11fit)
coefDCC11 <- coef(dcc11fit)

# Conditional Variance plot
DCC11_var <- ggplot(data = data.frame('rcov' = rcov(dcc11fit)[1,1,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from DCC GARCH (1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc11fit)[2,2,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc11fit)[3,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Correlation Plot
DCC11_cor <- ggplot(data = data.frame('rcor' = rcor(dcc11fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from DCC GARCH (1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' = rcor(dcc11fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = rcor(dcc11fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))

# Conditional Covariance plot
DCC11_cov <- ggplot(data = data.frame('rcov' = rcov(dcc11fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from DCC GARCH (1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc11fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc11fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))

# Flexible DCC GARCH 1,1 model
DCC11spec_f <- dccspec(multispec(c(uspec, uspec, uspec)), distribution = 'mvnorm', model = 'FDCC', groups = seq(1,3))
dcc11fit_f <- dccfit(DCC11spec_f, data = data[,2:4])
varcovDCC11_f <- rcov(dcc11fit_f)
cormatDCC11_f <- rcor(dcc11fit_f)

# Model Summary
summaryDCC11_f <- show(dcc11fit_f)
coefDCC11_f <- coef(dcc11fit_f)

# Conditional Variance plot
DCC11_f_var <- ggplot(data = data.frame('rcov' = rcov(dcc11fit_f)[1,1,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from Asymetric DCC GARCH (1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc11fit_f)[2,2,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc11fit_f)[3,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Correlation Plot
DCC11_f_cor <- ggplot(data = data.frame('rcor' = rcor(dcc11fit_f)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from Asymetric DCC GARCH (1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' = rcor(dcc11fit_f)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = rcor(dcc11fit_f)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))

# Conditional Covariance plot
DCC11_f_cov <- ggplot(data = data.frame('rcov' = rcov(dcc11fit_f)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from Asymetric DCC GARCH (1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc11fit_f)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc11fit_f)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))

# Asymetric DCC GARCH 1,1 model
DCC11spec_a <- dccspec(multispec(c(uspec, uspec, uspec)), distribution = 'mvt', model = "aDCC")
dcc11fit_a <- dccfit(DCC11spec_a, data = data[,2:4])
varcovDCC11_a <- rcov(dcc11fit_a)
cormatDCC11_a <- rcor(dcc11fit_a)

# Model Summary
summaryDCC11_a <- show(dcc11fit_a)
coefDCC11_a <- coef(dcc11fit_a)

# Conditional Variance plot
DCC11_a_var <- ggplot(data = data.frame('rcov' = rcov(dcc11fit_a)[1,1,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from Asymetric DCC GARCH (1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc11fit_a)[2,2,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc11fit_a)[3,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Correlation Plot
DCC11_a_cor <- ggplot(data = data.frame('rcor' = rcor(dcc11fit_a)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from Asymetric DCC GARCH (1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' = rcor(dcc11fit_a)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = rcor(dcc11fit_a)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))

# Conditional Covariance plot
DCC11_a_cov <- ggplot(data = data.frame('rcov' = rcov(dcc11fit_a)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from Asymetric DCC GARCH (1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc11fit_a)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc11fit_a)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))

#GO-GARCH (1,1)
GGARCHspec <- gogarchspec(mmean.model = 'AR')
GGARCHfit <-gogarchfit(GGARCHspec, data[,2:4])
varcovGGARCH <- rcov(GGARCHfit)
cormatGGARCH <- rcor(GGARCHfit)

#Model Summary
summaryGGARCH <- show(GGARCHfit)
coefGGARCH <- coef(GGARCHfit)

# Conditional Variance plot
GGARCH_var <- ggplot(data = data.frame('rcov' = rcov(GGARCHfit)[1,1,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from GO-GARCH') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(GGARCHfit)[2,2,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(GGARCHfit)[3,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Correlation Plot
GGARCH_cor <- ggplot(data = data.frame('rcor' = rcor(GGARCHfit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from GO-GARCH') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' = rcor(GGARCHfit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = rcor(GGARCHfit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))

# Conditional Covariance plot
GGARCH_cov <- ggplot(data = data.frame('rcov' = rcov(GGARCHfit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from GO-GARCH') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(GGARCHfit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(GGARCHfit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))


# Finding Optimal Univariate Settings as basis for multivariate models
uspec <- ugarchspec(variance.model = list(model = 'sGARCH'))

ugarchfit(spec = uspec, data[,2], distribution = 'mvt')
ugarchfit(spec = uspec, data[,3], distribution = 'mvt')
ugarchfit(spec = uspec, data[,4], distribution = 'mvt')

# Sign Bias in 1 Series ('EZU') and weak significance in 'EEM' (10%)

models_list = list(c('sGARCH','gjrGARCH', 'eGARCH', 'iGARCH', 'csGARCH', 'apARCH', 'fGARCH', 'fGARCH', 'fGARCH'))
submodels_list = list(c('GARCH','TGARCH','GJRGARCH'))

BIC_mat <- matrix(NA, nrow = lengths(models_list), ncol = 3)
rownames(BIC_mat) <- c('sGARCH','gjrGARCH', 'eGARCH', 'iGARCH', 'csGARCH', 'apARCH', 'fGARCH','fTGARCH','fGJRGARCH')
colnames(BIC_mat) <- c('EEM', 'SPY', 'EZU')

for (y in 2:length(data)){
  for (i in 1:lengths(models_list)){
    if (i >= 7){
      fit <- ugarchfit(ugarchspec(variance.model = list(model = models_list[[1]][i], submodel = submodels_list[[1]][i-6]), distribution.model = 'std'), data[,y])
      fit_val <- ugarchfit(ugarchspec(variance.model = list(model = models_list[[1]][i], submodel = submodels_list[[1]][i-6]),fixed.pars = list(coef(fit)), distribution.model = 'std'), data_val[,y])
      BIC_mat[i,y-1] <- infocriteria(fit_val)[2]
      } else if (i<7){
      fit <- ugarchfit(ugarchspec(variance.model = list(model = models_list[[1]][i]), distribution.model = 'std'), data[,y])
      fit_val <- ugarchfit(ugarchspec(variance.model = list(model = models_list[[1]][i]), fixed.pars = list(coef(fit)), distribution.model = 'std'), data_val[,y])
      BIC_mat[i,y-1] <- infocriteria(fit_val)[2]
    }
      }
}


min(BIC_mat[,1]) # Optimal Specification: EGARCH
min(BIC_mat[,2]) # Optimal Specification: EGARCH
min(BIC_mat[,3]) # Optimal Specification: fTGARCH

uspec_opt1 <- ugarchspec(variance.model = list(model = 'eGARCH'), distribution.model = 'std')
uspec_opt2 <- ugarchspec(variance.model = list(model = 'eGARCH'), distribution.model = 'std')
uspec_opt3 <- ugarchspec(variance.model = list(model = 'eGARCH', submodel = 'TGARCH'), distribution.model = 'std')

# DCC optimum 3 Stocks
DCC_opt_spec <- dccspec(multispec(c(uspec_opt1, uspec_opt2, uspec_opt3)), distribution = 'mvt', model = 'DCC')
dcc_opt_fit <- dccfit(DCC_opt_spec, data = data[,2:4])
varcovDCC_opt <- rcov(dcc_opt_fit)
cormatDCC_opt <- rcor(dcc_opt_fit)
# Model Summary
summaryDCC_opt <- show(dcc_opt_fit)
coefDCC_opt <- coef(dcc_opt_fit)

# Conditional Variance plot
DCC_opt_var <- ggplot(data = data.frame('rcov' = rcov(dcc_opt_fit)[1,1,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from DCC(1,1) and optimal univariate models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc_opt_fit)[2,2,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc_opt_fit)[3,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Correlation Plot
DCC_opt_cor <- ggplot(data = data.frame('rcor' = rcor(dcc_opt_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from DCC(1,1) and optimal univariate models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' = rcor(dcc_opt_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = rcor(dcc_opt_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))

# Conditional Covariance plot
DCC_opt_cov <- ggplot(data = data.frame('rcov' = rcov(dcc_opt_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from DCC(1,1) and optimal univariate models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc_opt_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc_opt_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))


# Asymetric DCC optimum 3 Stocks
DCC_opt_a_spec <- dccspec(multispec(c(uspec_opt1, uspec_opt2, uspec_opt3)), distribution = 'mvt', model = "aDCC")
dcc_opt_a_fit <- dccfit(DCC_opt_a_spec, data = data[,2:4])
varcovDCC_a_opt <- rcov(dcc_opt_a_fit)
cormatDCC_a_opt <- rcor(dcc_opt_a_fit)
# Model Summary
summaryDCC_a_opt <- show(dcc_opt_a_fit)
coefDCC_a_opt <- coef(dcc_opt_a_fit)

# Conditional Variance plot
DCC_a_opt_var <- ggplot(data = data.frame('rcov' = rcov(dcc_opt_a_fit)[1,1,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from Asymetric DCC(1,1) and optimal univariate models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc_opt_a_fit)[2,2,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc_opt_a_fit)[3,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Correlation Plot
DCC_a_opt_cor <- ggplot(data = data.frame('rcor' = rcor(dcc_opt_a_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from Asymetric DCC(1,1) and optimal univariate models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' = rcor(dcc_opt_a_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = rcor(dcc_opt_a_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))

# Conditional Covariance plot
DCC_a_opt_cov <- ggplot(data = data.frame('rcov' = rcov(dcc_opt_a_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from Asymetric DCC(1,1) and optimal univariate models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc_opt_a_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc_opt_a_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))

# Flexbile DCC optimum 3 Stocks
DCC_opt_f_spec <- dccspec(multispec(c(uspec_opt1, uspec_opt2, uspec_opt3)), model = "FDCC", groups = c(1,2,3))
dcc_opt_f_fit <- dccfit(DCC_opt_f_spec, data = data[,2:4])
varcovDCC_f_opt <- rcov(dcc_opt_f_fit)
cormatDCC_f_opt <- rcor(dcc_opt_f_fit)
# Model Summary
summaryDCC_f_opt <- show(dcc_opt_f_fit)
coefDCC_f_opt <- coef(dcc_opt_f_fit)

# Conditional Variance plot
DCC_f_opt_var <- ggplot(data = data.frame('rcov' = rcov(dcc_opt_f_fit)[1,1,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from Flexible DCC(1,1) and optimal univariate models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc_opt_f_fit)[2,2,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc_opt_f_fit)[3,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Correlation Plot
DCC_f_opt_cor <- ggplot(data = data.frame('rcor' = rcor(dcc_opt_f_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from Flexible DCC(1,1) and optimal univariate models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' = rcor(dcc_opt_f_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = rcor(dcc_opt_f_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))

# Conditional Covariance plot
DCC_f_opt_cov <- ggplot(data = data.frame('rcov' = rcov(dcc_opt_f_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from Flexible DCC(1,1) and optimal univariate models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(dcc_opt_f_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(dcc_opt_f_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))


# Copula DCC GARCH(1,1)
CGARCH11spec <- cgarchspec(multispec(c(uspec, uspec, uspec)), distribution.model = list(copula = c('mvt'), time.varying = T))
cgarch11_fit <- cgarchfit(CGARCH11spec, data = data[,2:4])
cgarch11_cov <- rcov(cgarch11_fit)
cgarch11_cor <- rcor(cgarch11_fit)
# Model Summary
summaryCGARCH11 <- show(cgarch11_fit)
coefCGARCH11 <- coef(cgarch11_fit)

# Conditional Variance plot
CGARCH11_var <- ggplot(data = data.frame('rcov' = rcov(cgarch11_fit)[1,1,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from Copula with GARCH(1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(cgarch11_fit)[2,2,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(cgarch11_fit)[3,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Correlation Plot
CGARCH11_cor <- ggplot(data = data.frame('rcor' = rcor(cgarch11_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from Copula with GARCH(1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' = rcor(cgarch11_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = rcor(cgarch11_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))

# Conditional Covariance plot
CGARCH11_cov <- ggplot(data = data.frame('rcov' = rcov(cgarch11_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from Copula with GARCH(1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(cgarch11_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(cgarch11_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))

# Asymetric Copula DCC GARCH(1,1)
CGARCH11spec_a <- cgarchspec(multispec(c(uspec, uspec, uspec)), distribution.model = list(copula = c('mvt'), time.varying = T, asymetric = T))
cgarch11_a_fit <- cgarchfit(CGARCH11spec_a, data = data[,2:4])
cgarch11_a_cov <- rcov(cgarch11_a_fit)
cgarch11_a_cor <- rcor(cgarch11_a_fit)
# Model Summary
summaryCGARCH11_a <- show(cgarch11_fit)
coefCGARCH11_a <- coef(cgarch11_fit)

# Conditional Variance plot
CGARCH11_a_var <- ggplot(data = data.frame('rcov' = rcov(cgarch11_a_fit)[1,1,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from Asymetric Copula with GARCH(1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(cgarch11_a_fit)[2,2,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(cgarch11_a_fit)[3,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Correlation Plot
CGARCH11_a_cor <- ggplot(data = data.frame('rcor' = rcor(cgarch11_a_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from Asymetric Copula with GARCH(1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' = rcor(cgarch11_a_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = rcor(cgarch11_a_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))

# Conditional Covariance plot
CGARCH11_a_cov <- ggplot(data = data.frame('rcov' = rcov(cgarch11_a_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from Asymetric Copula with GARCH(1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(cgarch11_a_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(cgarch11_a_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))


# Copula DCC with optimal models
CGARCH_opt_spec <- cgarchspec(multispec(c(uspec_opt1, uspec_opt2, uspec_opt3)), distribution.model = list(copula = c('mvt'), time.varying = T))
cgarch_opt_fit <- cgarchfit(CGARCH_opt_spec, data = data[,2:4])
cgarch_opt_cov <- rcov(cgarch_opt_fit)
cgarch_opt_cor <- rcor(cgarch_opt_fit)
# Model Summary
summaryCGARCH_opt <- show(cgarch_opt_fit)
coefCGARCH_opt <- coef(cgarch_opt_fit)

# Conditional Variance plot
CGARCH_opt_var <- ggplot(data = data.frame('rcov' = rcov(cgarch_opt_fit)[1,1,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from Copula with optimal univaraite models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(cgarch_opt_fit)[2,2,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(cgarch_opt_fit)[3,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Correlation Plot
CGARCH_opt_cor <- ggplot(data = data.frame('rcor' = rcor(cgarch_opt_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from Copula with optimal univaraite models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' = rcor(cgarch_opt_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = rcor(cgarch_opt_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))

# Conditional Covariance plot
CGARCH_opt_cov <- ggplot(data = data.frame('rcov' = rcov(cgarch_opt_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from Copula with optimal univaraite models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(cgarch_opt_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(cgarch_opt_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))

# Asymetric Copula DCC with optimal models
CGARCH_opt_a_spec <- cgarchspec(multispec(c(uspec_opt1, uspec_opt2, uspec_opt3)), distribution.model = list(copula = c('mvt'), time.varying = T, asymetric = T))
cgarch_opt_a_fit <- cgarchfit(CGARCH_opt_a_spec, data = data[,2:4])
cgarch_opt_a_cov <- rcov(cgarch_opt_a_fit)
cgarch_opt_a_cor <- rcor(cgarch_opt_a_fit)
# Model Summary
summaryCGARCH_opt_a <- show(cgarch11_fit)
coefCGARCH_opt_a <- coef(cgarch11_fit)

# Conditional Variance plot
CGARCH_opt_a_var <- ggplot(data = data.frame('rcov' = rcov(cgarch_opt_a_fit)[1,1,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from Asymetric Copula with optimal univaraite models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(cgarch_opt_fit)[2,2,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(cgarch_opt_a_fit)[3,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Correlation Plot
CGARCH_opt_a_cor <- ggplot(data = data.frame('rcor' = rcor(cgarch_opt_a_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from Asymetric Copula with optimal univaraite models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' = rcor(cgarch_opt_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = rcor(cgarch_opt_a_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))

# Conditional Covariance plot
CGARCH_opt_a_cov <- ggplot(data = data.frame('rcov' = rcov(cgarch_opt_a_fit)[1,2,], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from Asymetric Copula with optimal univaraite models') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = rcov(cgarch_opt_fit)[1,3,], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = rcov(cgarch_opt_a_fit)[2,3,], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))



# Fit model with optional fit option 
validation_models = list()
validation_models$dcc11fit_val <- dccfit(DCC11spec, data = data_val[,2:4], fit = dcc11fit)
validation_models$dcc11fit_f_val <- dccfit(DCC11spec_f, data = data_val[,2:4], fit = dcc11fit_f)
validation_models$dcc11fit_a_val <- dccfit(DCC11spec_a, data = data_val[,2:4], fit = dcc11fit_a)
validation_models$GGARCHfit_val <-gogarchfit(GGARCHspec, data_val[,2:4])
validation_models$dcc_opt_fit_val <- dccfit(DCC_opt_spec, data = data_val[,2:4], fit = dcc_opt_fit)
validation_models$dcc_opt_a_fit_val <- dccfit(DCC_opt_a_spec, data = data_val[,2:4], fit = dcc_opt_a_fit)
validation_models$dcc_opt_f_fit_val <-dccfit(DCC_opt_f_spec, data = data_pred[, 2:4], fit = dcc_opt_f_fit)

validation_models$cgarch11_fit_val <- cgarchfit(CGARCH11spec, data = data[,2:4], fit = cgarch11_fit)
validation_models$cgarch11_a_fit_val <- cgarchfit(CGARCH11spec_a, data = data_val[,2:4], fit = cgarch11_a_fit)
validation_models$cgarch_opt_fit_val <- cgarchfit(CGARCH_opt_spec, data = data_val[,2:4], fit = cgarch_opt_fit)
validation_models$cgarch_opt_a_fit_val <- cgarchfit(CGARCH_opt_a_spec, data = data_val[,2:4], fit = cgarch_opt_a_fit)

infoIC_table <- matrix(NA, nrow = 11, ncol = 2)
colnames(infoIC_table) <- c('BIC', 'AIC')
rownames(infoIC_table) <- c('DCC11','DCC11_F', 'DCC11_A', 'GGARCH11', 'DCC11_opt', 'DCC11_OPT_A','DCC11_OPT_F', 'CGARCH11', 'CGARCH11_A', 'CGARCH11_opt', 'CGARCH11_opt_A')


infoIC_table[1,1] <- infocriteria(validation_models$dcc11fit_val)[1]
infoIC_table[1,2] <- infocriteria(validation_models$dcc11fit_val)[2]
infoIC_table[2,1] <- infocriteria(validation_models$dcc11fit_f_val)[1]
infoIC_table[2,2] <- infocriteria(validation_models$dcc11fit_f_val)[2]
infoIC_table[3,1] <- infocriteria(validation_models$dcc11fit_a_val)[1]
infoIC_table[3,2] <- infocriteria(validation_models$dcc11fit_a_val)[2]
infoIC_table[4,2] <- 2*length(coef(validation_models$GGARCHfit_val)) - 2*log(likelihood(validation_models$GGARCHfit_val))
infoIC_table[4,1] <- length(coef(validation_models$GGARCHfit_val))*log(nrow(data_val)) - 2*log(likelihood(validation_models$GGARCHfit_val))
infoIC_table[5,1] <- infocriteria(validation_models$dcc_opt_fit_val)[1]
infoIC_table[5,2] <- infocriteria(validation_models$dcc_opt_fit_val)[2]
infoIC_table[6,1] <- infocriteria(validation_models$dcc_opt_a_fit_val)[1]
infoIC_table[6,2] <- infocriteria(validation_models$dcc_opt_a_fit_val)[2]
infoIC_table[7,1] <- infocriteria(validation_models$dcc_opt_f_fit_val)[1]
infoIC_table[7,2] <- infocriteria(validation_models$dcc_opt_f_fit_val)[2]
infoIC_table[8,1] <- -34.245 # as seen from the show() option as no infocriteria() available for Copula-Garch
infoIC_table[8,2] <- -34.284
infoIC_table[9,1] <- -36.792
infoIC_table[9,2] <- -36.969
infoIC_table[10,1] <- -36.879
infoIC_table[10,2] <- -37.082
infoIC_table[11,1] <- -36.879
infoIC_table[11,2] <- -37.082

# TOP 3 MODELS FOR PREDICTIONS
Rfast::nth(infoIC_table[,1], 1, descending = F, na.rm = T)
Rfast::nth(infoIC_table[,1], 2, descending = F, na.rm = T)
Rfast::nth(infoIC_table[,1], 3, descending = F, na.rm = T)

Rfast::nth(infoIC_table[,2], 4, descending = F, na.rm = T)
Rfast::nth(infoIC_table[,2], 2, descending = F, na.rm = T)
Rfast::nth(infoIC_table[,2], 3, descending = F, na.rm = T)

# DCC 1,1 with optimal univariate models
# Asymetric DCC 1,1 with optimal univariate models
# DCC 1,1 with GARCH 1,1

# With AIC
# Copula DCC 1,1 with optimal univariate models
# Asymetric Copula DCC 1,1 with optimal univariate models
# DCC 1,1 with optimal univariate models


# Predictions

rolling_predictions1 <- data.frame(data = matrix(NA, nrow = nrow(data_pred), ncol = 9))
rolling_predictions2 <- data.frame(data = matrix(NA, nrow = nrow(data_pred), ncol = 9))
rolling_predictions3 <- data.frame(data = matrix(NA, nrow = nrow(data_pred), ncol = 9))

rolling_predictions1$Date <- data_pred$DT
rolling_predictions2$Date <- data_pred$DT
rolling_predictions3$Date <- data_pred$DT

colnames(rolling_predictions1) <- c('varEEM', 'varSPY', 'varEZU', 'cov(EEM,SPY)', 'cov(EEM, EZU)', 'cov(SPY, EZU)', 'cor(EEM,SPY)', 'cor(EEM, EZU)', 'cor(SPY, EZU)', 'DT')
colnames(rolling_predictions2) <- c('varEEM', 'varSPY', 'varEZU', 'cov(EEM,SPY)', 'cov(EEM, EZU)', 'cov(SPY, EZU)', 'cor(EEM,SPY)', 'cor(EEM, EZU)', 'cor(SPY, EZU)', 'DT')
colnames(rolling_predictions3) <- c('varEEM', 'varSPY', 'varEZU', 'cov(EEM,SPY)', 'cov(EEM, EZU)', 'cov(SPY, EZU)', 'cor(EEM,SPY)', 'cor(EEM, EZU)', 'cor(SPY, EZU)', 'DT')

start_time <- Sys.time()
for (i in 1:nrow(data_pred)){
out_sample <- nrow(data_pred) - i + 1
fit_pred1 <- dccfit(DCC_opt_spec, data = data_full[,2:4], out.sample = out_sample)
fit_pred2 <- dccfit(DCC_opt_a_spec, data = data_full[,2:4], out.sample = out_sample)
fit_pred3 <- dccfit(DCC11spec, data = data_full[,2:4], out.sample = out_sample)
rolling_predictions1[i,1] <- rcov(dccforecast(fit_pred1, n.ahead = 1))[[1]][1]
rolling_predictions1[i,2] <- rcov(dccforecast(fit_pred1, n.ahead = 1))[[1]][5]
rolling_predictions1[i,3] <- rcov(dccforecast(fit_pred1, n.ahead = 1))[[1]][9]
rolling_predictions1[i,4] <- rcov(dccforecast(fit_pred1, n.ahead = 1))[[1]][2]
rolling_predictions1[i,5] <- rcov(dccforecast(fit_pred1, n.ahead = 1))[[1]][3]
rolling_predictions1[i,6] <- rcov(dccforecast(fit_pred1, n.ahead = 1))[[1]][6]
rolling_predictions1[i,7] <- rcor(dccforecast(fit_pred1, n.ahead = 1))[[1]][2]
rolling_predictions1[i,8] <- rcor(dccforecast(fit_pred1, n.ahead = 1))[[1]][3]
rolling_predictions1[i,9] <- rcor(dccforecast(fit_pred1, n.ahead = 1))[[1]][6]

rolling_predictions2[i,1] <- rcov(dccforecast(fit_pred2, n.ahead = 1))[[1]][1]
rolling_predictions2[i,2] <- rcov(dccforecast(fit_pred2, n.ahead = 1))[[1]][5]
rolling_predictions2[i,3] <- rcov(dccforecast(fit_pred2, n.ahead = 1))[[1]][9]
rolling_predictions2[i,4] <- rcov(dccforecast(fit_pred2, n.ahead = 1))[[1]][2]
rolling_predictions2[i,5] <- rcov(dccforecast(fit_pred2, n.ahead = 1))[[1]][3]
rolling_predictions2[i,6] <- rcov(dccforecast(fit_pred2, n.ahead = 1))[[1]][6]
rolling_predictions2[i,7] <- rcor(dccforecast(fit_pred2, n.ahead = 1))[[1]][2]
rolling_predictions2[i,8] <- rcor(dccforecast(fit_pred2, n.ahead = 1))[[1]][3]
rolling_predictions2[i,9] <- rcor(dccforecast(fit_pred2, n.ahead = 1))[[1]][6]

rolling_predictions3[i,1] <- rcov(dccforecast(fit_pred3, n.ahead = 1))[[1]][1]
rolling_predictions3[i,2] <- rcov(dccforecast(fit_pred3, n.ahead = 1))[[1]][5]
rolling_predictions3[i,3] <- rcov(dccforecast(fit_pred3, n.ahead = 1))[[1]][9]
rolling_predictions3[i,4] <- rcov(dccforecast(fit_pred3, n.ahead = 1))[[1]][2]
rolling_predictions3[i,5] <- rcov(dccforecast(fit_pred3, n.ahead = 1))[[1]][3]
rolling_predictions3[i,6] <- rcov(dccforecast(fit_pred3, n.ahead = 1))[[1]][6]
rolling_predictions3[i,7] <- rcor(dccforecast(fit_pred3, n.ahead = 1))[[1]][2]
rolling_predictions3[i,8] <- rcor(dccforecast(fit_pred3, n.ahead = 1))[[1]][3]
rolling_predictions3[i,9] <- rcor(dccforecast(fit_pred3, n.ahead = 1))[[1]][6]
}
end_time <- Sys.time() # Approx: 3.4-3.5 hours
end_time - start_time

library(mvtnorm)

n=length(rolling_predictions1[,1])
perf=rep(0,length(rolling_predictions1[,1])) 
for (i in 1:length(rolling_predictions1[,1]))
{
    perf[i]= length(rolling_predictions1[,1])*(-log(dmvnorm(data_pred[i,2:4],mean=as.numeric(colMeans(data_pred[,2:4])),sigma=array(c(rolling_predictions1[i,1], rolling_predictions1[i,4], rolling_predictions1[i,5], rolling_predictions1[i,4], rolling_predictions1[i,2], rolling_predictions1[i,6], rolling_predictions1[i,5], rolling_predictions1[i,6], rolling_predictions1[i,3]), dim = c(3,3))))+log(dmvnorm(data_pred[i,2:4],mean=as.numeric(colMeans(data_pred[,2:4])),sigma=array(c(rolling_predictions2[i,1], rolling_predictions2[i,4], rolling_predictions2[i,5], rolling_predictions2[i,4], rolling_predictions2[i,2], rolling_predictions2[i,6], rolling_predictions2[i,5], rolling_predictions2[i,6], rolling_predictions2[i,3]), dim = c(3,3))))) #QLIKE
}

sqrt(n)*mean(perf)/sqrt(spectrum(perf)$spec[1]) 
1-pnorm(sqrt(n)*mean(perf)/sqrt(spectrum(perf)$spec[1]))



n=length(rolling_predictions3[,1])
perf=rep(0,length(rolling_predictions3[,1])) 
for (i in 1:length(rolling_predictions3[,1]))
{
  perf[i]= length(rolling_predictions1[,1])*(-log(dmvnorm(data_pred[i,2:4],mean=as.numeric(colMeans(data_pred[,2:4])),sigma=array(c(rolling_predictions1[i,1], rolling_predictions1[i,4], rolling_predictions1[i,5], rolling_predictions1[i,4], rolling_predictions1[i,2], rolling_predictions1[i,6], rolling_predictions1[i,5], rolling_predictions1[i,6], rolling_predictions1[i,3]), dim = c(3,3))))+log(dmvnorm(data_pred[i,2:4],mean=as.numeric(colMeans(data_pred[,2:4])),sigma=array(c(rolling_predictions3[i,1], rolling_predictions3[i,4], rolling_predictions3[i,5], rolling_predictions3[i,4], rolling_predictions3[i,2], rolling_predictions3[i,6], rolling_predictions3[i,5], rolling_predictions3[i,6], rolling_predictions3[i,3]), dim = c(3,3))))) #QLIKE
}

sqrt(n)*mean(perf)/sqrt(spectrum(perf)$spec[1]) 
1-pnorm(sqrt(n)*mean(perf)/sqrt(spectrum(perf)$spec[1]))



n=length(rolling_predictions2[,1])
perf=rep(0,length(rolling_predictions2[,1])) 
for (i in 1:length(rolling_predictions2[,1]))
{
  perf[i]= length(rolling_predictions2[,1])*(-log(dmvnorm(data_pred[i,2:4],mean=as.numeric(colMeans(data_pred[,2:4])),sigma=array(c(rolling_predictions2[i,1], rolling_predictions2[i,4], rolling_predictions2[i,5], rolling_predictions2[i,4], rolling_predictions2[i,2], rolling_predictions2[i,6], rolling_predictions2[i,5], rolling_predictions2[i,6], rolling_predictions2[i,3]), dim = c(3,3))))+log(dmvnorm(data_pred[i,2:4],mean=as.numeric(colMeans(data_pred[,2:4])),sigma=array(c(rolling_predictions3[i,1], rolling_predictions3[i,4], rolling_predictions3[i,5], rolling_predictions3[i,4], rolling_predictions3[i,2], rolling_predictions3[i,6], rolling_predictions3[i,5], rolling_predictions3[i,6], rolling_predictions3[i,3]), dim = c(3,3))))) #QLIKE
}

sqrt(n)*mean(perf)/sqrt(spectrum(perf)$spec[1]) 
1-pnorm(sqrt(n)*mean(perf)/sqrt(spectrum(perf)$spec[1]))

library(quadprog)

n1=0
n2=nrow(data_pred)
k=3 #Number of asssets

a=matrix(,n2,k)
b=matrix(,n2,k)
d=matrix(,n2,k)

perf=matrix(,n2,3)

for (i in (n1+1):(n1+n2))
{
  a[i-n1,] = solve.QP(Dmat=array(c(rolling_predictions1[i,1], rolling_predictions1[i,4], rolling_predictions1[i,5], rolling_predictions1[i,4], rolling_predictions1[i,2], rolling_predictions1[i,6], rolling_predictions1[i,5], rolling_predictions1[i,6], rolling_predictions1[i,3]), dim = c(3,3)), dvec=array(0, dim = c(k,1)), Amat=t(array(1, dim = c(1,k))), bvec=1, meq = 1)$solution #Global minimum variance portfolio
  b[i-n1,] = solve.QP(Dmat=array(c(rolling_predictions2[i,1], rolling_predictions2[i,4], rolling_predictions2[i,5], rolling_predictions2[i,4], rolling_predictions2[i,2], rolling_predictions2[i,6], rolling_predictions2[i,5], rolling_predictions2[i,6], rolling_predictions2[i,3]), dim = c(3,3)), dvec=array(0, dim = c(k,1)), Amat=t(array(1, dim = c(1,k))), bvec=1, meq = 1)$solution
  d[i-n1,] = solve.QP(Dmat=array(c(rolling_predictions3[i,1], rolling_predictions3[i,4], rolling_predictions3[i,5], rolling_predictions3[i,4], rolling_predictions3[i,2], rolling_predictions3[i,6], rolling_predictions3[i,5], rolling_predictions3[i,6], rolling_predictions3[i,3]), dim = c(3,3)), dvec=array(0, dim = c(k,1)), Amat=t(array(1, dim = c(1,k))), bvec=1, meq = 1)$solution
  
  perf[i-n1,1]=t(a[i-n1,])%*%t(data_pred[i,2:4])%*%t(t(data_pred[i,2:4]))%*%a[i-n1,] #With cross returns as proxies
  perf[i-n1,2]=t(b[i-n1,])%*%t(data_pred[i,2:4])%*%t(t(data_pred[i,2:4]))%*%b[i-n1,]
  perf[i-n1,3]=t(d[i-n1,])%*%t(data_pred[i,2:4])%*%t(t(data_pred[i,2:4]))%*%d[i-n1,]
  
}

sqrt(n2)*mean(perf)/sqrt(spectrum(perf)$spec[1])  #DMW test
1-pnorm(sqrt(n2)*mean(perf)/sqrt(spectrum(perf)$spec[1]))

sqrt(n2)*mean(perf)/sqrt(spectrum(perf)$spec[2])  #DMW test
1-pnorm(sqrt(n2)*mean(perf)/sqrt(spectrum(perf)$spec[2]))

sqrt(n2)*mean(perf)/sqrt(spectrum(perf)$spec[3])  #DMW test
1-pnorm(sqrt(n2)*mean(perf)/sqrt(spectrum(perf)$spec[3]))

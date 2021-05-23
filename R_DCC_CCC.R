library(arrow)
library(rugarch)
library(rmgarch)
library(mgarchBEKK)
library(ccgarch)
#Install Archived CCGARCH
#url <- 'https://cran.r-project.org/src/contrib/Archive/ccgarch/ccgarch_0.2.3.tar.gz'

#pkgFile <- "ccgarch_0.2.3.tar.gz"
#download.file(url = url, destfile = pkgFile)

# Install package
#install.packages(pkgs=pkgFile, type="source", repos=NULL)

# Delete package tarball
#unlink(pkgFile)

path <- "C:/Users/Lazar/Desktop/Financial Volatility/Assignment/data.feather"
data <- arrow::read_feather(path)
data <- data.frame(data)

# CCC 1,1 3 Stocks
uspec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)))
u11fit = ugarchfit(data = data['EEM'], spec = uspec)
u11fit2 = ugarchfit(data = data['SPY'], spec = uspec)
u11fit3 = ugarchfit(data = data['EZU'], spec = uspec)
ccc11fit <- eccc.estimation(c(coef(u11fit)[4], coef(u11fit2)[4], coef(u11fit3)[4]), diag(c(coef(u11fit)[5], coef(u11fit2)[5], coef(u11fit3)[5])), diag(c(coef(u11fit)[6], coef(u11fit2)[6], coef(u11fit3)[6])), cor(data[,2:4]), data[,2:4], model = 'diagonal')
ccc11_var <- ccc11fit[['h']]
ccc11_coef_cor <- ccc11fit[['para.mat']]
ccc11_cov <- sqrt(ccc11_var)
ccc11_cov[,1] <- sqrt(ccc11_var)[,1]*sqrt(ccc11_var)[,2]*cor(data[,2:4])[1,2]
ccc11_cov[,2] <- sqrt(ccc11_var)[,1]*sqrt(ccc11_var)[,3]*cor(data[,2:4])[1,3]
ccc11_cov[,3] <- sqrt(ccc11_var)[,2]*sqrt(ccc11_var)[,3]*cor(data[,2:4])[2,3]

# Conditional Variance plot
number_ticks <- function(n) {function(limits) pretty(limits, n)}
CCC11_var <- ggplot(data = data.frame('rcov' = ccc11_var[,1], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from CCC GARCH (1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = ccc11_var[,2], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = ccc11_var[,3], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Conditional Covariance plot
CCC11_cov <- ggplot(data = data.frame('rcov' = ccc11_cov[,1], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from CCC GARCH (1,1)') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = ccc11_cov[,2], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = ccc11_cov[,3], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))


# DCC 1,1 3 Stocks
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

# BEKK Model
# Convert df to matrix for this to work.
BEKK.fit <- BEKK(as.matrix(data[,c(2,3,4)]))
coefBEKK <- BEKK.fit$est.params # Gives the three matrices required to form the H_t
BEKKH_t <- BEKK.fit$H.estimated # Gives H_t

varBEKK <- NULL
covBEKK <- NULL
for (i in 1:length(BEKKH_t)){
  varBEKK <- rbind(varBEKK, c(BEKKH_t[[i]][1,1], BEKKH_t[[i]][2,2], BEKKH_t[[i]][3,3]))
  covBEKK <- rbind(covBEKK, c(BEKKH_t[[i]][1,2], BEKKH_t[[i]][1,3], BEKKH_t[[i]][2,3]))
}

BEKK_Unc.Var. <- BEKK.fit$uncond.cov.matrix
BEKK_AIC <- BEKK.fit$aic

# Conditional Variance plot
BEKK_var <- ggplot(data = data.frame('rcov' = varBEKK[,1], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Variance') + ggtitle('Conditional Variance from BEKK') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = varBEKK[,2], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = varBEKK[,3], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Variance", labels = c("EEM", "SPY", 'EZU'))

# Conditional Covariance plot
BEKK_cov <- ggplot(data = data.frame('rcov' = covBEKK[,1], 'time' = data['DT']), aes(x = DT, y = rcov)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Covariance') + ggtitle('Conditional Covariance from BEKK') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcov' = covBEKK[,2], 'time' = data['DT']), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcov' = covBEKK[,3], 'time' = data['DT']), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Covariance", labels = c("cov(EEM,SPY)", "cov(EEM,EZU)", 'cov(SPY,EZU)'))

# Correlation Plot
BEKK_cor <- ggplot(data = data.frame('rcor' = BEKK.fit$cor[[1]][[2]][2:length(BEKK.fit$cor[[1]][[2]])], 'time' = data['DT'][2:nrow(data['DT']),1]), aes(x = time, y = rcor)) + geom_line(aes(colour = 'red')) + xlab('Date') + ylab('Conditional Correlation') + ggtitle('Conditional Correlation from BEKK') +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_datetime(limits = c(min(data$DT), max(data$DT)), breaks=number_ticks(10)) + geom_line(data = data.frame('rcor' =  BEKK.fit$cor[[1]][[3]][2:length(BEKK.fit$cor[[1]][[3]])], 'time' = data['DT'][2:nrow(data['DT']),1]), aes(colour = 'darkgreen'), alpha = 0.5) +
  geom_line(data = data.frame('rcor' = BEKK.fit$cor[[2]][[3]][2:length(BEKK.fit$cor[[2]][[3]])], 'time' = data['DT'][2:nrow(data['DT']),1]), aes(colour = 'blue'), alpha = 0.5) + scale_color_discrete(name = "Conditional Correlation", labels = c("cor(EEM,SPY)", "cor(EEM,EZU)", 'cor(SPY,EZU)'))


# Finding Optimal Univariate Settings as basis for multivariate models
uspec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)))

ugarchfit(spec = uspec, data[,2], distribution = 'mvt')
ugarchfit(spec = uspec, data[,3], distribution = 'mvt')
ugarchfit(spec = uspec, data[,4], distribution = 'mvt')

# Sign Bias in 1 Series ('EZU') and weak significance in 'EEM' (10%)

list_models <- list()
list_models <- c(list_models, uspec)
list_models <- c(list_models, ugarchspec(variance.model = list(model = 'gjrGARCH')))
list_models <- c(list_models, ugarchspec(variance.model = list(model = 'eGARCH')))
list_models <- c(list_models, ugarchspec(variance.model = list(model = 'iGARCH')))
list_models <- c(list_models, ugarchspec(variance.model = list(model = 'csGARCH')))
list_models <- c(list_models, ugarchspec(variance.model = list(model = 'apARCH')))
list_models <- c(list_models, ugarchspec(variance.model = list(model = 'fGARCH', submodel = 'GARCH')))
list_models <- c(list_models, ugarchspec(variance.model = list(model = 'fGARCH', submodel = 'TGARCH')))
list_models <- c(list_models, ugarchspec(variance.model = list(model = 'fGARCH', submodel = 'GJRGARCH')))

BIC_mat <- matrix(NA, nrow = length(list_models), ncol = 3)
rownames(BIC_mat) <- c('sGARCH','gjrGARCH', 'eGARCH', 'iGARCH', 'csGARCH', 'apARCH', 'fGARCH','fTGARCH','fGJRGARCH')
colnames(BIC_mat) <- c('EEM', 'SPY', 'EZU')

for (y in 2:length(data)){
  for (i in 1:length(list_models)){
    fit <- ugarchfit(list_models[[i]], data[,y])
    BIC_mat[i,y-1] <- infocriteria(fit)[2]
  }     
}
min(BIC_mat[,1]) # Optimal Specification: EGARCH
min(BIC_mat[,2]) # Optimal Specification: csGARCH
min(BIC_mat[,3]) # Optimal Specification: csGARCH

uspec_opt1 <- ugarchspec(variance.model = list(model = 'eGARCH'))
uspec_opt2 <- ugarchspec(variance.model = list(model = 'csGARCH'))
uspec_opt3 <- ugarchspec(variance.model = list(model = 'csGARCH'))

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
DCC_opt_f_spec <- dccspec(multispec(c(uspec_opt1, uspec_opt2, uspec_opt3)), distribution = 'mvnorm', model = "FDCC", groups = seq(1,3))
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

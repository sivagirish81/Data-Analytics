#Libraries

library('zoo')
library('xts')
library('forecast');
library('fma')
library('expsmooth')
library('lmtest')
library('tseries')
library('Quandl')
library('fpp');
library('urca')

BSensex<-read.csv("Datasets/BSESN.csv")
BSensex<-BSensex[,colnames(Sensex)!="Adj.Close"]
BSensex<-na.omit(BSensex)
str(BSensex)

summary(BSensex$Close)

Bst<-ts(BSensex$Close,frequency=365,start=c(2014,10,16),end = c(2019,10,15))
plot.ts(Bst)

adfe_test <- adf.test(Bst,alternative = 'stationary')
print(adfe_test)
#Naive Model
Naive<-naive(Bst)
plot.ts(Naive)
accuracy(Naive)

mamod<-ma(Bst,order=365)
plot.ts(mamod)
mfore<-forecast(mamod)
accuracy(mfore)

armod<-ar(Bst)
plot.ts(armod)
frmod<-forecast(armod)
accuracy(frmod)

Bstdiff<-diff(Bst,differences = 1)
plot.ts(Bstdiff)
#Small P-Value
LB_test <- Box.test(Bstdiff,lag=20, type='Ljung-Box')
summary(LB_test)
print(LB_test)

#Augmented Dickey Fuller Test
#Small P-Value
adf_test <- adf.test(Bstdiff,alternative = 'stationary')
print(adf_test)

#Kwiatkowski-Philips-Schmidt-Shin (KPSS) test
kpss_test <- kpss.test(Bstdiff)
print(kpss_test)

#Auto Correlation Function
acf(Bstdiff,lag.max=20)
acf(Bstdiff,lag.max=20,plot=FALSE)

  #Passive Auto Correlation Function
pacf(Bstdiff,lag.max=20)
pacf(Bstdiff,lag.max=20,plot=FALSE)

#We can notice that the acf and pacf lag values are not integral.
#They are decimal indicating that the

auto.arima(Bst)
auto.arima(Bstdiff)
#Auto Arima gives us the most optimal arima model

armamod<-arma(Bstdiff,order = c(1,1))
plot.ts(armamod)
farmamod<-forecast(armamod)
accuracy(farmamod)

arimamod<-arima(Bst,order = c(4,1,2))
farimamod<-forecast(arimamod)
plot.ts(farimamod)
accuracy(farimamod)

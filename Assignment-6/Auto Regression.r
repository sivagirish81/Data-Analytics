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

#Data
Sensex<-read.csv("Datasets/BSESN.csv")
Nifty<-read.csv("Datasets/NSEI.csv")
DowJones<-read.csv("Datasets/DJI.csv")
LBMAGold<-read.csv("Datasets/LBMA-GOLD.csv")
OilPrices<-read.csv("Datasets/OPEC-ORB.csv")
INRVSUSD<-read.csv("Datasets/USD_INR Historical Data.csv")

Mer<-merge(Sensex,LBMAGold,join='inner')
Mer

Mer<-na.omit(Mer)
#str(Mer)

Nmer<-Mer[,colnames(Mer)!="Adj.Close"]
Nmer<-Nmer[,colnames(Nmer)!="GBP..AM."]
Nmer<-Nmer[,colnames(Nmer)!="USD..AM."]
Fmer<-Nmer[,colnames(Nmer)!="EURO..AM."]

Fmer<-merge(Fmer,OilPrices,join='inner')
str(Fmer)
Fmer<-na.omit(Fmer)

INRVSUSD$ï..Date <- format(as.Date(INRVSUSD$ï..Date), "%d/%m/%Y")

Smer<-merge(Fmer,INRVSUSD,join='inner')
Smer<-na.omit(Smer)

                                            #Arima Model
BSensex<-read.csv("Datasets/BSESN.csv")
BSensex<-BSensex[,colnames(Sensex)!="Adj.Close"]
BSensex<-na.omit(BSensex)
str(BSensex)

Bst<-ts(BSensex$Close,frequency=365,start=c(2014,10,16),end = c(2019,10,15))
plot.ts(Bst)

Bstdiff<-diff(Bst,differences = 1)
plot.ts(Bstdiff)

                #Box-Ljung Test
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

auto.arima(Bst)

Bste<-ts(BSensex$Close,frequency=12,start=c(2014,10,16),end = c(2019,10,15))
plot.ts(Bste)


Bstediff<-diff(Bste,differences = 1)
plot.ts(Bstediff)

#Box-Ljung Test
#Small P-Value
LB_test <- Box.test(Bstediff,lag=20, type='Ljung-Box')
summary(LB_test)
print(LB_test)

#Augmented Dickey Fuller Test
#Small P-Value
adf_test <- adf.test(Bstediff,alternative = 'stationary')
print(adf_test)

#Kwiatkowski-Philips-Schmidt-Shin (KPSS) test
kpss_test <- kpss.test(Bstediff)
print(kpss_test)

#Auto Correlation Function
acf(Bstediff,lag.max=20)
acf(Bstediff,lag.max=20,plot=FALSE)

#Passive Auto Correlation Function
pacf(Bstediff,lag.max=20)
pacf(Bstediff,lag.max=20,plot=FALSE)

auto.arima(Bste)

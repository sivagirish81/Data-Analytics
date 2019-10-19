
Sensex<-read.csv("Datasets/BSESN.csv")
Nifty<-read.csv("Datasets/NSEI.csv")
DowJones<-read.csv("Datasets/DJI.csv")
LBMAGold<-read.csv("Datasets/LBMA-GOLD.csv")

#Goal to predict the trend in Sensex based on Gold Values

Mer<-merge(Sensex,LBMAGold,join='inner')
Mer

Mer<-na.omit(Mer)
str(Mer)

Nmer<-Mer[,colnames(Mer)!="Adj.Close"]
Nmer<-Nmer[,colnames(Nmer)!="GBP..AM."]
Nmer<-Nmer[,colnames(Nmer)!="USD..AM."]
Nmer<-Nmer[,colnames(Nmer)!="EURO..AM."]

str(Nmer)
Nmer$Close<-as.numeric(as.character(Nmer$Close))
Nmer<-na.omit(Nmer)
str(Nmer)

#Applied Linear regression on closing values of Sensex dataset 
linmod<-lm(Nmer$Close~Nmer$USD..PM.+Nmer$GBP..PM.+Nmer$EURO..PM.)
summary(linmod)

tsNmer<-ts(Nmer$Close,frequency=365,start=c(2014,10,16),end = c(2019,10,15))
plot.ts(tsNmer)

mplts<-decompose(tsNmer)
plot(mplts)

#Converting Time Series data to a dataframe
my_df_ts <- data.frame(Close = tsNmer, as.numeric(time(tsNmer)))
names(my_df_ts) <- c("Closedf", "timedf")

#Time Series Linear Model
#str(my_df_ts)
#TsLinRegModel<-tslm(my_df_ts$Closedf~season + trend + Nmer$USD..PM. +Nmer$GBP..PM. + Nmer$EURO..PM.)
?tslm
??tslm
library(forecast)

linmod1<-lm(Nmer$Close~Nmer$USD..PM.)
summary(linmod1)

linmod1<-lm(Nmer$Close~Nmer$EURO..PM.)
summary(linmod1)

linmod3<-lm(Nmer$Close~Nmer$GBP..PM.+Nmer$USD..PM.)
summary(linmod3)

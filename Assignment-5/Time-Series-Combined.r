
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

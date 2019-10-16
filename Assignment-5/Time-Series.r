

Sensex <- read.csv("BSESN.csv")
str(Sensex)

Sensex<-Sensex[,colnames(Sensex)!="Adj.Close"]
str(Sensex)

Sensex<-na.omit(Sensex)
head(Sensex)



Close_Values<-ts(Sensex$Close,frequency=365,start=c(2014,10,16),end = c(2019,10,15))
plot.ts(Close_Values)


LC<-log(Close_Values)
plot.ts(LC)
CHWF1<-HoltWinters(Close_Values,beta=NULL,gamma=NULL)
plot(CHWF1)
CHWF1$SSE
#SSE = 12195092

CHWF2<-HoltWinters(LC,beta=NULL,gamma=NULL)
plot(CHWF2)
CHWF2$SSE

#SSE = 46.45982

library(forecast)
class(LC)
fc<-forecast(CHWF1,h=365)

plot(fc)
accuracy(fc)

fc1<-forecast(CHWF2,h=730)

plot(fc1)
accuracy(fc1)





wine <- read.csv("./data/data.csv")

library(corrgram)
corrgram(wine)

#quality and alcohol
mymodel<-lm(cbind(wine$quality,wine$alcohol)~wine$fixed.acidity+wine$volatile.acidity+wine$citric.acid+wine$residual.sugar+wine$chlorides+wine$free.sulfur.dioxide+wine$total.sulfur.dioxide+wine$density+wine$pH+wine$sulphates,data=wine)
summary(mymodel)
coef(mymodel)

Mymodelfit<-manova(mymodel)
summary(Mymodelfit)

#Remove bloodpressure
#mymodel<-lm(cbind(wine$outcome,wine$age)~wine$pregnancies+wine$bloodpressure+wine$skinthickness+wine$insulin+wine$dpf+wine$glucose,data=wine)
#summary(mymodel)
#coef(mymodel)

#Remove skinthickness
#mymodel<-lm(cbind(wine$outcome,wine$age)~wine$pregnancies+wine$bloodpressure+wine$insulin+wine$dpf+wine$glucose+wine$bmi,data=wine)
#summary(mymodel)
#coef(mymodel)

#Remove bmi
#mymodel<-lm(cbind(wine$outcome,wine$age)~wine$pregnancies+wine$bloodpressure+wine$skinthickness+wine$insulin+wine$glucose+wine$dpf,data=wine)
#summary(mymodel)
#coef(mymodel)

#Mymodelfit<-manova(mymodel)
#summary(Mymodelfit)

#mymodel<-lm(cbind(wine$outcome,wine$age)~wine$pregnancies+wine$insulin+wine$glucose,data=wine)
#summary(mymodel)
coef(mymodel)

#library(corrplot)
#corrplot(cor(wine[,-9]),type = "lower", method = "number")

#mymodel<-lm(wine$outcome~wine$pregnancies+wine$bloodpressure+wine$skinthickness+wine$insulin+wine$bmi+wine$dpf+wine$glucose,data=wine)
#summary(mymodel)
#coef(mymodel)

#mymodel<-lm(cbind(wine$outcome,wine$bmi)~wine$pregnancies+wine$bloodpressure+wine$skinthickness+wine$insulin+wine$age+wine$dpf+wine$age,data=wine)
#summary(mymodel)
#coef(mymodel)

#mymodel<-lm(cbind(wine$outcome,wine$bmi)~wine$pregnancies+wine$bloodpressure+wine$skinthickness+wine$insulin+wine$age+wine$dpf+wine$age,data=wine)
#summary(mymodel)
#coef(mymodel)

#Testing

#mymodel<-lm(wine$outcome~wine$pregnancies+wine$bloodpressure+wine$skinthickness+wine$insulin+wine$age+wine$dpf+wine$age,data=wine)
#summary(mymodel)
#coef(mymodel)
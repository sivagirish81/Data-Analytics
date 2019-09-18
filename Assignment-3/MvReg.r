wine<-read.csv("winedata/winequality_white.csv")
head(wine)
summary(wine$quality)

library(corrgram)
corrgram(wine)

str(wine)
wine<-wine[complete.cases(wine),]

wine<-na.omit(wine)
str(wine)
#Multivariate Regression model
#Alcohol Content and quality are highly correlated
#We us them as dependent variables
library(corrplot)
corrplot(cor(wine),method="number")

#Fixing Quality as our dependent variable
#Shos high correlation with alcohol content

mymodel<-lm(cbind(wine$alcohol,wine$quality)~wine$fixed.acidity+wine$volatile.acidity+wine$citric.acid+wine$residual.sugar+wine$chlorides+wine$free.sulfur.dioxide+wine$total.sulfur.dioxide+wine$density+wine$pH+wine$sulphates,data=wine)
summary(mymodel)
coef(mymodel)

m1<-manova(mymodel)
summary(m1)

#correlation between free sulphur oxides and alcohol,quality is less
#Dropping them from the model

mymodel<-lm(cbind(wine$alcohol,wine$quality)~wine$fixed.acidity+wine$volatile.acidity+wine$citric.acid+wine$residual.sugar+wine$chlorides+wine$total.sulfur.dioxide+wine$density+wine$pH+wine$sulphates,data=wine)
summary(mymodel)
coef(mymodel)

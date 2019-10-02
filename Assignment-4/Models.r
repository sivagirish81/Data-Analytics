library("rmdformats") 

library("ResourceSelection")
library("corrgram")

library("MASS")
library("ggplot2")
library("naniar")
library("e1071")
library("lattice")
library("caret")
library("caTools")
wine<-read.csv("winedata/winequality_white.csv")
head(wine)

library(corrplot)
corrplot(cor(wine),method="number")

#Linear Model
#Our intentions are to predict the quality of the alcohol

LinMod<-lm(wine$quality~wine$fixed.acidity+wine$volatile.acidity+wine$citric.acid+wine$residual.sugar+wine$chlorides+wine$free.sulfur.dioxide+wine$total.sulfur.dioxide+wine$density+wine$pH+wine$sulphates,data=wine)
summary(LinMod)
#Very low R square values
#Remove attributes based on high p values

LinMod<-lm(wine$quality~wine$fixed.acidity+wine$volatile.acidity+wine$residual.sugar+wine$chlorides+wine$free.sulfur.dioxide+wine$total.sulfur.dioxide+wine$density+wine$pH+wine$sulphates,data=wine)
summary(LinMod)

#Removing Multi Collinear variables and values with low p score
#We are able to effectively predict the quality of white wines
#The Ad
LinMod<-lm(wine$quality~wine$volatile.acidity+wine$density+wine$alcohol+wine$residual.sugar+wine$pH+wine$sulphates,data=wine)
summary(LinMod)

LinMod1<-lm(wine$quality~wine$alcohol+wine$density+wine$chlorides)
summary(LinMod1)

str(wine)

dim(wine)
wine <- wine[!duplicated(wine), ]
dim(wine)

vis_miss(wine)
sum(is.na(wine))
attach(wine)

skewness(wine$quality)
detach(wine)
preprocess_wine <- preProcess(wine[,1:11], c("BoxCox", "center", "scale"))
new_wine <- data.frame(trans = predict(preprocess_wine, wine))
summary(new_wine)
new_wine <- new_wine[!abs(new_wine$trans.fixed.acidity) > 3,]
new_wine <- new_wine[!abs(new_wine$trans.volatile.acidity) > 3,]
new_wine <- new_wine[!abs(new_wine$trans.citric.acid) > 3,]
new_wine <- new_wine[!abs(new_wine$trans.residual.sugar) > 3,]
new_wine <- new_wine[!abs(new_wine$trans.chlorides) > 3,]
new_wine <- new_wine[!abs(new_wine$trans.density) > 3,]
new_wine <- new_wine[!abs(new_wine$trans.pH) > 3,]
new_wine <- new_wine[!abs(new_wine$trans.sulphates) > 3,]
new_wine <- new_wine[!abs(new_wine$trans.alcohol) > 3,]
summary(new_wine)
wine<-new_wine
summary(wine)
skewness(wine$trans.residual.sugar)

corrplot(cor(wine),method="number")

LinearModel<-lm(wine$trans.quality~.,data=wine)
summary(LinearModel)
#30% adjusted R square
#Very Low

#Variance inflation factor
vif(LinearModel)

LinearModel<-lm(wine$trans.quality~.-trans.density,data=wine)
summary(LinearModel)

LinearModel<-lm(wine$trans.quality~.-trans.density- trans.fixed.acidity - trans.citric.acid,data=wine)
summary(LinearModel)

#Linear Regression fails

#Logistic Regression
nwine<-wine
summary(nwine)
nwine$category[nwine$quality <= 5] <- 0
nwine$category[nwine$quality > 5] <- 1

nwine$category <- as.factor(nwine$category)
Lwine<-new_wine
Lwine$Category<-NA
Lwine$Category[Lwine$trans.quality <= 5] <- 0
Lwine$Category[Lwine$trans.quality > 5] <- 1
summary(Lwine$Category)
str(Lwine)

LogisticModel <- glm(Lwine$Category ~ ., data = Lwine, family=binomial(link = "logit"))
summary(LogisticModel)
hoslem.test(LogisticModel$Category,fitted(LogisticModel),g=10)
#rsq <- function (x, y) cor(x, y) ^ 2
LogisticModelStepwise <- step(LogisticModel)
Rsq(LogisticModelStepwise,adj=TRUE,data=Lwine)

Nul<-LogisticModel$null.deviance/-2
Prop<-LogisticModel$deviance/-2
R2<-(Nul-Prop)/Nul
R2

p_val<-1-pchisq(2*(Prop-Nul),df=length(LogisticModel$coefficients-1))
p_val

#P value by mcfadden pseudo R2 we get 0 and r sq is 1 
#Hence binomial logistic regression is the best model

PolyMod<-lm(wine$trans.quality~poly(wine$trans.volatile.acidity,4)+poly(wine$trans.citric.acid,4)+poly(wine$trans.residual.sugar,4)+poly(wine$trans.chlorides,4)+poly(wine$trans.free.sulfur.dioxide,4)+poly(wine$trans.total.sulfur.dioxide,4)+poly(wine$trans.pH,4)+poly(wine$trans.sulphates,4),data=wine)
summary(PolyMod)

#PolyMod1<-lm(wine$trans.quality~wine$trans.volatile.acidity+wine$trans.citric.acid+wine$trans.residual.sugar+wine$trans.chlorides+wine$trans.free.sulfur.dioxide+wine$trans.total.sulfur.dioxide+wine$trans.pH+wine$trans.sulphates,data=wine)
#summary(PolyMod1)

PolyMod2<-lm(wine$trans.quality~poly(wine$trans.volatile.acidity,4)+poly(wine$trans.citric.acid,4)+poly(wine$trans.residual.sugar,4)+poly(wine$trans.chlorides,4)+poly(wine$trans.total.sulfur.dioxide,4)+poly(wine$trans.sulphates,4),data=wine)
summary(PolyMod2)

LogisticModel <- glm(Lwine$Category ~ ., data = Lwine, family=binomial)
summary(LogisticModel)
hoslem.test(LogisticModel$Category,fitted(LogisticModel),g=10)

cwine<-wine

head(cwine)
cwine <- cwine[!duplicated(wine), ]
cwine$Category<-NA
cwine$Category[cwine$quality <= 5] <- 0
cwine$Category[cwine$quality > 5] <- 1

head(cwine)
spl = sample.split(cwine$Category, SplitRatio = 0.7)

trainer = subset(cwine, spl==TRUE)
tester = subset(cwine, spl==FALSE)



head(trainer)
LogisticModel <- glm(Category ~ ., data = trainer, family=binomial(link = "logit"))
summary(LogisticModel)
head(fitted(LogisticModel))
head(trainer)
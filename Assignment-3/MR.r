Iris <- read.csv('Iris.csv', header = TRUE)
summary(Iris)
str(Iris)

mymodel<-lm(cbind(Iris$SepalLengthCm,Iris$SepalWidthCm,Iris$PetalLengthCm,Iris$PetalWidthCm) ~ Iris$Species, data=Iris)
summary(mymodel)
coef(mymodel)


mymodel<-lm(cbind(Iris$SepalLengthCm,Iris$SepalWidthCm) ~ Iris$Species+Iris$PetalLengthCm+Iris$PetalWidthCm, data=Iris)
summary(mymodel)
coef(mymodel)

m1 <- manova(mymodel)
summary(m1) 

mymodel<-lm(cbind(Iris$SepalLengthCm,Iris$SepalWidthCm) ~ Iris$Species, data=Iris)
summary(mymodel)
coef(mymodel)
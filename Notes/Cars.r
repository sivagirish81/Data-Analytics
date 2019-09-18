data<-mtcars
cor(data)
library(corrgram)
corrgram(data)
#Distance and Similarity measures
x<-mtcars[1:4,1:4]
View(x)
dist(x,method="euclidean",diag=TRUE)
dist(x,method="manhattan",diag=TRUE)

dist(x,method="minkowski",diag=TRUE,p=1)
#if p is one then it is same as manhattan

dist(x,method="manhattan",diag=TRUE,p=2)
#if p is 2 then it is same as euclidean

x1<-c(0,0,1,1,1,1,0)
x2<-c(1,0,1,1,0,1,1)

dist(rbind(x1,x2),method="binary",diag=TRUE)

cor(mtcars)
mymodel<-lm(cbind(mtcars$mpg,mtcars$hp)~mtcars$cyl+mtcars$disp + mtcars$wt +mtcars$gear,data=mtcars)
summary(mymodel)
coef(mymodel)

Mymodelfit<-manova(mymodel)
summary(Mymodelfit)
mymodelfinal<-lm(cbind(mtcars$mpg,mtcars$hp)~mtcars$cyl+ mtcars$wt +mtcars$gear,data=mtcars)
summary(mymodelfinal)
coef(mymodelfinal)

Mymodelfitfinal<-manova(mymodelfinal)
summary(Mymodelfitfinal)

#We use pillai coeficient to remove an independent variable.
#Anything with significant pillai coefficient retain.

#glimpse(Mymodelfitfinal)
head(cars)

#How to identify the dependent variables



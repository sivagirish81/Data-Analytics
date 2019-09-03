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

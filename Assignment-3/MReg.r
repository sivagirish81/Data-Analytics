abalone<-read.csv("abalone-dataset/abalone.csv")
#data=subset(abalone,select=-c(Sex))
str(data)

data<-na.omit(data)
library(corrplot)
corrplot(cor(data),method="number")
str(data)


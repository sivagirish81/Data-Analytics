path<-"."
setwd(path)
data<-read.csv("googleplaystore.csv")
data$Rating
#Cleaning Dataset as well as converting to numbers
#maybe ignore categorical variables
data$Rating[is.na(data$Rating)]<-median(data$Rating,na.rm=TRUE)
data$Rating
data$Rating[is.na(data$Rating)]
library(dplyr)
glimpse(data)
library(psych)
describe(data)
data$App[is.na(data$App)]
data$Reviews[is.na(data$Reviews)]
data$Size[is.na(data$Size)]
#data<-data[!(data$Size=="Varies with device")]
#data$Size
summary(data$Type)
data$Type
data$Type[is.na(data$Type) & data$Price==0]<-"Free"
data$Type[is.na(data$Type) & !(data$Price==0)]<-"Paid"
summary(data$Size)
colnames(data)
str(data)
library(dummies)

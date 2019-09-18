
diabetes <- read.csv("./pima-indians-diabetes-database/diabetes.csv", header = TRUE)
#diabetes$Outcome <- factor(diabetes$Outcome)

# removing those observation rows with 0 in any of the variables
for (i in 2:6) {
  diabetes <- diabetes[-which(diabetes[, i] == 0), ]
}

# modify the data column names slightly for easier typing
names(diabetes)[7] <- "dpf"
names(diabetes) <- tolower(names(diabetes))

str(diabetes)
print(paste0("number of observations = ", dim(diabetes)[1]))
print(paste0("number of predictors = ", dim(diabetes)[2]))
cor(diabetes)
diabetes$Outcome <- factor(diabetes$Outcome)
library(corrgram)
corrgram(diabetes)


#outcome and age
mymodel<-lm(cbind(diabetes$outcome,diabetes$age)~diabetes$pregnancies+diabetes$bloodpressure+diabetes$skinthickness+diabetes$insulin+diabetes$bmi+diabetes$dpf+diabetes$glucose,data=diabetes)
summary(mymodel)
coef(mymodel)

Mymodelfit<-manova(mymodel)
summary(Mymodelfit)

#Remove bloodpressure
mymodel<-lm(cbind(diabetes$outcome,diabetes$age)~diabetes$pregnancies+diabetes$bloodpressure+diabetes$skinthickness+diabetes$insulin+diabetes$dpf+diabetes$glucose,data=diabetes)
summary(mymodel)
coef(mymodel)

#Remove skinthickness
mymodel<-lm(cbind(diabetes$outcome,diabetes$age)~diabetes$pregnancies+diabetes$bloodpressure+diabetes$insulin+diabetes$dpf+diabetes$glucose+diabetes$bmi,data=diabetes)
summary(mymodel)
coef(mymodel)

#Remove bmi
mymodel<-lm(cbind(diabetes$outcome,diabetes$age)~diabetes$pregnancies+diabetes$bloodpressure+diabetes$skinthickness+diabetes$insulin+diabetes$glucose+diabetes$dpf,data=diabetes)
summary(mymodel)
coef(mymodel)

Mymodelfit<-manova(mymodel)
summary(Mymodelfit)

mymodel<-lm(cbind(diabetes$outcome,diabetes$age)~diabetes$pregnancies+diabetes$insulin+diabetes$glucose,data=diabetes)
summary(mymodel)
coef(mymodel)

library(corrplot)
corrplot(cor(diabetes[,-9]),type = "lower", method = "number")

#mymodel<-lm(diabetes$outcome~diabetes$pregnancies+diabetes$bloodpressure+diabetes$skinthickness+diabetes$insulin+diabetes$bmi+diabetes$dpf+diabetes$glucose,data=diabetes)
#summary(mymodel)
#coef(mymodel)

mymodel<-lm(cbind(diabetes$outcome,diabetes$bmi)~diabetes$pregnancies+diabetes$bloodpressure+diabetes$skinthickness+diabetes$insulin+diabetes$age+diabetes$dpf+diabetes$age,data=diabetes)
summary(mymodel)
coef(mymodel)

mymodel<-lm(cbind(diabetes$outcome,diabetes$bmi)~diabetes$pregnancies+diabetes$bloodpressure+diabetes$skinthickness+diabetes$insulin+diabetes$age+diabetes$dpf+diabetes$age,data=diabetes)
summary(mymodel)
coef(mymodel)

#Testing

mymodel<-lm(diabetes$outcome~diabetes$pregnancies+diabetes$bloodpressure+diabetes$skinthickness+diabetes$insulin+diabetes$age+diabetes$dpf+diabetes$age,data=diabetes)
summary(mymodel)
coef(mymodel)


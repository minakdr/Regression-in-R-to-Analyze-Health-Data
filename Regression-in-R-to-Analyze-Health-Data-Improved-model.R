pacman::p_load(pacman ,rio )
data=import("D:\\Downloads\\insurance.csv")
data
plot(data)
#making the vlaues numerical 
data$sex <-as.numeric(factor(data$sex))
data$smoker<-as.numeric(factor(data$smoker))
data$region<-as.numeric(factor(data$region))


#creating interaction terms and polynomial terms to enhance the linear regression model.
data$age_bmi<- data$age * data$bmi #interaction term 
data$age_smoker <- data$age * data$smoker 
data$bmi_smoker <- data$bmi * data$smoker 
data$age2 <- data$age^2 #polynomial terms 
data$bmi2<- data$bmi^2 
 
#plots 
par(mfrow=c(2,3))

boxplot(charges~age_bmi , data=data, xlab = "age_bmi", ylab = "chargesi", main="Boxplot of charges by age_bmi ")
boxplot(charges~ age_smoker , data=data, xlab = "age_smoker", ylab = "charges", main="Boxplot of charges by age_smoker ")
boxplot(charges~ bmi_smoker , data=data, xlab = "bmi_smoker", ylab = "charges", main="Boxplot of charges by bmi_smoker")
boxplot(charges~ age2 , data=data, xlab = "age2", ylab = "charges", main="Boxplot of charges by age2 ")
boxplot(charges~ bmi2 , data=data, xlab = "age2", ylab = "charges", main="Boxplot of Charges by bmi2 ")
#correlation matrix
cor_matrix <- cor(data)
cor_matrix

x <- as.matrix(data[,-7])
y<- data[,7]             

#liner regression 
reg<-lm(y~x)
summary(reg)

 #this improved model is 86% accurate :) 
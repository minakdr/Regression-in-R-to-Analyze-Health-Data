pacman::p_load(pacman,rio)
rio_csv<- import ("D:\\Downloads\\insurance.csv")
plot(rio_csv)
rio_csv
# 1t things first : Convert categorical variables to numeric
rio_csv$sex <- as.numeric(factor(rio_csv$sex))
rio_csv$smoker <- as.numeric(factor(rio_csv$smoker))
rio_csv$region <- as.numeric(factor(rio_csv$region))

#the correlation matrix : 
correlation_matrix <- cor(rio_csv )
correlation_matrix

# we can see that the smoker really affects the charges the most then comes the age and bmi 
par(mfrow=c(3,3)) # this was used to show all the plots at the same time
# let's make box plots for a little visualistation 
boxplot(charges ~ age, data = rio_csv,xlab = "Age Group", ylab = "Charges",main = "Boxplot of Charges by Age Group")
boxplot(charges ~ sex ,data = rio_csv,xlab= " sex " ,ylab= "charges ", main =" Boxplot of Charges by sex  ")
boxplot(charges ~bmi , data=rio_csv , xlab = " Charges ", ylab=" Bmi ", main="Boxplot of charges by BMI")
boxplot(charges~ children , data=rio_csv, xlab = "charges", ylab = "children", main="Charges by children ")
boxplot(charges~ smoker , data=rio_csv, xlab = "charges", ylab = "smoker", main="Charges by smoker ")
boxplot(charges~ region , data=rio_csv, xlab = "charges", ylab = "region", main="Charges by region ")


#we apply the linear regression 
x<- as.matrix(rio_csv[,-7])
y<- rio_csv[,7]
reg=lm(y~x)
reg
# interpretation :
#xage (257.3): For each one-year increase in age, the insurance charges increase by 257.3 $ 
#xsex (-131.1): coded as 1 for female and 1 for male . The negative value (-131.1) indicates that, on average, being a female, decreases the charges by 131.1 $
#xbmi (332.6): For every one-unit increase in BMI insurance charges increase by 332.6 $
#xchildren (479.4): Each additional child increases insurance charges by 479.4 $
#xsmoker (23820.4):Being a smoker increases insurance charges by  23820.4 $.
#xregion (-353.6):The region has a small negative impact on insurance charges, reducing them by 353.6 $ 
summary(reg)
# the model is 75% accurate 

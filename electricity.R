library(car)
library(olsrr)
library(lawstat) ##levene test

electricity<-read.csv("C:/Users/Sidh Satam/Desktop/College/Sem III/SM-I/Final Project/Electricity.csv")

fit<-lm(Y~Temp+poly(Humidity,3)+Weekend+Region+TimeOfDay,data=electricity)
summary(fit)
vif(fit)
cor(e)
e<-read.csv("C:/Users/Sidh Satam/Downloads/Electricity.csv")
e$Region<-as.numeric(e$Region)
e$TimeOfDay<-as.numeric(e$TimeOfDay)
fit1<-lm(Y~poly(Humidity,3)+Weekend+Region+TimeOfDay,data=electricity)
summary(fit1)
ols_vif_tol(fit)


anova <- aov(Y ~ Weekend, data = electricity)
summary(anova)

#Assumption 1
#True

#Assumption 2
#True

#Assumption 3
plot(anova,which=5) #Parallel Lines

#Assumption 4
plot(anova$residuals) #Random plots

#Assumption 5
plot(anova) #2nd Graph(Remove Outliers)
electricity<-electricity[-c(223,12,14),]

#Assumption 6
leveneTest(Y~factor(Weekend), data=electricity) #Accepted

#Assumption 7
boxplot(electricity$Y~electricity$Region) #No Outliers




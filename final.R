e<-read.csv("C:/Users/Sidh Satam/Desktop/College/Sem III/SM-I/Final Project/Electricity.csv")
head(e)
dim(e)


lm0<-lm(Y~., data=e)
summary(lm0)
par(mfrow=c(1,1))

e$Weekend<-as.factor(e$Weekend)

lm2<-lm(Y~poly(Temp,6), data=e)
summary(lm2)
plot(lm2)

lm3<-lm(Y~poly(Humidity,3), data=e)
summary(lm3)
plot(lm3)

lm4<-lm(Y~(Weekend), data=e)
summary(lm4)
plot(lm4)

lm5<-lm(Y~(Region), data=e)
summary(lm5)
plot(lm5)

lm6<-lm(Y~TimeOfDay, data=e)    
summary(lm6)
plot(lm6)

lm8<-lm(Y~poly(Temp,6)+poly(Humidity,3)+Weekend+Region+TimeOfDay, data=e)
summary(lm8)


cor.test(e$Temp,e$Humidity)

lma<-lm(Y~poly(Humidity,3)+Weekend+Region+TimeOfDay, data=e)
summary(lma) #0.9106
plot(lma)

which.max(hatvalues(lma)) #126

ea<-e[c(-383,-384,-694,-335,-382,-430,-502,-385,-429,-431,-478,-336,-432,-695,-286,-428,-692,-287,-334,-503,-191,-241,-386,-239,-285,-479,-190,-192,-263,-189,-477),]
lma<-lm(Y~poly(Humidity,3)+Weekend+Region+TimeOfDay, data=ea)
summary(lma) #0.9244
plot(lma)
vif(lma)


lmb<-lm(Y~poly(Temp,4)+Weekend+Region+TimeOfDay, data=e)
summary(lmb) #0.9178
plot(lmb)

which.max(hatvalues(lmb)) #228

eb<-e[-c(383,384,430,431,382,478,695,385,429,286,479,239,477,432,335,480,428,287,263,336,241,285,386,238,334,240,237),]

lmb<-lm(Y~poly(Temp,1)+Weekend+Region+TimeOfDay, data=eb)
summary(lmb) #0.93
plot(lmb)


lm7<-lm(Y~Temp+Weekend+Region+TimeOfDay, data=eb) #for vif
summary(lm7)

library(car)
vif(lm7)


?add1 ##Compute all the single terms in the scope argument that can be added to or dropped from the model, fit those models and compute a table of the changes in fit.
add1(lm(Y~1 ,data=eb),Y~poly(Temp,4)+Weekend+Region+TimeOfDay , test="F")

AIC(lmb)
BIC(lmb)

##subsets
library(leaps)

s<-regsubsets(Y~.,data=eb,nvmax=8)
summary(s)

set.seed(1000)

library(caTools)
split = sample.split(eb$Y, SplitRatio = 0.70)
 train_set1 = subset(eb, split == TRUE)
 test_set1 = subset(eb, split == FALSE)

regfit.best=regsubsets(Y~.,data=train_set1,nvmax=8)
summary(regfit.best)
test.mat=model.matrix(Y~.,data=test_set1)
val.errors=rep(NA,8)
##Now we run a loop, and for each size i, we
##extract the coefficients from regfit.best for the best model of that size,
##multiply them into the appropriate columns of the test model matrix to
##form the predictions, and compute the test MSE.
for(i in 1:8){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((test_set1$Y-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,6)
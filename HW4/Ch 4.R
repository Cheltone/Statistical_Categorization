library(MASS)
crab = read.csv("crab.csv",header=TRUE)

crab.individual <- glm(y~width, data = crab, family = binomial())
summary(crab.individual)


crab.individual <- glm(y~width, data = crab, family = "binomial")
summary(crab.individual)

plot(predict(crab.individual,type="response")~crab$width,type = "p",ylab = "probability",xlab ="width")


p =predict(crab.individual,type = "response",se.fit=TRUE)
pl = p$fit -1.96*p$se.fit
pu = p$fit +1.96*p$se.fit
crab.predit = data.frame(predict=p,lower=pl,upper=pu)


crabs2<- matrix(c(22.69, 14,  5,
  23.84, 14,  4,
  24.77, 28, 17,
  25.84, 39, 21,
  26.79, 22, 15,
  27.74, 24, 20,
  28.67, 18, 15,
  30.41, 14, 14)
  ,8,3,byrow=TRUE)

crabs2 = as.data.frame(crabs2)
names(crabs2) = c("width","cases","satell")

crabs2$non.satell = crabs2$cases - crabs2$satell

crab.grouped <- glm(cbind(satell,non.satell)~width, data = crabs2, family = "binomial")
summary(crab.grouped)

#Problem 4.7  

library(rpart)
data(kyphosis)

#or

op.time = c(12, 15, 42, 52, 59, 73, 82, 91, 96, 105,
114, 120, 121, 128, 130, 139, 139, 157,1, 1, 2, 8, 11, 18, 22, 31, 37, 61, 72, 81, 97, 112,
118, 127, 131, 140, 151, 159, 177, 206)

kyph = c(rep(1,18),rep(0,22))

ky.dat =data.frame(op.time,kyph)

ky.mod1 = glm(kyph~op.time,data=ky.dat,family =binomial())
summary(ky.mod1)

library(MASS)
confint(ky.mod1)


par(mfrow=c(2,2))
plot(op.time~kyph,data=ky.dat)
plot(kyph~op.time,data=ky.dat)

ky.mod2 = glm(kyph~op.time+I(op.time^2),data=ky.dat,family =binomial())
summary(ky.mod2)

plot(ky.mod2$fit~ky.dat$op.time)


#Crab multiple regression.


crab.individual <- glm(y~width+factor(color), data = crab, family = binomial())
summary(crab.individual)
model.matrix(crab.individual)

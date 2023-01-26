' Statastics 578
  HW 3
  Rebecca Brink and Chad Melton
'


library(epiR)
library(ggplot2)
library(vcdExtra)
library(vcd)
library(MASS)
library(lmtest)



#3.13

#a)

crab = read.csv("C:\\Users\\chadm\\OneDrive\\Documents\\StatsData\\crab.csv",header=TRUE)

crab.model = glm(satell~weight,data=crab,family = poisson(link ="log"))
summary(crab.model)
plot(crab.model)
plot(crab$satell~crab$weight)
points(crab.model$fit~crab$weight,col = "red")

'log(muHat) = -0.42841 + 0.58930x'

#b)
x=2.44

muHat = exp(-0.42841 + 0.58930*x)

'The mean Y for female crabs of average weight 2.44 kg is 2.744179.'

#c)
confint(crab.model)
ciLow = exp(0.4597)
ciHi = exp(0.7167)

'The CI for muHat is (1.583599, 2.047665)'


#d) 
waldtest(crab.model)
'Wald test

Model 1: satell ~ weight
Model 2: satell ~ 1
  Res.Df Df      F    Pr(>F)    
1    171                        
2    172 -1 82.155 2.856e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1'


'The Wald test provides a value of 82.155 and one degree of freedom. Here, the P-Value is also ~0 which suggest that 
there is very strong evidence to reject the null hypothesis and shows that weight is significant.'


#e)
lrtest (crab.model)
'Model 1: satell ~ weight
Model 2: satell ~ 1
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1   2 -458.08                         
2   1 -494.04 -1 71.925  < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1''

The likelihood ratio tests provides a value of 71.295. 
Again, these results suggest strong evidence to reject the null hypothesis.'



#3.14
#a)

crab.model2 = glm.nb(satell~weight,data=crab)
summary(crab.model2)

'Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.8394  -1.4122  -0.3247   0.4744   2.1279  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.8647     0.4048  -2.136   0.0327 *  
weight        0.7603     0.1578   4.817 1.45e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for Negative Binomial(0.9311) family taken to be 1)

    Null deviance: 216.43  on 172  degrees of freedom
Residual deviance: 196.16  on 171  degrees of freedom
AIC: 754.64

Number of Fisher Scoring iterations: 1


              Theta:  0.931 
          Std. Err.:  0.168 

 2 x log-likelihood:  -748.644''

'
' The equation for the neg binomial model is log(mu2) = -0.8647 + 0.7603x. Here, the dispersion parameter
is 0.931 with a standard error of 0.168. Also, the ratio of the residual deviance to df is ~ 1.15 (overdispersion
but not drastic). These values suggest that the neg binomial model is a bette fit for these data.'

#b)

confint(crab.model2)
'The confidence interval is (0.4207032, 1.113666186).The interval range is wider for the neg binomial model 
due to the existance of the parameter that deals with overdispersion in model.




# Number 2.2. 
''The dvisits data comes from the Australian Health Survey of 1977 - 78 and consist of 5190
single adults where young and old have been oversampled. (Faraway, Extending the Linear
Model in R(2006,ex 3.5))'

data(dvisits)
head(dvisits)



#a)

Pos.model = glm(doctorco ~ sex + age + agesq + income + levyplus + freepoor + freerepa + illness + actdays + hscore + chcond1 + chcond2, family=poisson, data = dvisits)
Pos.model

summary(Pos.model)

#b) 
par(mfrow=c(2,2))
plot(Pos.model)
#EXPLAIN RED LINES

#c)
step(Pos.model, direction="backward")

#d)
#What sort of person would go?
#This model suggests an elderly person would visit more frequently than other types of people

#e)
predict(Pos.model, dvisits[1,], type="response")

for(i in 0:10) {
  print ("Doctor visits = ")
  print(i)
  print(dpois(i, lambda = 0.153))
  
  }
  

#f)
LinM <- lm(doctorco ~ sex + age + agesq + income + levyplus + freepoor + freerepa + illness + actdays + hscore + chcond1 + chcond2, data=dvisits)
LinM

summary(LinM)

predict(LinM, dvisits[5190,])










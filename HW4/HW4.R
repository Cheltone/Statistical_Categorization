'4.12,4.16,4.17,4.30(Fit model and explain coecients), 4.33'

library(MASS)
library(rpart)
library(epiR)
library(ggplot2)
library(vcdExtra)
library(vcd)
library(MASS)
library(lmtest)

## 4.12
#a)

def_est = -0.8678
vic_est = 2.4044

'The coefficients in these results suggest that a black defendent in a homicide where the victim is white is more likely to receive the death penalty.'



#Probabilities

WProb = (exp(vic_est+((0.6006*11))))/(1+exp(vic_est+(0.6006*11)))
WProb
#The probability is 0.999878.

BProb = (exp(def_est+((0))))/(1+exp(def_est+(0)))
BProb

# The probability is 0.2957123.

#b)
vrace = exp(vic_est)
vrace
#This result suggests that when a black defendeng receives the death penalty that the odds of the victim being white is 11.07189.

#c) 

CILow = exp(1.3068)
CIHigh = exp(3.7175)

CILow
CIHigh
#The 95% CI is (3.694, 41.161)

#d)

WT = (2.4044/0.6006)**2
WT
#The results of the Wald Test suggest enough evidence to reject the null hypothesis. The variables are not independent.

## 4.16
drink = read.csv("C:\\Users\\chadm\\OneDrive\\Desktop\\Fall_2020\\STATS 578\\HW\\HW4\\Ex4.16.csv",header=TRUE)


drink.fit <- glm(cbind(Yes,No)~tf+jp+EI+SN,data=drink, family = binomial())
summary(drink.fit)

'The equation would be -2.1140+0.6873TF + 0.2022JP - 0.5550EI -0.4292SN. 
Exp (-.555 = 0.57 so there is ~43% decrease in odds for introverts vs extroverts'

#b) 

ESTJ = -2.1140 + 0.6873*(1) + 0.2022*(0) - 0.5550*(0) -0.4292
piHat = exp(ESTJ)/(1+exp(ESTJ))
piHat

' The estimated value of piHat for a personality type ESTJ equals 0.1351817'

#c) 
ENTP = -2.1140 + 0.6873*(1) + 0.2022*(1) - 0.5550*(0) -0.42920*(0) 
ENTP
piHatc = exp(ENTP)/(1+exp(ENTP))
piHatc


'Here, ENTP will have the highest (piHat =  0.2271455) because the negative coefficients are not included in the calculation.'

## 4.16
#a)

'drink417.fit <- glm(cbind(Yes,No)~tf+EI,data=drink, family = binomial())
summary(drink417.fit)'

piHatInt = exp(-2.8291)/(1+exp(-2.8291))
piHatInt
'PiHat for someone who is introverted and feeling is 0.05577177'

#b)
EIOddsRat = exp(0.5805)
EIOddsRat
'Here, an extrovert is 1.79 more likely to drinking frequently than an introvert.'

#c) 
confint(drink417.fit)
CILo = exp(0.1589)
CIHi = exp(1.008)
CILo
CIHi
'The confidence interval is (1.1722,2.7401)'

#d)
ICoef = 1/1.79
ICoef

ICIHi = 1/exp(CILo)
ICILo = 1/exp(CIHi)

ICILo
ICIHi

#e)

'The likelihood ratio test provided a chisq value of 7.28, which translates into a p-Value of 0.0070. These results are significant and therefore
we reject the null hypothesis.'

plot(drink.fit)

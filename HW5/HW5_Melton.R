#Chad Melton HW 5


library(MASS)
library(rpart)
library(epiR)
library(ggplot2)
library(vcdExtra)
library(vcd)
library(lmtest)
library(caret)
library(ROCR)
library(pROC)



crab = read.csv("C:\\Users\\chadm\\OneDrive\\Desktop\\Fall_2020\\STATS 578\\HW\\HW5\\crab.csv",header=TRUE)





# 5.10
'For the horseshoe crab data, fit the logistic regression model with x = weight
as the sole predictor of the presence of satellites.'

crab.model <- glm(y~weight, data=crab, family=binomial(link="logit"))
summary(crab.model)

'y=-3.6947 +1.8151x'

'
a. For a classification table using the sample proportion of 0.642 as the cutoff,
report the sensitivity and specificity. Interpret.'

prob_pred = predict(crab.model, type = 'response')
y_thresh = ifelse(prob_pred > 0.642,1,0)



confusionMatrix(table(predict(crab.model, type="response") >= 0.642, crab$y == 1))

'Here, the Sensitivity is 0.7258 and the Specificity is 0.6126. The sensitivy value suggests that ~72.58 percent would have attached crabs at the requested threshold.
The specificity value suggests that ~61.26 percent of unattached would be correctly identified by this model at the requested threshold.


'

'b. Form a ROC curve, and report and interpret the area under it.'

crab.fit = predict(crab.model, type = "response")
pred.roc = prediction(crab.fit, crab$y)
perf.roc = performance(pred.roc,"tpr","fpr")
plot(perf.roc, main = "Weight Model Curve")


library(pROC)
rocCurve=roc(response = crab$y,predictor = crab.fit)
auc(rocCurve)  
plot(rocCurve,legacy.axes=TRUE)
ci(rocCurve)


'
c. Investigate the model goodness-of-fit using the Hosmer-Lemeshowstatistic
or some other model-checking approach. Interpret.'
library(ResourceSelection)
hos = hoslem.test(crab.model$y, fitted(crab.model), g = 10)
hos

'Here, Hosmer and Lemeshow goodness of fit (GOF) test provides a p-value of 0.4499 which is substantially greater than 0.05.
Therefore, we will fail to reject the null hypothesis that the model is a bad fit.'

'
d. Inferentially compare the model to the model with x and x2 as predictors.
Interpret.'


crab.model2 <- glm(y~weight+weight2, data=crab, family=binomial(link="logit"))
summary(crab.model2)
'
For the first crab model, the equation is y = -3.6947 + 1.8151x where the model with weight^2 is y = -1.8877+ 0.2182x + 0.3393x^2. The intercept and coefficients for
the first model are significant where as adding weight^2 drastically changes the significance.
weight        1.8151
e. Compare the models in (d) using the AIC. Interpret.'
AIC(crab.model)
AIC(crab.model2)
'The AIC goodness of fit test suggests that crab.model is better fit than crab.model2 (199.7371<201.4605)'

#5.20
' Refer to Table 2.7 on mother's drinking and infant malformations.'


birthdefects <- matrix(
  c(17066, 48, 0.0,14464,38,0.5, 788,5,1.5, 126,1,4.0, 37,1, 7.0), nrow=5, ncol=3, byrow=TRUE)
dimnames(birthdefects) <- list(alcohol=c("0","<1", "1-2", "3-5",">=6"),
                               defects=c( "No", "Yes", "Scores"))

as.data.frame(birthdefects)
birthdefects = as.data.frame(birthdefects)

bdfcts.model = glm(cbind(Yes, No)~Scores,data=birthdefects,family=binomial)
summary(bdfcts.model)



'a. Fit the logistic regression model using scores {0, 0.5, 1.5, 4, 7} for alcohol
consumption. Check goodness of fit.'
''
bdfcts.model = glm(cbind(Yes, No)~Scores,data=birthdefects,family=binomial)
summary(bdfcts.model)

pchisq(1.9487,3, lower.tail = FALSE)
'The test provides a  p-value of 0.58 which suggests we fail to reject the null hypothesis that the model 
has a poor fit. This conclusion is supported by the ratio Residual Deviance to degrees of freedom ~2/3'


'b. Test independence using the likelihood-ratio test for the model in (a). (The
trend test of Section 2.5.1 is the score test for this model.)'
lrtest (bdfcts.model)

'c. The sample proportion of malformations is much higher in the highest
alcohol category because, although it has only one malformation, its sample
size is only 38. Are the results sensitive to this single observation? Re-fit
the model without it, entering 0 malformations for 37 observations, and
compare the results of the likelihood-ratio test. (Because results are sensitive
to a single observation, it is hazardous to make conclusions, even though n
was extremely large.)'

birthdefects2 <- matrix(
  c(17066, 48, 0.0,14464,38,0.5, 788,5,1.5, 126,1,4.0, 0,1, 7.0), nrow=5, ncol=3, byrow=TRUE)
dimnames(birthdefects2) <- list(alcohol=c("0","<1", "1-2", "3-5",">=6"),
                               defects=c( "No", "Yes", "Scores"))


birthdefects2 = as.data.frame(birthdefects2)

bdfcts2.model = glm(cbind(Yes, No)~Scores,data=birthdefects2,family=binomial)
summary(bdfcts2.model)

lrtest (bdfcts2.model)

'The LRT of B provided a P-Value of 0.03917 while the result with the zeroed parameter was 0.0098, significantly
smaller. The substantial lessening in pvalues between both tests do suggest that the model is sensative to the single
observation.'


'd. Fit the model and conduct the test of independence for all the data using
scores {1, 2, 3, 4, 5}. Compare the results with (b). (Results for highly
unbalanced data can be sensitive to the choice of scores.)'

birthdefects3 <- matrix(
  c(17066, 48, 1,14464,38,2, 788,5,3, 126,1,4.0, 0,1, 5), nrow=5, ncol=3, byrow=TRUE)
dimnames(birthdefects3) <- list(alcohol=c("0","<1", "1-2", "3-5",">=6"),
                                defects=c( "No", "Yes", "Scores"))

birthdefects3 = as.data.frame(birthdefects3)

bdfcts3.model = glm(cbind(Yes, No)~Scores,data=birthdefects3,family=binomial)
summary(bdfcts2.model)
pchisq(8.4319,3,lower.tail = FALSE )

# 5.21



birthdefects <- matrix(
  c(17066, 48, 14464,38,788,5,126,1,37,1), nrow=5, ncol=2, byrow=TRUE)
dimnames(birthdefects) <- list(alcohol=c("0","<1", "1-2", "3-5",">=6"),
                               defects=c("Yes", "No"))
fisher.test(birthdefects)


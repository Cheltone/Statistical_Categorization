' Statastics 578
  HW 2
  Rebecca Brink and Chad Melton
'


library(epiR)
library(ggplot2)
library(vcdExtra)
library(vcd)


###Number 4, 2.18
'
a) To calculate the expected frequency of 35.8, sum the first row (290), then sum the
first column (168), and then sum all (1362)

Finally,

(290*168)/1362 = 35.77

b) The degrees of freedom are equal to (3-1)*(3-1)=4'''

Table <- matrix(c(21,159,110,53,372,221,94,249,83), 3, 3, byrow=TRUE)
dimnames(Table) <- list("x"=c("Above", "Ave", "Below"), "y"=c("NTH", "PH", "VH"))

ChisP=chisq.test(Table, correct = TRUE)

'Chi-squared test

data:  Table
X-squared = 73.352, df = 4, p-value = 4.444e-15

The P-Value is very small or ~ 0. This extremely low P-Value suggests that happiness and income are related.

c) Both of the standarized residuals in the cells in question suggest strong evidence that there are fewer than the hypothesis predicted. 

d) Both of the standarized residuals in the cells in question suggest strong evidence that there are greater than the hypothesis predicted.






###Number 4, 2.20'

.Table <- matrix(c(59,109,53,85,100,36), 2, 3, byrow=TRUE)
dimnames(.Table) <- list("x"=c("Advanced", "Prelim"), "y"=c("Alone", "Spouse", "Others"))
.Test <- chisq.test(.Table, correct=FALSE)


'
          y
x          Alone Spouse Others
  Advanced    59    109     53
  Prelim      85    100     36
  
Pearsons Chi-squared test

data:  .Table
X-squared = 8.3292, df = 2, p-value = 0.01554 or ~0.02

The pvalue of 0.02 gives evidence that the cancer stage and habitation situations are associated.





###Number 4, 2.21
a) The X2 test is not appropriate to use in the situation because the subjects (men or women) choices are not independent.
In essence, each gender could choose as many as they wish (and it looks like they did). 

.Table <- matrix(c(60,40,75,25), 2, 2, byrow=TRUE)
dimnames(.Table) <- list("Gender"=c("Men", "Women"), "Factor A (or B or C)"=c("Yes", "No"))

       Factor A (or B or C)
Gender  Yes No
  Men    60 40
  Women  75 25





###Number 5, 2.27
A study on educational aspirations of high school students (S. Crysdale, Int. J.
Comp. Sociol., 16: 19-36, 1975) measured aspirations using the scale (some
high school, high school graduate, some college, college graduate). For students
whose family income was low, the counts in these categories were (9,
44, 13, 10); when family income was middle, the counts were (11, 52, 23, 22);
when family income was high, the counts were (9, 41, 12, 27).'''



'a. Test independence of aspirations and family income using X2 or G2.
Interpret, and explain the deficiency of this test for these data.'

IncAsp.names <-vector("list",length=2)
IncAsp.names[[1]] <- c("Low","Mid", "High")
IncAsp.names[[2]] <- c("HS","HSGrad", "College", "CollegeG")

IncAsp <- matrix(c(9, 44, 13, 10, 11, 52, 23, 22, 9, 41, 12, 27),3,4,byrow = TRUE, dimnames = IncAsp.names)
indi=chisq.test(IncAsp, correct = FALSE)

'data:  IncAsp
X-squared = 8.8709, df = 6, p-value = 0.181

The P-Value being 0.181 suggests we should fail to reject the null hypothesis. The deficiancy of the X2 test is that the x2 is not the most
powerful test that could be employed here.' 



StdResiduals = indi$residual

'             HS     HSGrad     College   CollegeG
Low   0.3261620  0.9490117 -0.09920338 -1.5853146
Mid  -0.1395075 -0.2985367  0.92045021 -0.2774993
High -0.1477224 -0.5481045 -0.92227915  1.7706533

Yes, the standardize residuals suggest that there is association between higher income families and higher educational aspirations.'


'c. Conduct a more powerful test. Interpret results.'

CMHtest(IncAsp, rscores = 1:3, cscores = 1:4)
'Cochran-Mantel-Haenszel Statistics 

                 AltHypothesis  Chisq Df     Prob
cor        Nonzero correlation 4.7489  1 0.029317
rmeans  Row mean scores differ 4.8673  2 0.087717
cmeans  Col mean scores differ 7.2240  3 0.065090
general    General association 8.8384  6 0.182870'





###NUmber 6, 2.30
CancerDat.names = vector("list",length=2)
CancerDat.names[[1]] = c("Surgery","Radiation")
CancerDat.names[[2]] = c("Controlled","Not Controlled")
CancerDat = matrix(c(21,2,15,3),2,2,byrow = TRUE, dimnames = CancerDat.names)

FET <- fisher.test(CancerDat)

'Fishers Exact Test for Count Data

data:  CancerDat
p-value = 0.6384
alternative hypothesis: true odds ratio is not equal to 1
95 percent confidence interval:
  0.2089115 27.5538747
sample estimates:
  odds ratio 
2.061731 '





###Number 7, 2.33

DP.data = array(c(19,11,132,52,0,6,9,97),dim=c(2,2,2),dimnames=list(def=c("W","B"),DP = c("Y","N"),Vic=c("W","B")))
CMHtest(DP.data,strata="Vic",overall=TRUE)

DP.data = array(c(19,11,132,52,0.5,6.5,9.5,97.5),dim=c(2,2,2),dimnames=list(def=c("W","B"),DP = c("Y","N"),Vic=c("W","B")))
oddsratio(DP.data[,,1],log=FALSE)

marg.table = DP.data[,,1]+DP.data[,,2]
oddsratio(marg.table,log=FALSE)


#b)
.Table <- matrix(c(19.5,132.5,11.5,52.5), 2, 2, byrow=TRUE)
dimnames(.Table) <- list("Race"=c("White", "Black"),"Death Penalty"=c("Yes", "No"))
'
	Fishers Exact Test for Count Data

data:  .Table
p-value = 0.8956
alternative hypothesis: true odds ratio is greater than 1
95 percent confidence interval:
  0.3191049       Inf
sample estimates:
  odds ratio 
0.6579193


TABLE FOR WHITE VICTIM

     Death Penalty
Race    Yes  No
White  19.5 132.5
Black  11.5  52.5

The odds ratio with a 0.5 addition to each cell is equal to ~0.79.


TABLE FOR BLACK VICTIM'
.Table <- matrix(c(0.5,9.5,6.5,97.5), 2, 2, byrow=TRUE)
dimnames(.Table) <- list("rows"=c("White", "Black"), "columns"=c("Yes", "No"))
fisher.test(.Table,alternative = "greater")

'Fishers Exact Test for Count Data

data:  .Table
p-value = 1
alternative hypothesis: true odds ratio is greater than 1
95 percent confidence interval:
   0 Inf
sample estimates:
odds ratio 
         0 


       Death Penalty
Race    Yes No
  White   0.5  9.5
  Black   6.5 97.5
  
In this case,the R command for the Odds ratio rounded the 0.5 to zero. An actual calculation 
shows that the true odds ratio with a 0.5 addition to each cell is equal to ~0.79.
These results (odds being <1) suggest that a black defendant is more likely to get a death 
sentence than a white defendent regardless of the victims race.


c)'

.Table <- matrix(c(19,141,17,149), 2, 2, byrow=TRUE)
dimnames(.Table) <- list("Victims Race"=c("White", "Black"),"Death Penalty"=c("Yes", "No"))
'           
        Death Penalty
Victims Race Yes  No
White         19 141
Black         17 149

odds ratio 
  1.180455 
  
The marginal odds ratio suggests that a white defendent is 1.18 more likely to get the death penalty.
Because the marginal and conditional tables give us conflicting results, these data clearly 
give an example of Simpsons Paradox.'
  
  
  
  
  
###2.36
'An example of this situation and Simpsons Paradox can be observed when considering
COVID-19 CFR rates of China and Italy from the beginning stages of the pandemic.
A break down of decadal age groups amongst Italians suggests that individual age groups
are more likely to survive a COVID19 infection than Chinese citizens. However,
an overall analysis of CFR would show that Chinese COVID positive patients are more
likely to survive than Italians. This phenomenom is due to the fact that of age
demographics between the two populations. Though having a much larger population, 
the Chinese are generally much younger than Italians. This paradox occurend 
because COVID-19 is much more deadly to elderly people (~ <60 i.e., Italians) 
than younger people. It is highly conceivable that similar comparible situations 
are occuring in various other communities.'





###2.37
'a)

'
#Male Victim
ORM = (0.0263*.9951)/(.0049*.9737)
#These results suggest that the odds a non-white male is 5.485311 greater than a white male.

#Femal Victim
ORF = (.0072*.9977)/(.0023*.9928)
##These results suggest that the odds a non-white female is 3.145885 greater than a white male.

#b)
PnonW=(.0072*.5) + (.0263*.5)
PW = (.0049*.5) + (.5*.0023)

ORMarginal = PnonW/PW
# The marginal odds ratio between race and victim is 4.6528



DP.data = array(c(198,19736,193,19749),dim=c(2,2)
dimnames=list("x"=c("Asprin", "Placebo"), "y"=c("Yes", "No"))
oddsratio(DP.data,level=0.95,log=FALSE)






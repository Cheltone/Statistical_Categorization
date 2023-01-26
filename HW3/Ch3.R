#Crab data analysis
crab = read.csv("C:\\Users\\rzaretzk\\Documents\\Stat578-2010\\crab.csv",header=TRUE)

crab.model = glm(satell~weight,data=crab,family = poisson(link ="log"))
summary(crab.model)
plot(crab.model)
plot(crab$satell~crab$weight)
points(crab.model$fit~crab$weight,col = "red")

library(MASS)
crab.model2 = glm.nb(satell~width,data=crab)
summary(crab.model2)
plot(crab.model)
plot(crab$satell~crab$width)
points(crab.model2$fit~crab$width,col = "red")

a= matrix(c(0, 17066, 48, 17114,.5, 14502, 38, 14502, 1.5, 788, 5, 793,4.0, 126, 1, 127,7.0, 37, 1, 38),5,4,byrow=TRUE)
alc = as.data.frame(a)
names(alc) = c("score","nonmalform","malform","total")
answer = glm(cbind(malform,nonmalform) ~ score, data = alc, family =binomial(link="identity"))
summary(answer)


a= matrix(c(0, 17066, 48, 17114,.5, 14502, 38, 14502, 1.5, 788, 5, 793,4.0, 126, 1, 127,7.0, 37, 0, 37),5,4,byrow=TRUE)
alc = as.data.frame(a)
names(alc) = c("score","nonmalform","malform","total")
answer = glm(cbind(malform,nonmalform) ~ score, data = alc, family =binomial(link="logit"))
summary(answer)

silicon = matrix(c(0, 8, 0,
                   0, 7, 0,
                   0, 6, 0,
                   0, 6, 0,
                   0, 3, 0,
                   0, 4, 1,
                   0, 7, 1,
                   0, 2, 1,
                   0, 3, 1,
                   0, 4, 1,
                   1, 9, 0,
                   1, 9, 0,
                   1, 8, 0,
                   1, 14, 0,
                   1, 8, 0,
                   1, 13, 1,
                   1, 11, 1,
                   1, 5,  1,
                   1, 7,  1,
                   1, 6,  1) ,20,3, byrow=TRUE)

silicon.dat = as.data.frame(silicon)
names(silicon.dat) = c("factor","defects","thickness")

model2 = glm(log(defects)~factor,data=silicon.dat,family=poisson(link="identity"))
summary(model2)

challenger = read.csv("C:\\Users\\rzaretzk\\Documents\\STAT572-2013\\Simonoff\\Data\\challenger.csv",header=TRUE)
challenger.model = glm(cbind(Damaged,(O.rings-Damaged)) ~ Temperature, data = challenger, family =binomial)
summary(challenger.model)





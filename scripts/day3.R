
library(tidyverse)

#GLMs####
titanic<-read.csv("datasets/titanic_long.csv")
table(titanic$class, titanic$survived)

titanic %>%
  group_by(class, survived) %>%
  summarise(count = n())

##class####
tit.glm <- glm(survived ~ class,
               data = titanic,
               family = binomial)

library(effects)
allEffects(tit.glm)
summary(allEffects(tit.glm))

library(modelbased)
estimate_means(tit.glm)
estimate_contrasts(tit.glm)

#kable(xtable::xtable(tit.glm), digits = 2)   #this is for r markdown. Will produce a table
library(visreg)
visreg(tit.glm, scale = "response", rug = FALSE)

library(sjPlot)
sjPlot::plot_model(tit.glm, type = "eff")

#check if residuals are inside the error bounds.
binned_residuals(tit.glm)

library(DHARMa)
simulateResiduals(tit.glm, plot = TRUE)

##sex####
tit.sex<-glm(formula = survived ~ sex, family = binomial, data = titanic)
summary(tit.sex)
#model checking
simulateResiduals(tit.sex, plot = TRUE)

##sex and class#####
## Did women have higher survival because they travelled more in first class?
#lets look at data
table(titanic$class, titanic$survived, titanic$sex)

#two ways to look at it
   ##additive models and 
   ##interactive (multiplication) models

#additive
tit.sex.class.add<-glm(formula = survived ~ class + sex, family = binomial, data = titanic)
plot_model(tit.sex.class.add, type = "est")  #not sure what this does.
par(mfrow=c(2,2))
plot(tit.sex.class.add)
visreg(tit.sex.class.add)

#interactive
tit.sex.class.int<-glm(formula = survived ~ class * sex, family = binomial, data = titanic)
plot_model(tit.sex.class.int, type = "int")#type is interactive
par(mfrow=c(2,2))
plot(tit.sex.class.int)
visreg(tit.seq.class.int)

##compare
compare_performance(tit.sex.class.add, tit.sex.class.int)

#Other questions:
    #Is survival related to age?
    #Are age effects dependent on sex?







#Logistic regression for proportion data ie moves from 0-1

tit.prop<-read.csv("./datasets/titanic_prop.csv")
#Yes is people who survived and No is those that did not

prop.glm <- glm(cbind(Yes, No) ~ Class, data = tit.prop, family = binomial)
visreg(prop.glm)

cbind(`tit.prop`$Yes, `tit.prop`$No)



###UN gdp data

gdp<-read.csv("datasets/UN_GDP_infantmortality.csv")

gdp.glm <- glm(cbind(infant.mortality, 1000 - infant.mortality) ~ gdp,
               data = gdp, family = binomial)
#note:we are using binomial although both variables are continous, because lm isn't doing well
#dont be afrid to try differnt models for your data , then compare
allEffects(gdp.glm)
visreg(gdp.glm, scale = "response")
points(infant.mortality/1000 ~ gdp, data = gdp)
simulateResiduals(gdp.glm, plot = TRUE)


##Overdispersion
#Testing for overdispersion
library(DHARMa)
simres <- simulateResiduals(gdp.glm, refit = TRUE)
testDispersion(simres, plot = FALSE)


#quasibinomial allows us to model overdispersed binomial data
gdp.overdisp <- glm(cbind(infant.mortality, 1000 - infant.mortality) ~ gdp,
                    data = gdp, family = quasibinomial)

coef(gdp.overdisp)
coef(gdp.glm)

#Mean estimates do not change after accounting for overdispersion
#standard error in relation to estimate(dispersion)
#But standard errors (uncertainty) do!


#Not everything has to be linear…
#Residuals show non-linear pattern


#Assignment
soccer <- read.csv("datasets/soccer.csv")
soccer

lm(Nshots~Scored,data=soccer)  #data too small

##Two
seedset <- read.csv("datasets/seedset.csv")

lm(pcmass~seeds, data=seedset)

glm(cbind(infant.mortality, 1000 - infant.mortality) ~ gdp,
    data = gdp, family = binomial)





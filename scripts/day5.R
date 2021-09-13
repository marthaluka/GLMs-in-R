
install.packages("rstanarm")
library(rstanarm)
data("Wells")


###
library(lme4)
data("sleepstudy")

sleep <- lmer(Reaction ~ Days + (1+Days|Subject), data = sleepstudy)
summary(sleep)
visreg(sleep, xvar = "Days", by = "Subject", re.form = NULL)
Sorry

coef(sleep)

#Multilevel logistic regression#######
#trees dataset
trees<-read.csv("datasets/trees.csv")
plot(dead ~ dbh, data = trees)
plot(as.factor(dead) ~ dbh, data = trees)

#Fit simple logistic regression
simple.logis <- glm(dead ~ dbh, data = trees, family=binomial)
summary(simple.logis)

#Logistic regression with independent site effects
logis2 <- glm(dead ~ dbh + factor(site), data = trees, family=binomial)
summary(logis2)

#Fit multilevel logistic regression
mixed.logis <- glmer(dead ~ dbh + (1|site), data=trees, family = binomial)
summary(mixed.logis)

#Retrieve model coefficients
coef(mixed.logis)





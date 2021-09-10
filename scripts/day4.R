###Assignment

gdp<-read.csv("datasets/UN_GDP_infantmortality.csv")

gdp_model <- glm(infant.mortality ~ gdp,
                 data = gdp,
                 family = poisson)
summary(gdp_model)
simulateResiduals(gdp_model, plot = TRUE)

allEffects(gdp_model)
#equatiomatic::extract_eq(gdp_model)

library(parameters)
plot(simulate_parameters(gdp_model)) +
  geom_vline(xintercept = 0) +
  ggtitle("Effect of GDP on mortality")

plot(allEffects(gdp_model))
plot(gdp_model)

check_model(gdp_model)
simres <- simulateResiduals(gdp_model, refit = TRUE)
testDispersion(simres)



gdp_model_quasi<-glm(formula = infant.mortality ~ gdp,
                     data = gdp,
                     family = quasipoisson)

#DHARMa doesnt deal with quasi
summary(gdp_model_quasi)
check_model(gdp_model_quasi)

#Accounting for overdispersion using negative binomial
library(MASS)

gdp_model_nb <- glm.nb(infant.mortality ~ gdp, data = gdp)
check_model(gdp_model_nb)
simulateResiduals(gdp_model_nb, plot = TRUE)
visreg(gdp_model_nb) ##model is overestimating mortality in countrues with middle-GDPs

compare_models(gdp_model_nb, gdp_model_quasi, gdp_model)
compare_performance(gdp_model_nb, gdp_model_quasi, gdp_model)

###EGGS
#diameter poisson
eggs<-read.csv("./datasets/eggs.csv")
eggs_model1 <- glm(n.eggs ~ diameter,
                   data = eggs,
                   family = poisson)
summary(eggs_model1)
visreg(eggs_model1)
simulateResiduals(eggs_model1, plot = TRUE)


eggs_model1_1<-glm(formula = n.eggs ~ diameter,
                   data = eggs,
                   family = quasipoisson)
summary(eggs_model1_1)


#old
eggs_model2 <- glm(n.eggs ~ old,
                   data = eggs,
                   family = poisson)
summary(eggs_model1)
visreg(eggs_model1)
simulateResiduals(eggs_model1, plot = TRUE)

#old+diameter
eggs_model3 <- glm(n.eggs ~ old+diameter,
                   data = eggs,
                   family = poisson)

#old*diameter
eggs_model4 <- glm(n.eggs ~ old*diameter,
                   data = eggs,
                   family = poisson)


#GLMMs###########
  #involves "mixing" two or more GLMs eg binomial and poisson e.g in checking for number of eggs using 
    #size of nest and old (yes/no),,, we need to know whether or not the nest is occupied in the first place
    #some old nests may be un-occupied. This inflates the zeros in poisson model

#There are two ways to go about it:
#Zero-inflated Poisson/Negative Binomial (ZIP/ZINB)
  #1. ZIP/ZINB: a) Binomial model: probability of zero,  b) Count model (Poisson/NegBin) includes zero
  #2. Hurdle: a) Binomial model: probability of non-zero, b) Count model truncated at 1 (ie doesnt include 0)


library(glmmTMB)

#Example 1##########
#How many hives per skin area?
hives <- read.csv("datasets/hives.csv")
hist(hives$n.hives) #there are a lot of zeros BUT that does not mean we need zero-inflated model!

#Number of hives ~ age * vaccinated
coplot(n.hives ~ age | as.factor(vaccinated), data = hives)

##Trying Poisson GLM#
##Poisson#####
hives.poi <- glm(n.hives ~ vaccinated * age,
                 offset = log(area.cm2),   #offset by area because the larger the area, the more the hives.ie account for sampling bias 
                 data = hives,
                 family = poisson)
summary(hives.poi)

#Visualising fitted Poisson GLM
plot(hives.poi)

#Checking Poisson GLM
pp_check(hives.poi)  #this tells you about the performance of your model
hives.poi.res <- simulateResiduals(hives.poi, plot = TRUE)

#Checking overdispersion
testDispersion(hives.poi.res)
                                  #not sure the difference in the 2 visualizations
#Checking zero inflation
testZeroInflation(hives.poi.res)

##Accounting for zero-inflation with hurdle model
##Hurdle model##########
hives.hur <- glmmTMB(n.hives ~ vaccinated + age,
                     family = truncated_poisson,
                     ziformula = ~ 1,
                     offset = log(area.cm2),
                     data = hives)
summary(hives.hur)

#Checking hurdle model
pp_check(hives.hur)
hives.hur.res <- simulateResiduals(hives.hur, plot = TRUE) #DHARMa
testZeroInflation(hives.hur.res) #this is now good
testDispersion(hives.hur.res)

##ZIP/ZINB ###### 
#me trial
hives.zip <- glmmTMB(n.hives ~ vaccinated + age,
                    family = "poisson",
                    ziformula = ~ 1,
                    offset = log(area.cm2),
                    data = hives)

summary(hives.zip)

#Checking ZIP/ZINB model
pp_check(hives.zip)
hives.zip.res <- simulateResiduals(hives.zip, plot = TRUE) #DHARMa
testZeroInflation(hives.zip.res) #this is now good
testDispersion(hives.zip.res)


#Comparing models
compare_models(hives.poi, hives.hur, hives.zip)
compare_performance(hives.poi, hives.hur, hives.zip)


#hurdles and ZIP comparable in this case



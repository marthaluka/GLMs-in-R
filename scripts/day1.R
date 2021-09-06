
##GLM Physalia courses
#06-09-2021
#Day 1

require("pacman")
pacman::p_load(rmarkdown, knitr, dplyr, effects, modelbased, performance, 
         xtable, modelsummary,visreg, ggplot2, sjPlot, parameters, see, DHARMa,readr, broom, 
         equatiomatic, report, gtsummary, lme4, broom.mixed,mgcv, pmsampsize,brms, mvabund, here, 
         equatiomatic)

here::here()

trees<-read.csv("./datasets/trees.csv")

summary(trees)
head(trees)

#always plot your data first
plot(trees$height)
hist(trees$height)

plot(height~dbh, data=trees)

#linear regression
# outcome ~ predictor
m1 <- lm(height ~ dbh, data = trees)
summary(m1)

equatiomatic::extract_eq(m1)  #gives you equation of lm

library("parameters")
parameters(m1)

library("broom")
tidy(m1)

library("report")
report(m1)

library("xtable")
xtable(m1, digits = 2)



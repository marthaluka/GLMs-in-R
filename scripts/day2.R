
library(tidyverse)
library(see)
library(performance)


#day2
##Tree height 
here::here()
mydata<-read.csv("./datasets/trees.csv")

head(mydata)

m1<-lm(height ~ sex, data=mydata)
summary(m1)


###paperplanes data
library(paperplanes)
head(paperplanes)
table(paperplanes$paper)

#linear models. Find best predictor of flight distance
#model1
model1<-lm(distance~hour, data = paperplanes)
summary(model1)
plot(model1)
par(mfrow=c(2,2))
visreg(model1)

#model2
model2<- lm(distance~gender, data = paperplanes)
summary(model2)
plot(model2)

#model3
model3<- lm(distance~age, data = paperplanes)
summary(model3)
plot(model3)
visreg(model3)

#model4
model4<- lm(distance~plane, data = paperplanes)
summary(model4)
plot(model4)
visreg(model4)

#model5
paperplanes$paper<-as.factor(paperplanes$paper)
model5<- lm(distance~paper, data = paperplanes)
summary(model5)
plot(model5)
par(mfrow=c(2,2))
visreg(model5)

#model6
model6<- lm(distance~hour+gender+age+plane+paper, data = paperplanes)
summary(model6)
plot(model6)

compare_performance(model1, model2, model3, model4, model5, model6)

check_model(model1)

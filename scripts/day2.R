
#day2
here::here()
mydata<-read.csv("./datasets/trees.csv")

head(mydata)

model1<-lm(height ~ sex, data=mydata)
summary(model1)

#site on height
mydata$site<-as.factor(mydata$site)
model2<-lm(height ~ site, data=mydata)
summary(model2)

####
#Generating a time series data to explore.

rm(list=ls())

pacman::p_load(dplyr, tidyverse, zoo, shiny, leaflet, RColorBrewer, xts, rgdal, visreg,
               here, performance, see, DHARMa, nlme)

#read data####
owid_Data<-read.csv(
  "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",
  fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE
) %>%
  select(
    location, date, total_cases_per_million, new_cases_per_million, total_vaccinations_per_hundred,
    stringency_index, population_density, median_age, handwashing_facilities
  ) %>%
  filter(
    location == "Kenya"| location == "Uganda"| location =="Nigeria"| location =="South Africa"
  )

#pre-process####
owid_Data[c(
  "total_cases_per_million", "new_cases_per_million", "total_vaccinations_per_hundred"
)][is.na(owid_Data[c(
  "total_cases_per_million", "new_cases_per_million", "total_vaccinations_per_hundred")])] <- 0

owid_Data$date<- as.Date(owid_Data$date)

owid_Data1 <- owid_Data %>%
  dplyr::group_by(location) %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate(
    smoothed_new_cases_per_million = zoo::rollmean(new_cases_per_million, k = 14, fill = NA),  #smoothing
    smoothed_total_cases_per_million = zoo::rollmean(total_cases_per_million, k = 14, fill = NA),
    smoothed_total_vaccinations_per_hundred= zoo::rollmean(total_vaccinations_per_hundred, k=14, fill=NA)
  ) %>%   #biweekly rolling mean 4 smoothing
  
  dplyr::ungroup() 

here::here()
#write_csv(owid_Data1, "./datasets/glm_data_martha.csv")


#visualize to check if all is okay#######
ggplot(data=owid_Data1)+
  #geom_line(aes(x=date, y=smoothed_new_cases_per_million, colour=location))+
  #geom_line(aes(x=date, y=new_cases_per_million, colour=location))+
  geom_line(aes(x=date, y=stringency_index, colour=location))+
  #geom_line(aes(x=date, y=smoothed_total_vaccinations_per_hundred, colour=location))+
  #geom_line(aes(x=date, y=population_density, colour=location))+
  
  theme_bw()

#regression####

head(owid_Data1)
selected<- owid_Data1 %>%
  filter(
    location=="Kenya"
  )

#GOVT stringency#####
lm1 <- lm(smoothed_new_cases_per_million ~ stringency_index, data = selected)
summary(lm1)
visreg(lm1)
check_model(lm1)

##adding location
lm2 <- lm(smoothed_new_cases_per_million ~ stringency_index+location, data = owid_Data1)
summary(lm2)
visreg(lm2, xvar = "stringency", by="location")


m <- lm(smoothed_new_cases_per_million ~ date, data =selected)
testTemporalAutocorrelation(simulateResiduals(m), time = date)

##multiplying the location
lm3 <- lm(smoothed_new_cases_per_million ~ stringency_index*location, data = owid_Data1)
summary(lm3)
visreg(lm3, xvar="stringency_index", by="location")


#VACCINATIONS#####
lm01 <- lm(smoothed_new_cases_per_million ~ smoothed_total_vaccinations_per_hundred, 
           data = selected)
summary(lm01)
visreg(lm01)

##adding location
lm02 <- lm(smoothed_new_cases_per_million ~ smoothed_total_vaccinations_per_hundred+location, 
           data = owid_Data1)
summary(lm02)
visreg(lm02)

##multiplying the location
lm03 <- lm(smoothed_new_cases_per_million ~ smoothed_total_vaccinations_per_hundred*location, 
           data = owid_Data1)
summary(lm03)
visreg(lm03)



#median age
lmAge <- lm(smoothed_new_cases_per_million ~ median_age*location, data = owid_Data1)
summary(lmAge)
visreg(lmAge)

table(owid_Data1$population_density)

compare_performance(lm2, lm3)
compare_performance(lm02, lm03)


###############################
titanic<-read.csv("./datasets/titanic_long.csv")

class<-lm(survived~class, data=titanic)
summary(class)

#glm with binomial distribution

tit.glm <- glm(survived ~ sex,
               data = titanic,
               family = binomial)
library("effects")
allEffects(tit.glm)



confusingData<-read.csv("./datasets/UN_GDP_infantmortality.csv")
head(confusingData)
table(confusingData$gdp)


rm(list=ls())

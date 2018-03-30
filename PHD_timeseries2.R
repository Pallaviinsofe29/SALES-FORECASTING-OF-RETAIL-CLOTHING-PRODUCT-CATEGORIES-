library(forecast)
library(lubridate)
library(DataCombine)
library(imputeTS)
library(plyr)
library(dplyr)
library(TTR)
library(graphics)
library(data.table)
library(Quandl)
library(DMwR)
library(readxl)

rm(list = ls())
setwd("F:/insofe classes/phd/salesdata")

traindata<-read.csv("Train.csv")
sum(is.na(traindata))
colSums(is.na(traindata))
Event_holidays<-read_xlsx("Events_HolidaysData.xlsx")
MacroEco<-read_xlsx("MacroEconomicData.xlsx")
template<-read.csv("template.csv")
weather_data<-read_xlsx("WeatherData.xlsx")


summary(traindata$ProductCategory)


##Timeseries for mens data
mens_data<-traindata[traindata$ProductCategory=="MenClothing",]
sum(is.na(mens_data))

mens_data<-knnImputation(mens_data,k=3)

mensdatatime<-ts(mens_data$Sales.In.ThousandDollars.,frequency =24)
ggtsdisplay(mensdatatime)
mareco <- auto.arima(mensdatatime)
mareco
mensforecast <- forecast(mareco, level = c(95), h = 12)
plot(mensforecast)

write.csv(mensforecast,"mofre.csv")



##Timeseries for womens data
womensdata<-traindata[traindata$ProductCategory=="WomenClothing",]

sum(is.na(womensdata))
womentime<-ts(womensdata$Sales.In.ThousandDollars.,frequency =24)
ggtsdisplay(womentime)
womenarem <- auto.arima(womentime)
womenarem
womenforecast <- forecast(womenarem, level = c(95), h = 12)
plot(womenforecast)
write.csv(womenforecast,"wofre.csv")

##Timeseries for 
othersdata<-traindata[traindata$ProductCategory=="OtherClothing",]
sum(is.na(othersdata))
othersdata<-knnImputation(othersdata,k=3)
otherstime<-ts(othersdata$Sales.In.ThousandDollars.,frequency =24)
ggtsdisplay(otherstime)
othersarem <- auto.arima(otherstime)
othersarem
othersforecast <- forecast(othersarem, level = c(95), h = 12)
plot(othersforecast)
write.csv(othersforecast,"ofore.csv")















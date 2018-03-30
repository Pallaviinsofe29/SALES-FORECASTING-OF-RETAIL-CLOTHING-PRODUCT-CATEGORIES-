rm(list=ls(all=TRUE))

setwd("F:/insofe classes/phd/salesdata")


library(readxl)






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
library(gtools)
library(car)





#traindata
train_data=read.csv('Train.csv',header = TRUE,sep=',')
train_women_data=train_data[train_data$ProductCategory=='WomenClothing',]
#droping product category
train_women_data$ProductCategory=NULL
str(train_women_data)



#Holidays
Event_Holidays_data=read_xlsx('Events_HolidaysData.xlsx',sheet=1,col_names=TRUE)
str(Event_Holidays_data)


#MacroEconomics
Macro_Economics_data=read_xlsx('MacroEconomicData.xlsx',sheet=1,col_names=TRUE)
str(Macro_Economics_data)

Train_Macro_Economics_data=Macro_Economics_data[1:84,]
str(Train_Macro_Economics_data)


Test_Macro_Economics_data=Macro_Economics_data[85:96,]
str(Test_Macro_Economics_data)
Test_Macro_Economics_data$`Year-Month`=NULL
Test_Macro_Economics_data$PartyInPower=NULL
#Test_MacroEconomics$`AdvertisingExpenses (in Thousand Dollars)`=NULL
str(Test_Macro_Economics_data)





library(readxl)
Weather1=read_xlsx('WeatherData.xlsx',sheet = '2009',col_names=TRUE)
Weather2=read_xlsx('WeatherData.xlsx',sheet = '2010',col_names=TRUE)
Weather3=read_xlsx('WeatherData.xlsx',sheet = '2011',col_names=TRUE)
Weather4=read_xlsx('WeatherData.xlsx',sheet = '2012',col_names=TRUE)
Weather5=read_xlsx('WeatherData.xlsx',sheet = '2013',col_names=TRUE)
Weather6=read_xlsx('WeatherData.xlsx',sheet = '2014',col_names=TRUE)
Weather7=read_xlsx('WeatherData.xlsx',sheet = '2015',col_names=TRUE)
Weather8=read_xlsx('WeatherData.xlsx',sheet = '2016',col_names=TRUE)


str(Macro_Economics_data)
Event_Holidays_data=NULL

head(Event_Holidays_data)
Event_Holidays_data$MonthDate=as.character(Event_Holidays_data$MonthDate)
str(Event_Holidays_data)

Holiday_Rows=dim(Event_Holidays_data)[1]
Holiday_Col=dim(Event_Holidays_data)[2]
Event_Holidays_data$MonthDate=as.character(Event_Holidays_data$MonthDate)

print(Holiday_Rows)



##Macro Economics
sum(is.na(Train_Macro_Economics_data))
#droping AdvertisingExpenses (in Thousand Dollars)
#Train_MacroEconomics$`AdvertisingExpenses (in Thousand Dollars)=NULL

##BINDING WITH TRAIN DATA

Train_dataframe=cbind(train_women_data,Train_Macro_Economics_data)

str(Train_dataframe)
##checking column wise missing values
apply(Train_dataframe,2,function(x) sum(is.na(x)))

library(DMwR)
Train_dataframe=centralImputation(Train_dataframe)

##dropping variables
Train_dataframe$PartyInPower=NULL
Train_dataframe$`Year-Month`=NULL
Train_dataframe$Year=NULL
##converting factors
Train_dataframe$Month=as.factor(Train_dataframe$Month)


numerical_val=c("Monthly Nominal GDP Index (inMillion$)","Monthly Real GDP Index (inMillion$)"
                ,"CPI","unemployment rate","CommercialBankInterestRateonCreditCardPlans"
                ,"Finance Rate on Personal Loans at Commercial Banks, 24 Month Loan"
                ,"Earnings or wages  in dollars per hour"
                ,"Cotton Monthly Price - US cents per Pound(lbs)"
                ,"Change(in%)","Average upland planted(million acres)"
                ,"Average upland harvested(million acres)","yieldperharvested acre"
                ,"Production (in  480-lb netweright in million bales)"
                ,"Exports")

##Standerdizing Train data
library(caret)
preprocessing=preProcess(Train_dataframe[,numerical_val],method = c("center", "scale"))
Train_dataframe=predict(preprocessing,Train_dataframe)
names(Train_dataframe)
##applying linear model
model_basic=lm(Sales.In.ThousandDollars.~.,data=Train_dataframe)

#model_basic=lm(Sales.In.ThousandDollars.~.,data=Train_df)

##checking summary
summary(model_basic)

##plotting model
par(mfrow = c(2,2))
plot(model_basic)


##feature selection
library(MASS)
model_aic=stepAIC(model_basic)
model_aic$coefficients
summary(model_aic)
plot(model_aic)

Test_data=read.csv('template (2).csv',header = TRUE,sep=",")
Women_Test_data=Test_data[Test_data$ProductCategory=='WomenClothing',]
Women_Test_data$ProductCategory=NULL
Women_Test_data$Year=NULL

##CONVERTING TO FACTOR
Women_Test_data$Month=as.factor(Women_Test_data$Month)

str(Women_Test_data)
##binding macroeconmic data with testdata
Test_dataframe=cbind(Women_Test_data,Test_Macro_Economics_data)

##taking mean and sd from train and applying on test for standardization
Test_dataframe=predict(preprocessing,Test_dataframe)

##prediction on test data
pred_model=predict(model_basic,Test_dataframe)

write.csv(pred_model,'linear1.csv')
##Error Metrics
library(DMwR)
#regr.eval(Test_df, pred_model)



##prediction on test data for step aic
pred_aic=predict(model_aic,Test_dataframe)

summary(pred_aic)
write.csv(pred_aic,'linear.csv')
##Error Metrics
library(DMwR)
regr.eval(Test_dataframe$Sales.In.ThousandDollars., pred_aic)

#library(DAAG)
#cvResults <- suppressWarnings(CVlm(df=Train_df, form.lm=Sales.In.ThousandDollars.~, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."))
#attr(cvResults, 'ms')





library(rpart)
rpart(Sales.In.ThousandDollars.~.,data =Train_dataframe)->decision
summary(decision)

plot(decision)
prediction1 = predict(decision, Train_dataframe)

plot(prediction1)
library(DMwR)

regr.eval(Train_dataframe$Sales.In.ThousandDollars., prediction1)

prediction.test = predict(decision, Test_dataframe)

prediction.test

regr.eval(Test_dataframe$Sales.In.ThousandDollars., prediction.test)

decision$cptable[which.min(decision$cptable[,"xerror"]),"CP"]->min.xerror

rt.pruned <- prune(decision,cp = min.xerror) 

train.pred.rtree.p <- predict(rt.pruned,Train_dataframe)

#test.pred.rtree.p <- predict(train.pred.rtree.p,Test_df)

test.pred.rtree.p <- predict(rt.pruned,Test_dataframe)
write.csv(test.pred.rtree.p,'decision1.csv')






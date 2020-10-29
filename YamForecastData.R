#Yam data analysis - The case of Nigeria
#Upload the data
Yamdata<-read.csv(file.choose(), header = T)
#Upload the relevant libraries for the analysis
library(dplyr)
library(ggplot2)
library(forecast) 
library(MLmetrics)
library(lubridate)
library(scales)
library(tidyr)

#data statistics summary
summary(Yamdata)
str(Yamdata)
View(Yamdata)
names(Yamdata)
names(Yamdata)=c("Year","YamOutput","YamSupply", "NonFoodUsage","PostHarvestLoss","FoodLoss")
View(Yamdata)

#Four different techniques are adopted in this study
#They are thus stated as they applied in this analysis

#Partition Yam data into training and test data

#Traindata=window(Yamdata, start = ())

View(Yamdata)

#smp<-floor(0.70*nrow(Auto))
#Set the seed to produce the same result
#trainid<-sample(seq_len(nrow(Carseats)), size = smp)
#train<-Carseats[trainid,]
#test<-Carseats[-trainid,]
#Variable of interest
#names c("Year","YamOutput","YamSupply", "NonFoodUsage","PostHarvestLoss","FoodLoss")
#Create a timeseries for each of the key variable
#Yam Output, YO	Yam Supply, and YS	Non-food usage loss (ton)

YamOutput<-select(Yamdata, YamOutput)
YamOutput<-ts(YamOutput, start = 1980, end = 2017, frequency = 1)
str(YamOutput)

YamSupply<-select(Yamdata, YamSupply)
YamSupply<-ts(YamSupply,  start = 1980, end = 2017, frequency = 1)
str(YamSupply)

NonFoodUsage<-select(Yamdata, NonFoodUsage)
NonFoodUsage<-ts(NonFoodUsage,  start = 1980, end = 2017, frequency = 1)
str(NonFoodUsage)
View(NonFoodUsage)


#Yam output training and testing data
TrainingYamO<-window(YamOutput, start = 1980, end = 2007)
TestingYamO<-window(YamOutput, start=2007)
#Yam Supply training and tesing data
TrainingYamS<-window(YamSupply, start = 1980, end = 2007)
TestingYamS<-window(YamSupply, start=2007)
#Non Food Usage loss
TrainingNf<-window(NonFoodUsage, start = 1980, end = 2007)
TestingNf<-window(NonFoodUsage, start=2007)

#Traindata=Yamdata1[1:27,]
#Testdata=Yamdata1[28:38, ]

#Naive forecasting technique
naiveYamO<-snaive(TrainingYamO, h=length(TestingYamO))
MAPE(naiveYamO$mean, TestingYamO)*100
summary(naiveYamO)

#The four plots
par(mfrow=c(1,1))

plot(YamOutput, col='blue',xlab='Year', ylab='Yam Output in Nigeria',
     main='Seasonal Naive Forecast')
lines(naiveYamO$mean, col="red", lwd=2)

#Exponetical smoothing
#State space model

Yam_etsmod1=ets(TrainingYamO, allow.multiplicative.trend = T)
summary(Yam_etsmod1)

#Forecast using the ETS
ets_for<-forecast(Yam_etsmod1, h=length(T))
MAPE(ets_for$mean, TestingYamO)*100

plot(YamOutput, col='blue',xlab='Year', ylab='Yam Output in Nigeria',
     main='ETS Forecast')
lines(ets_for$mean, col="red", lwd=2)




#arima optimal=arim

Yarima<-auto.arima(TrainingYamO)
Yarima


library(astsa)

sarima_forecast<-sarima.for(TrainingYamO, n.ahead = length(TestingYamO), p=0,d=1,q=0)

MAPE(sarima_forecast$pred,TestingYamO)


plot(YamOutput, col='blue',xlab='Year', ylab='Yam Output in Nigeria',
     main='SARIMA Yam Output Forecast')
lines(sarima_forecast$pred, col="red", lwd=2)


?MAPE

#training<-window(AirPassengers, start=c(1949,1), end=c(1955,12))
#testing<-window(AirPassengers, start=c(1956,1))

#arima_opt<-auto.arima(training)
#arima_opt
#air_for<-sarima.for(training, n.ahead = length(testing), p=0,q=1,d=1,P=1,D=1,Q=0, S=12)

#air_for$pred
#MAPE(air_for$pred,testing)*100

#plot(AirPassengers, col='blue',xlab='Year', ylab='Yam Output in Nigeria',
#    main='SARIMA Yam Output Forecast')
#lines(air_for$pred, col="red", lwd=2)

#Sarima is used for forecasting due to its lowest MAPE
#Yam Output (YO),Yam Supply (YS), and YS	Non-food usage loss (NFS)
#YamOutput","YamSupply", "NonFoodUsage
#Forecasting Yam output in Nigeria to 2030
YOOpt<-auto.arima(YamOutput)
YOOpt
YO_for<-sarima.for(YamOutput, n.ahead = 13, p=1,q=1,d=1)
View(YO)

#YamSupply
YSOpt<-auto.arima(YamSupply)
YSOpt
YS_for<-sarima.for(YamSupply, n.ahead = 13, p=0,q=1,d=1)


#NonFoodUsage
NFUOpt<-auto.arima(NonFoodUsage)
NFUOpt
NFU_for<-sarima.for(NonFoodUsage, n.ahead = 13, p=1,q=1,d=0)

str(YamSupply)
str(NonFoodUsage)

#Combining the data together
Yam_actual<-cbind(YamOutput,YamSupply, NonFoodUsage)
Yam_forecast<-cbind(YO_for$pred, YS_for$pred, NFU_for$pred)
View(Yam_actual)
View(Yam_forecast)

colnames(Yam_actual)
Yam_actual1<-as.data.frame(Yam_actual)
#Changing the column name of the predicted data to the actual

names(Yam_actual1)

Yam_forecast1<-as.data.frame(Yam_forecast)
names(Yam_forecast1)<-c("YamOutput", "YamSupply","NonFoodUsage")
View(Yam_forecast1)

#Combining the actual with forecast data
Yamdata1<-rbind(Yam_actual1, Yam_forecast1)
View(Yamdata1)
Yamdata2<-mutate(Yamdata1, Year=1980:2030)
Yamdata2<-select(Yamdata2,"Year","YamOutput", "YamSupply", "NonFoodUsage")
View(Yamdata2)
summary(Yamdata2)
Yamdata3<-mutate(Yamdata2,PostHarvestLoss=YamOutput-YamSupply, FoodLoss= round((PostHarvestLoss/YamOutput*100),2))

View(Yamdata3)

Yamdata3<-mutate(Yamdata3,SupplyChainLoss=Yamdata3$PostHarvestLoss-NonFoodUsage)

#Save the forecast into the system
write.csv(Yamdata3, file = "Nigerian Yam Actual & Predict 1980 - 2030.csv")

View(Yamdata3)
names(Yamdata3)
#Plot the graph using the Ggplot
#Putting the Yamouput on the visual representation

ggplot(Yamdata3, aes(Year, YamOutput)) +geom_line(size=2)+geom_point(size=2)+
  theme_bw()+scale_y_continuous(labels=comma) + ylab("Yam Output (ton)")

#Putting the Yam Supply on the visual representation

ggplot(Yamdata3, aes(Year, YamSupply)) +geom_line(size=2)+geom_point(size=2)+
  theme_bw()+scale_y_continuous(labels=comma) + ylab("Yam Supply (ton)")

#Putting the Non-food usage loss (ton) on the visual representation

ggplot(Yamdata3, aes(Year, NonFoodUsage)) +geom_line(size=2)+geom_point(size=2)+
  theme_bw()+scale_y_continuous(labels=comma) + ylab("Non-food usage loss (ton)")

#Putting the Postharvest Loss (ton) on the visual representation

ggplot(Yamdata3, aes(Year,PostHarvestLoss)) +geom_line(size=2)+geom_point(size=2)+
  theme_bw()+scale_y_continuous(labels=comma) + ylab("Post-harvest Loss (ton)")


#Putting the % Food  Loss on the visual representation

ggplot(Yamdata3, aes(Year, FoodLoss)) +geom_line(size=2)+geom_point(size=2)+
  theme_bw()+scale_y_continuous(labels=comma) + ylab("Food  Loss (%)") +stat_smooth(color="red", method = "loess")

#The actual plot and forecast plots
#The actual data
Yamdata4<-Yamdata3[1:38,]
View(Yamdata4)
summary(Yamdata4)
library(scales)
#One actual and forecast
Yamdata5<-Yamdata3[38:51,]
View(Yamdata5)
summary(Yamdata5)

#Putting the Yamouput on the visual representation
ggplot() +geom_line(Yamdata4, mapping=aes(x=Year, y=YamOutput), color="blue",size=2)+geom_line(Yamdata5, mapping=aes(x=Year, y=YamOutput), color="red",size=2)+
  theme_bw()+scale_y_continuous(labels=comma) + ylab("Yam Output (ton)")

#Putting the Yam Supply on the visual representation
ggplot() +geom_line(Yamdata4, mapping=aes(x=Year, y=YamSupply), color="maroon",size=2)+
  geom_line(Yamdata5, mapping=aes(x=Year, y=YamSupply), color="red",size=2)+
  theme_bw()+scale_y_continuous(labels=comma) + ylab("Yam Supply (ton)")

#Putting the Non-food usage loss (ton) on the visual representation
ggplot() +geom_line(Yamdata4, mapping=aes(x=Year, y=NonFoodUsage), color="navy",size=2)+
  geom_line(Yamdata5, mapping=aes(x=Year, y=NonFoodUsage), color="red",size=2)+
  theme_bw()+scale_y_continuous(labels=comma) +ylab("Non-food usage loss (ton)")

#Putting the Postharvest Loss (ton) on the visual representation
ggplot() +geom_line(Yamdata4, mapping=aes(x=Year, y=PostHarvestLoss), color="coral",size=2)+
  geom_line(Yamdata5, mapping=aes(x=Year, y=PostHarvestLoss), color="red",size=2)+
  theme_bw()+scale_y_continuous(labels=comma) +ylab("Post-harvest Loss (ton)")

#Putting the % Food  Loss on the visual representation
ggplot() +geom_line(Yamdata4, mapping=aes(x=Year, y=FoodLoss), color="purple",size=2)+
  geom_line(Yamdata5, mapping=aes(x=Year, y=FoodLoss), color="red",size=2)+
  theme_bw()+scale_y_continuous(labels=comma) + ylab("Food  Loss (%)")


#Putting the Supply chain loss (ton) on the visual representation
ggplot() +geom_line(Yamdata4, mapping=aes(x=Year, y=SupplyChainLoss), color="pink",size=2)+
  geom_line(Yamdata5, mapping=aes(x=Year, y=SupplyChainLoss), color="red",size=2)+
  theme_bw()+scale_y_continuous(labels=comma) +ylab("Supply Chain Loss (ton)")


#The Aggregate Parameters

#The Total Parameters
names(Yamdata4)

ggplot() +geom_line(Yamdata4, mapping=aes(x=Year, y=YamOutput), color="pink",size=2)+
  geom_line(Yamdata4, mapping=aes(x=Year, y=YamSupply), color="red",size=2)+
  geom_line(Yamdata4, mapping=aes(x=Year, y=NonFoodUsage), color="pink",size=2)+
  geom_line(Yamdata4, mapping=aes(x=Year, y=PostHarvestLoss), color="blue",size=2)+
  geom_line(Yamdata4, mapping=aes(x=Year, y=SupplyChainLoss), color="yellow",size=2)+
  theme_bw()+scale_y_continuous(labels=comma) +ylab("Measured in Tonnes")

#Plotting all the actual parameters together in one's plot
Yamdata6 <- Yamdata4 %>%
  select(Year, YamOutput, YamSupply, NonFoodUsage, PostHarvestLoss, SupplyChainLoss) %>%
  gather(key = "Legend", value = "YamValue", -Year) 

names(Yamdata4)
View(Yamdata6) 


ggplot(Yamdata6, aes(x = Year, y = YamValue)) + 
  geom_line(aes(color = Legend), size = 2) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#008000","#FF0000", "#800000", "#808000", "#00FF00")) +
  theme_bw()+scale_y_continuous(labels=comma) +ylab("Measured in Tonnes")


#Plotting all the forecast parameters together in one's plot
Yamdata7 <- Yamdata4 %>%
  select(Year, YamOutput, YamSupply, NonFoodUsage, PostHarvestLoss, SupplyChainLoss) %>%
  gather(key = "Legend", value = "YamValue", -Year) 

names(Yamdata4)
View(Yamdata5) 


ggplot(Yamdata6, aes(x = Year, y = YamValue)) + 
  geom_line(aes(color = Legend), size = 2) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#008000","#FF0000", "#800000", "#808000", "#00FF00")) +
  theme_bw()+scale_y_continuous(labels=comma) +ylab("Measured in Tonnes")






data<-read.csv(file.choose())
names(data)

#regression model

model<-lm(log(SCL)~log(YO)+log(NFU)+log(PHL)+log(Ri)+log(Ex)+log(Yp)+log(FPI)+log(CPI)+log(EiA), data)
summary(model)

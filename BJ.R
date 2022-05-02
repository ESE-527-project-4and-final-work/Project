#' ---
#' title: "PM2.5_BJ"
#' date: "`r Sys.Date()`"
#' author: "Hang Ge"
#' output: pdf_document
#' ---
#' 

# loading data 
BJ = read.csv("/Users/katsurawataru/Desktop/ESE\ 527\ Practicum/FiveCitiePMData/Data/BeijingPM20100101_20151231.csv")

# length(BJ$PM_US.Post[!is.na(BJ$PM_US.Post)]) # 50387
library(readxl)
data = BJ[, c('No', 'PM_US.Post')]

null.index = data$No[c(which(is.na(data$PM_US.Post)))]
not.null.index = data$No[-c(which(is.na(data$PM_US.Post)))] # not null index 

head(data)
# Cleaning Missing data  -- BJ 
library(OutliersO3)
library(outliers)
# head(data$PM_US.Post[not.null.index])
# hist(which(scores(data$PM_US.Post[not.null.index], type = "z", prob = 0.95)), breaks = 100)
#length(data) # 50387 Not null data 
#length((which(scores(data$PM_US.Post[not.null.index], type = "z", prob = 0.95)))) # 3714 extreme data 
#head((which(scores(data$PM_US.Post[not.null.index], type = "z", prob = 0.95))))
extreme.index = data$No[(which(scores(data$PM_US.Post[not.null.index], type = "z", prob = 0.95)))] 
cleaned.data.index = data$No[-c(c(extreme.index, null.index))]
# hist(cleaned.data) distribution(density)
# plot(c(1:length(cleaned.data[5000:6000])), cleaned.data[5000:6000]) part of parttern 
# summary(BJ)
cleaned.BJ = BJ[BJ$No[cleaned.data.index],]



length(BJ$No)
length(cleaned.BJ$No)

# View data -- BJ 
plot(c(1:length(cleaned.data.index)),BJ$PM_US.Post[cleaned.data.index], type = 'l', 
     main = 'Line Graph for PM2.5 though Time in Beijing', xlab = 'Time(day)', ylab ='PM2.5 Concentration(ug/m^3)')
hist(cleaned.BJ$PM_US.Post, main = 'The Distribution of PM2.5 Data', 
     xlab = 'PM2.5 Concentration(ug/m^3)')

# correlation -- BJ 
length(which(is.na(cleaned.BJ$HUMI)))
length(which(is.na(cleaned.BJ$Iprec)))
# cleaned.BJ$No[is.na(cleaned.BJ$HUMI)]

## expel all null data for all attributes 
cor.index = cleaned.BJ$No[-c(unique(which(is.na(cleaned.BJ$HUMI)), which(is.na(cleaned.BJ$PRES)),
                                    which(is.na(cleaned.BJ$TEMP)), which(is.na(cleaned.BJ$Iws))
))]
cor.BJ.pre = cleaned.BJ[cleaned.BJ$No %in% cor.index, ]
cor.index = cor.BJ.pre$No[-c(unique(which(is.na(cor.BJ.pre$precipitation),which(is.na(cor.BJ.pre$Iprec))))
)]
cor.BJ = cor.BJ.pre[cor.BJ.pre$No %in% cor.index, ]
# head(table(cor.index))
cor.BJ = cleaned.BJ[cleaned.BJ$No %in% cor.index, ]
# correlation
cor(cor.BJ[c('PM_US.Post','DEWP', 'HUMI', 'PRES', 'TEMP', 'Iws', 'precipitation', 'Iprec')])
# correlation with log transformation 
cor.BJ$log_PM_US.Post = log(cor.BJ$PM_US.Post)
cor(cor.BJ[c('log_PM_US.Post','PM_US.Post','DEWP', 'HUMI', 'PRES', 'TEMP', 'Iws', 'precipitation', 'Iprec')])


length(BJ$No)
length(cor.BJ$No)
####### Data Visulazation:  
# relations with DEWP, HUMI, PRES, TEMP, Iws, precipitation, Iprec
BJ.ture = cor.BJ
library(ggplot2)

# DEWP (Dew Point -- the tempature to reach 100% humility for air)
BJ.DEWP = ggplot(BJ.ture, aes(x=DEWP,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# HUMI 
BJ.HUMI = ggplot(BJ.ture, aes(x=HUMI,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# PRES
BJ.PRES = ggplot(BJ.ture, aes(x=PRES,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# TEMP
BJ.TEMP = ggplot(BJ.ture, aes(x=TEMP,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()


library(ggpubr)
ggarrange(BJ.DEWP, BJ.HUMI, BJ.PRES, BJ.TEMP,
          ncol = 2, nrow = 2)


# Iws (wind speed)
BJ.Iws = ggplot(BJ.ture, aes(x=Iws,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
BJ.Iws = BJ.Iws + geom_vline(xintercept = quantile(BJ.ture$Iws,probs=c(0,0.95))[2], 
                             linetype = 'dotted', color = 'orange', size = 1)

# precipitation 
BJ.precipitation = ggplot(BJ.ture, aes(x=precipitation,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
BJ.precipitation = BJ.precipitation + geom_vline(xintercept = quantile(BJ.ture$precipitation,probs=c(0,0.95))[2], 
                                                 linetype = 'dotted', color = 'orange', size = 1)

# Iprec
BJ.Iprec = ggplot(BJ.ture, aes(x=Iprec,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
BJ.Iprec = BJ.Iprec + geom_vline(xintercept = quantile(BJ.ture$Iprec, probs=c(0,0.95))[2], 
                                 linetype = 'dotted', color = 'orange', size = 1)

ggarrange(BJ.Iws, BJ.precipitation, BJ.Iprec,
          ncol = 1, nrow = 3)


###### 
# ARIMA model
#library(DMwR)
library(DMwR2) # knnImputation
#library(performanceEstimation) # knnImp
#library(bnstruct) # knn.impute

BJ.PM2.5.df = data.frame(BJ$PM_US.Post,BJ$DEWP, BJ$HUMI, BJ$PRES, BJ$TEMP, BJ$Iws, BJ$precipitation, BJ$Iprec)
# [BJ$No[!is.na(BJ$PM_US.Post)][1]:length(BJ$PM_US.Post)]
length(BJ$No[!is.na(BJ$PM_US.Post)])
BJ.PM2.5 = knnImputation(BJ.PM2.5.df)

# length(BJ$PM_US.Post[is.na(BJ$PM_US.Post)]) # 2197 
# nrow(BJ.PM2.5.df) # 52584    null rate = 0.04178077 


# 
BJ.ture = cor.BJ
library(ggplot2)

# general graph 
# stationarity check 
time <- seq.POSIXt(from = as.POSIXct("2010-01-01 00:00:00"), to = as.POSIXct("2015-12-31 23:00:00 "), by = 'hour')
ggplot(BJ.PM2.5)+geom_line(aes(y=BJ.PM_US.Post, x=time[1:52584])) + xlab("Time(hour)")+labs(title = "The PM2.5 from 2010 to 2015")+
  theme(plot.title = element_text(hjust = 0.5))


# generating time series data 
library(forecast)
PM2.51 <- ts(BJ.PM2.5$BJ.PM_US.Post) # all
PM2.52 <- ts(BJ.PM2.5$BJ.PM_US.Post, frequency = 24) # day

PM2.53 <- ts(BJ.PM2.5$BJ.PM_US.Post, frequency = 8766) # year



# Box Plot
BJ.PM2.5.box = data.frame(BJ.PM2.5, BJ$year, BJ$month, BJ$day, BJ$hour, BJ$season)
# box plot by hour 
BJ.Box.hour = ggplot(BJ.PM2.5.box,aes(x = reorder(as.factor(BJ.hour), BJ$hour), y = BJ.PM_US.Post))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("hour")
# +geom_violin(fill="pink",alpha=0.3)

# box plot by day 
BJ.Box.day = ggplot(BJ.PM2.5.box,aes(x = reorder(as.factor(BJ.day), BJ$day), y = BJ.PM_US.Post))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("day")

# box plot by month 
BJ.Box.month = ggplot(BJ.PM2.5.box, aes(x = reorder(as.factor(BJ.month), BJ$month), y = BJ.PM_US.Post))+
  geom_boxplot(fill="lightblue", alpha=0.8) +  xlab("month")

# box plot by season 
BJ.Box.season = ggplot(BJ.PM2.5.box,aes(x = reorder(as.factor(BJ.season), BJ$season), y = BJ.PM_US.Post))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("season")
BJ.Box.season

library(ggpubr)
ggarrange(BJ.Box.hour, BJ.Box.day, BJ.Box.month, BJ.Box.season,
          ncol = 2, nrow = 2)



# ACF and PACF plot 
par(mfrow=c(2,1), mar = c(2,3,5,0), mgp=c(1,0.3,0))
acf(PM2.51, main = "ACF plot of PM2.5") 
pacf(PM2.51, main = "PACF plot of PM2.5")
# to show is time series is not AR or MA model. 
tail(acf(PM2.51, main = "ACF plot of PM2.5"))
head(pacf(PM2.51, main = "PACF plot of PM2.5"))

acf = acf(PM2.51, main = "ACF plot of PM2.5") 
pacf = pacf(PM2.51, main = "PACF plot of PM2.5")

tail(acf$acf[,1,1])
head(pacf$acf[,1,1])

# auto.arima 
library(forecast)
par(mfrow=c(1,1))
fitarima1 <- auto.arima(PM2.51,seasonal = F) 
fitarima2 <- auto.arima(PM2.52, seasonal = T)

fitarima1
fitarima2


fitarima1$coef
fitarima2$coef


# White noise check 
Box.test(fitarima1$residuals,lag=2,type='Ljung')
## X-squared = 0.14329, df = 2, p-value = 0.9309

Box.test(fitarima2$residuals,lag=48,type='Ljung')
## X-squared = 804.94, df = 48, p-value < 2.2e-16

# predict plot
time <- seq.POSIXt(from = as.POSIXct("2010-01-01 00:00:00"), 
                   to = as.POSIXct("2016-05-31 23:00:00 "), by = 'hour')

forecast_arima1 <- forecast(fitarima1, h = 300)
ggplot() + geom_line(aes(y=BJ.PM2.5$BJ.PM_US.Post[52297:52584],x=time[52297:52584]))+ 
  geom_point(aes(y=forecast_arima1$mean,x=time[52585:52884]),col="red")+
  geom_point(aes(y=forecast_arima1$lower[,1],x=time[52585:52884]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima1$upper[,1],x=time[52585:52884]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima1$lower[,2],x=time[52585:52884]),col="blue")+
  geom_point(aes(y=forecast_arima1$upper[,2],x=time[52585:52884]),col="blue")+
  labs(title  = "The Prediction of PM2.5 in BeiJing",subtitle="ARIMA(5,1,0)") +xlab(label = "Time") + 
  ylab(label = "PM2.5")+theme(plot.title  = element_text(hjust = 0.5))

ggplot() + geom_line(aes(y=BJ.PM2.5$BJ.PM_US.Post[52297:52584],x=time[52297:52584]))+ 
  geom_point(aes(y=forecast_arima2$mean,x=time[52585:52884]),col="red")+
  geom_point(aes(y=forecast_arima2$lower[,1],x=time[52585:52884]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima2$upper[,1],x=time[52585:52884]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima2$lower[,2],x=time[52585:52884]),col="blue")+
  geom_point(aes(y=forecast_arima2$upper[,2],x=time[52585:52884]),col="blue")+
  labs(title  = "The Prediction of PM2.5 in BeiJing",subtitle="ARIMA(0,1,5)(0,0,2)[24]") +xlab(label = "Time") + 
  ylab(label = "PM2.5")+theme(plot.title  = element_text(hjust = 0.5))


#head(forecast_arima2$lower[,2])

#!autoplot(forecast(fitarima1))
#autoplot(forecast(fitarima2))
#length(forecast_arima1$mean)
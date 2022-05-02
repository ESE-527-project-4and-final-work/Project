
#' ---
#' title: "PM2.5_SH"
#' date: "`r Sys.Date()`"
#' author: "Hang Ge"
#' output: pdf_document
#' ---
#' 
SH = read.csv("/Users/katsurawataru/Desktop/ESE\ 527\ Practicum/FiveCitiePMData/Data/ShanghaiPM20100101_20151231.csv")


library(readxl)
data = SH[, c('No', 'PM_US.Post')]

null.index = data$No[c(which(is.na(data$PM_US.Post)))]
not.null.index = data$No[-c(which(is.na(data$PM_US.Post)))] # not null index 

head(data)
# Cleaning Missing data  -- SH
library(OutliersO3)
library(outliers)
extreme.index = data$No[(which(scores(data$PM_US.Post[not.null.index], type = "z", prob = 0.95)))] 
cleaned.data.index = data$No[-c(c(extreme.index, null.index))]

cleaned.SH = SH[SH$No[cleaned.data.index],]

length(SH$No)
length(cleaned.SH$No)

# View data -- SH 
plot(c(1:length(cleaned.data.index)),SH$PM_US.Post[cleaned.data.index], type = 'l', 
     main = 'Line Graph for PM2.5 though Time in Chengdu', xlab = 'Time(day)', ylab ='PM2.5 Concentration(ug/m^3)')
hist(cleaned.SH$PM_US.Post, main = 'The Distribution of PM2.5 Data in Shanghai', 
     xlab = 'PM2.5 Concentration(ug/m^3)')

# correlation -- SH 
## expel all null data for all attributes 
list = unique(c(c(which(is.na(cleaned.SH$HUMI))),c(which(is.na(cleaned.SH$PRES))),
                c(which(is.na(cleaned.SH$TEMP))),c(which(is.na(cleaned.SH$Iws))),
                c(which(is.na(cleaned.SH$precipitation))),c(which(is.na(cleaned.SH$Iprec)))))
cor.index = cleaned.SH$No[-list]

cor.SH = cleaned.SH[cleaned.SH$No %in% cor.index, ]

# correlation
cor(cor.SH[c('PM_US.Post','DEWP', 'HUMI', 'PRES', 'TEMP', 'Iws', 'precipitation', 'Iprec')])
# correlation with log transformation 
cor.SH$log_PM_US.Post = log(cor.SH$PM_US.Post)
cor(cor.SH[c('log_PM_US.Post','PM_US.Post','DEWP', 'HUMI', 'PRES', 'TEMP', 'Iws', 'precipitation', 'Iprec')])


####### Data Visulazation:  
# relations with DEWP, HUMI, PRES, TEMP, Iws, precipitation, Iprec
SH.ture = cor.SH
library(ggplot2)

# DEWP (Dew Point -- the tempature to reach 100% humility for air)
SH.DEWP = ggplot(SH.ture, aes(x=DEWP,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# HUMI 
SH.HUMI = ggplot(SH.ture, aes(x=HUMI,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# PRES
SH.PRES = ggplot(SH.ture, aes(x=PRES,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# TEMP
SH.TEMP = ggplot(SH.ture, aes(x=TEMP,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()


library(ggpubr)
ggarrange(SH.DEWP, SH.HUMI, SH.PRES, SH.TEMP,
          ncol = 2, nrow = 2)


# Iws (wind speed)
SH.Iws = ggplot(SH.ture, aes(x=Iws,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
SH.Iws = SH.Iws + geom_vline(xintercept = quantile(SH.ture$Iws,probs=c(0,0.95))[2], 
                             linetype = 'dotted', color = 'orange', size = 1)

# precipitation 
SH.precipitation = ggplot(SH.ture, aes(x=precipitation,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
SH.precipitation = SH.precipitation + geom_vline(xintercept = quantile(SH.ture$precipitation,probs=c(0,0.95))[2], 
                                                 linetype = 'dotted', color = 'orange', size = 1)

# Iprec
SH.Iprec = ggplot(SH.ture, aes(x=Iprec,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
SH.Iprec = SH.Iprec + geom_vline(xintercept = quantile(SH.ture$Iprec, probs=c(0,0.95))[2], 
                                 linetype = 'dotted', color = 'orange', size = 1)

ggarrange(SH.Iws, SH.precipitation, SH.Iprec,
          ncol = 1, nrow = 3)


###### 
# ARIMA model
#library(DMwR)
library(DMwR2) # knnImputation
#library(performanceEstimation) # knnImp
#library(bnstruct) # knn.impute

hist(SH$No[is.na(SH$PM_US.Post)])
#length(SH$No[SH$year == 2010])
#length(SH$No[SH$year == 2011])
#length(SH$No[SH$year == 2012]) * 3
# SH$year[26304:26307] # 26305 is the fisrt day in 2013 
#length(SH$PM_US.Post[26305:52584])
SH.PM2.5.df = data.frame(SH$PM_US.Post[26305:52584] ,SH$DEWP[26305:52584], SH$HUMI[26305:52584],
                         SH$PRES[26305:52584], SH$TEMP[26305:52584], SH$Iws[26305:52584], 
                         SH$precipitation[26305:52584], SH$Iprec[26305:52584])
SH.PM2.5 = knnImputation(SH.PM2.5.df)

# length(SH$PM_US.Post[is.na(SH$PM_US.Post)]) # 2197 
# nrow(SH.PM2.5.df) # 52584    
# SH$month[SH$No == 26305]

# 
SH.ture = cor.SH
library(ggplot2)

# general graph 
# stationarity check 
time <- seq.POSIXt(from = as.POSIXct("2013-01-01 00:00:00"), to = as.POSIXct("2015-12-31 23:00:00 "), by = 'hour')
ggplot(SH.PM2.5)+geom_line(aes(y=SH$PM_US.Post[26305:52584], x=time[1:26280])) + 
  xlab("Time(hour)") + ylab('PM2.5')+labs(title = "The PM2.5 from 2013 to 2015")+
  theme(plot.title = element_text(hjust = 0.5))



# Box Plot
SH.PM2.5.box = data.frame(SH.PM2.5, SH$year[26305:52584], SH$month[26305:52584], 
                          SH$day[26305:52584], SH$hour[26305:52584], SH$season[26305:52584])

# box plot by hour 
SH.Box.hour = ggplot(SH.PM2.5.box,aes(x = reorder(as.factor(SH.hour.26305.52584.), SH.hour.26305.52584.), 
                                      y = SH.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("hour")+ ylab('PM2.5')
# +geom_violin(fill="pink",alpha=0.3)

# box plot by day 
SH.Box.day = ggplot(SH.PM2.5.box,aes(x = reorder(as.factor(SH.day.26305.52584.), SH.day.26305.52584.), 
                                     y = SH.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("day")+ ylab('PM2.5')

# box plot by month 
SH.Box.month = ggplot(SH.PM2.5.box, aes(x = reorder(as.factor(SH.month.26305.52584.), SH.month.26305.52584.), 
                                        y = SH.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue", alpha=0.8) +  xlab("month")+ ylab('PM2.5')

# box plot by season 
SH.Box.season = ggplot(SH.PM2.5.box,aes(x = reorder(as.factor(SH.season.26305.52584.), SH.season.26305.52584.), 
                                        y = SH.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("season")+ ylab('PM2.5')


library(ggpubr)
ggarrange(SH.Box.hour, SH.Box.day, SH.Box.month, SH.Box.season,
          ncol = 2, nrow = 2)




# generating time series data 
library(forecast)
PM2.51 <- ts(SH.PM2.5$SH.PM_US.Post.26305.52584.) # all
PM2.52 <- ts(SH.PM2.5$SH.PM_US.Post.26305.52584., frequency = 24) # day0


# ACF and PACF plot 
par(mfrow=c(2,1), mar = c(2,3,5,0), mgp=c(1,0.3,0))
acf(PM2.51, main = "ACF plot of PM2.5") 
pacf(PM2.51, main = "PACF plot of PM2.5")
# to show is time series is not AR or MA model. 
tail(acf(PM2.51, main = "ACF plot of PM2.5"))
head(pacf(PM2.51, main = "PACF plot of PM2.5"))

acf = acf(PM2.51, main = "ACF plot of PM2.5") 
pacf = pacf(PM2.51, main = "PACF plot of PM2.5")

# auto.arima 
library(forecast)
par(mfrow=c(1,1))
fitarima1 <- auto.arima(PM2.51,seasonal = F) 
fitarima2 <- auto.arima(PM2.52, seasonal = T)

fitarima1
fitarima2


# White noise check 
Box.test(fitarima1$residuals,lag=2,type='Ljung')
## X-squared = 0.2926, df = 2, p-value = 0.8639

Box.test(fitarima2$residuals,lag=48,type='Ljung')
## X-squared = 311.27, df = 48, p-value < 2.2e-16


# predict plot
time <- seq.POSIXt(from = as.POSIXct("2013-01-01 00:00:00"), 
                   to = as.POSIXct("2016-05-31 23:00:00 "), by = 'hour')



forecast_arima1 <- forecast(fitarima1, h = 300)
ggplot() + geom_line(aes(y=SH.PM2.5$SH.PM_US.Post.26305.52584.[26000:26280],x=time[26000:26280]))+ 
  geom_point(aes(y=forecast_arima1$mean,x=time[26281:26580]),col="red")+
  geom_point(aes(y=forecast_arima1$lower[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima1$upper[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima1$lower[,2],x=time[26281:26580]),col="blue")+
  geom_point(aes(y=forecast_arima1$upper[,2],x=time[26281:26580]),col="blue")+
  labs(title  = "The Prediction of PM2.5 in ShangHai",subtitle="ARIMA(5,1,0)") +xlab(label = "Time") + 
  ylab(label = "PM2.5")+theme(plot.title  = element_text(hjust = 0.5))

forecast_arima2 <- forecast(fitarima2, h = 300)
ggplot() + geom_line(aes(y=SH.PM2.5$SH.PM_US.Post.26305.52584.[26000:26280],x=time[26000:26280]))+ 
  geom_point(aes(y=forecast_arima2$mean,x=time[26281:26580]),col="red")+
  geom_point(aes(y=forecast_arima2$lower[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima2$upper[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima2$lower[,2],x=time[26281:26580]),col="blue")+
  geom_point(aes(y=forecast_arima2$upper[,2],x=time[26281:26580]),col="blue")+
  labs(title  = "The Prediction of PM2.5 in ShangHai",subtitle="ARIMA(0,1,5)") +xlab(label = "Time") + 
  ylab(label = "PM2.5")+theme(plot.title  = element_text(hjust = 0.5))


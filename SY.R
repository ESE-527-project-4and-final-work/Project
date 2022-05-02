
#' ---
#' title: "PM2.5_SY"
#' date: "`r Sys.Date()`"
#' author: "Hang Ge"
#' output: pdf_document
#' ---
#' 
SY = read.csv("/Users/katsurawataru/Desktop/ESE\ 527\ Practicum/FiveCitiePMData/Data/ShenyangPM20100101_20151231.csv")


library(readxl)
data = SY[, c('No', 'PM_US.Post')]

null.index = data$No[c(which(is.na(data$PM_US.Post)))]
not.null.index = data$No[-c(which(is.na(data$PM_US.Post)))] # not null index 

head(data)
# Cleaning Missing data  -- SY
library(OutliersO3)
library(outliers)
extreme.index = data$No[(which(scores(data$PM_US.Post[not.null.index], type = "z", prob = 0.95)))] 
cleaned.data.index = data$No[-c(c(extreme.index, null.index))]

cleaned.SY = SY[SY$No[cleaned.data.index],]

length(SY$No)
length(cleaned.SY$No)

# View data -- SY 
plot(c(1:length(cleaned.data.index)),SY$PM_US.Post[cleaned.data.index], type = 'l', 
     main = 'Line Graph for PM2.5 though Time in Shenyang', xlab = 'Time(day)', ylab ='PM2.5 Concentration(ug/m^3)')
hist(cleaned.SY$PM_US.Post, main = 'The Distribution of PM2.5 Data in Shenyang', 
     xlab = 'PM2.5 Concentration(ug/m^3)')

# correlation -- SY 
## expel all null data for all attributes 
list = unique(c(c(which(is.na(cleaned.SY$HUMI))),c(which(is.na(cleaned.SY$PRES))),
                c(which(is.na(cleaned.SY$TEMP))),c(which(is.na(cleaned.SY$Iws))),
                c(which(is.na(cleaned.SY$precipitation))),c(which(is.na(cleaned.SY$Iprec)))))
cor.index = cleaned.SY$No[-list]

cor.SY = cleaned.SY[cleaned.SY$No %in% cor.index, ]

# correlation
cor(cor.SY[c('PM_US.Post','DEWP', 'HUMI', 'PRES', 'TEMP', 'Iws', 'precipitation', 'Iprec')])
# correlation with log transformation 
cor.SY$log_PM_US.Post = log(cor.SY$PM_US.Post)
cor(cor.SY[c('log_PM_US.Post','PM_US.Post','DEWP', 'HUMI', 'PRES', 'TEMP', 'Iws', 'precipitation', 'Iprec')])


####### Data Visulazation:  
# relations with DEWP, HUMI, PRES, TEMP, Iws, precipitation, Iprec
SY.ture = cor.SY
library(ggplot2)

# DEWP (Dew Point -- the tempature to reach 100% humility for air)
SY.DEWP = ggplot(SY.ture, aes(x=DEWP,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# HUMI 
SY.HUMI = ggplot(SY.ture, aes(x=HUMI,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# PRES
SY.PRES = ggplot(SY.ture, aes(x=PRES,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# TEMP
SY.TEMP = ggplot(SY.ture, aes(x=TEMP,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()


library(ggpubr)
ggarrange(SY.DEWP, SY.HUMI, SY.PRES, SY.TEMP,
          ncol = 2, nrow = 2)


# Iws (wind speed)
SY.Iws = ggplot(SY.ture, aes(x=Iws,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
SY.Iws = SY.Iws + geom_vline(xintercept = quantile(SY.ture$Iws,probs=c(0,0.95))[2], 
                             linetype = 'dotted', color = 'orange', size = 1)

# precipitation 
SY.precipitation = ggplot(SY.ture, aes(x=precipitation,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
SY.precipitation = SY.precipitation + geom_vline(xintercept = quantile(SY.ture$precipitation,probs=c(0,0.95))[2], 
                                                 linetype = 'dotted', color = 'orange', size = 1)

# Iprec
SY.Iprec = ggplot(SY.ture, aes(x=Iprec,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
SY.Iprec = SY.Iprec + geom_vline(xintercept = quantile(SY.ture$Iprec, probs=c(0,0.95))[2], 
                                 linetype = 'dotted', color = 'orange', size = 1)

ggarrange(SY.Iws, SY.precipitation, SY.Iprec,
          ncol = 1, nrow = 3)


###### 
# ARIMA model
#library(DMwR)
library(DMwR2) # knnImputation
#library(performanceEstimation) # knnImp
#library(bnstruct) # knn.impute

#hist(SY$No[is.na(SY$PM_US.Post)])
#length(SY$No[SY$year == 2010])
#length(SY$No[SY$year == 2011])
#length(SY$No[SY$year == 2012]) * 3
# SY$year[26304:26307] # 26305 is the fisrt day in 2013 

SY.PM2.5.df = data.frame(SY$PM_US.Post[26305:52584] ,SY$DEWP[26305:52584], SY$HUMI[26305:52584],
                         SY$PRES[26305:52584], SY$TEMP[26305:52584], SY$Iws[26305:52584], 
                         SY$precipitation[26305:52584], SY$Iprec[26305:52584])
SY.PM2.5 = knnImputation(SY.PM2.5.df)
# table(is.na(SY$PM_US.Post[26305:52584]))
# length(SY$PM_US.Post[is.na(SY$PM_US.Post)]) # 2197 
# nrow(SY.PM2.5.df) # 52584    
# SY$month[SY$No == 26305]

# 
SY.ture = cor.SY
library(ggplot2)

# general graph 
# stationarity check 
time <- seq.POSIXt(from = as.POSIXct("2013-01-01 00:00:00"), to = as.POSIXct("2015-12-31 23:00:00 "), by = 'hour')
ggplot(SY.PM2.5)+geom_line(aes(y=SY$PM_US.Post[26305:52584], x=time[1:26280])) + 
  xlab("Time(hour)") + ylab('PM2.5')+labs(title = "The PM2.5 from 2013 to 2015")+
  theme(plot.title = element_text(hjust = 0.5))



# Box Plot
SY.PM2.5.box = data.frame(SY.PM2.5, SY$year[26305:52584], SY$month[26305:52584], 
                          SY$day[26305:52584], SY$hour[26305:52584], SY$season[26305:52584])

# box plot by hour 
SY.Box.hour = ggplot(SY.PM2.5.box,aes(x = reorder(as.factor(SY.hour.26305.52584.), SY.hour.26305.52584.), 
                                      y = SY.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("hour")+ ylab('PM2.5')
# +geom_violin(fill="pink",alpha=0.3)

# box plot by day 
SY.Box.day = ggplot(SY.PM2.5.box,aes(x = reorder(as.factor(SY.day.26305.52584.), SY.day.26305.52584.), 
                                     y = SY.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("day")+ ylab('PM2.5')

# box plot by month 
SY.Box.month = ggplot(SY.PM2.5.box, aes(x = reorder(as.factor(SY.month.26305.52584.), SY.month.26305.52584.), 
                                        y = SY.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue", alpha=0.8) +  xlab("month")+ ylab('PM2.5')

# box plot by season 
SY.Box.season = ggplot(SY.PM2.5.box,aes(x = reorder(as.factor(SY.season.26305.52584.), SY.season.26305.52584.), 
                                        y = SY.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("season")+ ylab('PM2.5')


library(ggpubr)
ggarrange(SY.Box.hour, SY.Box.day, SY.Box.month, SY.Box.season,
          ncol = 2, nrow = 2)




# generating time series data 
library(forecast)
PM2.51 <- ts(SY.PM2.5$SY.PM_US.Post.26305.52584.) # all
PM2.52 <- ts(SY.PM2.5$SY.PM_US.Post.26305.52584., frequency = 24) # day


# ACF and PACF plot 
par(mfrow=c(2,1), mar = c(2,3,5,0), mgp=c(1,0.3,0))
acf(PM2.51, main = "ACF plot of PM2.5") 
pacf(PM2.51, main = "PACF plot of PM2.5")
# to show is time series is not AR or MA model. 
tail(acf(PM2.51, main = "ACF plot of PM2.5"))
head(pacf(PM2.51, main = "PACF plot of PM2.5"))


# auto.arima 
library(forecast)
par(mfrow=c(1,1))
fitarima1 <- auto.arima(PM2.51,seasonal = F) 
fitarima2 <- auto.arima(PM2.52, seasonal = T)

fitarima1
fitarima2


# White noise check 
Box.test(fitarima1$residuals,lag=2,type='Ljung')
## X-squared = 1.932, df = 2, p-value = 0.3806

Box.test(fitarima2$residuals,lag=48,type='Ljung')
## X-squared = 716.28, df = 48, p-value < 2.2e-16


# predict plot
time <- seq.POSIXt(from = as.POSIXct("2013-01-01 00:00:00"), 
                   to = as.POSIXct("2016-05-31 23:00:00 "), by = 'hour')



forecast_arima1 <- forecast(fitarima1, h = 300)
ggplot() + geom_line(aes(y=SY.PM2.5$SY.PM_US.Post.26305.52584.[26000:26280],x=time[26000:26280]))+ 
  geom_point(aes(y=forecast_arima1$mean,x=time[26281:26580]),col="red")+
  geom_point(aes(y=forecast_arima1$lower[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima1$upper[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima1$lower[,2],x=time[26281:26580]),col="blue")+
  geom_point(aes(y=forecast_arima1$upper[,2],x=time[26281:26580]),col="blue")+
  labs(title  = "The Prediction of PM2.5 in Shenyang",subtitle="ARIMA(0,1,5)") +xlab(label = "Time") + 
  ylab(label = "PM2.5")+theme(plot.title  = element_text(hjust = 0.5))

forecast_arima2 <- forecast(fitarima2, h = 300)
ggplot() + geom_line(aes(y=SY.PM2.5$SY.PM_US.Post.26305.52584.[26000:26280],x=time[26000:26280]))+ 
  geom_point(aes(y=forecast_arima2$mean,x=time[26281:26580]),col="red")+
  geom_point(aes(y=forecast_arima2$lower[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima2$upper[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima2$lower[,2],x=time[26281:26580]),col="blue")+
  geom_point(aes(y=forecast_arima2$upper[,2],x=time[26281:26580]),col="blue")+
  labs(title  = "The Prediction of PM2.5 in Shenyang",subtitle="ARIMA(0,1,5)(0,0,2)[24]") +xlab(label = "Time") + 
  ylab(label = "PM2.5")+theme(plot.title  = element_text(hjust = 0.5))


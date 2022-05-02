
#' ---
#' title: "PM2.5_CD"
#' date: "`r Sys.Date()`"
#' author: "Hang Ge"
#' output: pdf_document
#' ---
#' 
CD = read.csv("/Users/katsurawataru/Desktop/ESE\ 527\ Practicum/FiveCitiePMData/Data/ChengduPM20100101_20151231.csv")


library(readxl)
data = CD[, c('No', 'PM_US.Post')]

null.index = data$No[c(which(is.na(data$PM_US.Post)))]
not.null.index = data$No[-c(which(is.na(data$PM_US.Post)))] # not null index 

head(data)
# Cleaning Missing data  -- CD
library(OutliersO3)
library(outliers)
extreme.index = data$No[(which(scores(data$PM_US.Post[not.null.index], type = "z", prob = 0.95)))] 
cleaned.data.index = data$No[-c(c(extreme.index, null.index))]

cleaned.CD = CD[CD$No[cleaned.data.index],]

length(CD$No)
length(cleaned.CD$No)

# View data -- CD 
plot(c(1:length(cleaned.data.index)),CD$PM_US.Post[cleaned.data.index], type = 'l', 
     main = 'Line Graph for PM2.5 though Time in Chengdu', xlab = 'Time(day)', ylab ='PM2.5 Concentration(ug/m^3)')
hist(cleaned.CD$PM_US.Post, main = 'The Distribution of PM2.5 Data in Chengdu', 
     xlab = 'PM2.5 Concentration(ug/m^3)')

# correlation -- CD 
## expel all null data for all attributes 
list = unique(c(c(which(is.na(cleaned.CD$HUMI))),c(which(is.na(cleaned.CD$PRES))),
                c(which(is.na(cleaned.CD$TEMP))),c(which(is.na(cleaned.CD$Iws))),
                c(which(is.na(cleaned.CD$precipitation))),c(which(is.na(cleaned.CD$Iprec)))))
cor.index = cleaned.CD$No[-list]

cor.CD = cleaned.CD[cleaned.CD$No %in% cor.index, ]

# correlation
cor(cor.CD[c('PM_US.Post','DEWP', 'HUMI', 'PRES', 'TEMP', 'Iws', 'precipitation', 'Iprec')])
# correlation with log transformation 
cor.CD$log_PM_US.Post = log(cor.CD$PM_US.Post)
cor(cor.CD[c('log_PM_US.Post','PM_US.Post','DEWP', 'HUMI', 'PRES', 'TEMP', 'Iws', 'precipitation', 'Iprec')])


####### Data Visulazation:  
# relations with DEWP, HUMI, PRES, TEMP, Iws, precipitation, Iprec
CD.ture = cor.CD
library(ggplot2)

# DEWP (Dew Point -- the tempature to reach 100% humility for air)
CD.DEWP = ggplot(CD.ture, aes(x=DEWP,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# HUMI 
CD.HUMI = ggplot(CD.ture, aes(x=HUMI,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# PRES
CD.PRES = ggplot(CD.ture, aes(x=PRES,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# TEMP
CD.TEMP = ggplot(CD.ture, aes(x=TEMP,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()


library(ggpubr)
ggarrange(CD.DEWP, CD.HUMI, CD.PRES, CD.TEMP,
          ncol = 2, nrow = 2)


# Iws (wind speed)
CD.Iws = ggplot(CD.ture, aes(x=Iws,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
CD.Iws = CD.Iws + geom_vline(xintercept = quantile(CD.ture$Iws,probs=c(0,0.95))[2], 
                             linetype = 'dotted', color = 'orange', size = 1)

# precipitation 
CD.precipitation = ggplot(CD.ture, aes(x=precipitation,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
CD.precipitation = CD.precipitation + geom_vline(xintercept = quantile(CD.ture$precipitation,probs=c(0,0.95))[2], 
                                                 linetype = 'dotted', color = 'orange', size = 1)

# Iprec
CD.Iprec = ggplot(CD.ture, aes(x=Iprec,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
CD.Iprec = CD.Iprec + geom_vline(xintercept = quantile(CD.ture$Iprec, probs=c(0,0.95))[2], 
                                 linetype = 'dotted', color = 'orange', size = 1)

ggarrange(CD.Iws, CD.precipitation, CD.Iprec,
          ncol = 1, nrow = 3)


###### 
# ARIMA model
#library(DMwR)
library(DMwR2) # knnImputation
#library(performanceEstimation) # knnImp
#library(bnstruct) # knn.impute

# hist(CD$No[is.na(CD$PM_US.Post)])
#length(CD$No[CD$year == 2010])
#length(CD$No[CD$year == 2011])
#length(CD$No[CD$year == 2012]) * 3
# CD$year[26304:26307] # 26305 is the fisrt day in 2013 
length(CD$PM_US.Post[26305:52584])
CD.PM2.5.df = data.frame(CD$PM_US.Post[26305:52584] ,CD$DEWP[26305:52584], CD$HUMI[26305:52584],
                         CD$PRES[26305:52584], CD$TEMP[26305:52584], CD$Iws[26305:52584], 
                         CD$precipitation[26305:52584], CD$Iprec[26305:52584])
CD.PM2.5 = knnImputation(CD.PM2.5.df)

# length(CD$PM_US.Post[is.na(CD$PM_US.Post)]) # 2197 
# nrow(CD.PM2.5.df) # 52584    
# CD$month[CD$No == 26305]

# 
CD.ture = cor.CD
library(ggplot2)

# general graph 
# stationarity check 
time <- seq.POSIXt(from = as.POSIXct("2013-01-01 00:00:00"), to = as.POSIXct("2015-12-31 23:00:00 "), by = 'hour')
ggplot(CD.PM2.5)+geom_line(aes(y=CD$PM_US.Post[26305:52584], x=time[1:26280])) + 
  xlab("Time(hour)") + ylab('PM2.5')+labs(title = "The PM2.5 from 2013 to 2015")+
  theme(plot.title = element_text(hjust = 0.5))



# Box Plot
CD.PM2.5.box = data.frame(CD.PM2.5, CD$year[26305:52584], CD$month[26305:52584], 
                          CD$day[26305:52584], CD$hour[26305:52584], CD$season[26305:52584])

# box plot by hour 
CD.Box.hour = ggplot(CD.PM2.5.box,aes(x = reorder(as.factor(CD.hour.26305.52584.), CD.PM2.5.box$CD.hour.26305.52584.), 
                                      y = CD.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("hour")+ ylab('PM2.5')
# +geom_violin(fill="pink",alpha=0.3)

# box plot by day 
CD.Box.day = ggplot(CD.PM2.5.box,aes(x = reorder(as.factor(CD.day.26305.52584.), CD.PM2.5.box$CD.day.26305.52584.), 
                                     y = CD.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("day")+ ylab('PM2.5')

# box plot by month 
CD.Box.month = ggplot(CD.PM2.5.box, aes(x = reorder(as.factor(CD.month.26305.52584.), CD.month.26305.52584.), 
                                        y = CD.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue", alpha=0.8) +  xlab("month")+ ylab('PM2.5')

# box plot by season 
CD.Box.season = ggplot(CD.PM2.5.box,aes(x = reorder(as.factor(CD.season.26305.52584.), CD.season.26305.52584.), 
                                        y = CD.PM_US.Post.26305.52584.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("season")+ ylab('PM2.5')


library(ggpubr)
ggarrange(CD.Box.hour, CD.Box.day, CD.Box.month, CD.Box.season,
          ncol = 2, nrow = 2)




# generating time series data 
library(forecast)
PM2.51 <- ts(CD.PM2.5$CD.PM_US.Post.26305.52584.) # all
PM2.52 <- ts(CD.PM2.5$CD.PM_US.Post.26305.52584., frequency = 24) # day


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
ggplot() + geom_line(aes(y=CD.PM2.5$CD.PM_US.Post.26305.52584.[26000:26280],x=time[26000:26280]))+ 
  geom_point(aes(y=forecast_arima1$mean,x=time[26281:26580]),col="red")+
  geom_point(aes(y=forecast_arima1$lower[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima1$upper[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima1$lower[,2],x=time[26281:26580]),col="blue")+
  geom_point(aes(y=forecast_arima1$upper[,2],x=time[26281:26580]),col="blue")+
  labs(title  = "The Prediction of PM2.5 in ChengDu",subtitle="ARIMA(3,1,2)") +xlab(label = "Time") + 
  ylab(label = "PM2.5")+theme(plot.title  = element_text(hjust = 0.5))

forecast_arima2 <- forecast(fitarima2, h = 300)
ggplot() + geom_line(aes(y=CD.PM2.5$CD.PM_US.Post.26305.52584.[26000:26280],x=time[26000:26280]))+ 
  geom_point(aes(y=forecast_arima2$mean,x=time[26281:26580]),col="red")+
  geom_point(aes(y=forecast_arima2$lower[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima2$upper[,1],x=time[26281:26580]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima2$lower[,2],x=time[26281:26580]),col="blue")+
  geom_point(aes(y=forecast_arima2$upper[,2],x=time[26281:26580]),col="blue")+
  labs(title  = "The Prediction of PM2.5 in ChengDu",subtitle="ARIMA(2,1,2)(2,0,0)[24]") +xlab(label = "Time") + 
  ylab(label = "PM2.5")+theme(plot.title  = element_text(hjust = 0.5))


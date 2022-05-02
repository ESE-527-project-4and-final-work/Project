
#' ---
#' title: "PM2.5_GZ"
#' date: "`r Sys.Date()`"
#' author: "Hang Ge"
#' output: pdf_document
#' ---
#' 
#' 
GZ = read.csv("/Users/katsurawataru/Desktop/ESE\ 527\ Practicum/FiveCitiePMData/Data/GuangzhouPM20100101_20151231.csv")


library(readxl)
data = GZ[, c('No', 'PM_US.Post', 'DEWP', 'HUMI')]

null.index = data$No[c(which(is.na(data$PM_US.Post)))]
not.null.index = data$No[-c(which(is.na(data$PM_US.Post)))] # not null index 

head(data)
# Cleaning Missing data  -- GZ
library(OutliersO3)
library(outliers)
extreme.index = data$No[(which(scores(data$PM_US.Post[not.null.index], type = "z", prob = 0.95)))]
cleaned.data.index = data$No[-c(c(extreme.index, null.index))]

cleaned.GZ = GZ[GZ$No[cleaned.data.index],]

length(GZ$No)
length(cleaned.GZ$No)

# View data -- GZ 
plot(c(1:length(cleaned.data.index)),GZ$PM_US.Post[cleaned.data.index], type = 'l', 
     main = 'Line Graph for PM2.5 though Time in Chengdu', xlab = 'Time(day)', ylab ='PM2.5 Concentration(ug/m^3)')
hist(cleaned.GZ$PM_US.Post, main = 'The Distribution of PM2.5 Data in Chengdu', 
     xlab = 'PM2.5 Concentration(ug/m^3)')

# correlation -- GZ 
## expel all null data for all attributes 
list = unique(c(c(which(is.na(cleaned.GZ$HUMI))),c(which(is.na(cleaned.GZ$PRES))),
                c(which(is.na(cleaned.GZ$TEMP))),c(which(is.na(cleaned.GZ$Iws))),
                c(which(is.na(cleaned.GZ$precipitation))),c(which(is.na(cleaned.GZ$Iprec)))))
cor.index = cleaned.GZ$No[-list]

cor.GZ = cleaned.GZ[cleaned.GZ$No %in% cor.index, ]

# correlation
cor(cor.GZ[c('PM_US.Post','DEWP', 'HUMI', 'PRES', 'TEMP', 'Iws', 'precipitation', 'Iprec')])
# correlation with log transformation 
cor.GZ$log_PM_US.Post = log(cor.GZ$PM_US.Post)
cor(cor.GZ[c('log_PM_US.Post','PM_US.Post','DEWP', 'HUMI', 'PRES', 'TEMP', 'Iws', 'precipitation', 'Iprec')])


####### Data Visulazation:  
# relations with DEWP, HUMI, PRES, TEMP, Iws, precipitation, Iprec
GZ.ture = cor.GZ
#table(is.na(GZ.ture$log_PM_US.Post))
library(ggplot2)
#table(GZ.ture$HUMI)
index =  unique(c(GZ.ture$No[GZ.ture$DEWP > -100], GZ.ture$No[GZ.ture$HUMI > -100]))
GZ.ture = GZ.ture[index,]
# DEWP (Dew Point -- the tempature to reach 100% humility for air)
GZ.DEWP = ggplot(GZ.ture, aes(x=DEWP,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# HUMI 
GZ.HUMI = ggplot(GZ.ture, aes(x=HUMI,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# PRES
GZ.PRES = ggplot(GZ.ture, aes(x=PRES,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()

# TEMP
GZ.TEMP = ggplot(GZ.ture, aes(x=TEMP,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()


library(ggpubr)
ggarrange(GZ.DEWP, GZ.HUMI, GZ.PRES, GZ.TEMP,
          ncol = 2, nrow = 2)


# Iws (wind speed)
GZ.Iws = ggplot(GZ.ture, aes(x=Iws,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
#GZ.Iws = GZ.Iws + geom_vline(xintercept = quantile(GZ.ture$Iws,probs=c(0,0.95))[2], 
#                             linetype = 'dotted', color = 'orange', size = 1)

# precipitation 
GZ.precipitation = ggplot(GZ.ture, aes(x=precipitation,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
#GZ.precipitation = GZ.precipitation + geom_vline(xintercept = quantile(GZ.ture$precipitation,probs=c(0,0.95))[2], 
#                                                 linetype = 'dotted', color = 'orange', size = 1)

# Iprec
GZ.Iprec = ggplot(GZ.ture, aes(x=Iprec,y=PM_US.Post))+geom_point(alpha=0.2)+geom_smooth()
#GZ.Iprec = GZ.Iprec + geom_vline(xintercept = quantile(GZ.ture$Iprec, probs=c(0,0.95))[2], 
#                                 linetype = 'dotted', color = 'orange', size = 1)

ggarrange(GZ.Iws, GZ.precipitation, GZ.Iprec,
          ncol = 1, nrow = 3)


###### 
# ARIMA model
#library(DMwR)
library(DMwR2) # knnImputation
#library(performanceEstimation) # knnImp
#library(bnstruct) # knn.impute

hist(GZ$No[is.na(GZ$PM_US.Post)], main = 'The distrubutiuon of null data')

#length(GZ$No[GZ$year == 2010])
#length(GZ$No[GZ$year == 2011])
#length(GZ$No[GZ$year == 2012]) * 3
# GZ$year[26304:26307] # 26305 is the fisrt day in 2013 
#length(GZ$PM_US.Post[26305:52584])
GZ.PM2.5.df = data.frame(GZ$PM_US.Post[26000:50000], GZ$DEWP[26000:50000], GZ$HUMI[26000:50000],
                         GZ$PRES[26000:50000], GZ$TEMP[26000:50000], GZ$Iws[26000:50000], 
                         GZ$precipitation[26000:50000], GZ$Iprec[26000:50000])
GZ.PM2.5 = knnImputation(GZ.PM2.5.df)

table(is.na(GZ$PM_US.Post[26000:50000])) # null rate or pm2.5 = 0.04795634
# length(GZ$PM_US.Post[is.na(GZ$PM_US.Post)]) # 2197 
# nrow(GZ.PM2.5.df) # 52584    
# GZ$month[GZ$No == 26305]

GZ[50000,]

library(ggplot2)
# general graph 
# stationarity check 
time <- seq.POSIXt(from = as.POSIXct("2012-12-19 07:00:00"), to = as.POSIXct("2015-09-15 08:00:00 "), by = 'hour')
ggplot(GZ.PM2.5)+geom_line(aes(y=GZ.PM_US.Post.26000.50000., x=time)) + 
  xlab("Time(hour)") + ylab('PM2.5')+labs(title = "The PM2.5 from 2013 to 2015")+
  theme(plot.title = element_text(hjust = 0.5))
length(time)


# Box Plot
GZ.PM2.5.box = data.frame(GZ.PM2.5, GZ$year[26000:50000], GZ$month[26000:50000], 
                          GZ$day[26000:50000], GZ$hour[26000:50000], GZ$season[26000:50000])

# box plot by hour 
GZ.Box.hour = ggplot(GZ.PM2.5.box,aes(x = reorder(as.factor(GZ.hour.26000.50000.), 
                                                  GZ.hour.26000.50000.), 
                                      y = GZ.PM_US.Post.26000.50000.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("hour")+ ylab('PM2.5')
# +geom_violin(fill="pink",alpha=0.3)

# box plot by day 
GZ.Box.day = ggplot(GZ.PM2.5.box,aes(x = reorder(as.factor(GZ.day.26000.50000.), GZ.day.26000.50000.), 
                                     y = GZ.PM_US.Post.26000.50000.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("day")+ ylab('PM2.5')

# box plot by month 
GZ.Box.month = ggplot(GZ.PM2.5.box, aes(x = reorder(as.factor(GZ.month.26000.50000.), GZ.month.26000.50000.), 
                                        y = GZ.PM_US.Post.26000.50000.))+
  geom_boxplot(fill="lightblue", alpha=0.8) +  xlab("month")+ ylab('PM2.5')

# box plot by season 
GZ.Box.season = ggplot(GZ.PM2.5.box,aes(x = reorder(as.factor(GZ.season.26000.50000.), GZ.season.26000.50000.), 
                                        y = GZ.PM_US.Post.26000.50000.))+
  geom_boxplot(fill="lightblue",alpha=0.8) +  xlab("season")+ ylab('PM2.5')


library(ggpubr)
ggarrange(GZ.Box.hour, GZ.Box.day, GZ.Box.month, GZ.Box.season,
          ncol = 2, nrow = 2)




# generating time series data 
library(forecast)
PM2.51 <- ts(GZ.PM2.5$GZ.PM_US.Post.26000.50000.) # all
PM2.52 <- ts(GZ.PM2.5$GZ.PM_US.Post.26000.50000., frequency = 24) # day


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
## X-squared = 0.2926, df = 2, p-value = 0.8639

Box.test(fitarima2$residuals,lag=48,type='Ljung')
## X-squared = 311.27, df = 48, p-value < 2.2e-16


# predict plot
time <- seq.POSIXt(from = as.POSIXct("2012-12-19 07:00:00"), 
                   to = as.POSIXct("2016-09-15 08:00:00 "), by = 'hour')
length(GZ.PM2.5$GZ.PM_US.Post.26000.50000.)


forecast_arima1 <- forecast(fitarima1, h = 300)
ggplot() + geom_line(aes(y=GZ.PM2.5$GZ.PM_US.Post.26000.50000.[23600:24001],x=time[23600:24001]))+ 
  geom_point(aes(y=forecast_arima1$mean,x=time[24002:24301]),col="red")+
  geom_point(aes(y=forecast_arima1$lower[,1],x=time[24002:24301]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima1$upper[,1],x=time[24002:24301]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima1$lower[,2],x=time[24002:24301]),col="blue")+
  geom_point(aes(y=forecast_arima1$upper[,2],x=time[24002:24301]),col="blue")+
  labs(title  = "The Prediction of PM2.5 in ChengDu",subtitle="ARIMA(2,1,1)") +xlab(label = "Time") + 
  ylab(label = "PM2.5")+theme(plot.title  = element_text(hjust = 0.5))

forecast_arima2 <- forecast(fitarima2, h = 300)
ggplot() + geom_line(aes(y=GZ.PM2.5$GZ.PM_US.Post.26000.50000.[23600:24001],x=time[23600:24001]))+ 
  geom_point(aes(y=forecast_arima2$mean,x=time[24002:24301]),col="red")+
  geom_point(aes(y=forecast_arima2$lower[,1],x=time[24002:24301]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima2$upper[,1],x=time[24002:24301]),col="dodgerblue2")+
  geom_point(aes(y=forecast_arima2$lower[,2],x=time[24002:24301]),col="blue")+
  geom_point(aes(y=forecast_arima2$upper[,2],x=time[24002:24301]),col="blue")+
  labs(title  = "The Prediction of PM2.5 in ChengDu",subtitle="ARIMA(1,1,1)(1,0,1)[24]") +xlab(label = "Time") + 
  ylab(label = "PM2.5")+theme(plot.title  = element_text(hjust = 0.5))


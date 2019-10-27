# a good tutorial https://www.neonscience.org/dc-brief-tabular-time-series-qplot-r


library(rworldmap)
library(ggplot2)
library(tidyverse)
library(scales)
library(xts)
library(zoo)
library(plyr)
library(forecast)
library(directlabels)

df <- read.csv(file="finaldata.csv", header=TRUE, sep="," )
# Correct the first column name as Cname
colnames(df)[1] <- "Cname"
df <- select(df, Cname, ISO_code, Region, Vaccine, Year, value)
str(df)

head(df)
tail(df)

unique(df$Year)

df[is.na(df)] <- 0
str(df)

unique(df$Region)

#### AFR ############################################
df <- subset(df, Region == "AFR")
#####################################################

#### AFR ############################################
df <- subset(df, Vaccine == "MCV1")
#####################################################

# Autoregressive Moving Average

###Overall Trend##############
overalltrend <- df %>%
  group_by(Year) %>%
  dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
overalltrend
class(overalltrend)
plot(overalltrend)

overalltrend_df <- subset(overalltrend, select=(CoverageRate))
overalltrend_ts <- ts(overalltrend_df, start=min(overalltrend$Year), end=max(overalltrend$Year))
overalltrend_ts
plot(overalltrend_ts)

acf(overalltrend_ts)
pacf(overalltrend_ts)

ndiffs(x=overalltrend_ts)
plot(diff(overalltrend_ts, 2))

tsBest <- auto.arima(x=overalltrend_ts)
tsBest

acf(tsBest$residuals)
pacf(tsBest$residuals)

coef(tsBest)

#predict(tsBest, n.head = 5, se.fit = TRUE)

theForecast <- forecast(object=tsBest, h=5)
plot(theForecast)
#############################################

###overall Trend by Country#######
country <- df %>%
  group_by(Cname, Year) %>%
  dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
country

ggplot(country, (aes(x = Year, y = CoverageRate, group = Cname, colour = Cname))) + 
  geom_line(size=1) +
  geom_dl(aes(label = Cname), method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8)) +
  geom_dl(aes(label = Cname), method = list(dl.trans(x = x - 0.1), "first.points", cex = 0.8)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 45),legend.position="bottom") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Trend analysis per Country") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=FALSE) 


overalltrend <- df %>%
  group_by(Cname, Year) %>%
  dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
overalltrend
class(overalltrend)
plot(overalltrend)

overalltrend_df <- subset(overalltrend, select=(CoverageRate))
overalltrend_ts <- ts(overalltrend_df, start=min(overalltrend$Year), end=max(overalltrend$Year))
overalltrend_ts
plot(overalltrend_ts)

acf(overalltrend_ts)
pacf(overalltrend_ts)

ndiffs(x=overalltrend_ts)
plot(diff(overalltrend_ts, 2))

tsBest <- auto.arima(x=overalltrend_ts)
tsBest

acf(tsBest$residuals)
pacf(tsBest$residuals)

coef(tsBest)

#predict(tsBest, n.head = 5, se.fit = TRUE)

theForecast <- forecast(object=tsBest, h=5)
plot(theForecast)
#############################################

###Overall Trend by Region: Time series analysis#######

trend_by_region <- function(region_selected) {
  
  region <- df %>%
    filter(Region == region_selected) %>%
    group_by(Year) %>%
    dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  overalltrend_df <- subset(region, select=(CoverageRate))
  overalltrend_ts <- ts(overalltrend_df, start=min(region$Year), end=max(region$Year))
  plot(overalltrend_ts, main=region_selected)
  #acf(overalltrend_ts)
  #pacf(overalltrend_ts)
  #ndiffs(x=overalltrend_ts)
  #plot(diff(overalltrend_ts, 1))
  tsBest <- auto.arima(x=overalltrend_ts)
  #tsBest
  #acf(tsBest$residuals)
  #pacf(tsBest$residuals)
  #coef(tsBest)
  #predict(tsBest, n.head = 5, se.fit = TRUE)
  theForecast <- forecast(object=tsBest, h=5)
  plot(theForecast, main=region_selected)  
}

for (i in unique(df$Region)) {
  trend_by_region(i)
} 


#####################################

###overall Trend by Vaccine Type#######
vaccine <- df %>%
  group_by(Vaccine, Year) %>%
  dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))

ggplot(vaccine, (aes(x = Year, y = CoverageRate, group = Vaccine, colour = Vaccine))) + 
  geom_line() +
  geom_dl(aes(label = Vaccine), method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8)) +
  geom_dl(aes(label = Vaccine), method = list(dl.trans(x = x - 0.1), "first.points", cex = 0.8)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 45),legend.position="bottom") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Trend analysis per Vaccine") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=FALSE) 

###Overall Trend by Region: Time series analysis#######

trend_by_vaccine <- function(vaccine_selected) {
  
  vaccine <- df %>%
    filter(Vaccine == vaccine_selected) %>%
    group_by(Year) %>%
    dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  tryCatch({
    overalltrend_df <- subset(vaccine, select=(CoverageRate))
    overalltrend_ts <- ts(overalltrend_df, start=min(vaccine$Year), end=max(vaccine$Year))
    plot(overalltrend_ts, main=vaccine_selected)
    #acf(overalltrend_ts)
    #pacf(overalltrend_ts)
    #ndiffs(x=overalltrend_ts)
    #plot(diff(overalltrend_ts, 1))
    tsBest <- auto.arima(x=overalltrend_ts)
    #tsBest
    #acf(tsBest$residuals)
    #pacf(tsBest$residuals)
    #coef(tsBest)
    #predict(tsBest, n.head = 5, se.fit = TRUE)
    theForecast <- forecast(object=tsBest, h=5)
    plot(theForecast, main=vaccine_selected)
  }, error=function(e){})
}

for (i in unique(df$Vaccine)) {
  trend_by_vaccine(i)
} 
#####################################

### Country##############

trend_by_vaccine_region <- function(vaccine_selected, region_selected) {
  print(paste(vaccine_selected, region_selected, sep=(" in ")))
  vaccine_name <- filter(df, Vaccine == vaccine_selected)
  
  region_vaccinetype <- vaccine_name %>%
    filter(Region == region_selected) %>%
    group_by(Year) %>%
    dplyr::summarize(CoverageRate = mean(value, na.rm=TRUE))
  
  tryCatch({
    overalltrend_df <- subset(region_vaccinetype, select=(CoverageRate))
    overalltrend_ts <- ts(overalltrend_df, start=min(region_vaccinetype$Year), 
                          end=max(region_vaccinetype$Year))
  
    plot(overalltrend_ts, main=paste(vaccine_selected, region_selected, sep=" in "))
    
    #acf(overalltrend_ts)
    #pacf(overalltrend_ts)
    #ndiffs(x=overalltrend_ts)
    #plot(diff(overalltrend_ts, 1))
    tsBest <- auto.arima(x=overalltrend_ts)
    #tsBest
    #acf(tsBest$residuals)
    #pacf(tsBest$residuals)
    #coef(tsBest)
    #predict(tsBest, n.head = 5, se.fit = TRUE)
    theForecast <- forecast(object=tsBest, h=5)
  
    plot(theForecast, main=paste(vaccine_selected, region_selected, sep=" in "))
  }, error=function(e){})
}

for (i in unique(df$Vaccine)) {
  for (j in unique(df$Region)) {
    trend_by_vaccine_region(i, j)
  } 
}



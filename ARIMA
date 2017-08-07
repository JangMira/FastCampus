str(AirPassengers)
plot(AirPassengers)
plot(stl(AirPassengers, s.window="periodic"))

par(mfrow=c(1,2))
acf(AirPassengers)
pacf(AirPassengers)

library(forecast)
ndiffs(x=AirPassengers)

par(mfrow=c(1,1))
plot(diff(AirPassengers,1))
plot(diff(log(AirPassengers),1))
Airs<-diff(log(AirPassengers),1)

library(tseries)
adf.test(Airs, alternative = "stationary", k=0)

Airbest<-auto.arima(x=AirPassengers)
Airbest

predict(Airbest, n.ahead=5, se.fit=TRUE) 

forecast(Airbest, h=40) %>%
plot()









library(WDI)
gdp<-WDI(country = c("US", "CA", "GB", "CN", "JP", "SG", "IL", "KR"),
         indicator=c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"),
         start=1960, end=2016
)


names(gdp)<-c("iso2c", "Country", "Year", "PerCapGDP", "GDP")
View(gdp)
head(gdp)
library(ggplot2)
library(scales)

ggplot(gdp, aes(Year, PerCapGDP, color=Country, linetype=Country)) + 
  geom_line()+scale_y_continuous(label=dollar)



us<-gdp$PerCapGDP[gdp$iso2c=="US"]
us<-ts(us)
us<-ts(us, start = c(1960, 1), end=c(2016, 1), frequency = 1)
plot(us, ylab="Per Capita GDP", xlab="Year")

par(mfrow=c(1,2))
acf(us)
pacf(us)
ndiffs(us)

usBest<-auto.arima(x=us)
usBest   

par(mfrow=c(1,1))
forecast(usBest, h=5)->usforecast
plot(usforecast)

#Fixing Starting Data
mytimeseries <- ts(FHHWMC[,-2], start = 1940)
head(mytimeseries)
#Plotting Main-Untouched Graph
autoplot(mytimeseries) + 
  ggtitle("Global Financial Inclusion Growth: FinTech") + 
  xlab("Year") + 
  ylab("Inclusion Number")
#Autocorrelation Function
ggAcf(mytimeseries)
#Autoplotting Forecasts
autoplot(mytimeseries) +
  autolayer(meanf(mytimeseriestest),
            series="Mean", PI=FALSE) +
  autolayer(rwf(mytimeseriestest),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(mytimeseriestest, drift=TRUE),
            series="Drift", PI=FALSE) +
  autolayer(snaive(mytimeseriestest),
            series = "Seasonal naive", PI=FALSE) +
  ggtitle("Global Financial Inclusion Growth: FinTech") + 
    xlab("Year") +
   ylab("Inclusion Number") +
  guides(colour=guide_legend(title="Forecast"))
#Assigning Forecasts
mean <- meanf(mytimeseriestest)
naive <- rwf(mytimeseriestest)
drift <- rwf(mytimeseriestest, drift=TRUE)
seasonalnaive <- snaive(mytimeseriestest)
#Testing Accuracy of Forecasts
accuracy(mean)
accuracy(naive)
accuracy(drift)
accuracy(seasonalnaive)
#Setting the Test Set
mytimeseriestest <- window(mytimeseries, start = 1990, end = 2018)
#White noise? Yes!
Box.test(mytimeseriestest, type = c("Ljung-Box"))
#Exponential Smoothing (To solve for trend)
exponentialtimeseries <- ses(mytimeseriestest, h = 10)
exponentialtimeseries1 <- ses(mytimeseriestest, alpha = .999, h = 10)
exponentialtimeseries %>%
  autoplot() +
  autolayer(fitted(exponentialtimeseries), series = "Fitted") +
  autolayer(fitted(exponentialtimeseries1), series = "Alpha = 0.1") +
  ylab("Financial Growth & Inclusion") + 
  xlab("Year")
#A Better Model?
mytimeseriesets <- ets(mytimeseriestest)
summary(mytimeseriesets) #AIC 328.91 AICc 332.91 BIC 334.14
autoplot(mytimeseriesets)
#Trendmethods
mytimeseriestest %>%
  autoplot() +
  ggtitle("Global Financial Inclusion: FinTech") +
  xlab("Year") + 
  ylab("Financial Growth & Inclusion")
--
damptrend <- holt(mytimeseriestest, h=10)
damptrend2 <- holt(mytimeseriestest, damped = TRUE, phi = 0.9, h=10, )
summary(damptrend)
--
mytimeseriestest %>%
  autoplot() +
  autolayer(damptrend, series="Holt's method", PI=FALSE) +
  autolayer(damptrend2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's Method") +
  xlab("Year") + ylab("Financial Growth & Inclusion") +
  guides(colour=guide_legend(title="Forecast"))
#Proving Lack of Seasonality
ggseasonplot(mytimeseries, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Financial Growth & Inclusion") +
  ggtitle("Year")
#Install ARIMA Packages
library(tseries)
library(urca)
#Stationary? No, needs 1 difference.
ndiffs(mytimeseriestest)
differencedmytimeseriestest <- diff(mytimeseriestest)
Acf(differencedmytimeseriestest)
adf.test(differencedmytimeseriestest)
ur.kpss(differencedmytimeseriestest)
summary(differencedmytimeseriestest)
Box.test(differencedmytimeseriestest, lag=10, type = "Ljung-Box")
#ARIMA
forecast(ARIMAts, h=10)
ggAcf(mytimeseriestest, main = "ACF for my time series")
ggPacf(mytimeseriestest, main = "PACF for my time series") #(1,1,0)

ARIMAts <- auto.arima(mytimeseriestest, seasonal = FALSE)
ARIMAts #(1,1,0)
ARIMAts2 <- Arima(mytimeseriestest, order = c(1,0,1))
ARIMAts2 #
ARIMAts3 <- Arima(mytimeseriestest, order = c(2,0,0))
ARIMAts3 #
checkresiduals(ARIMAts)
ARIMAts %>% forecast(h=12) %>% autoplot()
#Linear Regression
mydateseries <- ts(FHHWMC[-1], start = 1)
mydateseries
FHHWMC %>%
  as.data.frame() %>%
  ggplot(aes(x=mydateseries, y=mytimeseries)) +
  ylab("Value") +
  xlab("Year") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
summary(mydateseries)


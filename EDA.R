library(stringr)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(strucchange)
library(fable)
library(tsibble) 

BAFA <- read.csv("BirthsAndFertilityRatesAnnual.csv")
head(BAFA)
summary(BAFA)
#Filtering required Variables for time series analysis
#TLB 
TFR <- BAFA[1, ] 
#TFR
TLB <- BAFA[15, ]
#Tranposing the rows to columns
Tranposed_TLB <- t(TLB)
Transposed_TFR <- t(TFR)

# Help to convert the Tranposed_TLB to a data frame, and keeping the TLB numeric values
tlb_df <- data.frame( 
  Year = rownames(Tranposed_TLB)[-1], 
  TLB = as.numeric(Tranposed_TLB[-1,1])
  )

tfr_df <- data.frame( 
  Year = rownames(Transposed_TFR)[-1], 
  TFR = as.numeric(Transposed_TFR[-1,1])
)

tlb_df$Year <- as.numeric(str_remove(tlb_df$Year, "X"))
tfr_df$Year <- as.numeric(str_remove(tfr_df$Year, "X"))

tlb_df <- tlb_df[order(tlb_df$Year), ]
tfr_df <- tfr_df[order(tfr_df$Year), ]

df <- merge(tlb_df,tfr_df, by = "Year")

tlb_ts <- ts(tlb_df$TLB, start = 1960, frequency = 1)
tfr_ts <- ts(tfr_df$TFR, start = 1960, frequency = 1)

#EDA 

summary(tlb_ts)
summary(tfr_ts)
sd(tlb_ts)
sd(tfr_ts)

#initial outlook of the variables to year 
autoplot(tlb_ts) #long-term downward trend, seasonality does not seem obvious 
autoplot(tfr_ts) #long-term dowanward trend, seasonality does not seem present 

ggplot(df, aes (x= Year, y = TFR)) + 
  geom_line() + 
  ggtitle("Total Fertility Rate")

ggplot(df, aes (x= Year, y = TLB)) + 
  geom_line() + 
  ggtitle("Total Live Births")

#Trend analysis
#moving average to comfirm downward trend, and possisble polynominal fitting regressisive model more flexibility and arima models 

# TLB
autoplot(tlb_ts) +
  autolayer(ma(tlb_ts, order = 5), series = "5-year MA") +
  ggtitle("TLB with Moving Average")

# TFR
autoplot(tfr_ts) +
  autolayer(ma(tfr_ts, order = 5), series = "5-year MA") +
  ggtitle("TFR with Moving Average")

#Structural breaks in the data

bp_tlb <- breakpoints(TLB ~ Year, data = df)
summary(bp_tlb)
plot(bp_tlb)

bp_tfr <- breakpoints(TFR~ Year, data = df) 
summary(bp_tfr) 
plot(bp_tfr)

#time series decomposition 
#seasonality analysis 
#STL decomposition fail as the data is evidiently not havign and seasonality component
#Because the data are annual, seasonal decomposition using STL is not appropriate. The series do not contain within-year seasonal structure, so the temporal analysis should focus instead on trend, stationarity, autocorrelation, and structural change.

acf(tlb_ts) # show non-stationarity 
acf(tfr_ts) # shows non-stationarity
pacf(tlb_ts) # significant time autocorrelations spikes
pacf(tfr_ts)# significant time autocorrelations spikes(AR model?)

#Stationary and Autocorrelation analysis
#Difference to achieve stationary
#tlb_ts raw 
adf.test(tlb_ts) #comfirms nonstationarity on raw data 
kpss.test(tlb_ts) #comfrims nonstatioanrity
#tfr_ts raw 
adf.test(tfr_ts) #comfirms stationanrity on raw data
kpss.test(tfr_ts) #comfirms nonstationarity on raw data 
#conflicting results, given the strong result for kpss test and visual trend of the data it is non-stationary. indicates trend statioanrity

#applying first order differencing

#tfr first order differencing
adf.test(diff(tfr_ts)) #confirms staionarity 
kpss.test(diff(tfr_ts)) #comfirms stationarity, borderline passed will do another order diffriencing

acf(diff(tfr_ts)) # still a bit of decay
pacf(diff(tfr_ts))
autoplot(diff(tfr_ts)) # a little bit non-stationary apply a second order diffrencing

#tfr second order diffrencing
acf(diff(diff(tfr_ts))) 
pacf(diff(diff(tfr_ts))) 

autoplot(diff(diff(tfr_ts))) #completely stationary

adf.test(diff(diff(tlb_ts)))  #comfirms stationarity 
kpss.test(diff(diff(tlb_ts))) #comfirms stationarity
# must have second order difference, and inclusion of MA(1) significant spike at lag 1 for ARIMA

#tlb first order differencing
acf(diff(tlb_ts)) # indicate stationarity achieved
pacf(diff(tlb_ts))

adf.test(diff(tlb_ts))  #comfirms stationarity 
kpss.test(diff(tlb_ts)) #comfirms stationarity first order differencing good
# must have first order differenceand no MA component for ARIMA

#Spectral analysis 
spectrum(tlb_ts, main="Periodogram of TLB")
spectrum(tfr_ts, main= "Periodogram of TFR")

#Potential Time series model
#Split into training and test for different model
tlb_train <- window(tlb_ts, end = 2012)
tlb_test  <- window(tlb_ts, start = 2013)

tfr_train <- window(tfr_ts, end = 2012)
tfr_test  <- window(tfr_ts, start = 2013)

#linear regression model with polynomial fitting

# Fit Linear Model with cubic 
fit_trend_tfr <- tslm(tfr_train ~ trend + I(trend^2) +I(trend^3), lambda = 0)

fit_trend_tlb<- tslm(tlb_train ~ trend + I(trend^2) +I(trend^3), lambda = 0)

# Forecasting for linear regressive models
fc_trend_tfr<- forecast(fit_trend_tfr , h = length(tfr_test))

fc_trend_tlb <- forecast(fit_trend_tlb , h = length(tfr_test))

#Plotting
autoplot(fc_trend_tlb) +
  autolayer(tlb_ts, series = "Actual") +
  ggtitle("TLB Forecast with Full Time Series") +
  xlab("Year") + ylab("TLB")

autoplot(fc_trend_tfr) +
  autolayer(tfr_ts, series = "Actual") +
  ggtitle("TFR Forecast with Full Time Series") +
  xlab("Year") + ylab("TFR") 

#ARIMA 
 
fit_arima_tfr <- Arima(tfr_train, order = c(0,2,1)) #Non seasonality component, second order differencing, and MA(1) component
checkresiduals(fit_arima_tfr)

fit_arima_tlb <- Arima(tlb_train, order = c(0,1,0)) #Non seasonality component, first order differencing, and No MA component
checkresiduals(fit_arima_tlb)

#forcasting for ARIMA
fc_arima_tfr <- forecast(fit_arima_tfr, h = length(tfr_test))
fc_arima_tfr$mean 

autoplot(fc_arima_tfr)+
  autolayer(tfr_ts, series = "Actual") +
  ggtitle("TFR Forecast with Full Time Series") +
  xlab("Year") + ylab("TFR")

fc_arima_tlb <- forecast(fit_arima_tlb , h = length(tlb_test))
fc_arima_tlb$mean 

autoplot(fc_arima_tlb)+
  autolayer(tlb_ts, series = "Actual") +
  ggtitle("TLB Forecast with Full Time Series") +
  xlab("Year") + ylab("TLB")

# Forecastng models drift,naive, mean


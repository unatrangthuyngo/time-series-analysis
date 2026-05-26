library(stringr)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
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

autoplot(tlb_ts) 
autoplot(tfr_ts)

#Transformation on the Time Series data
#Box-Cox transformations

lambda_tlb <- BoxCox.lambda(tlb_ts)

lambda_tfr <- BoxCox.lambda(tfr_ts)

lambda_tlb
lambda_tfr

tlb_bc <- BoxCox(tlb_ts, lambda_tlb)
tfr_bc <- BoxCox(tfr_ts, lambda_tfr)

autoplot(tlb_bc) +
  ggtitle("Box-Cox Transformed TLB")

autoplot(tfr_bc) +
  ggtitle("Box-Cox Transformed TFR")

#Log transformation

log_tlb <- log(tlb_ts)
log_tfr <- log(tfr_ts)

autoplot(log_tlb)
autoplot(log_tfr)

#Differencing the time series

#No transformation 

#TLB

diff_tlb <- diff(tlb_ts)

kpss.test(diff_tlb)

autoplot(diff_tlb)

#TFR

diff_tfr <- diff(tfr_ts)

kpss.test(diff_tfr)

diff2_tfr <- diff(diff(tfr_ts))

kpss.test(diff2_tfr)

autoplot(diff2_tfr)

#Box-Cox transformation

#TLB

diff_tlb_c <- diff(tlb_bc)

kpss.test(diff_tlb_c)

autoplot(diff_tlb_c)

#TFR

diff_tfr_c <- diff(tfr_bc)

kpss.test(diff_tfr_c)

autoplot(diff_tfr_c)

#Log transfromation

#TLB

diff_log_tlb <- diff(log_tlb)

kpss.test(diff_log_tlb)

autoplot(diff_log_tlb)

#TFR

diff_log_tfr <- diff(log_tfr)

kpss.test(diff_log_tfr)

diff2_log_tfr <- diff(diff(log_tfr))

kpss.test(diff2_log_tfr)

autoplot(diff2_log_tfr)

#ACF and PACF plots 

#No transformation 
#TLB 
acf(diff_tlb, lag.max = 60)
pacf(diff_tlb, lag.max = 60)

#seasonal differencing 

acf(diff(diff_tlb, lag = 12), lag.max = 60)
pacf(diff(diff_tlb, lag = 12), lag.max = 60)

#TFR 
acf(diff2_tfr, lag.max = 60)
pacf(diff2_tfr, lag.max = 60)

#seasonal differencing

acf(diff(diff2_tfr, lag = 12), lag.max = 60)
pacf(diff(diff2_tfr, lag = 12), lag.max = 60)

#Log transformation 
#TLB 
acf(diff_log_tlb, lag.max = 60)
pacf(diff_log_tlb, lag.max = 60)

#seasonal differencing 

acf(diff(diff_log_tlb, lag = 12), lag.max = 60)
pacf(diff(diff_log_tlb, lag = 12), lag.max = 60)

#TFR 
acf(diff2_log_tfr,lag.max = 60)
pacf(diff2_log_tfr,lag.max = 60)

#seasonal differencing  
acf(diff(diff2_log_tfr, lag = 12), lag.max = 60)
pacf(diff(diff2_log_tfr, lag = 12), lag.max = 60)

#Significant lags at 11, 12, and 13 in PACF, repeating cylical pattern, at 12, 24, etc

#Spectrum Analysis
#TLB
spectrum(diff_tlb, main = "Spectrum of Differenced TLB")

#TFR 
spectrum(diff2_tfr, main = "Spectrum of Differenced TFR")

#log TLB 

spectrum(diff_log_tlb, main = "Spectrum of Differenced Log TLB")

#log TFR

spectrum(diff2_log_tfr, main = "Spectrum of Differenced Log TFR")


#Traning and Testing data sets
tlb_train <- window(tlb_ts, end = 2012)
tlb_test  <- window(tlb_ts, start = 2013)

tfr_train <- window(tfr_ts, end = 2012)
tfr_test  <- window(tfr_ts, start = 2013)

acf(diff(diff_tlb, lag = 12), lag.max = 60)
pacf(diff(diff_tlb, lag = 12), lag.max = 60)

#Fitting time series models 

#ARIMA 
#No transformation 
#TLB

arima1 <- Arima(tlb_train, order = c(2,1,0))

summary(arima1)

acf(arima1$residuals, lag.max = 60)
pacf(arima1$residual, lag.max = 60)

arima2 <- Arima(tlb_train, order = c(2,1,1)) 

summary(arima2)

checkresiduals(arima2, lag.max = 60)

arima3 <- Arima(tlb_train, order = c(12,1,3))

acf(arima3$residuals, lag.max = 60)
pacf(arima3$residuals, lag.max = 60)

arima4 <- Arima(tlb_train, order = c(13, 1, 2))

acf(arima4$residuals, lag.max = 60)
pacf(arima4$residuals, lag.max = 60)

checkresiduals(arima4,lag.max = 60)

AIC(arima1, arima2, arima3, arima4)

#Viable models: ARIMA, (11,1,4), (13, 1, 1), (12, 1, 4), (13, 1, 3), (13, 1, 2). (12,1,3)

#Sarima models


tlb_cycle1 <- Arima(
  tlb_train,
  order = c(2,1,1),
  seasonal = list(order = c(1,0,0), period = 12)
)

checkresiduals(tlb_cycle1, lag.max = 60)

tlb_cycle2 <- Arima(
  tlb_train,
  order = c(2,1,0),
  seasonal = list(order = c(0,0,1), period = 12)
)

checkresiduals(tlb_cycle2, lag.max = 60)
AIC(tlb_cycle2, tlb_cycle1)

tlb_cycle3 <- Arima(
  tlb_train,
  order = c(2,1,0),
  seasonal = list(order = c(1,0,0), period = 12)
)

checkresiduals(tlb_cycle3, lag.max = 60)


tlb_cycle4 <- Arima(
  tlb_train,
  order = c(2,1,0),
  seasonal = list(order = c(1,0,1), period = 12)
)

checkresiduals(tlb_cycle4, lag.max = 60)


tlb_cycle5 <- Arima(
  tlb_train,
  order = c(2,1,0),
  seasonal = list(order = c(2,0,0), period = 12)
)

checkresiduals(tlb_cycle5, lag.max = 60)

tlb_cycle6 <- Arima(
  tlb_train,
  order = c(2,1,2),
  seasonal = list(order = c(0,1,1), period = 12)
)

tlb_cycle7 <- Arima(
  tlb_train,
  order = c(0,1,0),
  seasonal = list(order = c(0,1,0), period = 12)
)

tlb_cycle8 <- Arima( 
  tlb_train, 
  order = c(4,1,0), 
  seasonal = list(order = c(0,1,0), period = 12))

acf(tlb_cycle8$residuals, lag.max = 60)
pacf(tlb_cycle8$residuals, lag.max = 60)

acf(tlb_cycle7$residuals, lag.max = 60)
pacf(tlb_cycle7$residual, lag.max = 60)

acf(tlb_cycle6$residuals, lag.max = 60)

pacf(tlb_cycle6$residuals, lag.max = 60)

checkresiduals(tlb_cycle6, lag.max = 60)

AIC(tlb_cycle1,tlb_cycle2, tlb_cycle4, tlb_cycle5, tlb_cycle3)

#Viable models: SARIMA, c(3,1,1)(1,1,0), period = 12), c(4,1,0)(0,1,0), period = 12),c(3,1,1)(0,1,1), period = 12) 
# c(3,1,1)(1,1,1), period = 12), c(2,1,2)(0,1,0), period = 12)

#(0,1,0), and (0,1,0) period 12 and seasonal 

#TFR

arimaTFR <- Arima(tfr_train, order = c(11,2,2))

acf(arimaTFR$residuals, lag.max = 60)
pacf(arimaTFR$residuals, lag.max = 60)

#Viable model: ARIMA, (11,2,2), (10,2,3), (12,2,3)

#Sarima Model

tfr_cycle1 <- Arima(
  tfr_train,
  order = c(2,2,2),
  seasonal = list(order = c(1,1,0), period = 12)
)

acf(tfr_cycle1$residuals, lag.max = 60)
pacf(tfr_cycle1$residuals, lag.max = 60)

tfr_cycle2 <- Arima(
  tfr_train,
  order = c(3,1,3),
  seasonal = list(order = c(2,1,0), period = 12)
)

tfr_cycle3 <- Arima( 
  tfr_train, 
  order = c(3,1,3),
  seasonal = list(order = c(1,1,1), period = 12)) 

acf(tfr_cycle1$residuals, lag.max = 60)
pacf(tfr_cycle1$residuals, lag.max = 60)

acf(tfr_cycle3$residuals, lag.max = 60)
pacf(tfr_cycle3$residuals, lag.max = 60)

acf(tfr_cycle2$residuals, lag.max = 60)
pacf(tfr_cycle2$residuals, lag.max = 60)

AIC(tfr_cycle2, tfr_cycle1)

#Viable Models: SARIMA (2,2,2)(1,1,0 period=12), (4,2,1)(0,1,0, period = 12), (4,2,2)(0,1,0, period = 12)
#(5,2,1)(0,1,0, period = 12), (5,2,2)(0,1,0, period = 12)

#principle of parisomny

#Log transformation
#TLB 

log_tlb_arima1 <- Arima(log(tlb_train), order = c(0,1,0))

acf(log_tlb_arima1$residuals, lag.max = 60)
pacf(log_tlb_arima1$residuals, lag.max = 60)

log_tlb_arima2 <- Arima(log(tlb_train), order = c(13,1,1))

acf(log_tlb_arima2$residuals, lag.max = 60)
pacf(log_tlb_arima2$residuals, lag.max = 60)

log_tlb_arima3 <- Arima(log(tlb_train), order = c(11,1,4))

acf(log_tlb_arima3$residuals, lag.max = 60)
pacf(log_tlb_arima3$residuals, lag.max = 60)

#Viable Models: ARIMA, (14,1,0), (13,1,1), (12,1,3), (11,1,4)

#Sarima models 

log_tlb_cycle1 <- Arima( 
  log(tlb_train), 
  order = c(3,1,3), 
  seasonal = list(order = c(0,1,1), period = 12))

log_tlb_cycle2 <- Arima( 
  log(tlb_train), 
  order = c(3,1,2), 
  seasonal = list(order = c(0,1,1), period = 12))

log_tlb_cycle3 <- Arima( 
  log(tlb_train), 
  order = c(3,1,2), 
  seasonal = list(order = c(1,1,1), period = 12))

acf(log_tlb_cycle3$residuals, lag.max = 60)
pacf(log_tlb_cycle3$residuals, lag.max = 60)

acf(log_tlb_cycle2$residuals, lag.max = 60)
pacf(log_tlb_cycle2$residuals, lag.max = 60)

acf(log_tlb_cycle1$residuals, lag.max = 60)
pacf(log_tlb_cycle1$residuals, lag.max = 60)

#Viable models: SARIMA: (3,1,3)(0,1,1, period = 12), (3,1,2)(0,1,1, period = 12), (3,1,3)(1,1,0 period = 12)
#(3,1,2)(1,1,1, period = 12)

#TFR
log_tfr_arima1 <- Arima(log(tfr_train), order = c(0,1,0))

acf(log_tfr_arima1$residuals, lag.max = 60)
pacf(log_tfr_arima1$residuals, lag.max = 60)

log_tfr_arima2 <- Arima(log(tfr_train), order = c(14,1,3))

acf(log_tfr_arima2$residuals, lag.max = 60)
pacf(log_tfr_arima2$residuals, lag.max = 60)

#Viable Models: ARIMA, (15,1,1), (12,1,4), (13,1,3), (14,1,3)
#Sarima models 

log_tfr_cycle1 <- Arima( 
  log(tfr_train), 
  order = c(3,1,1), 
  seasonal = list(order = c(0,1,1), period = 12))


acf(log_tfr_cycle1$residuals, lag.max = 60)
pacf(log_tfr_cycle1$residuals, lag.max = 60)

log_tfr_cycle2 <- Arima( 
  log(tfr_train), 
  order = c(3,1,1), 
  seasonal = list(order = c(1,1,0), period = 12))


AIC(log_tfr_cycle1, log_tfr_cycle2)
acf(log_tfr_cycle2$residuals, lag.max = 60)
pacf(log_tfr_cycle2$residuals, lag.max = 60)

#Viable Model: SARIMA: (3,1,1)(0,1,1, period= 12), (3,1,1)(1,1,0, period= 12),

#Ensure analysis of the arima and sarima models, noting special notes of the equation and qualities
#Predictive forcasting accuracy

#TLB successful models 

#No transform 

#log transform



#TFR successful models

#No transform 

#log transform



#MASE

#MAE

#SSE

#AIC


#Plotting predictive forecasting to actual testing data
#Compare time series models with AIC and look at the degrees of freedom, make sure final model are justified by literature




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

#Traning and Testing data sets
tlb_train <- window(tlb_ts, end = 2012)
tlb_test  <- window(tlb_ts, start = 2013)

tfr_train <- window(tfr_ts, end = 2012)
tfr_test  <- window(tfr_ts, start = 2013)

#EDA

autoplot(tlb_ts) 
autoplot(tfr_ts)

#Transformation on the Time Series data
#Log transformation

log_tlb <- log(tlb_train)
log_tfr <- log(tfr_train)

autoplot(log_tlb)
autoplot(log_tfr)

#Differencing the time series

#No transformation 

#TLB

diff_tlb <- diff(tlb_train)

kpss.test(diff_tlb)

autoplot(diff_tlb)

#TFR

diff_tfr <- diff(tfr_train)

kpss.test(diff_tfr)

diff2_tfr <- diff(diff(tfr_train))

kpss.test(diff2_tfr)

autoplot(diff2_tfr)

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
acf(diff_tlb, lag.max = 50)
pacf(diff_tlb, lag.max = 50)

#seasonal differencing 

acf(diff(diff_tlb, lag = 12), lag.max = 50)
pacf(diff(diff_tlb, lag = 12), lag.max = 50)

#TFR 
acf(diff2_tfr, lag.max = 60)
pacf(diff2_tfr, lag.max = 60)

#seasonal differencing

acf(diff(diff2_tfr, lag = 12), lag.max = 50)
pacf(diff(diff2_tfr, lag = 12), lag.max = 50)

#Log transformation 
#TLB 
acf(diff_log_tlb, lag.max = 50)
pacf(diff_log_tlb, lag.max = 50)

#seasonal differencing 

acf(diff(diff_log_tlb, lag = 12), lag.max = 50)
pacf(diff(diff_log_tlb, lag = 12), lag.max = 50)

#TFR 
acf(diff2_log_tfr ,lag.max = 50)
pacf(diff2_log_tfr ,lag.max = 50)

#seasonal differencing  
acf(diff(diff2_log_tfr, lag = 12), lag.max = 50)
pacf(diff(diff2_log_tfr, lag = 12), lag.max = 50)

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

#TLB successful models 

#Non-transform 
#ARIMA (ARIMA 1-6 no trans TLB)
m1 <- Arima(tlb_train, order = c(11,1,4))

acf(m1$residuals, lag.max = 60) #45-50 
pacf(m1$residuals, lag.max = 60)

m2 <- Arima(tlb_train, order = c(13, 1, 1))

acf(m2$residuals, lag.max = 60)
pacf(m2$residuals, lag.max = 60)

m3 <- Arima(tlb_train, order = c(12, 1, 4))

acf(m3$residuals, lag.max = 60)
pacf(m3$residuals, lag.max = 60)

m4 <- Arima(tlb_train, order = c (13, 1, 3))

acf(m4$residuals, lag.max = 60)
pacf(m4$residuals, lag.max = 60)

m5 <- Arima(tlb_train, order = c (13, 1, 2))

acf(m5$residuals, lag.max = 60)
pacf(m5$residuals, lag.max = 60)

#SARIMA (SARIMA 6-10 no trans TLB)
m6 <- Arima( 
  tlb_train, 
  order = c(3,1,1), 
  seasonal = list(order = c(1,1,0), period = 12))

acf(m6$residuals, lag.max = 60)
pacf(m6$residuals, lag.max = 60)

m7 <- Arima( 
  tlb_train, 
  order = c(4,1,0), 
  seasonal = list(order = c(0,1,0), period = 12))

acf(m7$residuals, lag.max = 60)
pacf(m7$residuals, lag.max = 60)

m8 <- Arima( 
  tlb_train, 
  order = c(3,1,1), 
  seasonal = list(order = c(0,1,1), period = 12))

acf(m8$residuals, lag.max = 60)
pacf(m8$residuals, lag.max = 60)

m9 <- Arima( 
  tlb_train, 
  order = c(3,1,1), 
  seasonal = list(order = c(1,1,1), period = 12))

acf(m9$residuals, lag.max = 60)
pacf(m9$residuals, lag.max = 60)

m10 <- Arima( 
  tlb_train, 
  order = c(2,1,2), 
  seasonal = list(order = c(0,1,0), period = 12))

acf(m10$residuals, lag.max = 60)
pacf(m10$residuals, lag.max = 60)


#log transform

#ARIMA (11-14 ARIMA log trans TLB)

m11 <- Arima(log(tlb_train), order = c(14,1,0))

acf(m11$residuals, lag.max = 60)
pacf(m11$residuals, lag.max = 60)

m12  <- Arima(log(tlb_train), order = c(13,1,1)) #overlap with the non transformation data 

acf(m12$residuals, lag.max = 60)
pacf(m12$residuals, lag.max = 60)

m13  <- Arima(log(tlb_train), order = c(12,1,3)) #overlap with the non transformation data

acf(m13$residuals, lag.max = 60)
pacf(m13$residuals, lag.max = 60)

m14 <- Arima(log(tlb_train), order = c(11,1,4)) #overlap with the non transformation data 

acf(m14$residuals, lag.max = 60)
pacf(m14$residuals, lag.max = 60)

#SARIMA (15-18 SARIMA log trans TLB)

m15 <- Arima( 
  log(tlb_train), 
  order = c(3,1,3), 
  seasonal = list(order = c(0,1,1), period = 12))

acf(m15$residuals, lag.max = 60)
pacf(m15$residuals, lag.max = 60)

m16 <- Arima( 
  log(tlb_train), 
  order = c(3,1,2), 
  seasonal = list(order = c(0,1,1), period = 12))

acf(m16$residuals, lag.max = 60)
pacf(m16$residuals, lag.max = 60)

m17 <- Arima( 
  log(tlb_train), 
  order = c(3,1,3), 
  seasonal = list(order = c(1,1,0), period = 12))

acf(m17$residuals, lag.max = 60)
pacf(m17$residuals, lag.max = 60)

m18 <- Arima( 
  log(tlb_train), 
  order = c(3,1,2), 
  seasonal = list(order = c(1,1,1), period = 12))

acf(m18$residuals, lag.max = 60)
pacf(m18$residuals, lag.max = 60)

#TFR successful models

#No transform 
#ARIMA (19-21 arima no trans TFR)
m19 <- Arima(tfr_train, order = c(11,2,2))

acf(m19$residuals, lag.max = 60)
pacf(m19$residuals, lag.max = 60)

m20 <- Arima(tfr_train, order = c(10,2,3))

acf(m20$residuals, lag.max = 60)
pacf(m20$residuals, lag.max = 60)

m21 <- Arima(tfr_train, c(12,2,3))

acf(m21$residuals, lag.max = 60)
pacf(m21$residuals, lag.max = 60)

#SARIMA (22-26 arima no trans TFR)

m22 <- Arima( 
  tfr_train, 
  order = c(2,2,2), 
  seasonal = list(order = c(1,1,0), period = 12))

acf(m22$residuals, lag.max = 60)
pacf(m22$residuals, lag.max = 60)

m23 <- Arima( 
  tfr_train, 
  order = c(4,2,1), 
  seasonal = list(order = c(0,1,0), period = 12))

acf(m23$residuals, lag.max = 60)
pacf(m23$residuals, lag.max = 60)

m24 <- Arima( 
  tfr_train, 
  order = c(4,2,2), 
  seasonal = list(order = c(0,1,0), period = 12))

acf(m24$residuals, lag.max = 60)
pacf(m24$residuals, lag.max = 60)

m25 <- Arima( 
  tfr_train, 
  order = c(5,2,1), 
  seasonal = list(order = c(0,1,0), period = 12))

acf(m25$residuals, lag.max = 60)
pacf(m25$residuals, lag.max = 60)

m26 <- Arima( 
  tfr_train, 
  order = c(5,2,2), 
  seasonal = list(order = c(0,1,0), period = 12))

acf(m26$residuals, lag.max = 60)
pacf(m26$residuals, lag.max = 60)

#log transform

#ARIMA (ARIMA 27-30 log trans TFR)
m27 <- Arima(log(tfr_train), order = c(12,2,3))
acf(m27$residuals, lag.max = 60)
pacf(m27$residuals, lag.max = 60)

m28 <- Arima(log(tfr_train), order = c(13,2,3))
acf(m28$residuals, lag.max = 60)
pacf(m28$residuals, lag.max = 60)

m29 <- Arima(log(tfr_train), order = c(13,2,2))
acf(m29$residuals, lag.max = 60)
pacf(m29$residuals, lag.max = 60)

m30 <- Arima(log(tfr_train), order = c(14,2,2))
acf(m30$residuals, lag.max = 60)
pacf(m30$residuals, lag.max = 60)

#SARIMA (SARIMA 31-32 log trans TFR)

m31 <- Arima( 
  log(tfr_train), 
  order = c(2,2,3), 
  seasonal = list(order = c(0,1,1), period = 12))
acf(m31$residuals, lag.max = 60)
pacf(m31$residuals, lag.max = 60)

m32 <- Arima( 
  log(tfr_train), 
  order = c(3,2,2), 
  seasonal = list(order = c(1,1,0), period = 12))
acf(m32$residuals, lag.max = 60)
pacf(m32$residuals, lag.max = 60)


#Predictive forecasting accuracy

#AIC

#TLB 

#Non Transfromed
#ARIMA
AIC(m1, m2, m3, m4, m5) 
#SARIMA
AIC(m6, m7, m8,m9, m10)

#Log Transformed
#ARIMA
AIC(m11, m12, m13, m14)
#SARIMA
AIC(m15, m16, m17, m18)

#TFR 

#Non Transformed 
#ARIMA
AIC(m19, m20, m21)
#SARIMA
AIC(m22, m23, m24, m25, m26)

#Log Transformed
#ARIMA
AIC(m27, m28, m29, m30)
#SARIMA
AIC(m31, m32)

#MAE, MSE (RMSE^2 =MSE)

#TLB 

fc_m1 <- forecast(m1, h = length(tlb_test))

accuracy(fc_m1, tlb_test)

fc_m2 <- forecast(m2, h = length(tlb_test))

accuracy(fc_m2, tlb_test)

fc_m3 <- forecast(m3, h = length(tlb_test))

accuracy(fc_m3, tlb_test)

fc_m4 <- forecast(m4, h = length(tlb_test))

accuracy(fc_m4, tlb_test)

fc_m5 <-  forecast(m5, h = length(tlb_test))

accuracy(fc_m5, tlb_test)

fc_m6 <- forecast(m6, h = length(tlb_test))

accuracy(fc_m6, tlb_test)

fc_m7 <- forecast(m7, h = length(tlb_test))

accuracy(fc_m7, tlb_test)

fc_m8 <-  forecast(m8, h = length(tlb_test))

accuracy(fc_m8, tlb_test)

fc_m9 <- forecast(m9, h = length(tlb_test))

accuracy(fc_m9, tlb_test)

fc_m10 <- forecast(m10, h = length(tlb_test))

accuracy(fc_m10, tlb_test)

#log transformation

fc_m11 <- forecast(m11, h = length(tlb_test))

accuracy(fc_m11, log(tlb_test))

fc_m12 <- forecast(m12, h = length(tlb_test))

accuracy(fc_m12, log(tlb_test))

fc_m13 <- forecast(m13, h = length(tlb_test))

accuracy(fc_m13, log(tlb_test))

fc_m14 <- forecast(m14, h = length(tlb_test))

accuracy(fc_m14, log(tlb_test))

fc_m15 <-  forecast(m15, h = length(tlb_test))

accuracy(fc_m15, log(tlb_test))

fc_m16 <-forecast(m16, h = length(tlb_test))

accuracy(fc_m16, log(tlb_test))

fc_m17 <- forecast(m17, h = length(tlb_test))

accuracy(fc_m17, log(tlb_test))

fc_m18 <-forecast(m18, h = length(tlb_test))

accuracy(fc_m18, log(tlb_test))

#TFR

fc_m19 <-forecast(m19, h = length(tfr_test))

accuracy(fc_m19, tfr_test)

fc_m20 <-forecast(m20, h = length(tfr_test))

accuracy(fc_m20, tfr_test)

fc_m21 <-forecast(m21, h = length(tfr_test))

accuracy(fc_m21, tfr_test)

fc_m22 <-forecast(m22, h = length(tfr_test))

accuracy(fc_m22, tfr_test)

fc_m23 <-forecast(m23, h = length(tfr_test))

accuracy(fc_m23, tfr_test)

fc_m24 <-forecast(m24, h = length(tfr_test))

accuracy(fc_m24, tfr_test)

fc_m25 <-forecast(m25, h = length(tfr_test))

accuracy(fc_m25, tfr_test)

fc_m26 <-forecast(m26, h = length(tfr_test))

accuracy(fc_m19, tfr_test)
  
#Log transform
fc_m27 <-forecast(m27, h = length(tfr_test))

accuracy(fc_m27, log(tfr_test))

fc_m28 <-forecast(m28, h = length(tfr_test))

accuracy(fc_m28, log(tfr_test))

fc_m29 <-forecast(m29, h = length(tfr_test))

accuracy(fc_m29, log(tfr_test))

fc_m30 <-forecast(m30, h = length(tfr_test))

accuracy(fc_m30, log(tfr_test))

fc_m31 <-forecast(m31, h = length(tfr_test))

accuracy(fc_m31, log(tfr_test))

fc_m32 <-forecast(m32, h = length(tfr_test))

accuracy(fc_m32, log(tfr_test))


#Plotting predictive forecasting to actual testing data

#TLB m15 the best 

#No trans form 
#(ARIMA 1-5 no trans TLB)
#(SARIMA 6-10 no trans TLB)

fc <- forecast(m9, h = length(tlb_test))

autoplot(fc) +
  autolayer(tlb_test, series = "Actual")

# Good looking prediction: 8,9

# log trans form 
#(11-14 ARIMA log trans TLB)
#(15-18 SARIMA log trans TLB)

fc <- forecast(m12, h = length(tlb_test))

autoplot(fc) +
  autolayer(log(tlb_test), series = "Actual")
# Good looking prediction:15,16,18  

#TFR 
#No trans 
#(19-21 arima no trans TFR)
#(22-26 Sarima no trans TFR)

fc <- forecast(m22, h = length(tfr_test))

autoplot(fc) +
  autolayer(tfr_test, series = "Actual")

# Good looking prediction: 21, 19, 22 

#log Trans 
#(ARIMA 27-30 log trans TFR)
#(SARIMA 31-32 log trans TFR)

fc <- forecast(m32, h = length(tfr_test))

autoplot(fc) +
  autolayer(log(tfr_test), series = "Actual")

# Good looking prediction: 28, 31 

#Compare time series models with AIC and look at the parameters, make sure final model are justified by literature




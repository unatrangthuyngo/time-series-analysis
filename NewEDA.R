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
#TFR 
acf(diff2_tfr, lag.max = 60)
pacf(diff2_tfr, lag.max = 60)

#Box cox transformation 
#TLB 
acf(diff_tlb_c, lag.max = 60)
pacf(diff_tlb_c, lag.max = 60)
#TFR 
acf(diff_tfr_c, lag.max = 60)
pacf(diff_tfr_c, lag.max = 60)

#Log transformation 
#TLB 
acf(diff_log_tlb, lag.max = 60)
pacf(diff_log_tlb, lag.max = 60)
#TFR 
acf(diff2_log_tfr,lag.max = 60)
pacf(diff2_log_tfr,lag.max = 60)

#Significant lags at 11, 12, and 13 in PACF, reepatign cylical pattern, at 12, 24, etc

#Traning and Testing data sets
tlb_train <- window(tlb_ts, end = 2012)
tlb_test  <- window(tlb_ts, start = 2013)

tfr_train <- window(tfr_ts, end = 2012)
tfr_test  <- window(tfr_ts, start = 2013)



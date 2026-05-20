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




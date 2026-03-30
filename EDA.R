library(stringr)
library(dplyr)
library(tsibble)
library(feasts)
library(fable)
library(ggplot2)
library(forecast)
BAFA <- read.csv("BirthsAndFertilityRatesAnnual.csv")
head(BAFA)
summary(BAFA)
#Filtering required Variables for time series analysis
#TLB 
TLB <- BAFA[1, ]
#TFR
TFR <- BAFA[15, ]
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
#initial outlook of the variables to year 
autoplot(tlb_ts)
autoplot(tfr_ts)

ggplot(df, aes (x= Year, y = TFR)) + 
  geom_line() + 
  ggtitle("Total Fertility Rate")

ggplot(df, aes (x= Year, y = TLB)) + 
  geom_line() + 
  ggtitle("Total Live Births")

#Difference to achieve stationary
autoplot(diff(tlb_ts))
autoplot(diff(diff(tlb_ts)))
autoplot(diff(tfr_ts))

#ACF and PACF plots

Acf(diff(tlb_ts))
Acf(tfr_ts)
Pacf(diff(diff(tlb_ts)))
Pacf(diff(tfr_ts))

diff(diff(tlb_ts)) |> Acf() |> autoplot()
diff(diff(tfr_ts)) |> Acf() |> autoplot()



#STL
tfr_ts |> model(stl = STL(TFR)) |> components() |> autoplot()

# Potential Time series model

#linear regression model with polynominal fitting
#Ar model 
#Arima model

model_tlb <- 
model_tfr <-


breakpoints(tfr_ts ~ 1)

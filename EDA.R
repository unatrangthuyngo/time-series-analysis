install.packages("tseries")
library(stringr)
library(dplyr)
library(tsibble)
library(feasts)
library(fable)
library(ggplot2)
library(forecast)
library(tseries)
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


#time series decomposition 
#seasonality analysis 

stl_decomp1 <- stl(tfr_ts, s.window = "periodic")
plot(stl_decomp)

stl_decomp2 <- stl(tlb_ts, s.window = "periodic")
plot(stl_decomp)

#STL
tfr_ts |> model(stl = STL(TFR)) |> components() |> autoplot()

#STL decomposition fail as the data is evidiently not havign and seasonality component
#Because the data are annual, seasonal decomposition using STL is not appropriate. The series do not contain within-year seasonal structure, so the temporal analysis should focus instead on trend, stationarity, autocorrelation, and structural change.


#box-cox analysis 
#installed to asses the need for varience stabilisation
BoxCox.lambda(tlb_ts)
BoxCox.lambda(tfr_ts)
#potential log transformation on 
#Stationary analysis 
#Difference to achieve stationary
#tfr
adf.test(tfr_ts) #comfirms nonstationarity on raw data 
kpss.test(tfr_ts) #comfrims nonstatioanrity

adf.test(tlb_ts) #comfirms nonstatioanriy on raw data
kpss.test(tlb_ts) #comfirms nonstationarity on raw data 
#conflicting results in the test suggesting the implementation of the log transforms

adf.test(log(tlb_ts)) 
kpss.test(log(tlb_ts))

#Difference to achieve stationary(visualisation) 
autoplot(diff(tlb_ts))
autoplot(diff(tfr_ts))
autoplot(diff(log(tlb_ts))) #statioanrity definitely improved from log transform
autoplot(diff(log(tfr_ts))) #statioanrity is seen as similar 
#comfirming stationarity is achieve through diffirencing
adf.test(diff(tfr_ts)) #staionrity is achecived through first order diffrencing
kpss.test(diff(tfr_ts))

adf.test(diff(tlb_ts))
kpss.test
#Correlation analysis
#autocorrelation analysis
#ACF and PACF plots
Acf(diff(tfr_ts))
Acf(diff(tlb_ts))
Pacf(diff(tlb_ts))
Pacf(diff(tfr_ts))

diff(diff(tlb_ts)) |> Acf() |> autoplot()
diff(diff(tfr_ts)) |> Acf() |> autoplot()

#Time series analysis 

# Potential Time series model
#given that 
#Given certain parameters, which the time series data exihibits 

#linear regression model with polynomial fitting
#Ar model 
#ARIMA (non-seasonal)

model_tlb <- 
model_tfr <-


breakpoints(tfr_ts ~ 1)

#ARIMA (non-seasonal)
#testing of forecasting of models
#Split into training and test for different model
#Research questions 1-3

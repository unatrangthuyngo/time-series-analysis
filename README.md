This project conducts an exploratory data analysiis (EDA) and time series modelling of Singapore': 

-Total live Births (TLB) 
-Total Fertility Rate (TFR) 

over the period 1960 to 2024

The aim is to: 
-Investigate the temporal characteristics of both series
-asses stationarity and structural changes
-develop appropriate forecasting models
-and evaluate model perfromance using statistical diagnostics and forecast accuracy measures. 

Through the analysis,  the following questions were decided: 

1. Do TLB and TFR exhibit synchronized structural breaks (e.g post-2008 or Covid-19), and how do
these shifts degrade the forecasting accuracy of the ARIMA models?

2.To what degree is the Long term decline in TFR driven by secular trends (e.g increasing maternal age)
versus cyclical shocks (e.g changes in healthcare access or contraceptive prevalence)

3.How does volatility of Total Live Births respond to macroeconomic fluctuations (e.g, CPI or Unemployment), and is there a consistent 9-12 month lagged effect?

## Data cleaning 
 Data: https://tablebuilder.singstat.gov.sg/table/TS/M810091
The dataset was transformed from a wide format into a time-series friendly format, where the following procedues were done: 

- Extracted TLB and TFR rows
- transposed into long format
- converted year values into numeric format
- sorted and merged into unified data set
- converted into time series object ('ts')

## Exploratory Data Analysis 
Key findings: 
-Both TLB and TFR exhibit strong non lienar downward trends
-No evidence of seasonality given the annual frequency 
presence of structural breaks in particularlt TLB 
-spectrial analysis shows no dominate cylical patterns 
-strong autocorrelation indicating temporal dependence

## Time Series modelling 

Models considered: 
- Mean model
- Naive model
- Naive model with drift
- polynominal regression
- ARIMA models

final selected models: 
TLB: ARIMA(0,1,0), and Naive
TFR: ARIMA(0,2,1) 

## Model Evaluations
- ACF plots
- Ljung-Box tests
- Visual comparison of forecats vs actual data

## Limitations
- Annual data limits detetction of short-term dynamics
- no external variables affect on the decline of TLB and TFR

#Packages used
-stringr
-dplyr
-ggplot2
-forecast 
-tseries 
-strucchnage
-fable 
-tsibble





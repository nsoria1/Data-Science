# Load required libraries
library(forecast)
library(devtools)
library(ggplot2)
library(ggfortify)

# Load dataset
bwi <- read.csv(file="C:/Users/nsoria/Google Drive/Trabajo/AMS Globales/TEC_BWI.csv", header=TRUE, sep=';', dec=",")

# Create time series starting in January 2015
ts_bwi <- ts(bwi$BWI, frequency = 12, start = c(2015,1))

# Pull out the seasonal, trend, and irregular components from the time series 
model <- stl(ts_bwi, s.window = "periodic")

# Predict the next 5 months of SLA
pred <- forecast(model, h = 5)

# Plot the results
autoplot(pred, ylab = "SLA Value", xlab = "Year", main = "SLA Prediction")

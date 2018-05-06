# Load required libraries
library(forecast)
library(lubridate)
library(tidyverse)
library(scales)
library(ggfortify)

# Load historical SLA definitions
bwi <- read.csv(file="C:/Users/nsoria/Documents/Development/Data Science/SLA Prediction/TEC_BWI.csv", header=TRUE, sep=';', dec=",")

# Create time series object
ts_bwi <- ts(bwi$SLA, frequency = 12, start = c(2015,1))

# Defining predictions bounds
a <- 0
b <- 1
y <- log((ts_bwi-a)/(b-ts_bwi))

############################################ STL Model Algorithm
# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(y, s.window = "periodic")

############################################ ARIMA Model Algorithm
# Pull out the seasonal, trend, and irregular components from the time series
#model_arima <- auto.arima(ts_bwi)

# Predict the next 5 month of SLA (STL)
pred <- forecast(model_stl, h = 5)

# Adjusting bounds
pred$mean <- (b-a)*exp(pred$mean)/(1+exp(pred$mean)) + a
pred$lower <- (b-a)*exp(pred$lower)/(1+exp(pred$lower)) + a
pred$upper <- (b-a)*exp(pred$upper)/(1+exp(pred$upper)) + a
pred$fitted <- (b-a)*exp(pred$fitted)/(1+exp(pred$fitted)) + a
pred$x <- ts_bwi

# Predict the next 5 month of SLA (ARIMA)
#pred <- forecast(model_arima, h = 5)

# Convert pred from list to data frame object
df1 <- fortify(pred) %>% as_tibble()

# Convert ts decimal time to Date class
df1$Date <- as.Date((df1$Index), "%Y-%m-%d")

# Download predictions into CSV
write.csv(df1, "C:/Users/nsoria/Documents/Development/Data Science/Visualization/SLA_Prediction_v2.csv")

# Remove Index column and rename other columns
# Select only data pts after 2017/10
df1 <- df1 %>% 
  select(-Index) %>% 
  filter(Date >= as.Date("2017-5-01")) %>% 
  rename("Low95" = "Lo 95",
         "Low80" = "Lo 80",
         "High95" = "Hi 95",
         "High80" = "Hi 80",
         "Forecast" = "Point Forecast")

# assign the last non-NA row of Data column to the corresponding row of other columns
lastNonNAinData <- max(which(complete.cases(df1$Data)))
df1[lastNonNAinData, !(colnames(df1) %in% c("Data", "Fitted", "Date"))] <- df1$Data[lastNonNAinData]

plt1 <- ggplot(df1, aes(x = Date)) +   
  ggtitle("SLA % Forecasting") +
  xlab("Time frame") + ylab("Quantity") +
  geom_ribbon(aes(ymin = Low95, ymax = High95, fill = "95%")) +
  geom_ribbon(aes(ymin = Low80, ymax = High80, fill = "80%")) +
  geom_point(aes(y = Data, colour = "Data"), size = 4) +
  geom_line(aes(y = Data, group = 1, colour = "Data"), 
            linetype = "dotted", size = 0.75) +
  geom_line(aes(y = Fitted, group = 2, colour = "Fitted"), size = 0.75) +
  geom_line(aes(y = Forecast, group = 3, colour = "Forecast"), size = 0.75) +
  scale_x_date(breaks = scales::pretty_breaks(), date_labels = "%b %y") +
  scale_colour_brewer(name = "Legend", type = "qual", palette = "Dark2") +
  scale_fill_brewer(name = "Intervals") +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  theme_bw(base_size = 14)
plt1

# Load required libraries
library(forecast)
library(lubridate)
library(tidyverse)
library(scales)
library(ggfortify)

# Load dataset
emea <- read.csv(file="C:/Users/nsoria/Documents/Data Science/Ticket Prediction/812_Finanzas.csv", header=TRUE, sep=';', dec=",")

# Create time series object
ts_fin <- ts(emea$Valor, deltat = 1/24, start = c(2015, 1))

############################################ STL Model Algorithm
# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_fin, s.window = "periodic")

############################################ ARIMA Model Algorithm
# Pull out the seasonal, trend, and irregular components from the time series
#model_arima <- auto.arima(ts_fin)

# Predict the next 3 bi weeks of tickets
pred <- forecast(model_stl, h = 5)

# Round values to better accuracy
pred$mean <- round(pred$mean)
pred$upper <- round(pred$upper)
pred$lower <- round(pred$lower)

# Convert pred from list to data frame object
df1 <- fortify(pred) %>% as_tibble()

# Convert ts decimal time to Date class
df1$Date <- as.Date(date_decimal(df1$Index), "%Y-%m-%d")

# Remove Index column and rename other columns
# Select only data pts after 2017/10
df1 <- df1 %>% 
  select(-Index) %>% 
  filter(Date >= as.Date("2017-11-01")) %>% 
  rename("Low95" = "Lo 95",
         "Low80" = "Lo 80",
         "High95" = "Hi 95",
         "High80" = "Hi 80",
         "Forecast" = "Point Forecast")

# assign the last non-NA row of Data column to the corresponding row of other columns
lastNonNAinData <- max(which(complete.cases(df1$Data)))
df1[lastNonNAinData, !(colnames(df1) %in% c("Data", "Fitted", "Date"))] <- df1$Data[lastNonNAinData]

plt1 <- ggplot(df1, aes(x = Date)) +   
  ggtitle("Ticket amount") +
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

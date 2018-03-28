# Cargar libreria de forecasting
library(forecast)

# Cargar dataset
bwi <- read.csv(file="C:/Users/nsoria/Downloads/AMS Globales/TEC_BWI.csv", header=TRUE, sep=';', dec=",")

# Create time series 
ts_bwi <- ts(bwi$BWI, frequency = 12, start = c(2015,1))

# pull out the seasonal, trend, and irregular components from the time series 
# (train the forecast model)
decom <- stl(ts_bwi, s.window = "periodic")
modelo <- auto.arima(ts_bwi)

# Para probar, se borran los ultimos 5 meses de SLA y se la predice.
train_bwi <- bwi[-c(30, 31, 32, 33, 34, 35),]

# predict the next 30 days of SLA
pred <- forecast(decom, h = 5)
pronostico <- forecast(modelo, h=5)
plot(pronostico)
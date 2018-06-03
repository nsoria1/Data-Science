# Load required libraries
library(ggfortify)
library(tidyverse)
library(RODBC)

# Connect to hadoop database
db <- odbcConnect(dsn="J&J EMEA", believeNRows=FALSE )

# Retrieve total sales query
totalSales <- sqlQuery(db, "select
	YEAR(jj_total.order_date) as year,
                       MONTH(jj_total.order_date) as month,
                       sum(jj_total.item_price_trx_cur) as total_sales
                       from
                       jj_total
                       group by
                       YEAR(jj_total.order_date),
                       MONTH(jj_total.order_date)
                       order BY
                       YEAR(jj_total.order_date) desc,
                       MONTH(jj_total.order_date) desc;")

# Close database connection
db = odbcClose(db)

# Merge columns
#totalSales$year_month <- paste(totalSales$month, totalSales$year, sep = "-")

# Write a CSV
#write.csv(totalSales, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/totalSales.csv")

# Read dummy data
dummyData <- read.csv(file = "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/dummyData.csv", header=TRUE, sep=';', dec=",")

# Merge real data with dummy data
totalData <- rbind(totalSales, dummyData)

# Create time series object
ts_sales <- ts(totalData$total_sales, frequency = 12, start = c(2016,1))

# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_sales, s.window = "periodic")

# Predict the next 5 months of sales
pred <- forecast(model_stl, h = 5)

# Prepare data frame for CSV download
pred_modif <- fortify(pred) %>% as_tibble()

# Write CSV for Superset
write.csv(pred_modif, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/totalForecast.csv")
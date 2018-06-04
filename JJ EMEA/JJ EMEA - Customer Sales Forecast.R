# Load required libraries
library(ggfortify)
library(tidyverse)
library(RODBC)

# Connect to hadoop database
db <- odbcConnect(dsn="J&J EMEA", believeNRows=FALSE )

# Retrieve total sales query
customerSales1 <- sqlQuery(db, "select
	jj_total.customer_id,
                           YEAR(jj_total.order_date) as year,
                           MONTH(jj_total.order_date) as month,
                           sum( jj_total.item_price_trx_cur ) total_sales
                           from
                           jj_total
                           WHERE
                           jj_total.customer_id = '8141271'
                           group by
                           YEAR(jj_total.order_date),
                           MONTH(jj_total.order_date),
                           jj_total.customer_id
                           order BY
                           jj_total.customer_id desc,
                           YEAR(jj_total.order_date) desc,
                           MONTH(jj_total.order_date) desc;")

customerSales2 <- sqlQuery(db, "select
	jj_total.customer_id,
                           YEAR(jj_total.order_date) as year,
                           MONTH(jj_total.order_date) as month,
                           sum( jj_total.item_price_trx_cur ) total_sales
                           from
                           jj_total
                           WHERE
                           jj_total.customer_id = '5710327'
                           group by
                           YEAR(jj_total.order_date),
                           MONTH(jj_total.order_date),
                           jj_total.customer_id
                           order BY
                           jj_total.customer_id desc,
                           YEAR(jj_total.order_date) desc,
                           MONTH(jj_total.order_date) desc;")

customerSales3 <- sqlQuery(db, "select
	jj_total.customer_id,
                           YEAR(jj_total.order_date) as year,
                           MONTH(jj_total.order_date) as month,
                           sum( jj_total.item_price_trx_cur ) total_sales
                           from
                           jj_total
                           WHERE
                           jj_total.customer_id = '5448040'
                           group by
                           YEAR(jj_total.order_date),
                           MONTH(jj_total.order_date),
                           jj_total.customer_id
                           order BY
                           jj_total.customer_id desc,
                           YEAR(jj_total.order_date) desc,
                           MONTH(jj_total.order_date) desc;")

customerSales4 <- sqlQuery(db, "select
	jj_total.customer_id,
                           YEAR(jj_total.order_date) as year,
                           MONTH(jj_total.order_date) as month,
                           sum( jj_total.item_price_trx_cur ) total_sales
                           from
                           jj_total
                           WHERE
                           jj_total.customer_id = '5447565'
                           group by
                           YEAR(jj_total.order_date),
                           MONTH(jj_total.order_date),
                           jj_total.customer_id
                           order BY
                           jj_total.customer_id desc,
                           YEAR(jj_total.order_date) desc,
                           MONTH(jj_total.order_date) desc;")

customerSales5 <- sqlQuery(db, "select
	jj_total.customer_id,
                           YEAR(jj_total.order_date) as year,
                           MONTH(jj_total.order_date) as month,
                           sum( jj_total.item_price_trx_cur ) total_sales
                           from
                           jj_total
                           WHERE
                           jj_total.customer_id = '5446829'
                           group by
                           YEAR(jj_total.order_date),
                           MONTH(jj_total.order_date),
                           jj_total.customer_id
                           order BY
                           jj_total.customer_id desc,
                           YEAR(jj_total.order_date) desc,
                           MONTH(jj_total.order_date) desc;")

# Close database connection
db = odbcClose(db)

# Merge columns
#totalSales$year_month <- paste(totalSales$month, totalSales$year, sep = "-")

# Write a CSV
#write.csv(totalSales, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/totalSales.csv")

# Read dummy data
dummyData <- read.csv(file = "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/customerDummyData.csv", header=TRUE, sep=';', dec=",")

################ Customer #1 ################
# Add item to the dataframe
dummyData$customer_id <- customerSales1[1, 1]
dummyData <- dummyData[, c(4,1,2,3)]

# Merge real data with dummy data
totalData <- rbind(customerSales1, dummyData)

# Create time series object
ts_sales <- ts(totalData$total_sales, frequency = 12, start = c(2016,1))

# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_sales, s.window = "periodic")

# Predict the next 5 months of sales
pred <- forecast(model_stl, h = 5)

# Prepare data frame for CSV download
pred_modif <- fortify(pred) %>% as_tibble()
pred_modif$customer_id <- customerSales1[1, 1]
pred_modif <- pred_modif[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]

# Write CSV for Superset
write.csv(pred_modif, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/customer1Forecast.csv")

################ Customer #2 ################
# Add item to the dataframe
dummyData$customer_id <- customerSales2[1, 1]
dummyData <- dummyData[, c(4,1,2,3)]

# Merge real data with dummy data
totalData <- rbind(customerSales2, dummyData)

# Create time series object
ts_sales <- ts(totalData$total_sales, frequency = 12, start = c(2016,1))

# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_sales, s.window = "periodic")

# Predict the next 5 months of sales
pred <- forecast(model_stl, h = 5)

# Prepare data frame for CSV download
pred_modif <- fortify(pred) %>% as_tibble()
pred_modif$customer_id <- customerSales2[1, 1]
pred_modif <- pred_modif[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]

# Write CSV for Superset
write.csv(pred_modif, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/customer2Forecast.csv")

################ Customer #3 ################
# Add item to the dataframe
dummyData$customer_id <- customerSales3[1, 1]
dummyData <- dummyData[, c(4,1,2,3)]

# Merge real data with dummy data
totalData <- rbind(customerSales3, dummyData)

# Create time series object
ts_sales <- ts(totalData$total_sales, frequency = 12, start = c(2016,1))

# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_sales, s.window = "periodic")

# Predict the next 5 months of sales
pred <- forecast(model_stl, h = 5)

# Prepare data frame for CSV download
pred_modif <- fortify(pred) %>% as_tibble()
pred_modif$customer_id <- customerSales3[1, 1]
pred_modif <- pred_modif[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]

# Write CSV for Superset
write.csv(pred_modif, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/customer3Forecast.csv")

################ Customer #4 ################
# Add item to the dataframe
dummyData$customer_id <- customerSales4[1, 1]
dummyData <- dummyData[, c(4,1,2,3)]

# Merge real data with dummy data
totalData <- rbind(customerSales4, dummyData)

# Create time series object
ts_sales <- ts(totalData$total_sales, frequency = 12, start = c(2016,1))

# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_sales, s.window = "periodic")

# Predict the next 5 months of sales
pred <- forecast(model_stl, h = 5)

# Prepare data frame for CSV download
pred_modif <- fortify(pred) %>% as_tibble()
pred_modif$customer_id <- customerSales1[1, 1]
pred_modif <- pred_modif[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]

# Write CSV for Superset
write.csv(pred_modif, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/customer4Forecast.csv")

################ Customer #5 ################
# Add item to the dataframe
dummyData$customer_id <- customerSales5[1, 1]
dummyData <- dummyData[, c(4,1,2,3)]

# Merge real data with dummy data
totalData <- rbind(customerSales5, dummyData)

# Create time series object
ts_sales <- ts(totalData$total_sales, frequency = 12, start = c(2016,1))

# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_sales, s.window = "periodic")

# Predict the next 5 months of sales
pred <- forecast(model_stl, h = 5)

# Prepare data frame for CSV download
pred_modif <- fortify(pred) %>% as_tibble()
pred_modif$customer_id <- customerSales1[1, 1]
pred_modif <- pred_modif[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]

# Write CSV for Superset
write.csv(pred_modif, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/customer5Forecast.csv")
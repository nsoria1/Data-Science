# Load required libraries
library(ggfortify)
library(tidyverse)
library(RODBC)

# Connect to hadoop database
db <- odbcConnect(dsn="J&J EMEA", believeNRows=FALSE )

# Retrieve total sales query
productSales1 <- sqlQuery(db, "select
	jj_total.product_id,
                          YEAR(jj_total.order_date) as year,
                          MONTH(jj_total.order_date) as month,
                          sum(jj_total.item_price_trx_cur) as total_sales
                          from
                          jj_total
                          
                          WHERE
                          jj_total.product_id IN (
                          'W525                     '
                          )
                          group by
                          YEAR(jj_total.order_date),
                          MONTH(jj_total.order_date),
                          jj_total.product_id
                          order BY
                          jj_total.product_id desc,
                          YEAR(jj_total.order_date) desc,
                          MONTH(jj_total.order_date) desc;")

productSales2 <- sqlQuery(db, "select
	jj_total.product_id,
                          YEAR(jj_total.order_date) as year,
                          MONTH(jj_total.order_date) as month,
                          sum(jj_total.item_price_trx_cur) as total_sales
                          from
                          jj_total
                          
                          WHERE
                          jj_total.product_id IN (
                          'W9335                    '
                          )
                          group by
                          YEAR(jj_total.order_date),
                          MONTH(jj_total.order_date),
                          jj_total.product_id
                          order BY
                          jj_total.product_id desc,
                          YEAR(jj_total.order_date) desc,
                          MONTH(jj_total.order_date) desc;")

productSales3 <- sqlQuery(db, "select
	jj_total.product_id,
                          YEAR(jj_total.order_date) as year,
                          MONTH(jj_total.order_date) as month,
                          sum(jj_total.item_price_trx_cur) as total_sales
                          from
                          jj_total
                          
                          WHERE
                          jj_total.product_id IN (
                          'W8522                    '
                          )
                          group by
                          YEAR(jj_total.order_date),
                          MONTH(jj_total.order_date),
                          jj_total.product_id
                          order BY
                          jj_total.product_id desc,
                          YEAR(jj_total.order_date) desc,
                          MONTH(jj_total.order_date) desc;")

productSales4 <- sqlQuery(db, "select
	jj_total.product_id,
                          YEAR(jj_total.order_date) as year,
                          MONTH(jj_total.order_date) as month,
                          sum(jj_total.item_price_trx_cur) as total_sales
                          from
                          jj_total
                          
                          WHERE
                          jj_total.product_id IN (
                          'V489H                    '
                          )
                          group by
                          YEAR(jj_total.order_date),
                          MONTH(jj_total.order_date),
                          jj_total.product_id
                          order BY
                          jj_total.product_id desc,
                          YEAR(jj_total.order_date) desc,
                          MONTH(jj_total.order_date) desc;")

productSales5 <- sqlQuery(db, "select
	jj_total.product_id,
                          YEAR(jj_total.order_date) as year,
                          MONTH(jj_total.order_date) as month,
                          sum(jj_total.item_price_trx_cur) as total_sales
                          from
                          jj_total
                          
                          WHERE
                          jj_total.product_id IN (
                          'V4930H                   '
                          )
                          group by
                          YEAR(jj_total.order_date),
                          MONTH(jj_total.order_date),
                          jj_total.product_id
                          order BY
                          jj_total.product_id desc,
                          YEAR(jj_total.order_date) desc,
                          MONTH(jj_total.order_date) desc;")

# Close database connection
db = odbcClose(db)

# Merge columns
#totalSales$year_month <- paste(totalSales$month, totalSales$year, sep = "-")

# Write a CSV
#write.csv(totalSales, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/totalSales.csv")

# Read dummy data
dummyData <- read.csv(file = "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/productDummyData.csv", header=TRUE, sep=';', dec=",")

################ Product #1 ################
# Add item to the dataframe
dummyData$product_id <- productSales1[1, 1]
dummyData <- dummyData[, c(4,1,2,3)]

# Merge real data with dummy data
totalData <- rbind(productSales1, dummyData)

# Create time series object
ts_sales <- ts(totalData$total_sales, frequency = 12, start = c(2016,1))

# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_sales, s.window = "periodic")

# Predict the next 5 months of sales
pred <- forecast(model_stl, h = 5)

# Prepare data frame for CSV download
pred_modif <- fortify(pred) %>% as_tibble()
pred_modif$product_id <- productSales1[1, 1]
pred_modif <- pred_modif[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]

# Write CSV for Superset
write.csv(pred_modif, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/product1Forecast.csv")

################ Product #2 ################
# Add item to the dataframe
dummyData$product_id <- productSales2[1, 1]
dummyData <- dummyData[, c(4,1,2,3)]

# Merge real data with dummy data
totalData <- rbind(productSales2, dummyData)

# Create time series object
ts_sales <- ts(totalData$total_sales, frequency = 12, start = c(2016,1))

# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_sales, s.window = "periodic")

# Predict the next 5 months of sales
pred <- forecast(model_stl, h = 5)

# Prepare data frame for CSV download
pred_modif <- fortify(pred) %>% as_tibble()
pred_modif$product_id <- productSales2[1, 1]
pred_modif <- pred_modif[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]

# Write CSV for Superset
write.csv(pred_modif, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/product2Forecast.csv")

################ Product #3 ################
# Add item to the dataframe
dummyData$product_id <- productSales3[1, 1]
dummyData <- dummyData[, c(4,1,2,3)]

# Merge real data with dummy data
totalData <- rbind(productSales3, dummyData)

# Create time series object
ts_sales <- ts(totalData$total_sales, frequency = 12, start = c(2016,1))

# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_sales, s.window = "periodic")

# Predict the next 5 months of sales
pred <- forecast(model_stl, h = 5)

# Prepare data frame for CSV download
pred_modif <- fortify(pred) %>% as_tibble()
pred_modif$product_id <- productSales3[1, 1]
pred_modif <- pred_modif[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]

# Write CSV for Superset
write.csv(pred_modif, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/product3Forecast.csv")

################ Product #4 ################
# Add item to the dataframe
dummyData$product_id <- productSales4[1, 1]
dummyData <- dummyData[, c(4,1,2,3)]

# Merge real data with dummy data
totalData <- rbind(productSales4, dummyData)

# Create time series object
ts_sales <- ts(totalData$total_sales, frequency = 12, start = c(2016,1))

# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_sales, s.window = "periodic")

# Predict the next 5 months of sales
pred <- forecast(model_stl, h = 5)

# Prepare data frame for CSV download
pred_modif <- fortify(pred) %>% as_tibble()
pred_modif$product_id <- productSales4[1, 1]
pred_modif <- pred_modif[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]

# Write CSV for Superset
write.csv(pred_modif, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/product4Forecast.csv")

################ Product #5 ################
# Add item to the dataframe
dummyData$product_id <- productSales5[1, 1]
dummyData <- dummyData[, c(4,1,2,3)]

# Merge real data with dummy data
totalData <- rbind(productSales5, dummyData)

# Create time series object
ts_sales <- ts(totalData$total_sales, frequency = 12, start = c(2016,1))

# Pull out the seasonal, trend, and irregular components from the time series
model_stl <- stl(ts_sales, s.window = "periodic")

# Predict the next 5 months of sales
pred <- forecast(model_stl, h = 5)

# Prepare data frame for CSV download
pred_modif <- fortify(pred) %>% as_tibble()
pred_modif$product_id <- productSales5[1, 1]
pred_modif <- pred_modif[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]

# Write CSV for Superset
write.csv(pred_modif, "C:/Users/nsoria/Documents/Development/Data Science/JJ EMEA/product5Forecast.csv")
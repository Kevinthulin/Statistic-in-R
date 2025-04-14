# Time Series Analysis and Forecasting in R

# 1. Required packages
install.packages(c("forecast", "tseries", "ggplot2", "dplyr"), 
                 repos = "https://cran.rstudio.com/")
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)

# 2. Load Data
# Option 1: Use built-in dataset (AirPassengers)
data("AirPassengers")
myts <- AirPassengers  # Monthly air passenger counts from 1949 to 1960

# Option 2: Generate synthetic data (if you don't have a dataset)
# set.seed(123)
# trend <- seq(from = 100, to = 200, length.out = 120)
# seasonal <- 15 * sin(2 * pi * seq(1:120)/12)
# irregular <- rnorm(120, mean = 0, sd = 10)
# synthetic_ts <- trend + seasonal + irregular
# myts <- ts(synthetic_ts, start = c(2010, 1), frequency = 12)

# Option 3: Import external dataset (uncomment and modify path)
# myts <- read.csv("path_to_your_data.csv")
# myts <- ts(myts$value, start = c(2010, 1), frequency = 12)  # Modify accordingly

# 3. Basic Time Series Exploration
plot(myts, main = "Original Time Series", ylab = "Value")

# 4. Manual First Difference Calculation (as shown in your example)
y <- as.numeric(myts)
n <- length(y)
d <- numeric(n-1)

for (i in 2:n) {
  d[i-1] <- y[i] - y[i-1]
}

# Print the first few differences
cat("Manual First Differences:\n")
print(head(d))

# 5. Using R's diff() function for differencing
diff_ts <- diff(myts)
cat("\nR's diff() function result:\n")
print(head(diff_ts))

# Verify that both methods give the same result
all.equal(d, as.numeric(diff_ts))

# 6. Visualize the differenced series
plot(diff_ts, main = "First Differenced Series", ylab = "Difference")

# 7. Time Series Decomposition
# Decompose into trend, seasonal, and irregular components
decomp <- decompose(myts)
plot(decomp)

# 8. Stationarity Check
adf_test <- adf.test(diff_ts)
print(adf_test)

# 9. ACF and PACF plots
par(mfrow = c(2, 1))
acf(diff_ts, main = "ACF of Differenced Series")
pacf(diff_ts, main = "PACF of Differenced Series")
par(mfrow = c(1, 1))

# 10. ARIMA Modeling
# Auto-select an ARIMA model
arima_model <- auto.arima(myts)
summary(arima_model)

# 11. Forecasting
forecast_periods <- 24  # Forecast next 24 periods
arima_forecast <- forecast(arima_model, h = forecast_periods)
plot(arima_forecast, main = "ARIMA Forecast")

# 12. Forecast Accuracy
# Holding back some data to test accuracy
# train_size <- length(myts) - 12
# train_ts <- window(myts, end = c(time(myts)[train_size]))
# test_ts <- window(myts, start = c(time(myts)[train_size + 1]))
# 
# train_model <- auto.arima(train_ts)
# train_forecast <- forecast(train_model, h = length(test_ts))
# 
# accuracy(train_forecast, test_ts)

# 13. Seasonal Decomposition using STL
stl_decomp <- stl(myts, s.window = "periodic")
plot(stl_decomp)

# 14. Export results
# write.csv(as.data.frame(arima_forecast), "forecast_results.csv")
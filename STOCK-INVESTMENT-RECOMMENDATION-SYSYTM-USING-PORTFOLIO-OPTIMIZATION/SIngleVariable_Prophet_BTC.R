library(prophet)
library(quantmod)
library(xts)
# Load data
#data <- read.csv("/Users/omkacham/Desktop/CSUEB/capstone/DATASET1/ML/apple1.csv")
getSymbols("BTC-USD", src = "yahoo")
# Format data for prophet
data_prophet <- data.frame(ds = index(`BTC-USD`), y = `BTC-USD`$`BTC-USD.Close` )
names(data_prophet)[names(data_prophet) == "BTC.USD.Close"] <- "y"

# Create and fit prophet model
model <- prophet(yearly.seasonality = TRUE,
                 weekly.seasonality = TRUE,
                 daily.seasonality = TRUE,
                 holidays = NULL, # Remove holidays
                 seasonality.mode = "additive", # Set seasonality mode to additive
                 changepoint.prior.scale = 0.0001)

model_fit <- fit.prophet(model, data_prophet)


#Final Forecasting for 30 days 
future <- make_future_dataframe(model_fit, periods = 30)


forecast <- predict(model_fit, future)

tail(forecast[, c("ds", "yhat")], 30)



######################Visulizing Performance of the model#######################

# Visualize the forecast
plot(model_fit, forecast)+
  xlab("Years") +
  ylab("Close Price") +
  ggtitle("S_V BTC before Cross validation and Evaluation") 

# Visualize components of the forecast
prophet_plot_components(model_fit, forecast)

############################  Evaluation.  #####################################

# Calculate RMSE for each variable
rmse_y <- sqrt(mean((data_prophet$y - forecast$yhat[1:nrow(data_prophet)])^2))

# Calculate MAPE for each variable
mape_y <- 100 * mean(abs(data_prophet$y - 
                           forecast$yhat[1:nrow(data_prophet)]) / data_prophet$y)

# Calculate MAE for each variable
n <- nrow(data_prophet)
mae_y <- (rmse_y * sqrt(n) * 100) / (sqrt(n) + mape_y)

# Print RMSE and MAPE values
cat("MAPE for y:", mape_y, "\n")
cat("RMSE for y:", rmse_y, "\n")
cat("MAE for y:", mae_y, "\n")


test_1 <- function(x, y) 
{
  m <- prophet(yearly.seasonality = TRUE,
               weekly.seasonality = TRUE,
               daily.seasonality = TRUE,
               holidays = NULL, # Remove holidays
               changepoint.prior.scale = x,
               seasonality.prior.scale = y)
  m.fit <- fit.prophet(m, data_prophet)
  df_cv <- cross_validation(m.fit, initial = 2200, period = 994, horizon = 30, units = 'days')
  df_p <- performance_metrics(df_cv, rolling_window = 1)
  df_p
}



# Initialize an empty data frame to store the results
results_df <- data.frame(changepoint.prior.scale = numeric(), seasonality.prior.scale = numeric(), col3 = numeric(), col4 = numeric(), col5 = numeric())

x <- c(0.001, 0.01, 0.1, 0.5)
y <- c(0.01, 0.1, 1.0, 10.0)

for (i in x) {
  for (j in y) {
    result <- test_1(i, j)
    # Select columns 3, 4, and 5 of the result
    selected_cols <- select(result, 3:5)
    # Add a new row to the results data frame
    new_row <- data.frame(changepoint.prior.scale = i, seasonality.prior.scale = j, col3 = selected_cols[1], col4 = selected_cols[2], col5 = selected_cols[3])
    results_df <- rbind(results_df, new_row)
  }
}

# Print the results data frame
print(results_df)

#Final Forecasting for 30 days 
m <- prophet(yearly.seasonality = TRUE,
             weekly.seasonality = TRUE,
             daily.seasonality = TRUE,
             holidays = NULL, # Remove holidays
             changepoint.prior.scale = 0.001,
             seasonality.prior.scale = 1.0)
m.fit <- fit.prophet(m, data_prophet)
future <- make_future_dataframe(m.fit, periods = 30)
forecast <- predict(m.fit, future)
tail(forecast[, c("ds", "yhat")], 30)


plot(m.fit, forecast)+
  xlab("Years") +
  ylab("Close Price") +
  ggtitle("S_V BTC after Cross validation and Evaluation") 

#changepoint.prior.scale=0.001                    
#seasonality.prior.scale = 1.00 
#rmse = 6857.049 
#mae = 6771.270 
#mape = 0.2373466

"""
> tail(forecast[, c("ds", "yhat")], 30)
ds     yhat
3145 2023-04-27 35948.00
3146 2023-04-28 35968.30
3147 2023-04-29 35988.02
3148 2023-04-30 35976.69
3149 2023-05-01 35973.37
3150 2023-05-02 35930.40
3151 2023-05-03 35936.36
3152 2023-05-04 35845.48
3153 2023-05-05 35789.09
3154 2023-05-06 35726.76
3155 2023-05-07 35629.67
3156 2023-05-08 35538.59
3157 2023-05-09 35407.68
3158 2023-05-10 35327.34
3159 2023-05-11 35153.57
3160 2023-05-12 35019.42
3161 2023-05-13 34886.05
3162 2023-05-14 34726.07
3163 2023-05-15 34581.50
3164 2023-05-16 34407.50
3165 2023-05-17 34295.24
3166 2023-05-18 34101.23
3167 2023-05-19 33958.74
3168 2023-05-20 33828.88
3169 2023-05-21 33683.93
3170 2023-05-22 33565.34
3171 2023-05-23 33427.45
3172 2023-05-24 33360.37
3173 2023-05-25 33219.39
3174 2023-05-26 33136.40
"""
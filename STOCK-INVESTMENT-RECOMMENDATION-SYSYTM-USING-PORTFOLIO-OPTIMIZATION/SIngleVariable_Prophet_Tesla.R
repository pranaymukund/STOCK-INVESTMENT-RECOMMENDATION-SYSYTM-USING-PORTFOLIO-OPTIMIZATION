library(prophet)
library(quantmod)
library(xts)
# Load data
#data <- read.csv("/Users/omkacham/Desktop/CSUEB/capstone/DATASET1/ML/apple1.csv")
getSymbols("TSLA", src = "yahoo")
# Format data for prophet
data_prophet <- data.frame(ds = index(TSLA), y = TSLA$TSLA.Close )
names(data_prophet)[names(data_prophet) == "TSLA.Close"] <- "y"

# Create and fit prophet model
model <- prophet(yearly.seasonality = TRUE,
                 weekly.seasonality = TRUE,
                 daily.seasonality = TRUE,
                 holidays = NULL, # Remove holidays
                 seasonality.mode = "additive", # Set seasonality mode to additive
                 changepoint.prior.scale = 0.05)

model_fit <- fit.prophet(model, data_prophet)


#Final Forecasting for 30 days 
future <- make_future_dataframe(model_fit, periods = 30)


forecast <- predict(model_fit, future)

tail(forecast[, c("ds", "yhat")], 30)



######################Visulizing Performance of the model#######################

# Visualize the forecast

library(ggplot2)
plot(model_fit, forecast)+
  xlab("Years") +
  ylab("Close Price") +
  ggtitle("S_V Tesla after Cross validation and Evaluation") 

# Visualize components of the forecast
prophet_plot_components(model_fit, forecast)


###########################Cross validation and Evaluation####################################
test_1 <- function(x, y) 
{
  m <- prophet(yearly.seasonality = TRUE,
               weekly.seasonality = TRUE,
               daily.seasonality = TRUE,
               holidays = NULL, # Remove holidays
               changepoint.prior.scale = x,
               seasonality.prior.scale = y)
  m.fit <- fit.prophet(m, data_prophet)
  df_cv <- cross_validation(m.fit, initial = 2260, period = 968, horizon = 30, units = 'days')
  df_p <- performance_metrics(df_cv, rolling_window = 1)
  df_p
}



# Initialize an empty data frame to store the results
library(dplyr)
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

# Visualize the forecast
plot(m.fit, forecast)+
  xlab("Years") +
  ylab("Close Price") +
  ggtitle("S_V Tesla after Cross validation and Evaluation") 





#changepoint.prior.scale=0.001                    
#seasonality.prior.scale = 1.00 
#rmse = 50.67814 
#mae = 32.31114 
#mape = 0.2753042

"""
> tail(forecast[, c("ds", "yhat")], 30)
ds     yhat
3229 2023-04-26 197.6412
3230 2023-04-27 196.9119
3231 2023-04-28 195.9831
3232 2023-04-29 202.8322
3233 2023-04-30 202.5374
3234 2023-05-01 195.7959
3235 2023-05-02 195.6687
3236 2023-05-03 195.5937
3237 2023-05-04 194.6342
3238 2023-05-05 193.4896
3239 2023-05-06 200.1430
3240 2023-05-07 199.6782
3241 2023-05-08 192.7973
3242 2023-05-09 192.5656
3243 2023-05-10 192.4243
3244 2023-05-11 191.4392
3245 2023-05-12 190.3108
3246 2023-05-13 197.0228
3247 2023-05-14 196.6578
3248 2023-05-15 189.9161
3249 2023-05-16 189.8601
3250 2023-05-17 189.9269
3251 2023-05-18 189.1776
3252 2023-05-19 188.3074
3253 2023-05-20 195.2938
3254 2023-05-21 195.2134
3255 2023-05-22 188.7598
3256 2023-05-23 188.9891
3257 2023-05-24 189.3323
3258 2023-05-25 188.8446
"""

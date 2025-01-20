library(prophet)
library(quantmod)
library(xts)
# Load data
#data <- read.csv("/Users/omkacham/Desktop/CSUEB/capstone/DATASET1/ML/apple1.csv")
getSymbols("AAPL", src = "yahoo")
# Format data for prophet
data_prophet <- data.frame(ds = index(AAPL), y = AAPL$AAPL.Close )
names(data_prophet)[names(data_prophet) == "AAPL.Close"] <- "y"

# Create and fit prophet model
model <- prophet(yearly.seasonality = TRUE,
                 weekly.seasonality = TRUE,
                 daily.seasonality = TRUE,
                 holidays = NULL, # Remove holidays
                 seasonality.mode = "additive", # Set seasonality mode to additive
                 changepoint.prior.scale = 1)

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
  ggtitle("S_V Apple before Cross validation and Evaluation") 

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
  df_cv <- cross_validation(m.fit, initial = 2895, period = 1210, horizon = 30, units = 'days')
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
             changepoint.prior.scale = 0.1,
             seasonality.prior.scale = 0.01)
m.fit <- fit.prophet(m, data_prophet)
future <- make_future_dataframe(m.fit, periods = 30)
forecast <- predict(m.fit, future)
tail(forecast[, c("ds", "yhat")], 30)

plot(m.fit, forecast)+
  xlab("Years") +
  ylab("Close Price") +
  ggtitle("S_V Apple after Cross validation and Evaluation for changepoint.prior.scale= 0.1 and seasonality.prior.scale= 0.01 ") 


#changepoint.prior.scale= 0.001 
#seasonality.prior.scale= 1.0     
#rmse= 110.78812   
#mae= 7.919656        
#mape= 0.1239223

"""
> tail(forecast[, c("ds", "yhat")], 30)
             ds     yhat
4107 2023-04-26 166.8018
4108 2023-04-27 166.7304
4109 2023-04-28 166.5685
4110 2023-04-29 167.2812
4111 2023-04-30 167.2750
4112 2023-05-01 166.7516
4113 2023-05-02 166.7385
4114 2023-05-03 166.7673
4115 2023-05-04 166.6501
4116 2023-05-05 166.4476
4117 2023-05-06 167.1260
4118 2023-05-07 167.0926
4119 2023-05-08 166.5499
4120 2023-05-09 166.5258
4121 2023-05-10 166.5522
4122 2023-05-11 166.4414
4123 2023-05-12 166.2539
4124 2023-05-13 166.9558
4125 2023-05-14 166.9537
4126 2023-05-15 166.4493
4127 2023-05-16 166.4699
4128 2023-05-17 166.5460
4129 2023-05-18 166.4890
4130 2023-05-19 166.3580
4131 2023-05-20 167.1177
4132 2023-05-21 167.1736
4133 2023-05-22 166.7258
4134 2023-05-23 166.8003
4135 2023-05-24 166.9266
4136 2023-05-25 166.9148
"""


#changepoint.prior.scale= 0.100 
#seasonality.prior.scale= 0.01     
#rmse= 11.48291    
#mae= 9.496621       
#mape= 0.1060051


"""
> tail(forecast[, c("ds", "yhat")], 30)
             ds     yhat
4107 2023-04-26 176.1170
4108 2023-04-27 176.0874
4109 2023-04-28 176.0200
4110 2023-04-29 176.2094
4111 2023-04-30 176.2469
4112 2023-05-01 176.3547
4113 2023-05-02 176.3058
4114 2023-05-03 176.3824
4115 2023-05-04 176.3051
4116 2023-05-05 176.1926
4117 2023-05-06 176.3405
4118 2023-05-07 176.3412
4119 2023-05-08 176.4179
4120 2023-05-09 176.3441
4121 2023-05-10 176.4028
4122 2023-05-11 176.3149
4123 2023-05-12 176.1993
4124 2023-05-13 176.3518
4125 2023-05-14 176.3646
4126 2023-05-15 176.4605
4127 2023-05-16 176.4125
4128 2023-05-17 176.5028
4129 2023-05-18 176.4517
4130 2023-05-19 176.3770
4131 2023-05-20 176.5733
4132 2023-05-21 176.6318
4133 2023-05-22 176.7741
4134 2023-05-23 176.7721
4135 2023-05-24 176.9068
4136 2023-05-25 176.8974
"""

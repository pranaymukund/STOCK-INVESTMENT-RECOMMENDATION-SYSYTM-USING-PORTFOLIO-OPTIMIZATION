library(prophet)
library(quantmod)
library(xts)


# Define a vector of stock symbols
symbols <- c("^GSPC","^DJI","^IXIC","^RUT",
             "CL=F","^FTSE","GC=F","SI=F",
             "^N225","JPY=X","GBPUSD=X","EURUSD=X")

# Download stock data for the symbols
getSymbols(symbols, src = "yahoo")


#Renaming Few DataFrame Names
CLF <- `CL=F`
JPY <- `JPY=X`
GBPUSD <- `GBPUSD=X`
EURUSD <- `EURUSD=X`
GCF <- `GC=F`
SIF <- `SI=F`


# Load data
getSymbols("TSLA", src = "yahoo")


# Combine the columns into one table
data <- cbind(TSLA$AAPL.Close, GSPC$GSPC.Close,DJI$DJI.Close,IXIC$IXIC.Close,
              RUT$RUT.Close,CLF$`CL=F.Close`,FTSE$FTSE.Close,
              GCF$`GC=F.Close`,SIF$`SI=F.Close`,N225$N225.Close,
              JPY$`JPY=X.Close`,GBPUSD$`GBPUSD=X.Close`,EURUSD$`EURUSD=X.Close`)


# Drop rows with missing values
df <- na.omit(data)

# Format data for prophet
data_prophet <- data.frame(ds = index(df),
                           y = df$AAPL.Close,
                           y1 = df$GSPC.Close,
                           y2 = df$DJI.Close,
                           y3 = df$IXIC.Close,
                           y4 = df$RUT.Close,
                           y5 = df$CL.F.Close,
                           y6 = df$FTSE.Close,
                           y7 = df$GC.F.Close,
                           y8 = df$SI.F.Close,
                           y9 = df$N225.Close,
                           y10 = df$JPY.X.Close,
                           y11 = df$GBPUSD.X.Close,
                           y12 = df$EURUSD.X.Close
)

names(data_prophet)[names(data_prophet) == "AAPL.Close"] <- "y"
names(data_prophet)[names(data_prophet) == "GSPC.Close"] <- "y1"
names(data_prophet)[names(data_prophet) == "DJI.Close"] <- "y2"
names(data_prophet)[names(data_prophet) == "IXIC.Close"] <- "y3"
names(data_prophet)[names(data_prophet) == "RUT.Close"] <- "y4"
names(data_prophet)[names(data_prophet) == "CL.F.Close"] <- "y5"
names(data_prophet)[names(data_prophet) == "FTSE.Close"] <- "y6"
names(data_prophet)[names(data_prophet) == "GC.F.Close"] <- "y7"
names(data_prophet)[names(data_prophet) == "SI.F.Close"] <- "y8"
names(data_prophet)[names(data_prophet) == "N225.Close"] <- "y9"
names(data_prophet)[names(data_prophet) == "JPY.X.Close"] <- "y10"
names(data_prophet)[names(data_prophet) == "GBPUSD.X.Close"] <- "y11"
names(data_prophet)[names(data_prophet) == "EURUSD.X.Close"] <- "y12"



# Create and fit prophet model
model <- prophet(yearly.seasonality = TRUE,
                 weekly.seasonality = TRUE,
                 daily.seasonality = TRUE)
model <- add_regressor(model,"y1")
model <- add_regressor(model,"y2")
model <- add_regressor(model,"y3")
model <- add_regressor(model,"y4")
model <- add_regressor(model,"y5")
model <- add_regressor(model,"y6")
model <- add_regressor(model,"y7")
model <- add_regressor(model,"y8")
model <- add_regressor(model,"y9")
model <- add_regressor(model,"y10")
model <- add_regressor(model,"y11")
model <- add_regressor(model,"y12")

model_fit <- fit.prophet(model, data_prophet)


future <- make_future_dataframe(model_fit, periods = 30)
future$y1 <- 0
future$y2 <- 0
future$y3 <- 0
future$y4 <- 0
future$y5 <- 0
future$y6 <- 0
future$y7 <- 0
future$y8 <- 0
future$y9 <- 0
future$y10 <- 0
future$y11 <- 0
future$y12 <- 0

forecast <- predict(model_fit, future)
tail(forecast[, c("ds", "yhat")], 31)


######################Visulizing Performance of the model#######################

# Visualize the forecast
plot(model_fit, forecast)+
  xlab("Years") +
  ylab("Close Price") +
  ggtitle("M_V Apple before Cross validation and Evaluation") 

# Visualize components of the forecast
prophet_plot_components(model_fit, forecast)



############################  Evaluation.  #####################################

# Calculate RMSE for each variable
rmse_y <- sqrt(mean((data_prophet$y - forecast$yhat[1:nrow(data_prophet)])^2))

# Calculate MAPE for each variable
mape_y <- 100 * mean(abs(data_prophet$y - forecast$yhat[1:nrow(data_prophet)]) / data_prophet$y)

# Calculate MAE for each variable
n <- nrow(data_prophet)
mae_y <- (rmse_y * sqrt(n) * 100) / (sqrt(n) + mape_y)

# Print RMSE and MAPE values
cat("MAPE for y:", mape_y, "\n")
cat("RMSE for y:", rmse_y, "\n")
cat("MAE for y:", mae_y, "\n")


###########################Cross validation and Evaluation####################################

test_1 <- function(x, y) 
{
  m <- prophet(yearly.seasonality = TRUE,
               weekly.seasonality = TRUE,
               daily.seasonality = TRUE,
               holidays = NULL, # Remove holidays
               changepoint.prior.scale = x,
               seasonality.prior.scale = y)
  model <- add_regressor(model,"y1")
  model <- add_regressor(model,"y2")
  model <- add_regressor(model,"y3")
  model <- add_regressor(model,"y4")
  model <- add_regressor(model,"y5")
  model <- add_regressor(model,"y6")
  model <- add_regressor(model,"y7")
  model <- add_regressor(model,"y8")
  model <- add_regressor(model,"y9")
  model <- add_regressor(model,"y10")
  model <- add_regressor(model,"y11")
  model <- add_regressor(model,"y12")
  m.fit <- fit.prophet(m, data_prophet)
  future <- make_future_dataframe(model_fit, periods = 30)
  future$y1 <- 0
  future$y2 <- 0
  future$y3 <- 0
  future$y4 <- 0
  future$y5 <- 0
  future$y6 <- 0
  future$y7 <- 0
  future$y8 <- 0
  future$y9 <- 0
  future$y10 <- 0
  future$y11 <- 0
  future$y12 <- 0
  df_cv <- cross_validation(m.fit, initial = 2312, period = 991, horizon = 30, units = 'days')
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
             changepoint.prior.scale = 0.500,
             seasonality.prior.scale = 0.01)
m.fit <- fit.prophet(m, data_prophet)
future <- make_future_dataframe(m.fit, periods = 30)
forecast <- predict(m.fit, future)
tail(forecast[, c("ds", "yhat")], 30)

# Visualize the forecast
plot(model_fit, forecast)+
  xlab("Years") +
  ylab("Close Price") +
  ggtitle("M_V Apple after Cross validation and Evaluation") 

#changepoint.prior.scale=0.5                   
#seasonality.prior.scale = 0.01 
#rmse = 14.94642
#mae =  11.42153
#mape = 0.1455021


"""
             ds     yhat
3304 2023-04-25 173.9981
3305 2023-04-26 174.0705
3306 2023-04-27 173.9613
3307 2023-04-28 173.6519
3308 2023-04-29 174.0289
3309 2023-04-30 174.0308
3310 2023-05-01 174.0995
3311 2023-05-02 174.2002
3312 2023-05-03 174.2365
3313 2023-05-04 174.0931
3314 2023-05-05 173.7523
3315 2023-05-06 174.1015
3316 2023-05-07 174.0798
3317 2023-05-08 174.1300
3318 2023-05-09 174.2177
3319 2023-05-10 174.2471
3320 2023-05-11 174.1032
3321 2023-05-12 173.7683
3322 2023-05-13 174.1300
3323 2023-05-14 174.1272
3324 2023-05-15 174.2024
3325 2023-05-16 174.3206
3326 2023-05-17 174.3855
3327 2023-05-18 174.2812
3328 2023-05-19 173.9893
3329 2023-05-20 174.3962
3330 2023-05-21 174.4400
3331 2023-05-22 174.5619
3332 2023-05-23 174.7260
3333 2023-05-24 174.8350
"""

#Converting to Excel
# Load the openxlsx package
library(openxlsx)
Excel_Apple <- tail(forecast[, c("ds", "yhat")], 30)
# Write the data frame to an Excel file
write.xlsx(Excel_Apple, "Apple_Prophet.xlsx", sheetName = "Sheet1")


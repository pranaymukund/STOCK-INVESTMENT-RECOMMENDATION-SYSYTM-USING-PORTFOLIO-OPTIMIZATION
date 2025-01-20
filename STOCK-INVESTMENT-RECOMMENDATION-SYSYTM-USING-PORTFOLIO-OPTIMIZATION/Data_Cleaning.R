library(quantmod)


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
getSymbols("AAPL", src = "yahoo")

# Combine the columns into one table
data <- cbind(AAPL$AAPL.Close, GSPC$GSPC.Close,DJI$DJI.Close,IXIC$IXIC.Close,
              RUT$RUT.Close,CLF$`CL=F.Close`,FTSE$FTSE.Close,
              GCF$`GC=F.Close`,SIF$`SI=F.Close`,N225$N225.Close,
              JPY$`JPY=X.Close`,GBPUSD$`GBPUSD=X.Close`,EURUSD$`EURUSD=X.Close`)

################################Data Cleaning###################################
#check for na values
is.na(data)
# Drop rows with missing values
df <- na.omit(data)

#############################Descriptive analytics:#############################
# Summay function
summary(df)
# Create line plot with different colors and legends
library(ggplot2)
ggplot(df, aes(x = index(df))) +
  geom_line(aes(y = df$AAPL.Close, color = "Apple"), size = 1) +
  geom_line(aes(y = df$GSPC.Close, color = "GSPC"), size = 1) +
  geom_line(aes(y = df$DJI.Close, color = "DJI"), size = 1) +
  geom_line(aes(y = df$IXIC.Close, color = "IXIC"), size = 1) +
  geom_line(aes(y = df$RUT.Close, color = "RUT"), size = 1) +
  geom_line(aes(y = df$CL.F.Close, color = "CL=F"), size = 1) +
  geom_line(aes(y = df$FTSE.Close, color = "FTSE"), size = 1) +
  geom_line(aes(y = df$GC.F.Close, color = "GC=F"), size = 1) +
  geom_line(aes(y = df$SI.F.Close, color = "SI=F"), size = 1) +
  geom_line(aes(y = df$N225.Close, color = "N225"), size = 1) +
  geom_line(aes(y = df$JPY.X.Close, color = "JPY/USD"), size = 1) +
  geom_line(aes(y = df$GBPUSD.X.Close, color = "GBPUSD"), size = 1) +
  geom_line(aes(y = df$EURUSD.X.Close, color = "EURUSD"), size = 1)+ 
  scale_color_manual(name = "Legend", 
                     values = c("Apple" = "black",
                                "GSPC" = "orange",
                                "DJI" = "yellow",
                                "IXIC" = "green",
                                "RUT" = "blue",
                                "CL=F" = "purple",
                                "FTSE" = "pink",
                                "GC=F" = "brown",
                                "SI=F" = "gray",
                                "N225" = "red",
                                "JPY/USD" = "darkcyan",
                                "GBPUSD" = "magenta",
                                "EURUSD" = "darkred"))+
  
xlab("Year") +  # Add x-axis label
ylab("CLosing Price") +  # Add y-axis label
ggtitle("Comparision of closing prices of all stocks")   # Add plot title 
  

#Corelation map 
library(corrplot)
corrplot(cor(df), method = "color", 
         type = "lower", tl.cex = 1.0, tl.col = "black")

#To check corelation bwtween Apple and NASDAQ Composite closing price
#applying the graph seperatley because apple has a smaller y axis scale 
#compared to IXIC

#For apple
ggplot(df, aes(x = index(df))) +
  geom_line(aes(y = df$AAPL.Close, color = "Apple"), size = 1) +
  scale_color_manual(name = "Legend", 
                     values = c("Apple" = "black"))+
  
xlab("Year") +  # Add x-axis label
ylab("CLosing Price") +  # Add y-axis label
ggtitle("Apple closing price")   # Add plot title 

#For IXIC
ggplot(df, aes(x = index(df))) +
  geom_line(aes(y = df$IXIC.Close, color = "IXIC"), size = 1) +
  scale_color_manual(name = "Legend", 
                     values = c("IXIC" = "green"))+
  
  xlab("Year") +  # Add x-axis label
  ylab("CLosing Price") +  # Add y-axis label
  ggtitle("NASDAQ Composite (IXIC) CLosing Price")   # Add plot title 




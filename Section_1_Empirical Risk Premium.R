rm(list = ls()) 

#install.packages("aTSA")
#install.packages("ggplot2")

# Load packages
library(ggplot2)
library(aTSA)
library(Rmisc)


# Provide data destination
destfile = ""  # Data path: Caution backslahs have to be substituted by slashes
File_Factors = "FF_Data.csv"                                                          # Data file

# Read data
Data_Factors = read.delim(paste(destfile, File_Factors, sep = "", collapse = NULL), header = TRUE, sep = ";", dec = ",")  

# Create proper dates
Data_Factors$Date = as.Date(paste(substr(Data_Factors$X,1,4),"-",substr(Data_Factors$X,5,6),"-01",sep=""))

temp = data.frame(date = as.Date(Data_Factors$Date), 
                  premium = Data_Factors$Mkt.RF,
                  return = Data_Factors$Mkt.RF + Data_Factors$RF)


## EXERCISE 2: Plot return time series and test for stationarity
ggplot(temp, aes(x=date, y=premium))+
      geom_line() +                                       # plot data as line
      theme_bw() +                                        # black line, white backgrounf
      theme(plot.title = element_text(hjust = 0.5))+      # center title
      ggtitle(paste("Marktrisikoprämie von", temp$date[1], "bis", temp$date[ nrow(temp)])) +
      scale_x_date(date_labels = "%Y") +
      xlab("Zeit") +                                      # label x axis
      ylab("Marktrisikoprämie") +                         # label y axis
      theme(plot.title = element_text(size=24),           # font setting
            axis.title.x = element_text(size=16), 
            axis.text.x = element_text(size=14), 
            axis.title.y = element_text(size=16), 
            axis.text.y = element_text(size=14))

# Test market risk premium for stationarity
# ADF test
adf_test_discrete = stationary.test(Data_Factors$Mkt.RF, "adf", nlag = 2)
adf_test_continuous = stationary.test(log(1+Data_Factors$Mkt.RF/100), "adf", nlag = 2)


## EXERCISES 3 and 4: 
#cumpute cumulative product of initial investment and append the time series for the starting value
prices = c(100,100*cumprod(1+temp$return/100))

# Compute month prior to first observation
AddDate = seq(as.Date(Data_Factors$Date[1]), length = 2, by = "-1 months")

# Construct data frame 
temp2 = data.frame(date = as.Date(c(AddDate[2], Data_Factors$Date)),
                   prices = prices,
                   returns = c(0, temp$return))


# Initialize plot list
myplot=list()

# Create plot for market returns
myplot[[1]] = ggplot(temp2, aes(x=date, y=prices))+
                     geom_line() +                                                                        # plot data as line
                     theme_bw() +                                                                         # black line, white backgrounf
                     theme(plot.title = element_text(hjust = 0.5))+                                       # center title
                     ggtitle(paste("Preiszeitreihe von", temp$date[1], "bis", temp$date[ nrow(temp)])) +
                     scale_x_date(date_labels = "%Y") +
                     xlab("Zeit") +                                                                       # label x axis
                     ylab("Indexierter Preis") +                                                          # label y axis
                     theme(plot.title = element_text(size=24),                                            # font setting
                           axis.title.x = element_text(size=16), 
                           axis.text.x = element_text(size=14), 
                           axis.title.y = element_text(size=16), 
                           axis.text.y = element_text(size=14))                   
                      
# Create plot for market prices
myplot[[2]] = ggplot(temp2, aes(x=date, y=returns))+
                     geom_line() +                                                                        # plot data as line
                     theme_bw() +                                                                         # black line, white backgrounf
                     theme(plot.title = element_text(hjust = 0.5))+                                       # center title
                     ggtitle(paste("Renditezeitreihe ", temp$date[1], "bis", temp$date[ nrow(temp)])) +
                     scale_x_date(date_labels = "%Y") +
                     xlab("Zeit") +                                                                       # label x axis
                     ylab("Rendite") +                                                                    # label y axis
                     theme(plot.title = element_text(size=24),                                            # font setting
                           axis.title.x = element_text(size=16), 
                           axis.text.x = element_text(size=14), 
                           axis.title.y = element_text(size=16), 
                           axis.text.y = element_text(size=14))                   

multiplot(plotlist=myplot, cols = 1)

# Test return and prices for stationarity
adf_test_prices = stationary.test(temp2$prices, "adf", nlag = 2)

adf_test_returns_discrete = stationary.test(temp2$returns, "adf", nlag = 2)
adf_test_returns_continuous = stationary.test(log(1+temp2$returns/100), "adf", nlag = 2)
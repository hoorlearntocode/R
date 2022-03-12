rm(list = ls()) 

library(reshape2)
library(latex2exp)
library(ggplot2)
destfile = "C:/Users/User/OneDrive/FH Köln/Unterricht/Finanzmarktökonometrie/Daten/"
File_Assets = "10_industry_portfolios_monthly.csv"
File_Factors = "CRSP_Monthly.csv"


Data_Assets = read.delim(paste(destfile, File_Assets, sep = "", collapse = NULL), header = TRUE, sep = ",", dec = ".")
Data_Factors = read.delim(paste(destfile, File_Factors, sep = "", collapse = NULL), header = TRUE, sep = ",", dec = ".")

# Transform to continuous
X = log(1+(Data_Factors[,2]+Data_Factors[,5])/100)*100

# Preallocate
Estimates = matrix(data=NA, ncol=2, nrow = ncol(Data_Assets)-1)
RSQR = matrix(data=NA, ncol=1, nrow = ncol(Data_Assets)-1)
Loss = matrix(data=NA, ncol=1, nrow = ncol(Data_Assets)-1)


for (Industry_i in 2 : ncol(Data_Assets)){
  
  # Transform to continuous
  Y = log(1+Data_Assets[,Industry_i]/100)*100
  
  # Linear regression
  fit = summary(lm(Y ~ X))
  
  # Save statistics
  Estimates[Industry_i-1,] = fit$coefficients[1:2]
  RSQR[Industry_i-1,] = fit$r.squared
  Loss[Industry_i-1,1] = (exp(fit$coefficients[1:2]%*%rbind(1, log(1-0.05)*100)/100)-1)*100
}
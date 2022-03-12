rm(list = ls()) 

library(reshape2)
library(latex2exp)
library(ggplot2)

destfile = "C:/Users/User/OneDrive/FH Köln/Unterricht/Finanzmarktökonometrie/Daten/"
File_Assets = "10_industry_portfolios_monthly.csv"
File_Factors = "CRSP_Monthly.csv"


Data_Assets = read.delim(paste(destfile, File_Assets, sep = "", collapse = NULL), header = TRUE, sep = ",", dec = ".")
Data_Factors = read.delim(paste(destfile, File_Factors, sep = "", collapse = NULL), header = TRUE, sep = ",", dec = ".")

N = ncol(Data_Assets)-1

# Transform to continuous
X = log(1+Data_Factors[,2]/100)

# Preallocate
Estimates = matrix(data=NA, ncol=2, nrow = N)
TValues = matrix(data=NA, ncol=2, nrow = N)
RSQR = matrix(data=NA, ncol=1, nrow = N)

for (Industry_i in 2 : ncol(Data_Assets)){
  
  # Transform to continuous
  Y = log(1+Data_Assets[,Industry_i]/100)-log(1+Data_Factors[,5]/100)
  
  # Linear regression
  fit = summary(lm(Y ~ X))
  
  # Save statistics
  Estimates[Industry_i-1,] = fit$coefficients[1:2]
  TValues[Industry_i-1,] = fit$coefficients[,3]
  RSQR[Industry_i-1,] = fit$r.squared
}

# Comupute expected returns for industry portfolios
ER_Market = mean(log(1+Data_Factors[,2]/100)+log(1+Data_Factors[,5]/100))                                              

# Compute continuous risk free rate (for 1 months). If expected returns for longer horizons are required, use the corresponding risk free rate for this particular maturity.
rf = log(1+Data_Factors[nrow(Data_Factors),5]/100) 

# Imprecise annualization due to mismatch in risk free (see comment above) 
ER_Industry = exp(12*(rf + Estimates[,2] * (ER_Market-rf)))-1


# Critical Value after Bonferroni correction
t_crit = qt(1-0.05/N, length(X)-2)

# Compare max values
t_max = max(TValues[,1])
t_min = min(TValues[,1])


# Plot Beta_0 against Beta_1
df = data.frame(X=Estimates[,2],Y=Estimates[,1]) 
fit = lm(Y ~ X, data = df)
df$predicted = predict(fit)   # Save the predicted values
df$residuals = residuals(fit) # Save the residual values

p1 = ggplot(df, aes(X, Y))+  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + # Plot regression slope
  geom_point() + 
  theme_bw() +
  xlab(latex2exp("$\\hat{\\beta}_1$")) +
  ylab(latex2exp("$\\hat{\\beta}_0$")) +
  theme(plot.title = element_text(size=24), 
        axis.title.x = element_text(size=16), 
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16), 
        axis.text.y = element_text(size=14))

ggsave("Alpha_Beta.png", width = 10, height = 5)
rm(list = ls())   
#install.packages("Rsolnp")
library(Rsolnp)   
library(aTSA)           
library(lubridate)

#read data
Path = "C:/Users/User/OneDrive/FH Köln/Unterricht/Finanzmarktökonometrie/Daten/"
File = "CRSP_Daily.csv"

Data = read.csv(paste(Path, File, sep = "", collapse = NULL), header=TRUE, sep =",", dec=".")
DailyDates = ymd(parse_date_time(as.numeric(Data$X), "%Y %m %d"))  


### Log-likelihood for GARCH(1,1) estimation
LL_GARCH = function(x0, purpose=1){
  T = length(Data$Mkt.RF)
  
  # Compute returns
  r = log(1+(Data$Mkt.RF + Data$RF)/100)*100
  
  # Translate parameters into Engle's (2001) notation
  omega=x0[1]
  alpha=x0[2]
  beta=x0[3]
  m = x0[4]
  
  # Initialize variance
  h = var(r)                                             # Initial guess of volatility (set to unconditional volatility)
  u = 0
  
  for (t in 1:T){
    
    # Conditional variance
    u[t] = (r[t]-m)
    h[t+1] = omega + alpha*u[t]^2 + beta*h[t]
  }
  
  # log-likelihood function value
  loglik = -0.5*sum((log(2*pi)+log(h[1:T])+u^2/h[1:T]))
  
  if (purpose == 1){
    return(-loglik)}
  else {
    return(h)}
}

### Optimization
# Inequality constraints for log-likelihood optimization
ineq <- function(x){ 
  s = c(0, 1, 1, 0) %*% x}

# Initial guess of optimization parameters
x0=c(0, 0.2, 0.8, 0.2)

# Log-likelihood optimization 
Opti = solnp(x0, fun = LL_GARCH, eqfun = NULL, eqB = NULL, LB = c(0, 0, 0, -Inf), UB = NULL, ineqfun = ineq, ineqLB = 0, ineqUB = 1)


### Compute and plot volatility
Vola_Uncond = sqrt(Opti$pars[1]/(1-Opti$pars[2]-Opti$pars[3]))*sqrt(252)
Vola_Cond = sqrt(LL_GARCH(Opti$pars, purpose=0)*252)

# Plot return and volatility time series
par(mfrow=c(2,1))
plot(DailyDates, log(1+(Data$Mkt.RF + Data$RF)/100)*100, type ="l",main = "Tägliche Rendite CRSP Index", xlab="Datum", ylab="Rendite in %", lty=1)
plot(DailyDates, Vola_Cond[1:length(DailyDates)], type ="l",main = "Annualisierte Volatilität", xlab="Datum", ylab="Volatilität in %", lty=1)
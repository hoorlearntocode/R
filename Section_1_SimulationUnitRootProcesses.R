### Simulation Unit Root Process

rm(list = ls())

# Installing required packages
#install.packages("ggplot2")
#install.packages("Rmisc")
install.packages("aTSA")

# Calling required packages
library(ggplot2)
library(Rmisc)
library(aTSA)            # load package



# set parameters
T = 101
N_delta = 6

# preallocate matrix
P = matrix(data=NA, nrow = T, ncol = N_delta)

# set initial price to zero
P[1,1:N_delta] = 0

# set seed for random number for replicability
set.seed(123456)

# generate random numbers ~ N(0,0.01)
Epsilon = rnorm(T-1)/10


# create list for plots
myplots=list()
adplots=list()

for (delta_i in 1 : N_delta){
  
  # increase the root
  delta = (delta_i-1)/(N_delta-2)
  
  # construct price time series
  for (t in 2:T){
    P[t,delta_i] = delta * P[t-1,delta_i] + Epsilon[t-1]
  }

  # create data frame from current data
  temp = as.data.frame(cbind(cbind(1:T), P[,delta_i]))
  
  # Generate ggplot and store in plotlist
  myplots[[delta_i]] = ggplot(temp, aes(V1, V2))+
                            geom_line() +                                       # plot data as line
                            theme_bw() +                                        # black line, white backgrounf
                            theme(plot.title = element_text(hjust = 0.5))+      # center title
                            ggtitle(bquote(delta == ~ .(round(delta,2)))) +
                            xlab("Beobachtung") +                               # label x axis
                            ylab("P") +                                         # label y axis
                            theme(plot.title = element_text(size=24),           # font setting
                                  axis.title.x = element_text(size=16), 
                                  axis.text.x = element_text(size=14), 
                                  axis.title.y = element_text(size=16), 
                                  axis.text.y = element_text(size=14))
  
  
  # ADF test
  adf_test = stationary.test(P[,delta_i], "adf", nlag = 2)
  
  # Generate ggplot and stor in plotlist
  adplots[[delta_i]] = ggplot(temp, aes(V1, V2))+
                              geom_line() +                                       # plot data as line
                              theme_bw() +                                        # black line, white backgrounf
                              theme(plot.title = element_text(hjust = 0.5))+      # center title
                              ggtitle(bquote(delta == ~ .(round(delta,2)) ~ "/ DF p-value: " ~ .(round(adf_test$type1[1,3],2)))) +
                              xlab("Beobachtung") +                               # label x axis
                              ylab("P") +                                         # label y axis
                              theme(plot.title = element_text(size=24),           # font setting
                                    axis.title.x = element_text(size=16), 
                                    axis.text.x = element_text(size=14), 
                                    axis.title.y = element_text(size=16), 
                                    axis.text.y = element_text(size=14))
}
# plot ggplots
multiplot(plotlist=myplots, cols = 3)


# plot ggplots
multiplot(plotlist=adplots, cols = 3)
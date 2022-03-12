# Simulationsstudie für die Eigenschaften von OLS

#install.packages("reshape2")
#install.packages("latex2exp") 
library(reshape2)
library(latex2exp)
library(ggplot2)


Iteration_N = 1000
Max_T = 1000

Beta_0 = 0
Beta_1 = 1

T = rbind(25, 50, 100, 1000)

# set seed for random number for replicability
set.seed(123456)

# Charakteristika der Variablen
X = rnorm(Max_T)*0.05
U = matrix(rnorm(Iteration_N*Max_T), ncol=Iteration_N)*0.025

# DGP
Y = Beta_0 + Beta_1 * X + U

# Preallocate
b_0 = matrix(data=NA, ncol=length(T), nrow = Iteration_N)
b_1 = matrix(data=NA, ncol=length(T), nrow = Iteration_N)

for (NObs_i in 1 : length(T)){
  for (Iteration_i in 1 : Iteration_N){ 
    temp = lm(Y[1:T[NObs_i],Iteration_i] ~ X[1:T[NObs_i]])
    b_0[Iteration_i, NObs_i] = temp$coefficients[1]
    b_1[Iteration_i, NObs_i] = temp$coefficients[2]
  }
}


# Plot für B_0
Obs_25 = b_0[,1]
Obs_50 = b_0[,2]
Obs_100 = b_0[,3]
Obs_1000 = b_0[,4]

df = data.frame(Obs_25, Obs_50, Obs_100, Obs_1000) 
df.m = melt(df)

p1 = ggplot(df.m) + geom_density(aes(x = value,colour = variable))  +
                    theme_bw() +
                    labs(x = latex2exp("$\\hat{\\beta}_0$")) +
                    labs(y = "Dichte") +
                    theme(plot.title = element_text(size=24), axis.title.x = element_text(size=16),  
                    axis.text.x = element_text(size=14), axis.title.y = element_text(size=16),
                    axis.text.y = element_text(size=14))

ggsave("Simulation_OLS_B0.png", width = 5, height = 5)



# Plot für B_1
Obs_25 = b_1[,1]
Obs_50 = b_1[,2]
Obs_100 = b_1[,3]
Obs_1000 = b_1[,4]

df = data.frame(Obs_25, Obs_50, Obs_100, Obs_1000) 
df.m = melt(df)

p2 = ggplot(df.m) + geom_density(aes(x = value,colour = variable))  +
  theme_bw() +
  labs(x = latex2exp("$\\hat{\\beta}_1$")) +
  labs(y = "Dichte") +
  theme(plot.title = element_text(size=24), axis.title.x = element_text(size=16),  
        axis.text.x = element_text(size=14), axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=14))

ggsave("Simulation_OLS_B1.png", width = 5, height = 5)
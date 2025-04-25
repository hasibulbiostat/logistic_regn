##### define true values #####
intercept = 0.5

slope =1 

##### simulate the data #### 
p = exp(intercept)/(1+exp(intercept))

binomial_data <- rbinom(1000,size=1,prob=p)



log_lik_fun <- function(beta_0,data)
{
  
  n = length(data)
  
  sum(data * (beta_0-log(1+exp(beta_0))) + (1-data) * (-1*log(1+exp(beta_0)))) 
  
  
  
  
  
}

optim(0,log_lik_fun,data=binomial_data,control= list(fnscale=-1))


grid_intercept_values <- seq(0,2,0.01)

log_lik <- sapply(grid_intercept_values,log_lik_fun,data=binomial_data)

plot(grid_intercept_values,log_lik,type="l")






#### model with 1 predictor ####

intercept = 0.5

slope =1 

age <- rpois(10000,5)

p1 = exp(intercept+slope*age)/(1+exp(intercept+slope*age))

binomial_data1 <- rbinom(10000,size=1,prob=p1)



log_lik_fun1 <- function(par,data,predictor)
{
  beta_0 = par[1]
  beta_1 = par[2]
  
  n = length(data)
  
  sum(data * (beta_0+beta_1*predictor-log(1+exp(beta_0+beta_1*predictor))) + (1-data) * (-1*log(1+exp(beta_0+beta_1*predictor)))) 
  
  
  
  
  
}

optim(c(0,0),log_lik_fun1,data=binomial_data1,predictor=age,control= list(fnscale=-1))


#

intercept_vals <- seq(0,2,0.01)

log_lik1 <-sapply(c(slope_vals,intercept_vals),log_lik_fun1,data=binomial_data1,predictor=age)

plot(grid_intercept_values,log_lik1,type="l")
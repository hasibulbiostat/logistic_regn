#### The following code will simulate data & estimate the parameters 
#### using the optim function 


#### Model with only intercept term 

#### define true values of the parameters #####
intercept = 0.5

n <- 1000000

 
#### set seed for reproducibility 
set.seed(55405)
#### generate a covariate 
age <- runif(n,0,100)

#### simulate the data #### 
p = exp(intercept)/(1+exp(intercept))

binomial_data <- rbinom(n,size=1,prob=p)


#### Write the log likelihood function 

log_lik_fun <- function(beta_0,data,age)
{
  
 
  
  
  
  
  
  n = length(data)
  
  sum(data * (beta_0 -log(1+exp(beta_0))) + (1-data) * (-1*log(1+exp(beta_0)))) 
  
  
  
  
  
}

#### Optimize the parameters using the optim function
#### Note fnscale=-1 turns it into a maximization problem 

intercept_estimate <- optim(0,log_lik_fun,age=age,data=binomial_data,control= list(fnscale=-1))$par 

#### Plotting the log likelihood function over a grid of values 

grid_intercept_values <- seq(0,2,0.01)

log_lik <- sapply(grid_intercept_values,log_lik_fun,data=binomial_data)

plot(grid_intercept_values,log_lik,type="l")






#### model with a single predictor ####

#### Define the true values of the parameters 

intercept = 0.5

slope =1 

#### set the seed for reproducibility 

set.seed(55405)

age <- rpois(n,5)

#### Simulate the data 

p1 = exp(intercept+slope*age)/(1+exp(intercept+slope*age))

binomial_data1 <- rbinom(n,size=1,prob=p1)


#### Define the log likelihood function 

log_lik_fun1 <- function(par,data,predictor)
{
  beta_0 = par[1]
  beta_1 = par[2]
  
  n = length(data)
  
  sum(data * (beta_0+beta_1*predictor-log(1+exp(beta_0+beta_1*predictor))) + (1-data) * (-1*log(1+exp(beta_0+beta_1*predictor)))) 
  
  
  
  
  
}

#### Estimate the parameters using the optim function 

par_est <- optim(c(0,0),log_lik_fun1,data=binomial_data1,predictor=age,control= list(fnscale=-1))$par


#

#intercept_vals <- seq(0,2,0.01)

#slope_vals <- seq(0,2,0.01)

#log_lik1 <-sapply(c(slope_vals,intercept_vals),log_lik_fun1,data=binomial_data1,predictor=age)

#plot(intercept_vals,log_lik1,type="l")
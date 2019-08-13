library(ISLR)
library(boot)

alpha <- function(x, y){
  vx <- var(x)
  vy <- var(y)
  cxy <- cov(x, y)
  (vy -cxy) / (vx + vy - 2*cxy)
} 

# check the function
alpha(Portfolio$X, Portfolio$Y)

# bootstrap statistic;
# a function which when applied to the data returns a vector 
# containing statistic of interest
alpha_fn <- function(data, index){
  with(data[index, ], alpha(X, Y))
}

set.seed(1)
boot_out <- boot(Portfolio, alpha_fn, 1000)


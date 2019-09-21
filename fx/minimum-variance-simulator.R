### Mean-Variance Portfolio Simulator ###

mvo.sim <- function(raw.returns)
  # simulates a portfolio of n random returns series, weighted randomly (but sum(weights)==1)
  #   
  #   Args:
  #     raw.returns: data.frame of column-wise raw component returns
  #   
  #   Returns:
  #     List of:
  #       portfolio.mean: mean of the portfolio arising from random weights
  #       portfolio.vol: volatility of the portfolio arising from random weights
  
{
  # get number of components
  n = length(raw.returns)
  
  # generate weights
  weights <- runif(n, -10, 10)
  weights <- weights/(sum(weights)) #normalize such sum(weights)==1
  
  # simulate portfolio
  portfolio.returns <- t(t(component.returns)*weights) #returns of individual components
  sum.portfolio.returns <- rowSums(portfolio.returns)  # total portfolio returns
  
  portfolio.mean <- mean(sum.portfolio.returns)
  portfolio.vol <- sd(sum.portfolio.returns)
  
  return(list(portfolio.mean, portfolio.vol))
  
}

#define number of portfolio components
n <- 30

# generate returns series
component.returns <- list()
for(i in c(1:n))
{
  component.returns[[i]] <- rnorm(1000, mean = 0.001, sd = 0.05)
}

# combine column-wise into data frame
component.returns <- do.call(cbind.data.frame, component.returns)
colnames(component.returns) <- as.character(c(1:n))

# define vectors for holding portfolio means and volatilities
means <- vector()
vols <- vector()
j <- 1

# run simulator many times on our returns data frame
for(i in c(1:100000))
{
  mv <- mvo.sim(component.returns)
  if(mv[[2]] < 0.05) # discard high vol portfolios
  {
    means[j] <- mv[[1]]
    vols[j] <- mv[[2]]
    j <- j+1
  }
  
}

# plot results
plot(vols, means, 
     col='blue', pch=5,
     xlab='Volatility', ylab='Mean Return',
     main='Mean-Variance of Randomly Constructed Portfolios')
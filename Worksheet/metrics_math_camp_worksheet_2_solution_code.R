#================================================================
# PROGRAM: metrics_math_camp_simulation_solution.R
# PURPOSE: MD's solutions for simulation exercise, 
#          Harvard econometrics math camp 2020
#================================================================

#================================================================
# PROBLEM 3
#================================================================

# Write a function that returns the mean of a random var
# that is the ratio of two standard normal vars with N obs
get_mean_of_norm<- function(N) {
  x1   <- rnorm(N)
  mean <- mean(x1)
  return(mean)
}

# Write a function that returns the mean of a random var
# that is the ratio of two indep standard normal vars with N obs
get_mean_of_norm_ratio <- function(N) {
  x1   <- rnorm(N)
  x2   <- rnorm(N)
  y    <- x1 / x2
  mean <- mean(y)
  return(mean)
}

# Mean of a normal variable as N increases
get_mean_of_norm(100)
get_mean_of_norm(1000)
get_mean_of_norm(10000)
get_mean_of_norm(100000)
get_mean_of_norm(1000000)

# Mean of the ratio of two indep std normal variables as N increases
get_mean_of_norm_ratio(100)
get_mean_of_norm_ratio(1000)
get_mean_of_norm_ratio(10000)
get_mean_of_norm_ratio(100000)
get_mean_of_norm_ratio(1000000)

#================================================================
# PROBLEM 4
#================================================================

#----------------------------------------------------------------
# METHOD 1. MANUAL
# COMMENT: This is how I first wrote the code before turning it
#          into a function of num_sims and N
#----------------------------------------------------------------

# Number of simulations
num_sims <- 10000

# Number of observations within each simulation
N        <- 1000

# Create matrix to store estimates - two params, num_sims draws
betas <- matrix(0,num_sims,2)

# Run simulations from i=1, 2, ..., num_sims
for (i in 1:num_sims) {
  
  # Generate data
  x1  <- rep(1,N)
  x2  <- rnorm(N)
  X   <- cbind(x1,x2)
  eps <- rnorm(N)
  Y   <- 1*x1 + 2*x2 + eps
  
  # Define X'X and (X'X)^{-1}
  XX     <- t(X) %*% X
  XX_inv <- solve(XX)
  
  # Compute OLS solution
  beta_hat <- XX_inv %*% t(X) %*% Y
  
  # Store OLS solution
  betas[i,] <- t(beta_hat)
  
}

# Histogram of results
hist(betas[,1])
hist(betas[,2])

#----------------------------------------------------------------
# METHOD 2. AS FUNCTION
# COMMENT: This was easy to construct after writing the above
#----------------------------------------------------------------

#------------------------------------------------
# Define function for running sims
#------------------------------------------------

run_sims <- function(N, num_sims) {
  
  # Create matrix to store estimates - two params, num_sims draws
  betas <- matrix(0,num_sims,2)
  
  # Run simulations from i=1, 2, ..., num_sims
  for (i in 1:num_sims) {
    
    # Generate data
    x1  <- rep(1,N)
    x2  <- rnorm(N)
    X   <- cbind(x1,x2)
    eps <- rnorm(N)
    Y   <- 1*x1 + 2*x2 + eps
    
    # Define X'X and (X'X)^{-1}
    XX     <- t(X) %*% X
    XX_inv <- solve(XX)
    
    # Compute OLS solution
    beta_hat <- XX_inv %*% t(X) %*% Y
    
    # Store OLS solution
    betas[i,] <- t(beta_hat)
    
  }
  
  # Return betas
  return(betas)
}

#------------------------------------------------
# Call function and plot histograms
#------------------------------------------------

# Distribution of beta_2, increasing K (num of sims)
betas <- run_sims(50,100)
hist(betas[,2])

betas <- run_sims(50,1000)
hist(betas[,2])

betas <- run_sims(50,10000)
hist(betas[,2])

# Distribution of beta_2, increasing N (num of obs)
betas <- run_sims(100,10000)
hist(betas[,2])

betas <- run_sims(1000,10000)
hist(betas[,2])

betas <- run_sims(10000,10000)
hist(betas[,2])
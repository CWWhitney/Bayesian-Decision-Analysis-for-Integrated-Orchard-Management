# Function 

### Bayesian updating #######
# observations = List of observations


observations <- c(40000, 45000, 50000, 42000, 47000)
apple_income_range <- c(3000, 60000)
prior <- apple_income_range

updating(prior = prior, observations = observations)


# Find a way to work in a new set of input tables 
# Ideally in an interactive interface like a google doc
# Observed data

updating <- function(prior, observations){
  
# Number of observations
n <- length(observations)

# Prior distribution for mean (assuming normal prior)
# Mean and standard deviation of prior
prior_mean <- mean(prior)
# Calculate the approximate prior standard deviation for a uniform distribution 
# The range of the uniform distribution is the difference between the maximum
# and minimum values: (b - a).  sqrt(12) 
# U(a, b) is (b - a) / sqrt(12) (this is approximately 3.46)
prior_sd <- (prior[2] - prior[1]) / sqrt(12)

# Bayesian updating for mean income
posterior_mean <- (prior_mean/prior_sd^2 + 
                     sum(observations)/prior_sd^2) / 
  (1/prior_sd^2 + n/prior_sd^2)

posterior_sd <- sqrt(1 / (1/prior_sd^2 + 
                            n/prior_sd^2))

# Updated 90% confidence parameters

lower_90 <- qnorm(0.05, mean = posterior_mean, sd = posterior_sd)
upper_90 <- qnorm(0.95, mean = posterior_mean, sd = posterior_sd)

return(c(lower_90, upper_90))
}


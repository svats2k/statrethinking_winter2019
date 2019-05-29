# Goal: Work with Posterior distributions

library(rethinking)


# First step is to draw from a posterior distribution, lets take the globe tossing model
N <- 1000
p_grid <- seq(from=0, to=1, length.out = N)
prob_p <- rep(1, N)
prob_data <- dbinom(6, size = 9, prob = p_grid) # Likelihood
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)
plot(p_grid, posterior)

# Let us draw samples from the posterior above
# We draw 10,000 samples from the posterior distribution.  The idea here is that the resulting samples will have the 
# same proportions as the exact posterior density.

samples <- sample(p_grid, size = 1e4, prob = posterior, replace = TRUE)
par(mfrow=c(1,2))
plot(samples, xlab= 'Sample number', ylab = 'Proportion of water')
mtext('Sampling parameter values')
dens(samples, xlab = 'probability of water (p)')
mtext('Density of Samples')

# Credible interval: an interval of the defined mass using the posterior probability
# Interval of defined mass
quantile(samples, c(0.1, 0.9)) 

# Percentile intervals, they do a good job of communicating the shape of distribution assuming they aren't very
# asymmetrical
PI(samples, prob = 0.8) # this is teh central 80% probability

# HDPI, narrowest probability interval containing the requested mass
# This interval captures the parameters with the highest probability
HPDI(samples, prob = 0.8)

# Point estimates for a posterior distribution - mean, median, mode(MAP)

#Likelihood function function works both ways
# - Given a realized observation, the function tells us how possible the realized observation is
# - Given the parameters, the likelihood defines the distribution of possible observations that we can sample from


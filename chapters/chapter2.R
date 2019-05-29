# Understanding the globe tossing model

# To begin with, we have one set of plausibilities assigned to each probability.  At the begining odof the experiment 
# we assign the same probability to all plausibility.

# The variable is the target inference, p, which is the proportion of water in the globe.  This is an 
# unobserved variable which is called as a parameter.  This is inferred through other paramters, count of water (W) 
# and count of land (L).  The number of globe tosses is W + L

# The goal is to count all the ways data could arise, given the assumptions.  In globe tossing model, for each 
# possible values of the unobserved variable (p) we need to define the relative number of ways, the probaibility
# that the values of each of the observed variables (W,L) could arise.

# Likelihood: A distribution function assigned to an observed variable.  The observed variable W & L are distributed
# binomially with probability 'p' of water on every toss
# The distribution assigned to the un-observed variables have their own variables, which are called the parameters.
# here 'p' is the parameter of the binomial distribution and is also the un-observed variable.

# dbinom(6, size=9, p=0.5) = 0.16.  This gives us the relative number of ways to get six water n 9 tosses with 
# probability of finding the water at 0.5.

# Prior: This is the initial plausibility assignment for each possible value of the parameter 

# Posterior distribution: Updating the prior to their logical consequences.
# posterior is proportional to the product of the prior and the porbability of the data


# Globe tossing model, the 5 steps:

N <- 200 # NUmber of data points

#1. define the grid
p_grid <- seq(from=0, to = 1, length.out = N)

#2. define the prior
prior <- rep(1, N)

#3. likelihood function (likelihood of each value of p)
likelihood <- dbinom(6,size = 9, prob = p_grid)

#4. COmpute product of likelhood and prior
unstd.posterior <- likelihood * prior

#5. Standardize posterior
std.posterior <- unstd.posterior/ sum(unstd.posterior)

# dislaying the posterior distribution
par(mfrow=c(1,2))
plot(p_grid, std.posterior)
plot(p_grid, prior)
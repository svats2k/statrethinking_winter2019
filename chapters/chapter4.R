library(rethinking)
library(pracma)

# Simulating random walk on a soccer field
pos <- replicate(1000, sum(runif(16,-1,1)))
par(mfrow=c(1,2))
simplehist(pos, xlab='Position')
plot(density(pos))
dev.off()

# Modeling hieghts
data("Howell1")
d <- Howell1
str(d)
par(mfrow=c(1,2))
simplehist(d$height)
dens(d$height)
dev.off()

par(mfrow=c(2,2))
# Prior distribution for mean of height
curve(dnorm(x,178,20), from = 100, to = 250, col='blue', xlab = 'mu', ylab = 'density')
mtext('mu ~ dnorm(178,20')
# Prior distribution for sigma of height
curve(dunif(x, 0,50), from = -10, to = 60, col='blue', xlab = 'sigma', ylab = 'density')
mtext('sigma ~ unif(0,50)')
# Prior predictive simulations
# Height distributions
sample_mu <- rnorm(1e4, 178, 20)
sample_signma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_signma)
dens(prior_h, xlab='height', ylab='Density')
mtext('h ~ dnorm(mu, sigma)')
# assuming a more liberal prior / flatter prior
sample_mu <- rnorm(1e4, 178, 100)
prior_h <- rnorm(1e4, sample_mu, sample_signma)
dens(prior_h, xlab='height', ylab='Density', col='red')
mtext('h ~ dnorm(mu, sigma)
      mu ~ dnorm(178,100')
dev.off()

## Grid approximation for the posterior distribution
N <- 200
mu.list <- seq(from=140, to=160, length.out = N) # establish the range for mu
sigma.list <- seq(from=4, to=9, length.out = N) # establish the range for sigma
post <- expand.grid(mu=mu.list, sigma=sigma.list) # creating a matrix of all possible combinations of mu and sigma
# computing log likelihood of each combinations of mu and sigma, converting to log, if not, 
# rounding off error will take it to zero
# using sapply we pass the unique combinations of mu and sigma on each of the post to a function that
# computes the log-likelihood of each observed height, and adds all of these log-likelihoods together
post$LL <- sapply(1:nrow(post), function(i) sum(dnorm(
  d$height,
  mean=post$mu[i],
  sd=post$sigma[i],
  log = TRUE )))
# we then multiply the prior by the likelihood to get the product that is proportional to the posterior density
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)
# getting back to probability scale by scaling with maximum of the log product
post$prob <- exp(post$prod - max(post$prod))

# Sampling from the distribution above
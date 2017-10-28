# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# Required packages for this exercise.

rm(list=ls())
require(rjags)
require(coda)

load(file = "gameofregression.Rdata")

n = dim(x)[1] # number of data points
p = dim(x)[2] # number of predictors


linear_regression ="
model {
  # Prior
    
    t ~ dgamma(0.01, 0.01)
    w0 ~ dnorm(0, 1.0)

    for (j in 1:p){
      w1[j] ~ dnorm(0, 1.0)
    }

    for (i in 1:n){
        mu[i] = w0 + inprod(t(w1),x[i])
    }
   
    # Likelihood
    for (i in 1:n){
    y[i] ~ dnorm(mu[i], t)
    }
}

"

niter = 10000
nchains = 4
data = list('n' = n,
            'p' = p,
            'x' = x,
            'y' = y)

jagsmodel <- jags.model(textConnection(linear_regression), 
                        data = data,
                        n.chains = nchains)

store_parameters = c('y') # you chose which parameters to monitor
samples = coda.samples(jagsmodel, store_parameters, n.iter = niter)

samplesMatrix = as.matrix(samples)



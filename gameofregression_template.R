# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# Required packages for this exercise.
require(rjags)
require(coda)



rm(list=ls())
require(rjags)
require(coda)

load(file = "gameofregression.Rdata")

n = dim(x)[1] # number of data points
p = dim(x)[2] # number of predictors


linear_regression ="
model {
  # Prior
  
  

  # Likelihood
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

store_parameters = c() # you chose which parameters to monitor
samples = coda.samples(jagsmodel, store_parameters, n.iter = niter)

samplesMatrix = as.matrix(samples)



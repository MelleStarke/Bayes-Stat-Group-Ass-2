# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())


# Required packages for this exercise.
require(rjags)
require(coda)


# Load data
load(file = "twolines.Rdata")
n = length(y)

## -------- Linear regression --------


linreg_model1 ="
model{      
  # Prior 
  
  # Likelihood

"

niter = 10000
nchains = 4

# Create your data structure here
data = list()

jagsmodel_linreg1 <- jags.model(textConnection(linreg_model1), 
                                data = data,
                                n.chains = nchains)

store_parameters = c()

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_oneline = coda.samples(jagsmodel_linreg1, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_oneline)


plot(x,y,pch=20)



## -------- Linear regression mixture --------

linreg_model2 ="
model{      
  # Prior 
  
  # Likelihood

}

"


niter = 10000
nchains = 4
# Create your data structure here
data = list()

jagsmodel_linreg2 <- jags.model(textConnection(linreg_model2), 
                                data = data,
                                n.chains = nchains)

store_parameters = c()

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_twolines = coda.samples(jagsmodel_linreg2, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_twolines)

plot(x,y,pch=20)



## -------- Model selection --------

linreg_model3 ="
model{      
  # Prior 
  
  # Likelihood

"

niter = 10000
nchains = 4
# Create your data structure here
data = list()

jagsmodel_linreg3 <- jags.model(textConnection(linreg_model3), 
                                data = data,
                                n.chains = nchains)

store_parameters = c()

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_oneortwolines = coda.samples(jagsmodel_linreg3, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_oneortwolines)

plot(x,y,pch=20)


posterior_model1_mcmc = sum(m==1) / length(m)
posterior_model2_mcmc = 1 - posterior_model1_mcmc


nsamples = 250
plot(x,y,pch=20)



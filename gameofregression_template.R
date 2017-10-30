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
   
    # Likelihood
    for (i in 1:n){
    mu[i] = w0 + (inprod(t(w1[]),x[i,]))
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

store_parameters = c('w0','w1') # you chose which parameters to monitor
samples = coda.samples(jagsmodel, store_parameters, n.iter = niter)

samplesMatrix = as.matrix(samples)
mcmcsummary_weights = summary(samples)
mcmcsummary_weights$statistics

hist(samplesMatrix[,'w0'], main = "Histogram of the posterior \ndistribution over w0 (intercept)", xlab = "w1")
hist(samplesMatrix[,'w1[1]'], main = "Histogram of the posterior \ndistribution over w1 (of mention in n pages)", xlab = "w0")
hist(samplesMatrix[,'w1[2]'], main = "Histogram of the posterior \ndistribution over w2 (of weddings attended)", xlab = "w1")
hist(samplesMatrix[,'w1[3]'], main = "Histogram of the posterior \ndistribution over w3 (of size of household)", xlab = "w1")
hist(samplesMatrix[,'w1[4]'], main = "Histogram of the posterior \ndistribution over w4 (of length in cm)", xlab = "w1")
hist(samplesMatrix[,'w1[5]'], main = "Histogram of the posterior \ndistribution over w5 (of nr of 'winter is coming'said)", xlab = "w1")

# We clearly see that the size of the household with weight w3 (mean of -0.009) hardly influences the age of death.

#1.3.2 - 1, 0, 5, 184, 20
new_char <- c(1,0,5,184,20)
w0_pred <- mcmcsummary_weights$statistics[1,'Mean']
w1_pred <- mcmcsummary_weights$statistics[2:6,'Mean']
prediction1 <- w0_pred + w1_pred %*% new_char
sprintf("the character is predicted to die at age %.1f", prediction1)

new_char2 <- c(1,0,5,380,20)
prediction2 <- w0_pred + w1_pred %*% new_char2
sprintf("the character is predicted to die at age %.1f", prediction2)

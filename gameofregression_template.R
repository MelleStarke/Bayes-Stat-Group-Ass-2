# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# Required packages for this exercise.
require(rjags)
require(coda)

load(file = "gameofregression.Rdata")

n = dim(x)[1] # number of data points
p = dim(x)[2] # number of predictors

results <- x[,1]
predictors <- x[,2]

linear_regression ="
model{      
  # Prior 
    t ~ dgamma(0.01, 0.01)
    w0 ~ dnorm(0, 1.0)
    w1 ~ dnorm(0, 1.0)
    for (i in 1:n){
      mu[i] = w0 + w1*x[i]
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
            'x' = predictors,
            'y' = results)

jagsmodel <- jags.model(textConnection(linear_regression), 
                        data = data,
                        n.chains = nchains)

store_parameters = c('w0','w1','y') # you chose which parameters to monitor
samples = coda.samples(jagsmodel, store_parameters, n.iter = niter)

samplesMatrix = as.matrix(samples)

mcmcsummary_1 = summary(samples)
mcmcsummary_1$statistics

hist(samplesMatrix[,'w0'])
hist(samplesMatrix[,'w1'])


plot(predictors,results, xlab = "change in predictor", ylab = "change in outcome variable", pch=20)

mean_w0 <- mcmcsummary_1$statistics['w0','Mean']
mean_w1 <- mcmcsummary_1$statistics['w1','Mean']

abline(mean_w0,mean_w1)


samples = sample(seq(nrow(samplesMatrix)), 500)
for (i in samples){
  abline(samplesMatrix[i,'w0'], samplesMatrix[i,'w1'], col=rgb(0.8, 0.2, 0.2, max = 1.0, alpha = 0.1))
}


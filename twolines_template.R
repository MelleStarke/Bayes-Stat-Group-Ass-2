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
            'x' = x,
            'y' = y)

jagsmodel <- jags.model(textConnection(linear_regression), 
                        data = data,
                        n.chains = nchains)

store_parameters = c('w0','w1','y') # you chose which parameters to monitor
samples_oneline = coda.samples(jagsmodel, store_parameters, n.iter = niter)

samplesMatrix = as.matrix(samples_oneline)

mcmcsummary_oneline = summary(samples_oneline)
mcmcsummary_oneline$statistics

hist(samplesMatrix[,'w0'])
hist(samplesMatrix[,'w1'])


plot(x,y, xlab = "change in predictor", ylab = "change in outcome variable", pch=20)

mean_w0 <- mcmcsummary_oneline$statistics['w0','Mean']
mean_w1 <- mcmcsummary_oneline$statistics['w1','Mean']

abline(mean_w0,mean_w1)


samples_oneline = sample(seq(nrow(samplesMatrix)), 500)
for (i in samples_oneline){
  abline(samplesMatrix[i,'w0'], samplesMatrix[i,'w1'], col=rgb(0.8, 0.2, 0.2, max = 1.0, alpha = 0.1))
}





## -------- Linear regression mixture --------

linreg_model2 ="
 model{      
# Prior 
t ~ dgamma(0.01, 0.01)

w0[1] ~ dnorm(0, 1.0)
w1[1] ~ dnorm(0, 1.0)

w0[2] ~ dnorm(0, 1.0)
w1[2] ~ dnorm(0, 1.0)


for (i in 1:n){
  z[i] ~ dcat(c(0.5,0.5))

  mu[i] = w0[z[i]] + w1[z[i]]*x[i]
  
  y[i] ~ dnorm(mu[i], t)

  }
}

"


niter = 10000
nchains = 4
# Create your data structure here
data = list('x' = x,
            'y' = y,
            'n' = n)

jagsmodel_linreg2 <- jags.model(textConnection(linreg_model2), 
                                data = data,
                                n.chains = nchains)

store_parameters = c('w0','w1','y')

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_twolines = coda.samples(jagsmodel_linreg2, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_twolines)

mcmcsummary_twolines = summary(samples_twolines)
mcmcsummary_twolines$statistics

plot(x,y,pch=20)

mean_w0 <- mcmcsummary_twolines$statistics[1:2,'Mean']
mean_w1 <- mcmcsummary_twolines$statistics[3:4,'Mean']

abline(mean_w0[1],mean_w1[1], col = 'red')
abline(mean_w0[2],mean_w1[2], col = 'blue')

#sampling
samples_twolines = sample(seq(nrow(samplesMatrix)), 500)
for (i in samples_twolines){
  abline(samplesMatrix[i,'w0[1]'], samplesMatrix[i,'w1[1]'], col=rgb(1,0,0, alpha = 0.1))
  abline(samplesMatrix[i,'w0[2]'], samplesMatrix[i,'w1[2]'], col=rgb(0,0,1, alpha = 0.1))
}

# Answer to 1.2.4. We see that we end up with two regression lines with a very good fit (based on visual inspection). However, the plot of the expectation of the lines (with a and b being means of w0 and w1) lie 'in between' the two found regression lines. The reason for this is that there is still some classification error, which means that some data points are considered to be on the other regression line. Some blue lines are plotted on the dominantly red line and vice versa. This in turn, has the result that the expectation of the one line is ' pulled' a bit to the other. Some blue lines are plotted on the dominantly red line and vice versa.

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



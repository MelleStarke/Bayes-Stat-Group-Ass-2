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

plot(x,y,pch=20, xlab = "change in predictor", ylab = "change in outcome variable")

#sampling
samples_twolines = sample(seq(nrow(samplesMatrix)), 500)
for (i in samples_twolines){
  abline(samplesMatrix[i,'w0[1]'], samplesMatrix[i,'w1[1]'], col=rgb(1,0,0, max = 1.0, alpha = 0.1))
  abline(samplesMatrix[i,'w0[2]'], samplesMatrix[i,'w1[2]'], col=rgb(0,0,1, max = 1.0, alpha = 0.1))
}


#estimation
mean_w0 <- mcmcsummary_twolines$statistics[1:2,'Mean']
mean_w1 <- mcmcsummary_twolines$statistics[3:4,'Mean']

abline(mean_w0[1],mean_w1[1], col = rgb(1, 0.5, 0.5), lwd = 5)
abline(mean_w0[2],mean_w1[2], col = rgb(0.5, 0.5, 1), lwd = 5)
abline(mean_w0[1],mean_w1[1], col = rgb(1, 0, 0))
abline(mean_w0[2],mean_w1[2], col = rgb(0, 0, 1))

# Answer to 1.2.4. We see that we end up with two regression lines with a very good fit (based on visual inspection). However, the plot of the expectation of the lines (with a and b being means of w0 and w1) lie 'in between' the two found regression lines. The reason for this is that there is still some classification error, which means that some data points are considered to be on the other regression line. Some blue lines are plotted on the dominantly red line and vice versa. This in turn, has the result that the expectation of the one line is ' pulled' a bit to the other. Some blue lines are plotted on the dominantly red line and vice versa.


## -------- Model selection --------


linreg_model3 ="
model{     
    t ~ dgamma(0.01, 0.01)

  #m1
    w0_m1 ~ dnorm(0, 1.0)
    w1_m1 ~ dnorm(0, 1.0)
    for (i in 1:n){
    mu_m1[i] = w0_m1 + w1_m1*x[i]
    }

  #m2
    w0_m2[1] ~ dnorm(0, 1.0)
    w1_m2[1] ~ dnorm(0, 1.0)
    
    w0_m2[2] ~ dnorm(0, 1.0)
    w1_m2[2] ~ dnorm(0, 1.0)
    
    
    for (i in 1:n){
      z[i] ~ dcat(c(0.5,0.5))
      mu_m2[i] = w0_m2[z[i]] + w1_m2[z[i]]*x[i]
      }
    
    m_prob[1] = 0.5
    m_prob[2] = 0.5

    m ~ dcat(m_prob[])

    # Likelihood
    for (i in 1:n){
    mu_picked[i] = equals(m,1)*mu_m1[i] + equals(m,2)*mu_m2[i] 
    y[i] ~ dnorm(mu_picked[i], t)
    }
}
"
#we increased the number of iterations from 10.000 to 40.000 because we considered the standard deviation on the estimate for m too large
niter = 40000
nchains = 4
# Create your data structure here
data = list(
          'x' = x,
          'y' = y,
          'n' = n
)

jagsmodel_linreg3 <- jags.model(textConnection(linreg_model3), 
                                data = data,
                                n.chains = nchains)

store_parameters = c('m','w0_m1','w1_m1','w0_m2','w1_m2')

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_oneortwolines = coda.samples(jagsmodel_linreg3, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_oneortwolines)

#Summary statistics
mcmcsummary_model1 = summary(samples_oneortwolines)
mcmcsummary_model1 $ statistics

#Posterior of models
m = samplesMatrix[,"m"]
posterior_model1_mcmc = sum(m==1) / length(m)
posterior_model2_mcmc = 1 - posterior_model1_mcmc

#Plotting
nsamples = 250
plot(x,y,pch=20, xlab = "change in predictor", ylab = "change in outcome variable")

samplesMatrix_m1 = samplesMatrix[samplesMatrix[,"m"] == 1,]
samplesMatrix_m2 = samplesMatrix[samplesMatrix[,"m"] == 2,]

#sampling nsamples each for m==1 and m==2 
samples_m1 = sample(seq(nrow(samplesMatrix_m1)), nsamples)
samples_m2 = sample(seq(nrow(samplesMatrix_m2)), nsamples)

# plotting line from m1
for (i in samples_m1){
  abline(samplesMatrix[i,'w0_m1'], samplesMatrix[i,'w1_m1'], col=rgb(1,0,0, max = 1.0, alpha = 0.1))
}

# plotting lines from m2
for (i in samples_m2){
  abline(samplesMatrix[i,'w0_m2[1]'], samplesMatrix[i,'w1_m2[1]'], col=rgb(0,0,1, max = 1.0, alpha = 0.1))
  abline(samplesMatrix[i,'w0_m2[2]'], samplesMatrix[i,'w1_m2[2]'], col=rgb(0,0,1, max = 1.0, alpha = 0.1))
}

posterior_model1_mcmc
posterior_model2_mcmc

# Because our prior odds are 0.5/0.5, the bayes factor is simply equal to posterior_model1 / posterior_model2
# Bayes factor is really small, so we strongly prefer model 2
bayes_factor = posterior_model1_mcmc / posterior_model2_mcmc; bayes_factor

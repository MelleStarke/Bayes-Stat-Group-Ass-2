# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# Helper functions
number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  binary_vector = binary_vector[-(1:(length(binary_vector) - noBits))]
  binary_vector[binary_vector==0] = -1
  return(binary_vector)
}

plot_evidences = function(evidences, sortBy) {
  sorted = order(evidences[,sortBy], decreasing = T)
  matplot(evidences[sorted,], type = 'b', lty = rep(1,nmodels), pch=20, col = 1:4, xlab = 'Dataset number', ylab = 'Evidence') 
  legend("topright", legend = c('Model 0', 'Model 1', 'Model 2', 'Model 3'), col=1:4, pch=20) 
  return(sorted)
}

likelihood_m2 <- function(w,y,x){
  z <- list()
  for (i in 1:9){
    z[i] <- 1/(1+exp(-y[i]*w[1]*x[i,1]))
  } 
  return(Reduce('*',z,1))
}

likelihood_m3 <- function(w,y,x){
  z <- list()
  for(i in 1:9){
    z[i] <- 1/(1 + exp(-y[i]*(w[1]*x[i,1]+w[2]*x[i,2])))
  }
  return(Reduce('*',z,1))
}

likelihood_m4 <- function(w,y,x){
  z <- list()
  for(i in 1:9){
    z[i] <- 1/(1 + exp(-y[i]*(w[1] + w[2]*x[i,1] + w[3]*x[i,2]))) 
  }
  return(Reduce('*',z,1))
}

likelihoodz <- c(likelihood_m2,likelihood_m3,likelihood_m4)

# Create all the possible data sets
npixels = 9
ndatasets = 2^npixels
nmodels = 4

datasets = matrix(0, nrow = ndatasets, ncol = npixels)

for (d in 1:ndatasets) {
  datasets[d,] = number2binary(d, npixels)
}


x = matrix(0, npixels, 2)
x[1,] = c(-1, 1)
x[2,] = c(0, 1)
x[3,] = c(1, 1)
x[4,] = c(-1, 0)
x[5,] = c(0, 0)
x[6,] = c(1, 0)
x[7,] = c(-1, -1)
x[8,] = c(0, -1)
x[9,] = c(1, -1)


# Compute the evidences for each data set
evidences = matrix(0, nrow = ndatasets, ncol = nmodels)
evidences[,1] = 1/512 # fill in

# Parameters for the prior weights
mu = 0
sigma = 10

# The number of samples to get
nsamples = 10000


for (d in 1:ndatasets) {
  y = datasets[d,]
  # compute evidence using Monte Carlo
  
  # For m_0 you have already computed the evidence above.
  for (i in 2:nmodels) {
    evidence = 0
    for (s in 1:nsamples) {
      # get weights from prior
      w = rnorm(i-1,0,10) # fill in
      pDwm = likelihoodz[[i-1]](w,y,x) # fill in
        
      
      
      evidence = evidence + pDwm # fill in
    }
    evidences[d,i] = evidence / nsamples # fill in
  }
}

sorted_order = plot_evidences(evidences, sortBy= 4)

indices_m0_over_m3 <- nrow(evidences[which(evidences[,1] > evidences[,4]),])

preference_matrix <- matrix(nrow = 4, ncol = 4)
for (i in 1:4){
  for(j in 1:4){
    preference_matrix[i,j] <- nrow(evidences[which(evidences[,i] > evidences[,j]),])
    }
  }
better_models <- list()
for (i in 1:4){
  better_models[i] <- Reduce('+',preference_matrix[i,],0)
}
better_models <- unlist(better_models)

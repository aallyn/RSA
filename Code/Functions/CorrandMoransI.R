############################### Spatail Autocorrelation Functions

######## Functions for fitting correlograms
gauss.corr<- function(mu, sigma) {
  -sum(dnorm(x, mu, sigma, log = TRUE))
}

exp.corr<- function(rate) {
  -sum(dexp(x, rate, log = TRUE)) 
}

morani <- function(variable, adj.mat) {
  zvar <- variable-mean(variable)
  morani.basic <- (t(zvar)%*%adj.mat%*%zvar) / sum(zvar^2)
  morani.stat <- (length(zvar)/sum(adj.mat)) * morani.basic
  return(morani.stat)
}
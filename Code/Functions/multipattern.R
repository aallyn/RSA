################################################################################
### This function generates multiple autocorrelated patterns that are        ###
### correlated with each other. The autocorrelation can be varied (tau) if   ###
### desired within the replicates. The patterns are based on the same phases ###
### (base_phi), which are shifted by a fixed distance with 50% probability   ###
### either 0 or a distance given by the between pattern correlation measure  ###
### (corr). NB the expected between-pattern correlation is only guaranteed   ###
### if all patterns have the same autocorrelation.                           ###
################################################################################


multiPattern <- function(n=2, dim=c(20,20), taus = seq(1,100,length = n), corr=0, 
                         tol = 0.01, cmatrix=F, retain_base = F, base_phi = NULL)
  
  
  # dim is a two element vector containing the dimensions of the patterns
  # deltaphase is the phase difference between the patterns (in degrees)
  # deltaphase controls the correlation between patterns
  
  # if retain_base = F, returns a LIST of matrices, else returns a list containing
  # the list of matrices with a second element, the base_phi used to generate the pattern.
  # This allows the function to be used with a random seed together with the base_phi
  # and always gives the same first pattern given seed and tau.
{
  spatialPatternExp <- function(dim,base_phi,tau=1,corr=0, first = FALSE, rmat = NULL) {
    # Generate a 2d spatial pattern that has an exponential power law spectral density function
    # The autocorrelation function is exp(-t /tau) 
    # which gives a spectral power of 4 * tau / ( (2*pi*tau)^2 * f^2 + 1 )
    
    # this version takes as argument an array containing the 'base phases' (base_phi)
    # and then offsets these by a further random phase shift, the magnitude of which is given by
    # 'deltaphase'.
    
    dimTemp <- 2*dim    # make the pattern twice as large (cut it in half later)
    
    # Generate the phase shift and add to base_phi
    # : generate a matrix of -1 or +1 : means add the phase or subtract it
    if(!first) {
      deltaphase=acos(2*abs(corr) - 1 )                                               
      
      y=c( rep(1,prod(dimTemp)/2),rep(0,prod(dimTemp)/2 ) )
      z=y[ rank(runif(prod(dimTemp) ) ) ]
      rmat=array(z,dimTemp) # matrix of {0,1} exactly 50% 1 & 50% 0
      #print(rmat)
      rand_phi <- deltaphase*rmat
      phi <- rand_phi+base_phi
      #print(head(phi))
    } else phi <- base_phi
    
    # Generate an array with the frequencies in it
    u <- array(0,dimTemp)
    u[1:dimTemp[1],1:dimTemp[2]] <- c(0:floor(dimTemp[1]/2), (-ceiling(dimTemp[1]/2)+1):-1) / dimTemp[1]
    v <- array(0,c(dimTemp[2],dimTemp[1]))
    v[1:dimTemp[2],1:dimTemp[2]] <- c(0:floor(dimTemp[2]/2), (-ceiling(dimTemp[2]/2)+1):-1) / dimTemp[2]
    v <- t(v)
    
    # Now generate spectral function
    Sf <- ( 4*tau / ( (2*pi*tau)^2 * (u^2 + v^2) + 1 )  )^0.5
    Sf[Sf==Inf] <- 0
    Sfcoeff <- array(complex(prod(2*dim),real=Sf*sin(phi),imag=Sf*cos(phi)),2*dim)
    x <- fft(Sfcoeff, inverse=TRUE)
    x <- Re(x[1:dim[1],1:dim[2]])
    if(corr<0) x <- -x
    
    if(first) return (list(x, rmat)) else return(x)
  }
  if(cmatrix) data = array(-9999,c(prod(dim),n) )
  
  allPat <- list()
  
  dimTemp=2*dim
  if(is.null(base_phi)) base_phi = 2*pi*array(runif(prod(dimTemp)),dimTemp) else   #'base' phases
    runif(prod(dimTemp))                                                          #use up runifs to maintain seed
  
  
  def.par <- par(mfrow = c(ceiling(sqrt(n)), ceiling( sqrt(n))))
  
  Pat1<-spatialPatternExp(dim,base_phi,exp(taus[1]),corr, first = T)
  
  allPat[[1]]<-Pat1[[1]]
  image(Pat1[[1]],col=rainbow(30, s = 1, v = 1, start = 0.4, end=0.0) )
  
  if(cmatrix) { data[,1] <- Pat1[[1]][1:prod(dim)] }
  
  for(i in 2:n) {
    Pat<-spatialPatternExp(dim,base_phi,exp(taus[i]),corr, rmat = Pat1[[2]])
    
    allPat[[i]]<-Pat
    
    image(Pat,col=rainbow(30, s = 1, v = 1, start = 0.4, end=0.0) )
    
    if(cmatrix) { data[,i] <- Pat[1:prod(dim)] }
    
  }
  
  #scale all patterns to mean zero, variance 1
  
  if(cmatrix) { 
    data<-scale(data)
    corrs <- cor(data) 
    print(corrs)
  }
  
  par(def.par)
  
  if(retain_base) return(list(patterns = allPat, base_phi = base_phi)) else
    return(allPat)
}


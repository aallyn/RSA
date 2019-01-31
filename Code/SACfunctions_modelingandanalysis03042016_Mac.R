###################### R SAC Simulation Functions ########################
##########################################################################

code.dir<- "./Code/Functions"
#code.dir <- "Z:/Users/Andrew/sac/Code"

source(paste(code.dir, "/biostats01292014.R", sep=""))
source(paste(code.dir, "/WaveletFunctions.R", sep=""))
source(paste(code.dir, "/geefunctions.R", sep = ""))

##########################################################################

##### Generating landscape surfaces
{
## For rescaling
rescale.function<- function(x, min, lowerbound, max, upperbound) {
  lowerbound + (((x - min)/(max-min)) * (upperbound - lowerbound))
}

## Surface generation main
surface.gen<- function(realizations = 1, 
                       steps = 1, 
                       grid.min.x = 0, 
                       grid.max.x = 100, 
                       grid.min.y= 0, 
                       grid.max.y = 100, 
                       method, 
                       psill.use, 
                       range.use, 
                       model.use, 
                       nugget.use, 
                       nobs.use = 20, 
                       dist.use = FALSE, 
                       fractal.param = NULL, 
                       rescale, 
                       lower.bound = NULL, 
                       upper.bound = NULL, 
                       randomize) {
  
  # For stepping through data generation process
  if(FALSE) {
    realizations = 1
    steps = 1
    grid.min.x = 0
    grid.max.x = 100
    grid.min.y= 0
    grid.max.y = 100
    method = "gstat"
    psill.use = 100
    range.use = 50
    model.use = "Gau"
    nugget.use = 0.05
    nobs.use = 20
    dist.use = FALSE
    fractal.param = NULL
    rescale = TRUE
    lower.bound = 0
    upper.bound = 1
    randomize = FALSE
  }
  
  # Libraries used by both methods
  library(raster)
  library(spdep)
  library(fields)
  
  # Set up surface grid dimensions and x/y coords
  x.seq<- seq(from = grid.min.x, to = grid.max.y, by = steps)
  y.seq<- seq(from = grid.min.y, to = grid.max.y, by = steps)
  
  # Surface generation using "gstat" methods
  if(method == "gstat") {
    library(gstat)
    xy<- expand.grid(x.seq, y.seq)
    names(xy)<- c("x", "y")
    if(dist.use) {
      spat.model<- gstat(formula = z~1, 
                         locations = ~x+y, 
                         dummy = TRUE, 
                         beta = 1, 
                         model = vgm(psill = psill.use, 
                                     range = range.use, 
                                     model = model.use, nugget = nugget.use), 
                         maxdist = dist.use)
      } else {
        spat.model<- gstat(formula = z~1, 
                           locations = ~x+y, 
                           dummy = TRUE, 
                           beta = 1, 
                           model = vgm(psill = psill.use, 
                                       range = range.use, 
                                       model = model.use, 
                                       nugget = nugget.use), 
                           nmax = nobs.use)
      }
    
    # Create raster from generated values
    predicted<- predict(spat.model, newdata = xy, nsim = 1)
    gridded(predicted)<- ~x+y
    rast.fracsurface<- raster(predicted)
    
    # Rescale or randomize generated values and store as sp pointsdataframe
    if(rescale) {
      rast.fracsurface@data@values<- rescale.function(x = rast.fracsurface@data@values, min = min(rast.fracsurface@data@values), max = max(rast.fracsurface@data@values), lowerbound = lower.bound, upperbound = upper.bound)
      grid.fracsurface<- as(rast.fracsurface, "SpatialPointsDataFrame")
    }
    if(randomize) {
      rast.fracsurface@data@values<- sample(rast.fracsurface@data@values, replace = FALSE)
      grid.fracsurface<- as(rast.fracsurface, "SpatialPointsDataFrame")
      } else {
        grid.fracsurface<- as(rast.fracsurface, "SpatialPointsDataFrame")
      }
  }
  
  # Use GaussRF function instead of gstat. Not complete.
  if(method == "gaussRF") {
    library(RandomFields)
    gauss.surface<- GaussRF(n = realizations, 
                            x=x.seq, 
                            y=y.seq, 
                            grid = TRUE, 
                            gridtriple = FALSE, 
                            model = list("fractalB", 
                                         fractal.param))
    rast.fracsurface<- raster(gauss.surface, 
                              xmn = grid.min.x, 
                              xmx = grid.max.x, 
                              ymn = grid.min.y, 
                              ymx = grid.max.y)
    rast.fracsurface@data@values<- rescale.function(x = rast.fracsurface@data@values, min = min(rast.fracsurface@data@values), max = max(rast.fracsurface@data@values), lowerbound = lower.bound, upperbound = upper.bound)
    
    if (randomize) {
      rast.fracsurface@data@values<- sample(rast.fracsurface@data@values, replace = FALSE)
      grid.fracsurface<- as(rast.fracsurface, "SpatialPointsDataFrame")
      } else {
        grid.fracsurface<- as(rast.fracsurface, "SpatialPointsDataFrame")
      }
  }
  
  # Store function results as a list
  list(rast.fracsurface, grid.fracsurface, data.frame(cbind(min(rast.fracsurface@extent@xmin), max(rast.fracsurface@extent@xmax), min(rast.fracsurface@extent@ymin), max(rast.fracsurface@extent@ymax))))

}

###### End generating landscape surfaces functions
}
############################################################################

##### Data generation functions 
{
## Generating probability values to supply to rbinom and determine pres/abs
p.sac<- function(nvars, 
                 pts.evaluatexy, 
                 x1, 
                 x2 = NULL,
                 exp.param,
                 logistic.env, 
                 ndraws = sample.size,
                 census = census, census.file = census.file,...) {
  
  # Biological only process indicator 
  bio.only<- FALSE
  if(exp.param !=0 && length(logistic.env) == 0) {
    bio.only<- TRUE
  }
  
  # Environment only scenarios
  if(exp.param == 0 && nvars == 1) {
    a<- logistic.env$a
    b<- logistic.env$b
    p.deterministic<- (exp(a+b*x1))/(1 + (exp(a+b*x1)))
    return(p.deterministic)
  }
  if(exp.param == 0 && nvars == 2) {
    a<- logistic.env$a
    b<- logistic.env$b
    c<- logistic.env$c
    p.deterministic<- (exp(a+ b*x1 + c*x2))/(1 + (exp(a+ b*x1 + c*x2)))
    return(p.deterministic)
  }
  
  # Biological process only scenarios
  if(bio.only == TRUE) {
    library(fields)
    
    if(census == TRUE) {
      chol.weight.mat.t <- read.csv(paste(census.file))
    }
    
    if(census == FALSE) {
      dist.mat <- rdist(pts.evaluatexy)
      diag(dist.mat) <- 0
      weight.mat <- exp(-exp.param*dist.mat)
      chol.weight.mat <- chol(solve(weight.mat))
      chol.weight.mat.t <- solve(chol.weight.mat)
    }
    
    errors.random <- rnorm(ndraws, 0, 1)
    choleski.errors <- chol.weight.mat.t%*%errors.random
    p.deterministic <- 0.5
    p.sac <- p.deterministic + (sqrt(p.deterministic*(1-p.deterministic))*choleski.errors)
    return(p.sac)
  }
  
  # Combination environment and biological process scenarios
  if(bio.only == FALSE && nvars == 1) {
    library(fields)
    if(census == TRUE) {
      chol.weight.mat.t <- read.csv(paste(census.file))
    }
    
    if(census == FALSE) {
      dist.mat <- rdist(pts.evaluatexy)
      diag(dist.mat) <- 0
      weight.mat <- exp(-exp.param*dist.mat)
      chol.weight.mat <- chol(solve(weight.mat))
      chol.weight.mat.t <- solve(chol.weight.mat)
    }
    
    errors.random <- rnorm(ndraws, 0, 1)
    choleski.errors <- chol.weight.mat.t%*%errors.random
    a<- logistic.env$a
    b<- logistic.env$b
    p.deterministic<- (exp(a+b*x1))/(1 + (exp(a+b*x1)))
    p.sac <- p.deterministic + (sqrt(p.deterministic*(1-p.deterministic))*choleski.errors)
    return(p.sac)
  }
  
  if(bio.only == FALSE && nvars == 2) {
    library(fields)
    if(census == TRUE) {
      chol.weight.mat.t <- read.csv(paste(census.file))
    }
    
    if(census == FALSE) {
      dist.mat <- rdist(pts.evaluatexy)
      diag(dist.mat) <- 0
      weight.mat <- exp(-exp.param*dist.mat)
      chol.weight.mat <- chol(solve(weight.mat))
      chol.weight.mat.t <- solve(chol.weight.mat)
    }
    
    errors.random <- rnorm(ndraws, 0, 1)
    choleski.errors <- chol.weight.mat.t%*%errors.random
    a<- logistic.env$a
    b<- logistic.env$b
    c<- logistic.env$c
    p.deterministic<- (exp(a+ b*x1 + c*x2))/(1 + (exp(a+ b*x1 + c*x2)))
    p.sac <- p.deterministic + (sqrt(p.deterministic*(1-p.deterministic))*choleski.errors)
    return(p.sac)
  }
}
  
 
## Calculating spatial autocovariates that may be used to account for SAC in the modeling process
autocov.custom<- function(approach, 
                          dataset = dataset, 
                          points = pts.evaluatexy, 
                          autocov.param, 
                          zero.policy = TRUE, 
                          nbs = NULL, 
                          style = "W", 
                          longlat = NULL) {
  
  # For stepping through the function
  if(FALSE) {
    approach<- "inverse"
    dataset = dataset
    pts.evaluatexy <- cbind(dataset$x, dataset$y)
    points = pts.evaluatexy
    autocov.param = 1
    zero.policy = TRUE
    nbs = 1
    style = "W"
    longlat = NULL
  }

  # Calculate distance matrix
  library(fields)
  dist.mat <- rdist(points)
  
  # Calculate traditional inverse weighted autocovariate
  if(approach == "inverse") {
    library(spdep)
    if (is.null(zero.policy)) 
      zero.policy <- get("zeroPolicy", envir = .spdepOptions)
    stopifnot(is.logical(zero.policy))
    stopifnot(is.vector(dataset$zero.one))
    if (inherits(points, "SpatialPoints")) {
      if ((is.null(longlat) || !is.logical(longlat)) && !is.na(is.projected(points)) && !is.projected(points)) {
        longlat <- TRUE
      }
      else longlat <- FALSE
      points <- coordinates(points)
    }
    else if (is.null(longlat) || !is.logical(longlat)) 
      longlat <- FALSE
    stopifnot(ncol(points) == 2)
    if (longlat) {
      bb <- bbox(points)
      if (!sp:::.ll_sanity(bb)) 
        warning("Coordinates are not geographical: longlat argument wrong")
    }
    nb <- dnearneigh(points, 0, nbs, longlat = longlat)
    if (any(card(nb) == 0)) 
      # warning(paste("With value", nbs, "some points have no neighbours"))
    nbd <- nbdists(nb, points, longlat = longlat)
    if (autocov.param == 0) {
      lw <- nb2listw(nb, style = style, zero.policy = zero.policy)
    } else {
      gl <- lapply(nbd, function(x) 1/(x^autocov.param))
      lw <- suppressWarnings(nb2listw(nb, glist = gl, style = style, zero.policy = zero.policy))
    }
    spat.var<- lag(lw, dataset$zero.one, zero.policy = zero.policy)
    return(spat.var)
  }
  
  # Calculate autocovariate as an interpolated kernel of presences
  if(approach == "interpolation") {
    wt.mat<- exp(-autocov.param*dist.mat)
    diag(wt.mat)<- 0
    kern <- wt.mat%*%dataset$zero.one
    spat.var<- as.numeric(kern/rowSums(wt.mat))
    return(spat.var)
  }
  
  # Calculate neighborhood intensity kernel based on presences and absences
  if(approach == "nb.intensity") {
    wt.mat<- exp(-autocov.param*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(dataset$zero.one == 0, -1, 1)
    spat.var<- as.numeric(wt.mat%*%ind.vec)
    return(spat.var)
  }
}

## Generating a full dataset 
sac.stochastic.data<- function(nsim, 
                               method = "gstat", 
                               psill.use = NULL, 
                               range.env1 = NULL, 
                               range.env2 = NULL, 
                               correlated.env = NULL, 
                               corr.param = NULL, 
                               model.use = NULL, 
                               nugget.use = NULL, 
                               nobs.use = 20, 
                               dist.use = FALSE, 
                               rescale = NULL, 
                               lbound.env = NULL, 
                               ubound.env = NULL, 
                               fractal.rand = 1, 
                               fractal.env1 = NULL, 
                               fractal.env2 = NULL, 
                               p.rbinom = TRUE, 
                               sample.size,
                               census = FALSE,
                               census.file,
                               grid.side = 100, 
                               cell.size=1, 
                               logistic.env, 
                               exp.param, ...) {
  
  # For stepping through the function
  if(FALSE) {
    nsim = 2
    method = "gstat"
    psill.use = 100
    range.env1 = 25
    range.env2 = 25
    model.use = "Gau"
    nugget.use = 0.05
    nobs.use = 20
    dist.use = FALSE
    rescale = TRUE
    lbound.env = -250
    ubound.env = 0
    fractal.rand = NULL
    fractal.env1 = NULL
    fractal.env2 = NULL
    randomize = FALSE
    p.rbinom = TRUE
    sample.size = 10201
    census = TRUE 
    census.file = NULL
    grid.side = 100
    cell.size = 1
    logistic.env<- data.frame(cbind (a = 5, b = 0.025, c = 0.025))
    exp.param = 0.3
    correlated.env = FALSE
    corr.param = NULL
  }
  
  # Creating results storage, which will be a list of dataframes
  data.sets.list<- vector("list", nsim)
  
  # House keeping to determine how many surfaces to generate and which function is used to calculate probability values, which are then supplied to rbinom
  logistic.formula.length<- sum(is.na(logistic.env))
  bio.only<- FALSE
  if(exp.param != 0 && logistic.formula.length == 3) {
    bio.only<- TRUE
  }
  
  # Begin dataset generation for 1:nsim, where nsim is the number of datasets
  for(i in 1:nsim) {
    
    # Creating dataset (i.e., dataframe) objects to store data information
    if(exp.param == 0 && logistic.formula.length == 1) {
      dataset<- data.frame(matrix(nrow = sample.size, ncol = 6))
      names(dataset)[1:6]<- c("id", "x", "y", "x1", "x.random", "zero.one")
    }
    if(exp.param == 0 && logistic.formula.length == 0) {
      dataset<- data.frame(matrix(nrow = sample.size, ncol = 7))
      names(dataset)[1:7]<- c("id", "x", "y", "x1", "x2", "x.random", "zero.one")
    }
    if(bio.only == TRUE) {
      dataset<- data.frame(matrix(nrow = sample.size, ncol = 5))
      names(dataset)[1:5]<- c("id", "x", "y", "x.random", "zero.one")
    }
    if(exp.param != 0 && logistic.formula.length == 1) {
      dataset<- data.frame(matrix(nrow = sample.size, ncol = 6))
      names(dataset)[1:6]<- c("id", "x", "y", "x1", "x.random", "zero.one")
    }
    if(exp.param != 0 && logistic.formula.length == 0) {
      dataset<- data.frame(matrix(nrow = sample.size, ncol = 7))
      names(dataset)[1:7]<- c("id", "x", "y", "x1", "x2", "x.random", "zero.one")
    }
    
    # Creating surfaces
    if(logistic.formula.length == 3) {
      surface.rand<- surface.gen(method = method, 
                                 psill.use = psill.use, 
                                 range.use = 5, 
                                 model.use = model.use, 
                                 nugget.use = nugget.use, 
                                 nobs.use = nobs.use, 
                                 dist.use = dist.use, 
                                 rescale = FALSE, 
                                 lower.bound = NULL, 
                                 upper.bound = NULL, 
                                 randomize = TRUE)
    }
    if(logistic.formula.length == 1) {
      surface1<- surface.gen(method = method, 
                             psill.use = psill.use, 
                             range.use = range.env1, 
                             model.use = model.use, 
                             nugget.use = nugget.use, 
                             nobs.use = nobs.use, 
                             dist.use = dist.use, 
                             rescale = rescale, 
                             lower.bound = lbound.env, 
                             upper.bound = ubound.env, 
                             randomize = FALSE)
      surface.rand<- surface.gen(method = method, 
                                 psill.use = psill.use, 
                                 range.use = 5, 
                                 model.use = model.use, 
                                 nugget.use = nugget.use, 
                                 nobs.use = nobs.use, 
                                 dist.use = dist.use, 
                                 rescale = FALSE, 
                                 lower.bound = NULL, 
                                 upper.bound = NULL, 
                                 randomize = TRUE)
    }
    if(logistic.formula.length == 0 && correlated.env == FALSE){
      surface1<- surface.gen(method = method, 
                             psill.use = psill.use, 
                             range.use = range.env1, 
                             model.use = model.use, 
                             nugget.use = nugget.use, 
                             nobs.use = nobs.use, 
                             dist.use = dist.use, 
                             rescale = rescale, 
                             lower.bound = lbound.env, 
                             upper.bound = ubound.env, 
                             randomize = FALSE)
      surface.rand<- surface.gen(method = "gstat", 
                                 psill.use = psill.use, 
                                 nugget.use = nugget.use, 
                                 range.use = 5, 
                                 model.use = model.use, 
                                 nobs.use = nobs.use, 
                                 dist.use = dist.use, 
                                 rescale = FALSE, 
                                 randomize = TRUE)
      surface2<- surface.gen(method = method, 
                             psill.use = psill.use, 
                             range.use = range.env2, 
                             model.use = model.use, 
                             nugget.use = nugget.use, 
                             nobs.use = nobs.use, 
                             dist.use = dist.use, 
                             rescale = rescale, 
                             lower.bound = lbound.env, 
                             upper.bound = ubound.env, 
                             randomize = FALSE)
    }
    if(logistic.formula.length == 0 && correlated.env == TRUE){
      library(ecodist)
      surface1<- surface.gen(method = method, 
                             psill.use = psill.use, 
                             range.use = range.env1, 
                             model.use = model.use, 
                             nugget.use = nugget.use, 
                             nobs.use = nobs.use, 
                             dist.use = dist.use, 
                             rescale = rescale, 
                             lower.bound = lbound.env, 
                             upper.bound = ubound.env, 
                             randomize = FALSE)
      surface.rand<- surface.gen(method = "gstat", 
                                 psill.use = psill.use, 
                                 nugget.use = nugget.use, 
                                 range.use = 5, 
                                 model.use = model.use, 
                                 nobs.use = nobs.use, 
                                 dist.use = dist.use, 
                                 rescale = FALSE, 
                                 randomize = TRUE)
      surface2<- surface1
      surface2[[1]]@data@values <- as.numeric(corgen(x = surface1[[1]]@data@values, r = corr.param, population = FALSE)[[2]]) 
      surface2[[1]]@data@values<- rescale.function(x = surface2[[1]]@data@values, min = min(surface2[[1]]@data@values), max = max(surface2[[1]]@data@values), lowerbound = -250, upperbound = 0)
    }
    
    # Sampling surfaces, extracting coordinates and covariate values at sample locations, calculating p(presence), determining prese/absence based on p(presence) and storing all values into corresponding columns of the dataset
    
    # Envrionmental process only
    if(bio.only == FALSE && exp.param == 0) {
      # print("environment.only")
      if(census == TRUE) {
        pts.evaluatexy<- surface1[[2]]@coords
        dataset$x <- pts.evaluatexy[,1]
        dataset$y <- pts.evaluatexy[,2]
        dataset$x1 <- surface1[[1]]@data@values
        dataset$x.random <- surface.rand[[1]]@data@values
        dataset$id <- seq(from=1, to=sample.size)
        nvars<- 2-logistic.formula.length
      }
      
      if(census == FALSE) {
        sample.rows<- sample(nrow(surface1[[2]]@coords), size = sample.size)
        pts.evaluatexy <- surface1[[2]]@coords[sample.rows,]
        dataset$x<- pts.evaluatexy[,1]
        dataset$y<- pts.evaluatexy[,2]
        dataset$x1 <- extract(surface1[[1]], matrix(pts.evaluatexy, nrow=sample.size, ncol=2), method = "simple") 
        dataset$x.random <- extract(surface.rand[[1]], matrix(pts.evaluatexy, nrow = sample.size, ncol = 2), method = "simple")
        dataset$id<- seq(from=1, to=sample.size)
        nvars<- 2 - logistic.formula.length
      }
      
      if(nvars == 1) { 
        prob<- p.sac(nvars = nvars, 
                     logistic.env = logistic.env, 
                     x1 = dataset$x1,
                     exp.param = exp.param, 
                     ndraws = sample.size, 
                     census = census, census.file = census.file)
      }
      
      if(nvars == 2) {
        
        if(census == TRUE) {
          dataset$x2 <- surface2[[1]]@data@values
          prob<- p.sac(nvars = nvars, 
                       logistic.env = logistic.env, 
                       pts.evaluatexy = pts.evaluatexy, 
                       x1 = dataset$x1, 
                       x2 = dataset$x2, 
                       exp.param = exp.param, 
                       ndraws = sample.size,
                       census = census, census.file = census.file)
        }
        
        if(census == FALSE) {
          dataset$x2 <- extract(surface2[[1]], matrix(pts.evaluatexy, nrow = sample.size, ncol = 2), method = "simple")
          prob<- p.sac(nvars = nvars, 
                     logistic.env = logistic.env, 
                     x1 = dataset$x1, 
                     x2 = dataset$x2, 
                     exp.param = exp.param, 
                     ndraws = sample.size,
                     census = census, census.file = census.file)
        }
      }
      
      # Use probability in rbinom, or use threshold?
      if(p.rbinom == TRUE) {
        prob<- ifelse(prob>1, 1, ifelse(prob<0, 0, prob))
        dataset$zero.one<- rbinom(n=sample.size, size=1, prob = prob)
      }
      if(p.rbinom == FALSE) {
        dataset$zero.one<- ifelse(prob>= 0.5, 1, 0)
      }
    }
    
    # Biological process only
    if(bio.only == TRUE) {
      # print("bio.only")
      
      if(census == TRUE) {
        pts.evaluatexy <- surface.rand[[2]]@coords
        dataset$x <- pts.evaluatexy[,1]
        dataset$y <- pts.evaluatexy[,2]
        dataset$x.random <- surface.rand[[1]]@data@values
        dataset$id <- seq(from=1, to=sample.size)
      }
      
      if(census == FALSE) {
        sample.rows<- sample(nrow(surface.rand[[2]]@coords), size = sample.size)
        pts.evaluatexy <- surface.rand[[2]]@coords[sample.rows,]
        dataset$x <- pts.evaluatexy[,1]
        dataset$y <- pts.evaluatexy[,2]
        dataset$x.random <- extract(surface.rand[[1]], matrix(pts.evaluatexy, nrow = sample.size, ncol = 2), method = "simple")
        dataset$id <- seq(from=1, to=sample.size)
      }
      
      prob<- p.sac(nvars = nvars, 
                   logistic.env = NULL, 
                   pts.evaluatexy = pts.evaluatexy, 
                   exp.param = exp.param, 
                   ndraws = sample.size,
                   census = census, census.file = census.file)
      if(p.rbinom == TRUE) {
        prob<- ifelse(prob>1, 1, ifelse(prob<0, 0, prob))
        dataset$zero.one<- rbinom(n=sample.size, size=1, prob = prob)
      }
      if(p.rbinom == FALSE) {
        dataset$zero.one<- ifelse(prob>= 0.5, 1, 0)
      }
    }
    
    # Environmental and Biological Process
    if(bio.only == FALSE && exp.param != 0) {
      # print("combined")
      
      if(census == TRUE) {
        pts.evaluatexy<- surface1[[2]]@coords
        dataset$x <- pts.evaluatexy[,1]
        dataset$y <- pts.evaluatexy[,2]
        dataset$x1 <- surface1[[1]]@data@values
        dataset$x.random <- surface.rand[[1]]@data@values
        dataset$id <- seq(from=1, to=sample.size)
        nvars<- 2-logistic.formula.length
      }
      
      if(census == FALSE) {
        sample.rows<- sample(nrow(surface1[[2]]@coords), size = sample.size)
        pts.evaluatexy <- surface1[[2]]@coords[sample.rows,]
        dataset$x <- pts.evaluatexy[,1]
        dataset$y <- pts.evaluatexy[,2]
        dataset$x1 <- extract(surface1[[1]], matrix(pts.evaluatexy, nrow=sample.size, ncol=2), method = "simple")
        dataset$x.random <- extract(surface.rand[[1]], matrix(pts.evaluatexy, nrow = sample.size, ncol = 2), method = "simple")
        dataset$id <- seq(from=1, to=sample.size)
        nvars<- 2-logistic.formula.length
      }

      
      if(nvars == 1) {
        prob<- p.sac(nvars = nvars, 
                     logistic.env = logistic.env, 
                     pts.evaluatexy = pts.evaluatexy, 
                     x1 = dataset$x1, 
                     exp.param = exp.param, 
                     ndraws = sample.size,
                     census = census, census.file = census.file)
      }
      
      if(nvars == 2) {
        
        if(census == TRUE) {
          dataset$x2 <- surface2[[1]]@data@values
          prob<- p.sac(nvars = nvars, 
                     logistic.env = logistic.env, 
                     pts.evaluatexy = pts.evaluatexy, 
                     x1 = dataset$x1, 
                     x2 = dataset$x2, 
                     exp.param = exp.param, 
                     ndraws = sample.size,
                     census = census, census.file = census.file)
        }
        
        if(census == FALSE) {
          dataset$x2<- extract(surface2[[1]], matrix(pts.evaluatexy, nrow = sample.size, ncol = 2), method = "simple")
          prob<- p.sac(nvars = nvars, 
                       logistic.env = logistic.env, 
                       pts.evaluatexy = pts.evaluatexy, 
                       x1 = dataset$x1, 
                       x2 = dataset$x2, 
                       exp.param = exp.param, 
                       ndraws = sample.size,
                       census = census, census.file = census.file)
        }
      }
      
      if(p.rbinom == TRUE) {
        prob<- ifelse(prob>1, 1, ifelse(prob<0, 0, prob))
        dataset$zero.one<- rbinom(n=sample.size, size=1, prob=prob)
      }
      if(p.rbinom == FALSE) {
        dataset$zero.one<- ifelse(prob>= 0.5, 1, 0)
      }
    }
    
    # Store dataset in the list and then remove it to clear workspace before next loop iteration
    data.sets.list[[i]]<- dataset
    rm(dataset)
  }
  
  # Return list of datasets, where length of list = nsim = number of datasets
  return(data.sets.list)
  
}

##### End data generation functions
}
############################################################################

##### Data modeling functions 

## Morans I function
morani <- function(variable, adj.mat) {
  zvar <- variable-mean(variable)
  morani.basic <- (t(zvar)%*%adj.mat%*%zvar) / sum(zvar^2)
  morani.stat <- (length(zvar)/sum(adj.mat)) * morani.basic
  return(morani.stat)
}

## Probability quantiles functions for morans I
p025 <- function(x) quantile(x, prob=.025, na.rm=TRUE)
p975 <- function(x) quantile(x, prob=.975, na.rm=TRUE)

## GLMMPQL hack
glmmPQL.hack<- function (fixed, random, family, data, correlation, weights, 
                         control, niter = 10, verbose = TRUE, ...) 
{
  if (!require("nlme")) 
    stop("package 'nlme' is essential")
  if (is.character(family)) 
    family <- get(family)
  if (is.function(family)) 
    family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
  m <- mcall <- Call <- match.call()
  nm <- names(m)[-1L]
  keep <- is.element(nm, c("weights", "data", "subset", "na.action"))
  for (i in nm[!keep]) m[[i]] <- NULL
  allvars <- if (is.list(random)) 
    allvars <- c(all.vars(fixed), names(random), unlist(lapply(random, 
                                                               function(x) all.vars(formula(x)))))
  else c(all.vars(fixed), all.vars(random))
  Terms <- if (missing(data)) 
    terms(fixed)
  else terms(fixed, data = data)
  off <- attr(Terms, "offset")
  if (length(off <- attr(Terms, "offset"))) 
    allvars <- c(allvars, as.character(attr(Terms, "variables"))[off + 
                                                                   1])
  if (!missing(correlation) && !is.null(attr(correlation, "formula"))) 
    allvars <- c(allvars, all.vars(attr(correlation, "formula")))
  Call$fixed <- eval(fixed)
  Call$random <- eval(random)
  m$formula <- as.formula(paste("~", paste(allvars, collapse = "+")))
  environment(m$formula) <- environment(fixed)
  m$drop.unused.levels <- TRUE
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval.parent(m)
  off <- model.offset(mf)
  if (is.null(off)) 
    off <- 0
  wts <- model.weights(mf)
  if (is.null(wts)) 
    wts <- rep(1, nrow(mf))
  mf$wts <- wts
  fit0 <- glm(formula = fixed, family = family, data = mf, 
              weights = wts, ...)
  w <- fit0$prior.weights
  eta <- fit0$linear.predictors
  zz <- eta + fit0$residuals - off
  wz <- fit0$weights
  fam <- family
  nm <- names(mcall)[-1L]
  keep <- is.element(nm, c("fixed", "random", "data", "subset", 
                           "na.action", "control"))
  for (i in nm[!keep]) mcall[[i]] <- NULL
  fixed[[2L]] <- quote(zz)
  mcall[["fixed"]] <- fixed
  mcall[[1L]] <- quote(nlme::lme)
  mcall$random <- random
  mcall$method <- "ML"
  if (!missing(correlation)) 
    mcall$correlation <- correlation
  mcall$weights <- quote(varFixed(~invwt))
  mf$zz <- zz
  mf$invwt <- 1/wz
  mcall$data <- mf
  for (i in seq_len(niter)) {
    if (verbose) 
      message(gettextf("iteration %d", i), domain = NA)
    fit <- eval(mcall)
    etaold <- eta
    eta <- fitted(fit) + off
    if (sum((eta - etaold)^2) < 1e-06 * sum(eta^2)) 
      break
    mu <- fam$linkinv(eta)
    mu.eta.val <- fam$mu.eta(eta)
    mf$zz <- eta + (fit0$y - mu)/mu.eta.val - off
    wz <- w * mu.eta.val^2/fam$variance(mu)
    mf$invwt <- 1/wz
    mcall$data <- mf
  }
  #attributes(fit$logLik) <- NULL
  fit$call <- Call
  fit$family <- family
  #fit$logLik <- as.numeric(NA)
  oldClass(fit) <- c("glmmPQL", oldClass(fit))
  fit
}

## Data modeling main function 
sac.model<- function(nsim,
                     model.fit,
                     resids.type = "pearson",
                     model.subset = NULL,
                     custom.fit = NULL, 
                     mle2.method = NULL, 
                     start.params = NULL, 
                     datasets, 
                     datasets.group,
                     model.formula,
                     grid.side = 100, 
                     cell.size = 1, 
                     directory = NULL, 
                     date = NULL, 
                     scenario = NULL, 
                     bin.size = 1, 
                     moransi.minpairs = NULL, 
                     dnn.max = NULL, 
                     neg.exp.ev = NULL, 
                     mu.ev = NULL, 
                     sigma.ev = NULL, 
                     spev.neg.exp = NULL, 
                     spev.gauss = NULL,
                     spev.traditional = NULL,
                     spGLM.knots = NULL,
                     spGLM.phi.start = NULL,
                     spGLM.sigmasq.start = NULL,
                     spGLM.phi.tune = NULL,
                     spGLM.sigmasq.tune = NULL,
                     spGLM.nbatch = NULL,
                     spGLM.batchlength = NULL,
                     spGLM.accept = NULL,
                     spGLM.covmodel = NULL,
                     spGLM.burnin = NULL,...){
  
  # For stepping through the function
  if(FALSE) {
    load("Z:/Users/Andrew/sac/simulated.data/emm.emm.bm.Rdata")
    model.par<- read.csv(paste("Z:/Users/Andrew/sac/Input/", "models.newruns.02022016.csv", sep = ""), as.is = TRUE)
    #load("~/R/Allyn/PhD/SAC/Anthill/simulated.data/emm.ewf.bf.Rdata")
    #model.par<- read.csv("~/Desktop/SAC_batchlist_models_spGLM_convergencecheck.csv", as.is = TRUE)

    ri = 302
    set.seed(ri)
    nsim = model.par$nsim[ri]
    model.fit = model.par$model.fit[ri]
    resids.type = "pearson"
    model.subset = model.par$model.subset[ri]
    mle2.method = model.par$mle2.method[ri]
    datasets = d
    datasets.group = model.par$datasets.group[ri]
    model.formula = model.par$model.formula[ri]
    start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(model.par$neg.exp.param[ri]), mu = as.numeric(model.par$mu[ri]), sigma = as.numeric(model.par$sigma[ri]))))
    grid.side = model.par$grid.side[ri]
    cell.size = 1
    bin.size = model.par$bin.size[ri]
    moransi.minpairs = model.par$moransi.minpairs[ri]
    dnn.max = suppressWarnings(as.numeric(model.par$dnn.max[ri]))
    custom.fit = model.par$custom.fit[ri]
    spev.traditional = model.par$spev.traditional[ri]
    spev.neg.exp = model.par$spev.neg.exp[ri]
    spev.gauss = model.par$spev.gauss[ri]
    neg.exp.ev = suppressWarnings(as.numeric(model.par$neg.exp.ev[ri]))
    mu.ev = suppressWarnings(as.numeric(model.par$mu.ev[ri]))
    sigma.ev = suppressWarnings(as.numeric(model.par$sigma.ev[ri]))
    spGLM.knots = suppressWarnings(as.numeric(model.par$spGLM.knots[ri]))
    spGLM.phi.start = suppressWarnings(as.numeric(model.par$spGLM.phi.start[ri]))
    spGLM.sigmasq.start = suppressWarnings(as.numeric(model.par$spGLM.sigmasq.start[ri]))
    spGLM.phi.tune = suppressWarnings(as.numeric(model.par$spGLM.phi.tune[ri]))
    spGLM.sigmasq.tune = suppressWarnings(as.numeric(model.par$spGLM.sigma.sq.tune[ri]))
    spGLM.nbatch = suppressWarnings(as.numeric(model.par$spGLM.nbatch[ri]))
    spGLM.batchlength = suppressWarnings(as.numeric(model.par$spGLM.batchlength[ri]))
    spGLM.accept = suppressWarnings(as.numeric(model.par$spGLM.accept[ri]))
    spGLM.covmodel = model.par$spGLM.covmodel[ri]
    spGLM.burnin = as.numeric(model.par$spGLM.burnin[ri])
    date = model.par$date[ri]
    directory = model.par$directory[ri]
    scenario = model.par$scenario[ri]
  }
  
  ##### Set up datasets, results objects, and indices to help track/store results
  
  # Libraries used by all models
  suppressWarnings(library(hier.part))
  suppressWarnings(library(gtools))
  suppressWarnings(library(lmtest))
  suppressWarnings(library(bbmle))
  suppressWarnings(library(PresenceAbsence))
  suppressWarnings(library(epiR))
  suppressWarnings(library(fields))
  
  # Split list of 1000 datasets into groups of 100 for Anthill
  if(datasets.group == "a"){
    datasets<- datasets[1:100]
  }
  if(datasets.group == "b") {
    datasets<- datasets[101:200]
  }
  if(datasets.group == "c") {
    datasets <- datasets[201:300]
  }
  if(datasets.group == "d") {
    datasets<- datasets[301:400]
  }
  if(datasets.group == "e") {
    datasets<- datasets[401:500]
  }
  if(datasets.group == "f") {
    datasets<- datasets[501:600]
  }
  if(datasets.group == "g") {
    datasets<- datasets[601:700]
  }
  if(datasets.group == "h") {
    datasets<- datasets[701:800]
  }
  if(datasets.group == "i") {
    datasets<- datasets[801:900]
  }
  if(datasets.group == "j") {
    datasets<- datasets[901:1000]
  }
  if(datasets.group == "Dormann") {
    datasets <- datasets[1:10]
  }
  if(datasets.group == "conv.check") {
    random.sample<- sample(seq(1, 1000), 2, replace = FALSE)
    datasets<- datasets[c(random.sample[1], random.sample[2])]
  }
  
  # Split list of 1000 datasets into groups of 5 for spBayes
  if(datasets.group == "a.1"){
    datasets<- datasets[1:5]
  }
  if(datasets.group == "a.2"){
    datasets<- datasets[6:10]
  }
  if(datasets.group == "a.3"){
    datasets<- datasets[11:15]
  }
  if(datasets.group == "a.4"){
    datasets<- datasets[16:20]
  }
  if(datasets.group == "a.5"){
    datasets<- datasets[21:25]
  }
  if(datasets.group == "a.6"){
    datasets<- datasets[26:30]
  }
  if(datasets.group == "a.7"){
    datasets<- datasets[31:35]
  }
  if(datasets.group == "a.8"){
    datasets<- datasets[36:40]
  }
  if(datasets.group == "a.9"){
    datasets<- datasets[41:45]
  }
  if(datasets.group == "a.10"){
    datasets<- datasets[46:50]
  }
  if(datasets.group == "a.11"){
    datasets<- datasets[51:55]
  }
  if(datasets.group == "a.12"){
    datasets<- datasets[56:60]
  }
  if(datasets.group == "a.13"){
    datasets<- datasets[61:65]
  }
  if(datasets.group == "a.14"){
    datasets<- datasets[66:70]
  }
  if(datasets.group == "a.15"){
    datasets<- datasets[71:75]
  }
  if(datasets.group == "a.16"){
    datasets<- datasets[76:80]
  }
  if(datasets.group == "a.17"){
    datasets<- datasets[81:85]
  }
  if(datasets.group == "a.18"){
    datasets<- datasets[86:90]
  }
  if(datasets.group == "a.19"){
    datasets<- datasets[91:95]
  }
  if(datasets.group == "a.20"){
    datasets<- datasets[96:100]
  }
  
  if(datasets.group == "b.1"){
    datasets<- datasets[101:105]
  }
  if(datasets.group == "b.2"){
    datasets<- datasets[106:110]
  }
  if(datasets.group == "b.3"){
    datasets<- datasets[111:115]
  }
  if(datasets.group == "b.4"){
    datasets<- datasets[116:120]
  }
  if(datasets.group == "b.5"){
    datasets<- datasets[121:125]
  }
  if(datasets.group == "b.6"){
    datasets<- datasets[126:130]
  }
  if(datasets.group == "b.7"){
    datasets<- datasets[131:135]
  }
  if(datasets.group == "b.8"){
    datasets<- datasets[136:140]
  }
  if(datasets.group == "b.9"){
    datasets<- datasets[141:145]
  }
  if(datasets.group == "b.10"){
    datasets<- datasets[146:150]
  }
  if(datasets.group == "b.11"){
    datasets<- datasets[151:155]
  }
  if(datasets.group == "b.12"){
    datasets<- datasets[156:160]
  }
  if(datasets.group == "b.13"){
    datasets<- datasets[161:165]
  }
  if(datasets.group == "b.14"){
    datasets<- datasets[166:170]
  }
  if(datasets.group == "b.15"){
    datasets<- datasets[171:175]
  }
  if(datasets.group == "b.16"){
    datasets<- datasets[176:180]
  }
  if(datasets.group == "b.17"){
    datasets<- datasets[181:185]
  }
  if(datasets.group == "b.18"){
    datasets<- datasets[186:190]
  }
  if(datasets.group == "b.19"){
    datasets<- datasets[191:195]
  }
  if(datasets.group == "b.20"){
    datasets<- datasets[196:200]
  }
  
  if(datasets.group == "c.1"){
    datasets<- datasets[201:205]
  }
  if(datasets.group == "c.2"){
    datasets<- datasets[206:210]
  }
  if(datasets.group == "c.3"){
    datasets<- datasets[211:215]
  }
  if(datasets.group == "c.4"){
    datasets<- datasets[216:220]
  }
  if(datasets.group == "c.5"){
    datasets<- datasets[221:225]
  }
  if(datasets.group == "c.6"){
    datasets<- datasets[226:230]
  }
  if(datasets.group == "c.7"){
    datasets<- datasets[231:235]
  }
  if(datasets.group == "c.8"){
    datasets<- datasets[236:240]
  }
  if(datasets.group == "c.9"){
    datasets<- datasets[241:245]
  }
  if(datasets.group == "c.10"){
    datasets<- datasets[246:250]
  }
  if(datasets.group == "c.11"){
    datasets<- datasets[251:255]
  }
  if(datasets.group == "c.12"){
    datasets<- datasets[256:260]
  }
  if(datasets.group == "c.13"){
    datasets<- datasets[261:265]
  }
  if(datasets.group == "c.14"){
    datasets<- datasets[266:270]
  }
  if(datasets.group == "c.15"){
    datasets<- datasets[271:275]
  }
  if(datasets.group == "c.16"){
    datasets<- datasets[276:280]
  }
  if(datasets.group == "c.17"){
    datasets<- datasets[281:285]
  }
  if(datasets.group == "c.18"){
    datasets<- datasets[286:290]
  }
  if(datasets.group == "c.19"){
    datasets<- datasets[291:295]
  }
  if(datasets.group == "c.20"){
    datasets<- datasets[296:300]
  }
  
  if(datasets.group == "d.1"){
    datasets<- datasets[301:305]
  }
  if(datasets.group == "d.2"){
    datasets<- datasets[306:310]
  }
  if(datasets.group == "d.3"){
    datasets<- datasets[311:315]
  }
  if(datasets.group == "d.4"){
    datasets<- datasets[316:320]
  }
  if(datasets.group == "d.5"){
    datasets<- datasets[321:325]
  }
  if(datasets.group == "d.6"){
    datasets<- datasets[326:330]
  }
  if(datasets.group == "d.7"){
    datasets<- datasets[331:335]
  }
  if(datasets.group == "d.8"){
    datasets<- datasets[336:340]
  }
  if(datasets.group == "d.9"){
    datasets<- datasets[341:345]
  }
  if(datasets.group == "d.10"){
    datasets<- datasets[346:350]
  }
  if(datasets.group == "d.11"){
    datasets<- datasets[351:355]
  }
  if(datasets.group == "d.12"){
    datasets<- datasets[356:360]
  }
  if(datasets.group == "d.13"){
    datasets<- datasets[361:365]
  }
  if(datasets.group == "d.14"){
    datasets<- datasets[366:370]
  }
  if(datasets.group == "d.15"){
    datasets<- datasets[371:375]
  }
  if(datasets.group == "d.16"){
    datasets<- datasets[376:380]
  }
  if(datasets.group == "d.17"){
    datasets<- datasets[381:385]
  }
  if(datasets.group == "d.18"){
    datasets<- datasets[386:390]
  }
  if(datasets.group == "d.19"){
    datasets<- datasets[391:395]
  }
  if(datasets.group == "d.20"){
    datasets<- datasets[396:400]
  }
  
  if(datasets.group == "e.1"){
    datasets<- datasets[401:405]
  }
  if(datasets.group == "e.2"){
    datasets<- datasets[406:410]
  }
  if(datasets.group == "e.3"){
    datasets<- datasets[411:415]
  }
  if(datasets.group == "e.4"){
    datasets<- datasets[416:420]
  }
  if(datasets.group == "e.5"){
    datasets<- datasets[421:425]
  }
  if(datasets.group == "e.6"){
    datasets<- datasets[426:430]
  }
  if(datasets.group == "e.7"){
    datasets<- datasets[431:435]
  }
  if(datasets.group == "e.8"){
    datasets<- datasets[436:440]
  }
  if(datasets.group == "e.9"){
    datasets<- datasets[441:445]
  }
  if(datasets.group == "e.10"){
    datasets<- datasets[446:450]
  }
  if(datasets.group == "e.11"){
    datasets<- datasets[451:455]
  }
  if(datasets.group == "e.12"){
    datasets<- datasets[456:460]
  }
  if(datasets.group == "e.13"){
    datasets<- datasets[461:465]
  }
  if(datasets.group == "e.14"){
    datasets<- datasets[466:470]
  }
  if(datasets.group == "e.15"){
    datasets<- datasets[471:475]
  }
  if(datasets.group == "e.16"){
    datasets<- datasets[476:480]
  }
  if(datasets.group == "e.17"){
    datasets<- datasets[481:485]
  }
  if(datasets.group == "e.18"){
    datasets<- datasets[486:490]
  }
  if(datasets.group == "e.19"){
    datasets<- datasets[491:495]
  }
  if(datasets.group == "e.20"){
    datasets<- datasets[496:500]
  }
  
  if(datasets.group == "f.1"){
    datasets<- datasets[501:505]
  }
  if(datasets.group == "f.2"){
    datasets<- datasets[506:510]
  }
  if(datasets.group == "f.3"){
    datasets<- datasets[511:515]
  }
  if(datasets.group == "f.4"){
    datasets<- datasets[516:520]
  }
  if(datasets.group == "f.5"){
    datasets<- datasets[521:525]
  }
  if(datasets.group == "f.6"){
    datasets<- datasets[526:530]
  }
  if(datasets.group == "f.7"){
    datasets<- datasets[531:535]
  }
  if(datasets.group == "f.8"){
    datasets<- datasets[536:540]
  }
  if(datasets.group == "f.9"){
    datasets<- datasets[541:545]
  }
  if(datasets.group == "f.10"){
    datasets<- datasets[546:550]
  }
  if(datasets.group == "f.11"){
    datasets<- datasets[551:555]
  }
  if(datasets.group == "f.12"){
    datasets<- datasets[556:560]
  }
  if(datasets.group == "f.13"){
    datasets<- datasets[561:565]
  }
  if(datasets.group == "f.14"){
    datasets<- datasets[566:570]
  }
  if(datasets.group == "f.15"){
    datasets<- datasets[571:575]
  }
  if(datasets.group == "f.16"){
    datasets<- datasets[576:580]
  }
  if(datasets.group == "f.17"){
    datasets<- datasets[581:585]
  }
  if(datasets.group == "f.18"){
    datasets<- datasets[586:590]
  }
  if(datasets.group == "f.19"){
    datasets<- datasets[591:595]
  }
  if(datasets.group == "f.20"){
    datasets<- datasets[596:600]
  }
  
  if(datasets.group == "g.1"){
    datasets<- datasets[601:605]
  }
  if(datasets.group == "g.2"){
    datasets<- datasets[606:610]
  }
  if(datasets.group == "g.3"){
    datasets<- datasets[611:615]
  }
  if(datasets.group == "g.4"){
    datasets<- datasets[616:620]
  }
  if(datasets.group == "g.5"){
    datasets<- datasets[621:625]
  }
  if(datasets.group == "g.6"){
    datasets<- datasets[626:630]
  }
  if(datasets.group == "g.7"){
    datasets<- datasets[631:635]
  }
  if(datasets.group == "g.8"){
    datasets<- datasets[636:640]
  }
  if(datasets.group == "g.9"){
    datasets<- datasets[641:645]
  }
  if(datasets.group == "g.10"){
    datasets<- datasets[646:650]
  }
  if(datasets.group == "g.11"){
    datasets<- datasets[651:655]
  }
  if(datasets.group == "g.12"){
    datasets<- datasets[656:660]
  }
  if(datasets.group == "g.13"){
    datasets<- datasets[661:665]
  }
  if(datasets.group == "g.14"){
    datasets<- datasets[666:670]
  }
  if(datasets.group == "g.15"){
    datasets<- datasets[671:675]
  }
  if(datasets.group == "g.16"){
    datasets<- datasets[676:680]
  }
  if(datasets.group == "g.17"){
    datasets<- datasets[681:685]
  }
  if(datasets.group == "g.18"){
    datasets<- datasets[686:690]
  }
  if(datasets.group == "g.19"){
    datasets<- datasets[691:695]
  }
  if(datasets.group == "g.20"){
    datasets<- datasets[696:700]
  }
  
  if(datasets.group == "h.1"){
    datasets<- datasets[701:705]
  }
  if(datasets.group == "h.2"){
    datasets<- datasets[706:710]
  }
  if(datasets.group == "h.3"){
    datasets<- datasets[711:715]
  }
  if(datasets.group == "h.4"){
    datasets<- datasets[716:720]
  }
  if(datasets.group == "h.5"){
    datasets<- datasets[721:725]
  }
  if(datasets.group == "h.6"){
    datasets<- datasets[726:730]
  }
  if(datasets.group == "h.7"){
    datasets<- datasets[731:735]
  }
  if(datasets.group == "h.8"){
    datasets<- datasets[736:740]
  }
  if(datasets.group == "h.9"){
    datasets<- datasets[741:745]
  }
  if(datasets.group == "h.10"){
    datasets<- datasets[746:750]
  }
  if(datasets.group == "h.11"){
    datasets<- datasets[751:755]
  }
  if(datasets.group == "h.12"){
    datasets<- datasets[756:760]
  }
  if(datasets.group == "h.13"){
    datasets<- datasets[761:765]
  }
  if(datasets.group == "h.14"){
    datasets<- datasets[766:770]
  }
  if(datasets.group == "h.15"){
    datasets<- datasets[771:775]
  }
  if(datasets.group == "h.16"){
    datasets<- datasets[776:780]
  }
  if(datasets.group == "h.17"){
    datasets<- datasets[781:785]
  }
  if(datasets.group == "h.18"){
    datasets<- datasets[786:790]
  }
  if(datasets.group == "h.19"){
    datasets<- datasets[791:795]
  }
  if(datasets.group == "h.20"){
    datasets<- datasets[796:800]
  }
  
  
  if(datasets.group == "i.1"){
    datasets<- datasets[801:805]
  }
  if(datasets.group == "i.2"){
    datasets<- datasets[806:810]
  }
  if(datasets.group == "i.3"){
    datasets<- datasets[811:815]
  }
  if(datasets.group == "i.4"){
    datasets<- datasets[816:820]
  }
  if(datasets.group == "i.5"){
    datasets<- datasets[821:825]
  }
  if(datasets.group == "i.6"){
    datasets<- datasets[826:830]
  }
  if(datasets.group == "i.7"){
    datasets<- datasets[831:835]
  }
  if(datasets.group == "i.8"){
    datasets<- datasets[836:840]
  }
  if(datasets.group == "i.9"){
    datasets<- datasets[841:845]
  }
  if(datasets.group == "i.10"){
    datasets<- datasets[846:850]
  }
  if(datasets.group == "i.11"){
    datasets<- datasets[851:855]
  }
  if(datasets.group == "i.12"){
    datasets<- datasets[856:860]
  }
  if(datasets.group == "i.13"){
    datasets<- datasets[861:865]
  }
  if(datasets.group == "i.14"){
    datasets<- datasets[866:870]
  }
  if(datasets.group == "i.15"){
    datasets<- datasets[871:875]
  }
  if(datasets.group == "i.16"){
    datasets<- datasets[876:880]
  }
  if(datasets.group == "i.17"){
    datasets<- datasets[881:885]
  }
  if(datasets.group == "i.18"){
    datasets<- datasets[886:890]
  }
  if(datasets.group == "i.19"){
    datasets<- datasets[891:895]
  }
  if(datasets.group == "i.20"){
    datasets<- datasets[896:900]
  }
  
  
  if(datasets.group == "j.1"){
    datasets<- datasets[901:905]
  }
  if(datasets.group == "j.2"){
    datasets<- datasets[906:910]
  }
  if(datasets.group == "j.3"){
    datasets<- datasets[911:915]
  }
  if(datasets.group == "j.4"){
    datasets<- datasets[916:920]
  }
  if(datasets.group == "j.5"){
    datasets<- datasets[921:925]
  }
  if(datasets.group == "j.6"){
    datasets<- datasets[926:930]
  }
  if(datasets.group == "j.7"){
    datasets<- datasets[931:935]
  }
  if(datasets.group == "j.8"){
    datasets<- datasets[936:940]
  }
  if(datasets.group == "j.9"){
    datasets<- datasets[941:945]
  }
  if(datasets.group == "j.10"){
    datasets<- datasets[946:950]
  }
  if(datasets.group == "j.11"){
    datasets<- datasets[951:955]
  }
  if(datasets.group == "j.12"){
    datasets<- datasets[956:960]
  }
  if(datasets.group == "j.13"){
    datasets<- datasets[961:965]
  }
  if(datasets.group == "j.14"){
    datasets<- datasets[966:970]
  }
  if(datasets.group == "j.15"){
    datasets<- datasets[971:975]
  }
  if(datasets.group == "j.16"){
    datasets<- datasets[976:980]
  }
  if(datasets.group == "j.17"){
    datasets<- datasets[981:985]
  }
  if(datasets.group == "j.18"){
    datasets<- datasets[986:990]
  }
  if(datasets.group == "j.19"){
    datasets<- datasets[991:995]
  }
  if(datasets.group == "j.20"){
    datasets<- datasets[996:1000]
  }
  
  # Create dataframe, "results" to store simulation results.  
  if(model.fit != "custom") {
    n.predictors<- length(attr(terms(as.formula(model.formula)), "term.labels"))
    all.subsets.list<- vector("list", length = length(datasets))
    if(n.predictors == 1) {
      results<- data.frame(matrix(nrow = length(datasets), ncol = 28))
      names(results)[1:28]<- c("intercept", "intercept.z", "intercept.p", "intercept.se", "x1", "x1.z", "x1.p", "x1.se", "deviance.explained", "mod.nll", "null.nll", "lrtest", "full.kappa", "full.auc", "full.ccc.mean", "full.ccc.low", "full.ccc.up", "omission.err.full", "commission.err.full", "split.kappa", "split.auc", "split.ccc.mean", "split.ccc.low", "split.ccc.up", "omission.err.split", "commission.err.split", "AICc", "SPEV")
      results$datasets.group<- rep(datasets.group, length(datasets))
    }
    if(n.predictors == 2) {
      results<- data.frame(matrix(nrow = length(datasets), ncol = 32))
      names(results)[1:32]<- c("intercept", "intercept.z", "intercept.p", "intercept.se", "x1", "x1.z", "x1.p", "x1.se", "random", "random.z", "random.p", "random.se", "deviance.explained", "mod.nll", "null.nll", "lrtest", "full.kappa", "full.auc", "full.ccc.mean", "full.ccc.low", "full.ccc.up", "omission.err.full", "commission.err.full", "split.kappa", "split.auc", "split.ccc.mean", "split.ccc.low", "split.ccc.up", "omission.err.split", "commission.err.split", "AICc", "SPEV")
      results$datasets.group<- rep(datasets.group, length(datasets))
    }
    if(n.predictors == 3) {
      results<- data.frame(matrix(nrow = length(datasets), ncol = 36))
      names(results)[1:36]<- c("intercept", "intercept.z", "intercept.p", "intercept.se", "x1", "x1.z", "x1.p", "x1.se", "x2", "x2.z", "x2.p", "x2.se", "random", "random.z", "random.p", "random.se", "deviance.explained", "mod.nll", "null.nll", "lrtest", "full.kappa", "full.auc", "full.ccc.mean", "full.ccc.low", "full.ccc.up", "omission.err.full", "commission.err.full", "split.kappa", "split.auc", "split.ccc.mean", "split.ccc.low", "split.ccc.up", "omission.err.split", "commission.err.split", "AICc", "SPEV")
      results$datasets.group<- rep(datasets.group, length(datasets))
    }
    if(n.predictors == 4) {
      results<- data.frame(matrix(nrow = length(datasets), ncol = 40))
      names(results)[1:40]<- c("intercept", "intercept.z", "intercept.p", "intercept.se", "x1", "x1.z", "x1.p", "x1.se", "x2", "x2.z", "x2.p", "x2.se", "dslope", "dslope.z", "dslope.p", "dslope.se", "random", "random.z", "random.p", "random.se", "deviance.explained", "mod.nll", "null.nll", "lrtest", "full.kappa", "full.auc", "full.ccc.mean", "full.ccc.low", "full.ccc.up", "omission.err.full", "commission.err.full", "split.kappa", "split.auc", "split.ccc.mean", "split.ccc.low", "split.ccc.up", "omission.err.split", "commission.err.split", "AICc", "SPEV")
      results$datasets.group<- rep(datasets.group, length(datasets))
    }
  }
  
  if(model.fit == "custom") {
    n.predictors<- length(attr(terms(as.formula(model.formula)), "term.labels"))
    
    if(custom.fit == "neg.exp.free.full") {
      results<- data.frame(matrix(nrow = length(datasets), ncol = 40))
      names(results)[1:40]<- c("intercept", "intercept.z", "intercept.p", "intercept.se", "x1", "x1.z", "x1.p", "x1.se", "x2", "x2.z", "x2.p", "x2.se", "dslope", "dslope.z", "dslope.p", "dslope.se", "random", "random.z", "random.p", "random.se", "neg.exp.param","deviance.explained", "mod.nll", "null.nll", "lrtest", "full.kappa", "full.auc", "full.ccc.mean", "full.ccc.low", "full.ccc.up", "omission.err.full", "commission.err.full", "split.kappa", "split.auc", "split.ccc.mean", "split.ccc.low", "split.ccc.up", "omission.err.split", "commission.err.split", "AICc")
      results$datasets.group<- rep(datasets.group, length(datasets))
    }
    
    if(custom.fit == "neg.exp.free.red") {
      results<- data.frame(matrix(nrow = length(datasets), ncol = 36))
      names(results)[1:36]<- c("intercept", "intercept.z", "intercept.p", "intercept.se", "x1", "x1.z", "x1.p", "x1.se", "dslope", "dslope.z", "dslope.p", "dslope.se", "random", "random.z", "random.p", "random.se", "neg.exp.param","deviance.explained", "mod.nll", "null.nll", "lrtest", "full.kappa", "full.auc", "full.ccc.mean", "full.ccc.low", "full.ccc.up", "omission.err.full", "commission.err.full", "split.kappa", "split.auc", "split.ccc.mean", "split.ccc.low", "split.ccc.up", "omission.err.split", "commission.err.split", "AICc")
      results$datasets.group<- rep(datasets.group, length(datasets))
    }
    
    if(custom.fit == "gauss.free.full") {
      results<- data.frame(matrix(nrow = length(datasets), ncol = 41))
      names(results)[1:41]<- c("intercept", "intercept.z", "intercept.p", "intercept.se", "x1", "x1.z", "x1.p", "x1.se", "x2", "x2.z", "x2.p", "x2.se", "dslope", "dslope.z", "dslope.p", "dslope.se", "random", "random.z", "random.p", "random.se", "mu", "sigma", "deviance.explained", "mod.nll", "null.nll", "lrtest", "full.kappa", "full.auc", "full.ccc.mean", "full.ccc.low", "full.ccc.up", "omission.err.full", "commission.err.full", "split.kappa", "split.auc", "split.ccc.mean", "split.ccc.low", "split.ccc.up", "omission.err.split", "commission.err.split", "AICc")
      results$datasets.group<- rep(datasets.group, length(datasets))
    }
    
    if(custom.fit == "gauss.free.red") {
      results<- data.frame(matrix(nrow = length(datasets), ncol = 37))
      names(results)[1:37]<- c("intercept", "intercept.z", "intercept.p", "intercept.se", "x1", "x1.z", "x1.p", "x1.se", "dslope", "dslope.z", "dslope.p", "dslope.se", "random", "random.z", "random.p", "random.se", "mu", "sigma", "deviance.explained", "mod.nll", "null.nll", "lrtest", "full.kappa", "full.auc", "full.ccc.mean", "full.ccc.low", "full.ccc.up", "omission.err.full", "commission.err.full", "split.kappa", "split.auc", "split.ccc.mean", "split.ccc.low", "split.ccc.up", "omission.err.split", "commission.err.split", "AICc")
      results$datasets.group<- rep(datasets.group, length(datasets))
    }
  }
  
  ## Housekeeping for calculating Lin's CCC and Moran's I on residuals 
  # Lin's CCC
  bins<- seq(from = 0, to = 1, by = 0.2)
  midpts<- seq(from = 0.1, to = 1, by = 0.2)
  
  # Morans I 
  morans.bins<- seq(from = bin.size, to = ((grid.side*cell.size)*(1/2)), by = bin.size) 
  morans.i<- data.frame(matrix(nrow = length(morans.bins)*length(datasets), ncol = 3))
  names(morans.i)[1:3]<- c("boot.iter", "dist.bin", "moransI")
  
  # Selection index and bootstrap id for help with tracking and storing results
  si<- 1

  boot.id.sequence<- switch(datasets.group,
                   a = seq(from = 1, to = 100, by = 1),
                   b = seq(from = 101, to = 200, by = 1),
                   c = seq(from = 201, to = 300, by = 1),
                   d = seq(from = 301, to = 400, by = 1),
                   e = seq(from = 401, to = 500, by = 1),
                   f = seq(from = 501, to = 600, by = 1),
                   g = seq(from = 601, to = 700, by = 1),
                   h = seq(from = 701, to = 800, by = 1),
                   i = seq(from = 801, to = 900, by = 1),
                   j = seq(from = 901, to = 1000, by = 1),
                   
                   # For spglm
                   a.1 = seq(from = 1, to = 5, by = 1),
                   a.2 = seq(from = 6, to = 10, by = 1),
                   a.3 = seq(from = 11, to = 15, by = 1),
                   a.4 = seq(from = 16, to = 20, by = 1),
                   a.5 = seq(from = 21, to = 25, by = 1),
                   a.6 = seq(from = 26, to = 30, by = 1),
                   a.7 = seq(from = 31, to = 35, by = 1),
                   a.8 = seq(from = 36, to = 40, by = 1),
                   a.9 = seq(from = 41, to = 45, by = 1),
                   a.10 = seq(from = 46, to = 50, by = 1),
                   a.11 = seq(from = 51, to = 55, by = 1),
                   a.12 = seq(from = 56, to = 60, by = 1),
                   a.13 = seq(from = 61, to = 65, by = 1),
                   a.14 = seq(from = 66, to = 70, by = 1),
                   a.15 = seq(from = 71, to = 75, by = 1),
                   a.16 = seq(from = 76, to = 80, by = 1),
                   a.17 = seq(from = 81, to = 85, by = 1),
                   a.18 = seq(from = 86, to = 90, by = 1),
                   a.19 = seq(from = 91, to = 95, by = 1),
                   a.20 = seq(from = 96, to = 100, by = 1),
                   
                   b.1 = seq(from = 101, to = 105, by = 1),
                   b.2 = seq(from = 106, to = 110, by = 1),
                   b.3 = seq(from = 111, to = 115, by = 1),
                   b.4 = seq(from = 116, to = 120, by = 1),
                   b.5 = seq(from = 121, to = 125, by = 1),
                   b.6 = seq(from = 126, to = 130, by = 1),
                   b.7 = seq(from = 131, to = 135, by = 1),
                   b.8 = seq(from = 136, to = 140, by = 1),
                   b.9 = seq(from = 141, to = 145, by = 1),
                   b.10 = seq(from = 146, to = 150, by = 1),
                   b.11 = seq(from = 151, to = 155, by = 1),
                   b.12 = seq(from = 156, to = 160, by = 1),
                   b.13 = seq(from = 161, to = 165, by = 1),
                   b.14 = seq(from = 166, to = 170, by = 1),
                   b.15 = seq(from = 171, to = 175, by = 1),
                   b.16 = seq(from = 176, to = 180, by = 1),
                   b.17 = seq(from = 181, to = 185, by = 1),
                   b.18 = seq(from = 186, to = 190, by = 1),
                   b.19 = seq(from = 191, to = 195, by = 1),
                   b.20 = seq(from = 196, to = 200, by = 1),
                  
                   c.1 = seq(from = 201, to = 205, by = 1),
                   c.2 = seq(from = 206, to = 210, by = 1),
                   c.3 = seq(from = 211, to = 215, by = 1),
                   c.4 = seq(from = 216, to = 220, by = 1),
                   c.5 = seq(from = 221, to = 225, by = 1),
                   c.6 = seq(from = 226, to = 230, by = 1),
                   c.7 = seq(from = 231, to = 235, by = 1),
                   c.8 = seq(from = 236, to = 240, by = 1),
                   c.9 = seq(from = 241, to = 245, by = 1),
                   c.10 = seq(from = 246, to = 250, by = 1),
                   c.11 = seq(from = 251, to = 255, by = 1),
                   c.12 = seq(from = 256, to = 260, by = 1),
                   c.13 = seq(from = 261, to = 265, by = 1),
                   c.14 = seq(from = 266, to = 270, by = 1),
                   c.15 = seq(from = 271, to = 275, by = 1),
                   c.16 = seq(from = 276, to = 280, by = 1),
                   c.17 = seq(from = 281, to = 285, by = 1),
                   c.18 = seq(from = 286, to = 290, by = 1),
                   c.19 = seq(from = 291, to = 295, by = 1),
                   c.20 = seq(from = 296, to = 300, by = 1),
                   
                   d.1 = seq(from = 301, to = 305, by = 1),
                   d.2 = seq(from = 306, to = 310, by = 1),
                   d.3 = seq(from = 311, to = 315, by = 1),
                   d.4 = seq(from = 316, to = 320, by = 1),
                   d.5 = seq(from = 321, to = 325, by = 1),
                   d.6 = seq(from = 326, to = 330, by = 1),
                   d.7 = seq(from = 331, to = 335, by = 1),
                   d.8 = seq(from = 336, to = 340, by = 1),
                   d.9 = seq(from = 341, to = 345, by = 1),
                   d.10 = seq(from = 346, to = 350, by = 1),
                   d.11 = seq(from = 351, to = 355, by = 1),
                   d.12 = seq(from = 356, to = 360, by = 1),
                   d.13 = seq(from = 361, to = 365, by = 1),
                   d.14 = seq(from = 366, to = 370, by = 1),
                   d.15 = seq(from = 371, to = 375, by = 1),
                   d.16 = seq(from = 376, to = 380, by = 1),
                   d.17 = seq(from = 381, to = 385, by = 1),
                   d.18 = seq(from = 386, to = 390, by = 1),
                   d.19 = seq(from = 391, to = 395, by = 1),
                   d.20 = seq(from = 396, to = 400, by = 1),
                   
                   e.1 = seq(from = 401, to = 405, by = 1),
                   e.2 = seq(from = 406, to = 410, by = 1),
                   e.3 = seq(from = 411, to = 415, by = 1),
                   e.4 = seq(from = 416, to = 420, by = 1),
                   e.5 = seq(from = 421, to = 425, by = 1),
                   e.6 = seq(from = 426, to = 430, by = 1),
                   e.7 = seq(from = 431, to = 435, by = 1),
                   e.8 = seq(from = 436, to = 440, by = 1),
                   e.9 = seq(from = 441, to = 445, by = 1),
                   e.10 = seq(from = 446, to = 450, by = 1),
                   e.11 = seq(from = 451, to = 455, by = 1),
                   e.12 = seq(from = 456, to = 460, by = 1),
                   e.13 = seq(from = 461, to = 465, by = 1),
                   e.14 = seq(from = 466, to = 470, by = 1),
                   e.15 = seq(from = 471, to = 475, by = 1),
                   e.16 = seq(from = 476, to = 480, by = 1),
                   e.17 = seq(from = 481, to = 485, by = 1),
                   e.18 = seq(from = 486, to = 490, by = 1),
                   e.19 = seq(from = 491, to = 495, by = 1),
                   e.20 = seq(from = 496, to = 500, by = 1),
                   
                   f.1 = seq(from = 501, to = 505, by = 1),
                   f.2 = seq(from = 506, to = 510, by = 1),
                   f.3 = seq(from = 511, to = 515, by = 1),
                   f.4 = seq(from = 516, to = 520, by = 1),
                   f.5 = seq(from = 521, to = 525, by = 1),
                   f.6 = seq(from = 526, to = 530, by = 1),
                   f.7 = seq(from = 531, to = 535, by = 1),
                   f.8 = seq(from = 536, to = 540, by = 1),
                   f.9 = seq(from = 541, to = 545, by = 1),
                   f.10 = seq(from = 546, to = 550, by = 1),
                   f.11 = seq(from = 551, to = 555, by = 1),
                   f.12 = seq(from = 556, to = 560, by = 1),
                   f.13 = seq(from = 561, to = 565, by = 1),
                   f.14 = seq(from = 566, to = 570, by = 1),
                   f.15 = seq(from = 571, to = 575, by = 1),
                   f.16 = seq(from = 576, to = 580, by = 1),
                   f.17 = seq(from = 581, to = 585, by = 1),
                   f.18 = seq(from = 586, to = 590, by = 1),
                   f.19 = seq(from = 591, to = 595, by = 1),
                   f.20 = seq(from = 596, to = 600, by = 1),
                   
                   g.1 = seq(from = 601, to = 605, by = 1),
                   g.2 = seq(from = 606, to = 610, by = 1),
                   g.3 = seq(from = 611, to = 615, by = 1),
                   g.4 = seq(from = 616, to = 620, by = 1),
                   g.5 = seq(from = 621, to = 625, by = 1),
                   g.6 = seq(from = 626, to = 630, by = 1),
                   g.7 = seq(from = 631, to = 635, by = 1),
                   g.8 = seq(from = 636, to = 640, by = 1),
                   g.9 = seq(from = 641, to = 645, by = 1),
                   g.10 = seq(from = 646, to = 650, by = 1),
                   g.11 = seq(from = 651, to = 655, by = 1),
                   g.12 = seq(from = 656, to = 660, by = 1),
                   g.13 = seq(from = 661, to = 665, by = 1),
                   g.14 = seq(from = 666, to = 670, by = 1),
                   g.15 = seq(from = 671, to = 675, by = 1),
                   g.16 = seq(from = 676, to = 680, by = 1),
                   g.17 = seq(from = 681, to = 685, by = 1),
                   g.18 = seq(from = 686, to = 690, by = 1),
                   g.19 = seq(from = 691, to = 695, by = 1),
                   g.20 = seq(from = 696, to = 700, by = 1),
                   
                   h.1 = seq(from = 701, to = 705, by = 1),
                   h.2 = seq(from = 706, to = 710, by = 1),
                   h.3 = seq(from = 711, to = 715, by = 1),
                   h.4 = seq(from = 716, to = 720, by = 1),
                   h.5 = seq(from = 721, to = 725, by = 1),
                   h.6 = seq(from = 726, to = 730, by = 1),
                   h.7 = seq(from = 731, to = 735, by = 1),
                   h.8 = seq(from = 736, to = 740, by = 1),
                   h.9 = seq(from = 741, to = 745, by = 1),
                   h.10 = seq(from = 746, to = 750, by = 1),
                   h.11 = seq(from = 751, to = 755, by = 1),
                   h.12 = seq(from = 756, to = 760, by = 1),
                   h.13 = seq(from = 761, to = 765, by = 1),
                   h.14 = seq(from = 766, to = 770, by = 1),
                   h.15 = seq(from = 771, to = 775, by = 1),
                   h.16 = seq(from = 776, to = 780, by = 1),
                   h.17 = seq(from = 781, to = 785, by = 1),
                   h.18 = seq(from = 786, to = 790, by = 1),
                   h.19 = seq(from = 791, to = 795, by = 1),
                   h.20 = seq(from = 796, to = 800, by = 1),
                   
                   i.1 = seq(from = 801, to = 805, by = 1),
                   i.2 = seq(from = 806, to = 810, by = 1),
                   i.3 = seq(from = 811, to = 815, by = 1),
                   i.4 = seq(from = 816, to = 820, by = 1),
                   i.5 = seq(from = 821, to = 825, by = 1),
                   i.6 = seq(from = 826, to = 830, by = 1),
                   i.7 = seq(from = 831, to = 835, by = 1),
                   i.8 = seq(from = 836, to = 840, by = 1),
                   i.9 = seq(from = 841, to = 845, by = 1),
                   i.10 = seq(from = 846, to = 850, by = 1),
                   i.11 = seq(from = 851, to = 855, by = 1),
                   i.12 = seq(from = 856, to = 860, by = 1),
                   i.13 = seq(from = 861, to = 865, by = 1),
                   i.14 = seq(from = 866, to = 870, by = 1),
                   i.15 = seq(from = 871, to = 875, by = 1),
                   i.16 = seq(from = 876, to = 880, by = 1),
                   i.17 = seq(from = 881, to = 885, by = 1),
                   i.18 = seq(from = 886, to = 890, by = 1),
                   i.19 = seq(from = 891, to = 895, by = 1),
                   i.20 = seq(from = 896, to = 900, by = 1),
                   
                   j.1 = seq(from = 901, to = 905, by = 1),
                   j.2 = seq(from = 906, to = 910, by = 1),
                   j.3 = seq(from = 911, to = 915, by = 1),
                   j.4 = seq(from = 916, to = 920, by = 1),
                   j.5 = seq(from = 921, to = 925, by = 1),
                   j.6 = seq(from = 926, to = 930, by = 1),
                   j.7 = seq(from = 931, to = 935, by = 1),
                   j.8 = seq(from = 936, to = 940, by = 1),
                   j.9 = seq(from = 941, to = 945, by = 1),
                   j.10 = seq(from = 946, to = 950, by = 1),
                   j.11 = seq(from = 951, to = 955, by = 1),
                   j.12 = seq(from = 956, to = 960, by = 1),
                   j.13 = seq(from = 961, to = 965, by = 1),
                   j.14 = seq(from = 966, to = 970, by = 1),
                   j.15 = seq(from = 971, to = 975, by = 1),
                   j.16 = seq(from = 976, to = 980, by = 1),
                   j.17 = seq(from = 981, to = 985, by = 1),
                   j.18 = seq(from = 986, to = 990, by = 1),
                   j.19 = seq(from = 991, to = 995, by = 1),
                   j.20 = seq(from = 996, to = 1000, by = 1),
                   
                   Dormann = seq(from = 1, to = 10, by = 1),
                   conv.check = seq(from = 1, to = 2, by = 1))

  ##### End Set up 
  ############################################
  
  ##### Start fitting models and storing results
  
  ### Fit GLM or GLM plus autocovariate term
  if(model.fit == "glm" || model.fit == "glm.autocov" || model.fit == "glm.rac") {
    #print("glm")
    
    ## Loop through each dataset in the list of datasets
    for (k in seq(along=datasets)) {
      
      # Get the dataset and then split for two-fold cross validation
      boot.id<- boot.id.sequence[k]
      dataset.temp<- datasets[[k]]
      sample.size<- nrow(dataset.temp)
      dataset.temp.split<- sample(dataset.temp$id, size = sample.size/2)
      training1<- dataset.temp[dataset.temp.split, ]
      training2<- dataset.temp[-dataset.temp.split, ]
      
      # Prediction terms
      terms.predict<- attr(terms(as.formula(model.formula)),"term.labels")
      
      # Calculate inverse distance weighted autocovariate, if needed
      if(model.fit == "glm.autocov") {
        suppressWarnings(library(spdep))
        suppressWarnings(library(sp))
        nb.list <- dnearneigh(as.matrix(dataset.temp[,c("x", "y")]), 0, dnn.max) 
        nb.weights <- nb2listw(nb.list, style = "B", zero.policy = TRUE)
        coords<-as.matrix(cbind(dataset.temp$x, dataset.temp$y))
        dataset.temp$x.autocov <- suppressWarnings(autocov_dist(dataset.temp$zero.one, coords, nbs = 1, type = "inverse", style = "B", zero.policy = TRUE))
        
        # Trainging 1
        nb.list.t1 <- dnearneigh(as.matrix(training1[,c("x", "y")]), 0, dnn.max) 
        nb.weights.t1 <- nb2listw(nb.list.t1, style = "B", zero.policy = TRUE)
        coords.t1 <- as.matrix(cbind(training1$x, training1$y))
        training1$x.autocov <- suppressWarnings(autocov_dist(training1$zero.one, coords.t1, nbs = 1, type = "inverse", style = "B", zero.policy = TRUE))
        
        # Trainging 2
        nb.list.t2 <- dnearneigh(as.matrix(training2[,c("x", "y")]), 0, dnn.max) 
        nb.weights.t2 <- nb2listw(nb.list.t2, style = "B", zero.policy = TRUE)
        coords.t2 <- as.matrix(cbind(training2$x, training2$y))
        training2$x.autocov <- suppressWarnings(autocov_dist(training2$zero.one, coords.t2, nbs = 1, type = "inverse", style = "B", zero.policy = TRUE))  
      }
      
      # Fit glm and glm with an autocovariate
      if(model.fit == "glm" || model.fit == "glm.autocov") {
        mod<- suppressWarnings(glm(model.formula, data = dataset.temp, family = binomial(link=logit), maxit = 1000))
        mod.train1<- suppressWarnings(glm(model.formula, data = training1, family = binomial(link=logit), maxit = 1000))
        mod.train2<- suppressWarnings(glm(model.formula, data = training2, family = binomial(link=logit), maxit = 1000))
      }
      
      # If intercept.se > 25, indicating bad fit, go to next dataset
      if(as.numeric(as.character(coef(summary(mod))[1,2])) > 25) {
        next
      }
      
      # For good fits, store model parameter estimates, SE, z and p values 
      nullmod <- glm(zero.one ~ 1, data=dataset.temp, family=binomial)
      results$intercept[k]<- as.numeric(as.character(coefficients(mod)[1]))
      results$intercept.z[k]<- as.numeric(as.character(coef(summary(mod))[1,3]))
      results$intercept.p[k]<- as.numeric(as.character(coef(summary(mod))[1,4]))
      results$intercept.se[k]<- as.numeric(as.character(coef(summary(mod))[1,2]))
      results$x1[k]<- as.numeric(as.character(coefficients(mod)[2]))
      results$x1.z[k]<- as.numeric(as.character(coef(summary(mod))[2,3]))
      results$x1.p[k]<- as.numeric(as.character(coef(summary(mod))[2,4]))
      results$x1.se[k]<- as.numeric(as.character(coef(summary(mod))[2,2]))
      
      if(n.predictors == 2) {
        results$random[k]<- as.numeric(as.character(coefficients(mod)[3]))
        results$random.z[k]<- as.numeric(as.character(coef(summary(mod))[3,3]))
        results$random.p[k]<- as.numeric(as.character(coef(summary(mod))[3,4]))
        results$random.se[k]<- as.numeric(as.character(coef(summary(mod))[3,2]))
      }
      
      if(n.predictors == 3) {
        if(any(terms.predict == "x.autocov") && any(is.na(as.numeric(as.character(coefficients(mod)))))) {
          results$x2[k]<- "NA"
          results$x2.z[k]<- "NA"
          results$x2.p[k]<- "NA"
          results$x2.se[k]<- "NA"
          results$random[k]<- as.numeric(as.character(coefficients(mod)[4]))
          results$random.z[k]<- as.numeric(as.character(coef(summary(mod))[3,3]))
          results$random.p[k]<- as.numeric(as.character(coef(summary(mod))[3,4]))
          results$random.se[k]<- as.numeric(as.character(coef(summary(mod))[3,2]))
        } else {
          results$x2[k]<- as.numeric(as.character(coefficients(mod)[3]))
          results$x2.z[k]<- as.numeric(as.character(coef(summary(mod))[3,3]))
          results$x2.p[k]<- as.numeric(as.character(coef(summary(mod))[3,4]))
          results$x2.se[k]<- as.numeric(as.character(coef(summary(mod))[3,2]))
          results$random[k]<- as.numeric(as.character(coefficients(mod)[4]))
          results$random.z[k]<- as.numeric(as.character(coef(summary(mod))[4,3]))
          results$random.p[k]<- as.numeric(as.character(coef(summary(mod))[4,4]))
          results$random.se[k]<- as.numeric(as.character(coef(summary(mod))[4,2]))
        }
      }
      
      if(n.predictors == 4) {
        results$x2[k]<- as.numeric(as.character(coefficients(mod)[3]))
        results$x2.z[k]<- as.numeric(as.character(coef(summary(mod))[3,3]))
        results$x2.p[k]<- as.numeric(as.character(coef(summary(mod))[3,4]))
        results$x2.se[k]<- as.numeric(as.character(coef(summary(mod))[3,2]))
        
        if(any(is.na(as.numeric(as.character(coefficients(mod)))))) {
          results$dslope[k]<- "NA"
          results$dslope.z[k]<- "NA"
          results$dslope.p[k]<- "NA"
          results$dslope.se[k]<- "NA"
        } else {
          results$dslope[k]<- as.numeric(as.character(coefficients(mod)[4]))
          results$dslope.z[k]<- as.numeric(as.character(coef(summary(mod))[4,3]))
          results$dslope.p[k]<- as.numeric(as.character(coef(summary(mod))[4,4]))
          results$dslope.se[k]<- as.numeric(as.character(coef(summary(mod))[4,2]))
          results$random[k]<- as.numeric(as.character(coefficients(mod)[5]))
          results$random.z[k]<- as.numeric(as.character(coef(summary(mod))[5,3]))
          results$random.p[k]<- as.numeric(as.character(coef(summary(mod))[5,4]))
          results$random.se[k]<- as.numeric(as.character(coef(summary(mod))[5,2]))
        }
      }
      
      # Calculating and storing model selection and prediction statistics
      results$deviance.explained[k]<- (mod$null - mod$deviance)/mod$null
      likelihood.ratio.results<- lrtest(mod, nullmod)
      results$mod.nll[k]<- -1*likelihood.ratio.results$LogLik[1]
      results$null.nll[k]<- -1*likelihood.ratio.results$LogLik[2]
      results$lrtest[k]<- likelihood.ratio.results$Pr[2]
      results$AICc[k]<- AICc(mod, nobs = sample.size)
      
      # Model predictions, no space as 0/1 needed to calculate autocovariate
      dataset.pred<- dataset.temp
      dataset.pred$x.autocov<- 0
      fullmod.predictions<- suppressWarnings(predict.glm(mod, dataset.pred, type = "response"))
      dat.for.kappa.full<- data.frame(cbind(dataset.temp$id, dataset.temp$zero.one, fullmod.predictions))
      opt.thresh.full<- optimal.thresholds(DATA = dat.for.kappa.full, threshold = 50, opt.methods = "MaxKappa")
      thresh.use.full<- opt.thresh.full[,2]
      confusion.mat.full<- cmx(DATA = dat.for.kappa.full, threshold = thresh.use.full)
      kappa.full<- Kappa(CMX = confusion.mat.full, st.dev = TRUE)
      results$full.kappa[k]<- kappa.full[1,1]
      results$full.auc[k]<- auc(DATA = dat.for.kappa.full, st.dev = FALSE)
      predictions.full.binned<- data.frame(summary(cut(fullmod.predictions, breaks = bins)))[,1]
      prop.full.presences<- suppressWarnings(aggregate(dataset.temp$zero.one, by = list(cut(fullmod.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.full.binned)
      # Occassionally have 0 obs in a bin, set to 0 instead of "Inf"
      prop.full.presences[prop.full.presences == "Inf"]<- 0
      
      # Model prediction statistics
      ccc.results.full<- epi.ccc(prop.full.presences, midpts)
      results$full.ccc.mean[k]<- ccc.results.full$rho.c$est
      results$full.ccc.low[k]<- ccc.results.full$rho.c$lower
      results$full.ccc.up[k]<- ccc.results.full$rho.c$upper
      results$omission.err.full[k]<- confusion.mat.full[2,1]/sum(confusion.mat.full[,1])
      results$commission.err.full[k]<- confusion.mat.full[1,2]/sum(confusion.mat.full[,2])
      
      # Two-fold cross validation
      training1.pred<- training1
      training1.pred$x.autocov<- 0
      
      training2.pred<- training2
      training2.pred$x.autocov<- 0
      
      crossvalid.predictions<- c(suppressWarnings(predict.glm(mod.train2, newdata = training1.pred, type= "response")), suppressWarnings(predict.glm(mod.train1, newdata = training2.pred, type= "response")))
      crossvalid.fulldataset.temp<- rbind(training1, training2)
      dat.for.kappa.crossvalid<- data.frame(cbind(crossvalid.fulldataset.temp$id, crossvalid.fulldataset.temp$zero.one, crossvalid.predictions))
      opt.thresh.crossvalid<- optimal.thresholds(DATA = dat.for.kappa.crossvalid, threshold = 50, opt.methods = "MaxKappa")
      thresh.use.crossvalid<- opt.thresh.crossvalid[,2]
      confusion.mat.crossvalid<- cmx(DATA = dat.for.kappa.crossvalid, threshold = thresh.use.crossvalid)
      kappa.crossvalid<- Kappa(CMX = confusion.mat.crossvalid, st.dev = TRUE)
      results$split.kappa[k]<- kappa.crossvalid[1,1]
      results$split.auc[k]<- auc(DATA = dat.for.kappa.crossvalid, st.dev = FALSE)
      predictions.split.binned<- data.frame(summary(cut(crossvalid.predictions, breaks = bins)))[,1]
      prop.split.presences<- suppressWarnings(aggregate(crossvalid.fulldataset.temp$zero.one, by = list(cut(crossvalid.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.split.binned)
      prop.split.presences[prop.split.presences == "Inf"] <- 0
      ccc.results.split<- epi.ccc(prop.split.presences, midpts)
      results$split.ccc.mean[k]<- ccc.results.split$rho.c$est
      results$split.ccc.low[k]<- ccc.results.split$rho.c$lower
      results$split.ccc.up[k]<- ccc.results.split$rho.c$upper
      results$omission.err.split[k]<- confusion.mat.crossvalid[2,1]/sum(confusion.mat.crossvalid[,1])
      results$commission.err.split[k]<- confusion.mat.crossvalid[1,2]/sum(confusion.mat.crossvalid[,2])
    
      # Calculating Moran's I on model residuals
      fullmod.resids<- residuals.glm(mod, dataset.temp, type = resids.type)
      
      for (j in 1:length(morans.bins)) {
        mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
        mat<- dataset.temp[,c(2:3)] 
        dist.mat<- rdist(as.matrix(mat))
        diag(dist.mat)<- 0
        if(j == 1) {
          dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
          } else {
            dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
          }
        if(sum(dist.binary) <= moransi.minpairs) {
          morani.result <- NA
          } else {
            morani.result<- morani(fullmod.resids, dist.binary)
          }
        morans.i$boot.iter[si]<- boot.id
        morans.i$dist.bin[si]<- morans.bins[j]
        morans.i$moransI[si]<- morani.result
        si<- si+1
      }
    
      # Calculating model subset information from biostats
      if(n.predictors == 2 || n.predictors == 3 || n.predictors == 4) {
        if(model.fit == "glm") {
          response.index<- which(colnames(dataset.temp) == "zero.one")
          terms<- attr(terms(as.formula(model.formula)), "term.labels")
          x.index<- which(colnames(dataset.temp) %in% terms)
          all.subsets.list<- all.subsets.glm(y = dataset.temp[,response.index], x1 = dataset.temp[,x.index], family = binomial(link=logit), maxp = length(x.index), select = "AIC", varimp = TRUE, delta.AIC = 10, coef.table = TRUE, barplot = FALSE)
        }
        if(model.fit == "glm.autocov") {
          response.index<- which(colnames(dataset.temp) == "zero.one")
          terms<- attr(terms(as.formula(model.formula)), "term.labels")
          terms.reduced<- terms[-match("x.autocov", terms)]
          x.index<- which(colnames(dataset.temp) %in% terms.reduced)
          x.force.use<- data.frame(dataset.temp[,which(colnames(dataset.temp) %in% "x.autocov")])
          names(x.force.use)<- "x.autocov"
          all.subsets.list<- all.subsets.glm(y = dataset.temp[,response.index], x1 = dataset.temp[,x.index], x.force = x.force.use, family = binomial(link=logit), maxp = length(terms), select = "AIC", varimp = TRUE, delta.AIC = 10, coef.table = TRUE, barplot = FALSE)
        }
        modstats.temp<- all.subsets.list$model.statistics
        modstats.temp$boot<- rep(boot.id, nrow(modstats.temp))
        names(modstats.temp)[1:8]<- c("id", "model", "d2", "AICc", "deltaAICc", "wgtAICc", "rank", "boot")
        
        # Create modstats.result if it doesn't exist, add to existing if it does
        exists.logical<- exists("modstats.results")
        if(exists.logical == FALSE) {
          modstats.results<- modstats.temp
          names(modstats.results)[1:8]<- c("id", "model", "d2", "AICc", "deltaAICc", "wgtAICc", "rank", "boot")
          } else {
            modstats.results<- data.frame(rbind(modstats.results, modstats.temp))
            names(modstats.results)[1:8]<- c("id", "model", "d2", "AICc", "deltaAICc", "wgtAICc", "rank", "boot")
          }
      }
      print(k)
    }
    
    ## Morans I residuals results
    moransI.name<- paste(directory, date, scenario, "moransI.csv", sep = "")
  
    #Get lock and writing out results
    #get.lock(moransI.name)
    append<- file.exists(moransI.name)
    write.table(morans.i, file = moransI.name, append = append,
                row.names = FALSE, col.names = !append, sep = ",")
    #return.lock(moransI.name)

    ## Statistical inference and model selection results
    boot.globalmodresults<- paste(directory, date, scenario, "globalmodresults.csv", sep = "")
    
    # Get lock and write out results
    #get.lock(boot.globalmodresults)
    append<- file.exists(boot.globalmodresults)
    write.table(results, file = boot.globalmodresults, append = append,
            row.names = FALSE, col.names = !append, sep = ",")
    #return.lock(boot.globalmodresults)
    
    if(n.predictors == 2 || n.predictors == 3 || n.predictors == 4) {
      modstats.name<- paste(directory, date, scenario, "modstats.csv", sep = "")
      #get.lock(modstats.name)
      append<- file.exists(modstats.name)
      write.table(modstats.results, file = modstats.name, append = append,
            row.names = FALSE, col.names = !append, sep = ",")
      #return.lock(modstats.name)

      # Remove to free up space
      rm(results, morans.i, modstats.results)
      } else {
        rm(results, morans.i)
      }
  }
  
  ### End Fit GLM or GLM plus autocovariate term

  ### Fit GAM
  if(model.fit == "gam.s" || model.fit == "gam.te") {
    suppressWarnings(library(mgcv))
    # print("gam")

    for (k in seq(along=datasets)) {
      boot.id<- boot.id.sequence[k]
      dataset.temp<- datasets[[k]]
      sample.size<- nrow(dataset.temp)
      dataset.temp.split<- sample(dataset.temp$id, size = sample.size/2)
      training1<- dataset.temp[dataset.temp.split, ]
      training2<- dataset.temp[-dataset.temp.split, ]
      
      if(model.fit == "gam.s" || model.fit == "gam.te") {
        mod<- try(gam(as.formula(model.formula), data = dataset.temp, family = binomial(link=logit)), silent = TRUE)
        mod.train1<- try(gam(as.formula(model.formula), data = training1, family = binomial(link=logit), maxit = 1000), silent = TRUE)
        mod.train2<- try(gam(as.formula(model.formula), data = training2, family = binomial(link=logit), maxit = 1000), silent = TRUE)
        
        # Break to next loop iteration if there is an error or bad fit
        if(class(mod) == "try-error" || summary(mod)$p.table[1,2] >25 || 
             class(mod.train1) == "try-error" || class(mod.train2)== "try-error") {
          print("skip")
          next
        }
      }
      
      # Manually doing model subsets, so if all we need is AICc value, calculate it and move to the next loop iteration and do not continue to calculate statistical inference or model selection values
      if(model.subset == TRUE) {
        results$AICc[k]<- AICc(mod, nobs = sample.size)
        next
      }
      
      if(model.subset == FALSE) {
        nullmod <- try(gam(zero.one ~ 1, data=dataset.temp, family=binomial), silent = TRUE)
        results$intercept[k]<- as.numeric(as.character(coefficients(mod)[1]))
        results$intercept.z[k]<- summary(mod)$p.table[1,3]
        results$intercept.p[k]<- summary(mod)$p.table[1,4]
        results$intercept.se[k]<- summary(mod)$p.table[1,2]
        results$x1[k]<- as.numeric(as.character(coefficients(mod)[2]))
        results$x1.z[k]<- summary(mod)$p.table[2,3]
        results$x1.p[k]<- summary(mod)$p.table[2,4]
        results$x1.se[k]<- summary(mod)$p.table[2,2]
        if(n.predictors == 2) {
          results$random[k]<- as.numeric(as.character(coefficients(mod)[3]))
          results$random.z[k]<- summary(mod)$p.table[3,3]
          results$random.p[k]<- summary(mod)$p.table[3,4]
          results$random.se[k]<- summary(mod)$p.table[3,2]
        }
        if(n.predictors == 3) {
          results$random[k]<- as.numeric(as.character(coefficients(mod)[3]))
          results$random.z[k]<- summary(mod)$p.table[3,3]
          results$random.p[k]<- summary(mod)$p.table[3,4]
          results$random.se[k]<- summary(mod)$p.table[3,2]
        }
        if(n.predictors == 4) {
          results$x2[k]<- as.numeric(as.character(coefficients(mod)[3]))
          results$x2.z[k]<- summary(mod)$p.table[3,3]
          results$x2.p[k]<- summary(mod)$p.table[3,4]
          results$x2.se[k]<- summary(mod)$p.table[3,2]
          results$random[k]<- as.numeric(as.character(coefficients(mod)[4]))
          results$random.z[k]<- summary(mod)$p.table[4,3]
          results$random.p[k]<- summary(mod)$p.table[4,4]
          results$random.se[k]<- summary(mod)$p.table[4,2]
        }
        results$deviance.explained[k]<- (mod$null - mod$deviance)/mod$null

        if(any(class(nullmod) != "try-error")) {
          likelihood.ratio.results<- lrtest(mod, nullmod)
          results$mod.nll[k]<- -1*likelihood.ratio.results$LogLik[1]
          results$null.nll[k]<- -1*likelihood.ratio.results$LogLik[2]
          results$lrtest[k]<- likelihood.ratio.results$Pr[2]
        }
        
        results$AICc[k]<- AICc(mod, nobs = sample.size)

        # Model prediction and statistics
        fullmod.predictions<- predict.gam(mod, newdata = dataset.temp, type = "response")
        dat.for.kappa.full<- data.frame(cbind(dataset.temp$id, dataset.temp$zero.one, fullmod.predictions))
        opt.thresh.full<- optimal.thresholds(DATA = dat.for.kappa.full, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.full<- opt.thresh.full[,2]
        confusion.mat.full<- cmx(DATA = dat.for.kappa.full, threshold = thresh.use.full)
        kappa.full<- Kappa(CMX = confusion.mat.full, st.dev = TRUE)
        results$full.kappa[k]<- kappa.full[1,1]
        results$full.auc[k]<- auc(DATA = dat.for.kappa.full, st.dev = FALSE)
        predictions.full.binned<- data.frame(summary(cut(fullmod.predictions, breaks = bins)))[,1]
        prop.full.presences<- suppressWarnings(aggregate(dataset.temp$zero.one, by = list(cut(fullmod.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.full.binned)

        # Occassionally have 0 obs in a bin, set to 0 instead of "Inf"
        prop.full.presences[prop.full.presences == "Inf"]<- 0
        ccc.results.full<- epi.ccc(prop.full.presences, midpts)
        results$full.ccc.mean[k]<- ccc.results.full$rho.c$est
        results$full.ccc.low[k]<- ccc.results.full$rho.c$lower
        results$full.ccc.up[k]<- ccc.results.full$rho.c$upper
        results$omission.err.full[k]<- confusion.mat.full[2,1]/sum(confusion.mat.full[,1])
        results$commission.err.full[k]<- confusion.mat.full[1,2]/sum(confusion.mat.full[,2])
        
        # Two-fold cross validation
        crossvalid.predictions<- c(predict.gam(mod.train2, newdata = training1, type= "response"), predict.gam(mod.train1, newdata = training2, type= "response"))
        crossvalid.fulldataset.temp<- rbind(training1, training2)
        dat.for.kappa.crossvalid<- data.frame(cbind(crossvalid.fulldataset.temp$id, crossvalid.fulldataset.temp$zero.one, crossvalid.predictions))
        opt.thresh.crossvalid<- optimal.thresholds(DATA = dat.for.kappa.crossvalid, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.crossvalid<- opt.thresh.crossvalid[,2]
        confusion.mat.crossvalid<- cmx(DATA = dat.for.kappa.crossvalid, threshold = thresh.use.crossvalid)
        kappa.crossvalid<- Kappa(CMX = confusion.mat.crossvalid, st.dev = TRUE)
        results$split.kappa[k]<- kappa.crossvalid[1,1]
        results$split.auc[k]<- auc(DATA = dat.for.kappa.crossvalid, st.dev = FALSE)
        predictions.split.binned<- data.frame(summary(cut(crossvalid.predictions, breaks = bins)))[,1]
        prop.split.presences<- try(suppressWarnings(aggregate(crossvalid.fulldataset.temp$zero.one, by = list(cut(crossvalid.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.split.binned), silent = TRUE)
        
        if(class(prop.split.presences) != "try-error") {
          prop.split.presences[prop.split.presences == "Inf"] <- 0
          ccc.results.split<- try(epi.ccc(prop.split.presences, midpts), silent = TRUE)

          if(class(ccc.results.split) != "try-error") {
            results$split.ccc.mean[k]<- ccc.results.split$rho.c$est
            results$split.ccc.low[k]<- ccc.results.split$rho.c$lower
            results$split.ccc.up[k]<- ccc.results.split$rho.c$upper
          }
        }

        results$omission.err.split[k]<- confusion.mat.crossvalid[2,1]/sum(confusion.mat.crossvalid[,1])
        results$commission.err.split[k]<- confusion.mat.crossvalid[1,2]/sum(confusion.mat.crossvalid[,2])
    
        # Morans I
        fullmod.resids<- residuals.gam(mod, dataset.temp, type = resids.type)
        
        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
          mat<- dataset.temp[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
          } else {
            dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
          }
          if(sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
          } else {
            morani.result<- morani(fullmod.resids, dist.binary)
          }
          morans.i$boot.iter[si]<- boot.id
          morans.i$dist.bin[si]<- morans.bins[j]
          morans.i$moransI[si]<- morani.result
          si<- si+1
        }
      }
      print(k)
    }

    ## Morans I residuals
    if(model.subset == FALSE) {
      moransI.name<- paste(directory, date, scenario, "moransI.csv", sep = "")
      #get.lock(moransI.name)
      append<- file.exists(moransI.name)
      write.table(morans.i, file = moransI.name, append = append,
            row.names = FALSE, col.names = !append, sep = ",")
      #return.lock(moransI.name)
    }

    ## Write out model selection results
    boot.globalmodresults<- paste(directory, date, scenario, "globalmodresults.csv", sep = "")
    #get.lock(boot.globalmodresults)
    append<- file.exists(boot.globalmodresults)
    write.table(results, file = boot.globalmodresults, append = append,
            row.names = FALSE, col.names = !append, sep = ",")
    #return.lock(boot.globalmodresults)
    rm(results, morans.i)
  }
  
  ### End Fit GAM
  
  ### Fit Spatial Eigenvector
  if(model.fit == "glm.spev") {
    suppressWarnings(library(spdep))
    print("glm.spev")
    
    for (k in seq(along = datasets)) {
      boot.id<- boot.id.sequence[k]
      dataset.temp<- datasets[[k]]
      sample.size<- nrow(dataset.temp)
      
      # SPEV set up
      coords <- as.matrix(cbind(dataset.temp$x, dataset.temp$y))
      nb1.0 <- dnearneigh(as.matrix(dataset.temp[,c("x", "y")]), 0, dnn.max)
      nb1.0_dists <- nbdists(nb1.0, coords)
      
      # NB weighting function negative exp or Gaussian
      if(spev.traditional == TRUE) {
        nb1.0_sims <- lapply(nb1.0_dists, function(x) (1-((x/4)^2)))
      }
      if(spev.neg.exp == TRUE) {
        nb1.0_sims <- lapply(nb1.0_dists, function(x) exp(-neg.exp.ev*x))
      }
      if(spev.gauss == TRUE) {
        nb1.0_sims <- lapply(nb1.0_dists, function(x) (1/(sqrt(2*pi*sigma.ev^2)))*exp(-(x - mu.ev)^2/(sqrt(2*sigma.ev^2))))
      }
      
      # SPEV model fit
      ME.listw <- suppressWarnings(nb2listw(nb1.0, glist=nb1.0_sims, style="B", zero.policy = TRUE))
      spev <- try(ME(model.formula, data=dataset.temp, family=binomial, listw=ME.listw), silent = TRUE)
      if(class(spev) == "try-error") {
        mod<- glm(as.formula(model.formula), data = dataset.temp, family = binomial)
        results$SPEV[k]<- 0
      } else {
        model.formula.full<- c(paste(model.formula, "+", "fitted(spev)"))
        mod<- try(glm(as.formula(model.formula.full), data = dataset.temp, family = binomial), silent = TRUE)
        results$SPEV[k]<- 1
      }
      
      # Cross validated SPEV set up and model fit
      dataset.temp.split<- sample(dataset.temp$id, size = sample.size/2)
      training1<- dataset.temp[dataset.temp.split, ]
      coords.t1<-as.matrix(cbind(training1$x, training1$y))
      nb1.0.t1 <- dnearneigh(as.matrix(training1[,c("x", "y")]), 0, dnn.max)
      nb1.0_dists.t1 <- nbdists(nb1.0.t1, coords.t1)
      if(spev.traditional == TRUE) {
        nb1.0_sims.t1 <- lapply(nb1.0_dists.t1, function(x) (1-((x/4)^2)))
      }
      if (spev.neg.exp == TRUE) {
        nb1.0_sims.t1 <- lapply(nb1.0_dists.t1, function(x) exp(-neg.exp.ev*x))
      }
      if (spev.gauss == TRUE) {
        nb1.0_sims.t1 <- lapply(nb1.0_dists.t1, function(x) (1/(sqrt(2*pi*sigma.ev^2)))*exp(-(x - mu.ev)^2/(sqrt(2*sigma.ev^2))))
      }
      
      ME.listw.t1 <- suppressWarnings(nb2listw(nb1.0.t1, glist=nb1.0_sims.t1, style="B", zero.policy = TRUE))
      spev.t1 <- try(ME(model.formula, data=training1, family=binomial, listw=ME.listw.t1), silent = TRUE)
      if(class(spev.t1) == "try-error") {
        mod.train1<- glm(as.formula(model.formula), data = training1, family = binomial)
      } else {
        model.formula.t1<- c(paste(model.formula, "+", "fitted(spev.t1)"))
        mod.train1<- try(glm(as.formula(model.formula.t1), data = training1, family = binomial), silent = TRUE)
      }

      training2<- dataset.temp[-dataset.temp.split, ]
      coords.t2<-as.matrix(cbind(training2$x, training2$y))
      nb1.0.t2 <- dnearneigh(as.matrix(training2[,c("x", "y")]), 0, dnn.max)
      nb1.0_dists.t2 <- nbdists(nb1.0.t2, coords.t2)
      if(spev.traditional == TRUE) {
        nb1.0_sims.t2 <- lapply(nb1.0_dists.t2, function(x) (1-((x/4)^2)))
      }
      if (spev.neg.exp == TRUE) {
        nb1.0_sims.t2 <- lapply(nb1.0_dists.t2, function(x) exp(-neg.exp.ev*x))
      }
      if (spev.gauss == TRUE) {
        nb1.0_sims.t2 <- lapply(nb1.0_dists.t2, function(x) (1/(sqrt(2*pi*sigma.ev^2)))*exp(-(x - mu.ev)^2/(sqrt(2*sigma.ev^2))))
      }
      
      ME.listw.t2 <- suppressWarnings(nb2listw(nb1.0.t2, glist=nb1.0_sims.t2, style="B", zero.policy = TRUE))
      spev.t2 <- try(ME(model.formula, data=training2, family=binomial, listw=ME.listw.t2), silent = TRUE)
      if(class(spev.t2) == "try-error") {
        mod.train2<- glm(as.formula(model.formula), data = training2, family = binomial)
      } else {
          model.formula.t2<- c(paste(model.formula, "+", "fitted(spev.t2)"))
          mod.train2<- try(glm(as.formula(model.formula.t2), data = training2, family = binomial), silent = TRUE)
      }
      
      if(class(mod) == "try-error" || class(mod.train1) == "try-error" || class(mod.train2) == "try-error"){
        print("skip")
        next
      }
      
      # If intercept.se > 25, indicating bad fit, go to next dataset
      if(as.numeric(as.character(coef(summary(mod))[1,2])) > 25) {
        next
      }
      
      # Calculating and storing results for good fits
      nullmod <- glm(zero.one ~ 1, data=dataset.temp, family=binomial)
      results$intercept[k]<- as.numeric(as.character(coefficients(mod)[1]))
      results$intercept.z[k]<- as.numeric(as.character(coef(summary(mod))[1,3]))
      results$intercept.p[k]<- as.numeric(as.character(coef(summary(mod))[1,4]))
      results$intercept.se[k]<- as.numeric(as.character(coef(summary(mod))[1,2]))
      results$x1[k]<- as.numeric(as.character(coefficients(mod)[2]))
      results$x1.z[k]<- as.numeric(as.character(coef(summary(mod))[2,3]))
      results$x1.p[k]<- as.numeric(as.character(coef(summary(mod))[2,4]))
      results$x1.se[k]<- as.numeric(as.character(coef(summary(mod))[2,2]))
      if(n.predictors == 2) {
        results$random[k]<- as.numeric(as.character(coefficients(mod)[3]))
        results$random.z[k]<- as.numeric(as.character(coef(summary(mod))[3,3]))
        results$random.p[k]<- as.numeric(as.character(coef(summary(mod))[3,4]))
        results$random.se[k]<- as.numeric(as.character(coef(summary(mod))[3,2]))
      }
      if(n.predictors == 3) {
        results$x2[k]<- as.numeric(as.character(coefficients(mod)[3]))
        results$x2.z[k]<- as.numeric(as.character(coef(summary(mod))[3,3]))
        results$x2.p[k]<- as.numeric(as.character(coef(summary(mod))[3,4]))
        results$x2.se[k]<- as.numeric(as.character(coef(summary(mod))[3,2]))
        results$random[k]<- as.numeric(as.character(coefficients(mod)[4]))
        results$random.z[k]<- as.numeric(as.character(coef(summary(mod))[4,3]))
        results$random.p[k]<- as.numeric(as.character(coef(summary(mod))[4,4]))
        results$random.se[k]<- as.numeric(as.character(coef(summary(mod))[4,2]))
      }
      if(n.predictors == 4) {
        results$x2[k]<- as.numeric(as.character(coefficients(mod)[3]))
        results$x2.z[k]<- as.numeric(as.character(coef(summary(mod))[3,3]))
        results$x2.p[k]<- as.numeric(as.character(coef(summary(mod))[3,4]))
        results$x2.se[k]<- as.numeric(as.character(coef(summary(mod))[3,2]))
        results$dslope[k]<- as.numeric(as.character(coefficients(mod)[4]))
        results$dslope.z[k]<- as.numeric(as.character(coef(summary(mod))[4,3]))
        results$dslope.p[k]<- as.numeric(as.character(coef(summary(mod))[4,4]))
        results$dslope.se[k]<- as.numeric(as.character(coef(summary(mod))[4,2]))
        results$random[k]<- as.numeric(as.character(coefficients(mod)[5]))
        results$random.z[k]<- as.numeric(as.character(coef(summary(mod))[5,3]))
        results$random.p[k]<- as.numeric(as.character(coef(summary(mod))[5,4]))
        results$random.se[k]<- as.numeric(as.character(coef(summary(mod))[5,2]))
      }
      results$deviance.explained[k]<- (mod$null - mod$deviance)/mod$null
      likelihood.ratio.results<- lrtest(mod, nullmod)
      results$mod.nll[k]<- -1*likelihood.ratio.results$LogLik[1]
      results$null.nll[k]<- -1*likelihood.ratio.results$LogLik[2]
      results$lrtest[k]<- likelihood.ratio.results$Pr[2]
      results$AICc[k]<- AICc(mod, nobs = sample.size)
      
      # Full model predictions and statistics
      if(n.predictors == 3) {
        fullmod.predictions<- plogis(as.numeric(coef(mod)[1]) + as.numeric(coef(mod)[2])*dataset.temp$x1 + as.numeric(coef(mod)[3])*dataset.temp$x2 + as.numeric(coef(mod)[4])*dataset.temp$x.random)
        
        crossvalid.predictions<- c(plogis(as.numeric(coef(mod.train2)[1]) + as.numeric(coef(mod.train2)[2])*training1$x1 + as.numeric(coef(mod.train2)[3])*training1$x2 + as.numeric(coef(mod.train2)[4])*training1$x.random), 
                                   plogis(as.numeric(coef(mod.train1)[1]) + as.numeric(coef(mod.train1)[2])*training2$x1 + as.numeric(coef(mod.train1)[3])*training2$x2 + as.numeric(coef(mod.train1)[4])*training2$x.random))
      }
      
      if(n.predictors == 2) {
        fullmod.predictions<- plogis(as.numeric(coef(mod)[1]) + as.numeric(coef(mod)[2])*dataset.temp$x1 + as.numeric(coef(mod)[3])*dataset.temp$x.random)
        
        crossvalid.predictions<- c(plogis(as.numeric(coef(mod.train2)[1]) + as.numeric(coef(mod.train2)[2])*training1$x1 + as.numeric(coef(mod.train2)[3])*training1$x.random),
                                   plogis(as.numeric(coef(mod.train1)[1]) + as.numeric(coef(mod.train1)[2])*training2$x1 + as.numeric(coef(mod.train1)[3])*training2$x.random))
      }

      dat.for.kappa.full<- data.frame(cbind(dataset.temp$id, dataset.temp$zero.one, fullmod.predictions))
      opt.thresh.full<- optimal.thresholds(DATA = dat.for.kappa.full, threshold = 50, opt.methods = "MaxKappa")
      thresh.use.full<- opt.thresh.full[,2]
      confusion.mat.full<- cmx(DATA = dat.for.kappa.full, threshold = thresh.use.full)
      kappa.full<- Kappa(CMX = confusion.mat.full, st.dev = TRUE)
      results$full.kappa[k]<- kappa.full[1,1]
      results$full.auc[k]<- auc(DATA = dat.for.kappa.full, st.dev = FALSE)
      predictions.full.binned<- data.frame(summary(cut(fullmod.predictions, breaks = bins)))[,1]
      prop.full.presences<- suppressWarnings(aggregate(dataset.temp$zero.one, by = list(cut(fullmod.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.full.binned)
      
      # Occassionally have 0 obs in a bin, set to 0 instead of "Inf"
      prop.full.presences[prop.full.presences == "Inf"]<- 0
      
      if(length(prop.full.presences) == length(midpts)) {
        ccc.results.full<- epi.ccc(prop.full.presences, midpts)
        results$full.ccc.mean[k]<- ccc.results.full$rho.c$est
        results$full.ccc.low[k]<- ccc.results.full$rho.c$lower
        results$full.ccc.up[k]<- ccc.results.full$rho.c$upper
      }
      
      if(length(prop.full.presences) != length(midpts)) {
        results$full.ccc.mean[k]<- NA
        results$full.ccc.low[k]<- NA
        results$full.ccc.up[k]<- NA
      }
      
      results$omission.err.full[k]<- confusion.mat.full[2,1]/sum(confusion.mat.full[,1])
      results$commission.err.full[k]<- confusion.mat.full[1,2]/sum(confusion.mat.full[,2])
      
      # Two-fold cross validation
      crossvalid.fulldataset.temp<- rbind(training1, training2)
      dat.for.kappa.crossvalid<- data.frame(cbind(crossvalid.fulldataset.temp$id, crossvalid.fulldataset.temp$zero.one, crossvalid.predictions))
      opt.thresh.crossvalid<- optimal.thresholds(DATA = dat.for.kappa.crossvalid, threshold = 50, opt.methods = "MaxKappa")
      thresh.use.crossvalid<- opt.thresh.crossvalid[,2]
      confusion.mat.crossvalid<- cmx(DATA = dat.for.kappa.crossvalid, threshold = thresh.use.crossvalid)
      kappa.crossvalid<- Kappa(CMX = confusion.mat.crossvalid, st.dev = TRUE)
      results$split.kappa[k]<- kappa.crossvalid[1,1]
      results$split.auc[k]<- auc(DATA = dat.for.kappa.crossvalid, st.dev = FALSE)
      predictions.split.binned<- data.frame(summary(cut(crossvalid.predictions, breaks = bins)))[,1]
      prop.split.presences<- suppressWarnings(aggregate(crossvalid.fulldataset.temp$zero.one, by = list(cut(crossvalid.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.split.binned)
      prop.split.presences[prop.split.presences == "Inf"] <- 0
      
      if(length(prop.split.presences) == length(midpts)) {
        ccc.results.split<- epi.ccc(prop.split.presences, midpts)
        results$split.ccc.mean[k]<- ccc.results.split$rho.c$est
        results$split.ccc.low[k]<- ccc.results.split$rho.c$lower
        results$split.ccc.up[k]<- ccc.results.split$rho.c$upper
      }
      
      if(length(prop.split.presences) != length(midpts)) {
        results$split.ccc.mean[k]<- NA
        results$split.ccc.low[k]<- NA
        results$split.ccc.up[k]<- NA
      }
      
      results$omission.err.split[k]<- confusion.mat.crossvalid[2,1]/sum(confusion.mat.crossvalid[,1])
      results$commission.err.split[k]<- confusion.mat.crossvalid[1,2]/sum(confusion.mat.crossvalid[,2])
      
      # Morans I
      fullmod.resids<- residuals.glm(mod, dataset.temp, type = resids.type)
      
      for (j in 1:length(morans.bins)) {
        mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
        mat<- dataset.temp[,c(2:3)] 
        dist.mat<- rdist(as.matrix(mat))
        diag(dist.mat)<- 0
        if(j == 1) {
          dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
        } else {
          dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
        }
        if(sum(dist.binary) <= moransi.minpairs) {
          morani.result <- NA
        } else {
          morani.result<- morani(fullmod.resids, dist.binary)
        }
        morans.i$boot.iter[si]<- boot.id
        morans.i$dist.bin[si]<- morans.bins[j]
        morans.i$moransI[si]<- morani.result
        si<- si+1
      }
      
      # All subsets
      if(n.predictors == 2 || n.predictors == 3 || n.predictors == 4) {
        response.index<- which(colnames(dataset.temp) == "zero.one")
        terms<- attr(terms(as.formula(model.formula)), "term.labels")
        x.index<- which(colnames(dataset.temp) %in% terms)
        if(class(spev) == "try-error") {
          all.subsets.list<- all.subsets.glm(y = dataset.temp[,response.index], x1 = dataset.temp[,x.index], family = binomial(link=logit), maxp = length(terms), select = "AIC", varimp = TRUE, delta.AIC = 10, coef.table = TRUE, barplot = FALSE)
        } 
        if(class(spev) != "try-error"){
          all.subsets.list<- all.subsets.glm(y = dataset.temp[,response.index], x1 = dataset.temp[,x.index], x.force = data.frame(fitted(spev)), family = binomial(link=logit), maxp = (length(x.index)+ncol(data.frame(fitted(spev)))), select = "AIC", varimp = TRUE, delta.AIC = 10, coef.table = TRUE, barplot = FALSE)
        }
        modstats.temp<- all.subsets.list$model.statistics
        modstats.temp$boot<- rep(boot.id, nrow(modstats.temp))
        names(modstats.temp)[1:8]<- c("id", "model", "d2", "AICc", "deltaAICc", "wgtAICc", "rank", "boot")

        exists.logical<- exists("modstats.results")
        if(exists.logical == FALSE) {
          modstats.results<- modstats.temp
          names(modstats.results)[1:8]<- c("id", "model", "d2", "AICc", "deltaAICc", "wgtAICc", "rank", "boot")
        } else {
          modstats.results<- data.frame(rbind(modstats.results, modstats.temp))
          names(modstats.results)[1:8]<- c("id", "model", "d2", "AICc", "deltaAICc", "wgtAICc", "rank", "boot")
        }
      }
      print(k)
    }
    
    ## Morans I residuals
    moransI.name<- paste(directory, date, scenario, "moransI.csv", sep = "")
    #get.lock(moransI.name)
    append<- file.exists(moransI.name)
    write.table(morans.i, file = moransI.name, append = append,
                row.names = FALSE, col.names = !append, sep = ",")
    #return.lock(moransI.name)

    ## Write out model selection results
    boot.globalmodresults<- paste(directory, date, scenario, "globalmodresults.csv", sep = "")

    #get.lock(boot.globalmodresults)
    append<- file.exists(boot.globalmodresults)
    write.table(results, file = boot.globalmodresults, append = append,
                row.names = FALSE, col.names = !append, sep = ",")
    #return.lock(boot.globalmodresults)

    if(n.predictors == 2 || n.predictors == 3 || n.predictors == 4) {
      modstats.name<- paste(directory, date, scenario, "modstats.csv", sep = "")
      
      #get.lock(modstats.name)
      append<- file.exists(modstats.name)
      write.table(modstats.results, file = modstats.name, append = append,
                  row.names = FALSE, col.names = !append, sep = ",")
      #return.lock(modstats.name)

      rm(results, morans.i, modstats.results)
    } else {
      rm(results, morans.i)
    }
  }
  
  ### End Fit spev
  
  ### Fit GLMM
  if(model.fit == "glmmPQL") {
    suppressWarnings(library(MASS))
    suppressWarnings(library(nlme))
    # print("glmm")
    
    for (k in seq(along=datasets)) {
      
      boot.id<- boot.id.sequence[k]
      dataset.temp<- datasets[[k]]
      dataset.temp$group<- rep("a", nrow(dataset.temp))
      
      if(model.fit == "glmmPQL") {
        sample.size<- nrow(dataset.temp)
        dataset.temp.split<- sample(dataset.temp$id, size = sample.size/2)
        training1<- dataset.temp[dataset.temp.split, ]
        training2<- dataset.temp[-dataset.temp.split, ]
        
        # GLMM with exponential correlation structure
        attach(dataset.temp)
        random.formula <- "~1|group"
        mod <- try(glmmPQL.hack(as.formula(model.formula), random = as.formula(random.formula), data = dataset.temp, correlation=corExp(form = ~x + y), family=binomial), silent = TRUE)
        detach(dataset.temp)
        
        attach(training1)
        mod.train1<- try(glmmPQL.hack(as.formula(model.formula), random = as.formula(random.formula), data = training1, correlation=corExp(form = ~x + y), family=binomial), silent = TRUE)
        detach(training1)
        
        attach(training2)
        mod.train2<- try(glmmPQL.hack(as.formula(model.formula), random = as.formula(random.formula), data = training2, correlation=corExp(form = ~x + y), family=binomial), silent = TRUE)
        detach(training2)

        if(class(mod) == "try-error" || class(mod.train1) == "try-error" || class(mod.train2) == "try-error") {
          #print("skip")
          next
        }
        
        # Break if bad fit
        if(summary(mod)$tTable[1,2] > 25) {
          next
        }
      }
  
      if(model.subset == TRUE) {
        results$AICc[k]<- AICc(mod$logLik, nobs = sample.size, k = 2)
        next
      }
      
      if(model.subset == FALSE) {
        nullmod <- glm(zero.one ~ 1, data=dataset.temp, family=binomial)
      
        ## Storing results
        results$intercept[k]<- as.numeric(as.character(coefficients(mod)[1]))
        results$intercept.z[k]<- summary(mod)$tTable[1,4]
        results$intercept.p[k]<- summary(mod)$tTable[1,5]
        results$intercept.se[k]<- summary(mod)$tTable[1,2]
        results$x1[k]<- as.numeric(as.character(coefficients(mod)[2]))
        results$x1.z[k]<- summary(mod)$tTable[2,4]
        results$x1.p[k]<- summary(mod)$tTable[2,5]
        results$x1.se[k]<- summary(mod)$tTable[2,2]
        if(n.predictors == 2) {
          results$random[k]<- as.numeric(as.character(coefficients(mod)[3]))
          results$random.z[k]<- summary(mod)$tTable[3,4]
          results$random.p[k]<- summary(mod)$tTable[3,5]
          results$random.se[k]<- summary(mod)$tTable[3,2]
        }
        if(n.predictors == 3) {
          results$x2[k]<- as.numeric(as.character(coefficients(mod)[3]))
          results$x2.z[k]<- summary(mod)$tTable[3,4]
          results$x2.p[k]<- summary(mod)$tTable[3,5]
          results$x2.se[k]<- summary(mod)$tTable[3,2]
          results$random[k]<- as.numeric(as.character(coefficients(mod)[4]))
          results$random.z[k]<- summary(mod)$tTable[4,4]
          results$random.p[k]<- summary(mod)$tTable[4,5]
          results$random.se[k]<- summary(mod)$tTable[4,2]
        }
        if(n.predictors == 4) {
          results$x2[k]<- as.numeric(as.character(coefficients(mod)[3]))
          results$x2.z[k]<- summary(mod)$tTable[3,4]
          results$x2.p[k]<- summary(mod)$tTable[3,5]
          results$x2.se[k]<- summary(mod)$tTable[3,2]
          results$dslope[k]<- as.numeric(as.character(coefficients(mod)[4]))
          results$dslope.z[k]<- summary(mod)$tTable[4,4]
          results$dslope.p[k]<- summary(mod)$tTable[4,5]
          results$dslope.se[k]<- summary(mod)$tTable[4,2]
          results$random[k]<- as.numeric(as.character(coefficients(mod)[5]))
          results$random.z[k]<- summary(mod)$tTable[5,4]
          results$random.p[k]<- summary(mod)$tTable[5,5]
          results$random.se[k]<- summary(mod)$tTable[5,2]
        }
        results$AICc[k]<-  AICc(mod$logLik, nobs = sample.size, k = 2)
        fullmod.predictions<- as.numeric(predict(mod, type = "response"))
        dat.for.kappa.full<- data.frame(cbind(dataset.temp$id, dataset.temp$zero.one, fullmod.predictions))
        opt.thresh.full<- optimal.thresholds(DATA = dat.for.kappa.full, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.full<- opt.thresh.full[,2]
        confusion.mat.full<- cmx(DATA = dat.for.kappa.full, threshold = thresh.use.full)
        kappa.full<- Kappa(CMX = confusion.mat.full, st.dev = TRUE)
        results$full.kappa[k]<- kappa.full[1,1]
        results$full.auc[k]<- auc(DATA = dat.for.kappa.full, st.dev = FALSE)
        predictions.full.binned<- data.frame(summary(cut(fullmod.predictions, breaks = bins)))[,1]
        prop.full.presences<- suppressWarnings(aggregate(dataset.temp$zero.one, by = list(cut(fullmod.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.full.binned)
      
        # Occassionally have 0 obs in a bin, set to 0 instead of "Inf"
        prop.full.presences[prop.full.presences == "Inf"]<- 0
        ccc.results.full<- epi.ccc(prop.full.presences, midpts)
        results$full.ccc.mean[k]<- ccc.results.full$rho.c$est
        results$full.ccc.low[k]<- ccc.results.full$rho.c$lower
        results$full.ccc.up[k]<- ccc.results.full$rho.c$upper
        results$omission.err.full[k]<- confusion.mat.full[2,1]/sum(confusion.mat.full[,1])
        results$commission.err.full[k]<- confusion.mat.full[1,2]/sum(confusion.mat.full[,2])
        crossvalid.predictions<- as.numeric(c(predict(mod.train2, newdata = training1, type= "response"), predict(mod.train1, newdata = training2, type= "response")))
        crossvalid.fulldataset.temp<- rbind(training1, training2)
        dat.for.kappa.crossvalid<- data.frame(cbind(crossvalid.fulldataset.temp$id, crossvalid.fulldataset.temp$zero.one, crossvalid.predictions))
        opt.thresh.crossvalid<- optimal.thresholds(DATA = dat.for.kappa.crossvalid, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.crossvalid<- opt.thresh.crossvalid[,2]
        confusion.mat.crossvalid<- cmx(DATA = dat.for.kappa.crossvalid, threshold = thresh.use.crossvalid)
        kappa.crossvalid<- Kappa(CMX = confusion.mat.crossvalid, st.dev = TRUE)
        results$split.kappa[k]<- kappa.crossvalid[1,1]
        results$split.auc[k]<- auc(DATA = dat.for.kappa.crossvalid, st.dev = FALSE)
        predictions.split.binned<- data.frame(summary(cut(crossvalid.predictions, breaks = bins)))[,1]
        prop.split.presences<- suppressWarnings(aggregate(crossvalid.fulldataset.temp$zero.one, by = list(cut(crossvalid.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.split.binned)
        prop.split.presences[prop.split.presences == "Inf"] <- 0
        ccc.results.split<- epi.ccc(prop.split.presences, midpts)
        results$split.ccc.mean[k]<- ccc.results.split$rho.c$est
        results$split.ccc.low[k]<- ccc.results.split$rho.c$lower
        results$split.ccc.up[k]<- ccc.results.split$rho.c$upper
        results$omission.err.split[k]<- confusion.mat.crossvalid[2,1]/sum(confusion.mat.crossvalid[,1])
        results$commission.err.split[k]<- confusion.mat.crossvalid[1,2]/sum(confusion.mat.crossvalid[,2])
      
        # Morans I
        fullmod.resids<- dataset.temp$zero.one - as.numeric(predict(mod, type = "response"))
        
        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
          mat<- dataset.temp[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
          } else {
            dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
          }
          if(sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
          } else {
            morani.result<- morani(fullmod.resids, dist.binary)
          }
          morans.i$boot.iter[si]<- boot.id
          morans.i$dist.bin[si]<- morans.bins[j]
          morans.i$moransI[si]<- morani.result
          si<- si+1
        }
      }
      #print(k)
    }
    
    ## Morans I residuals
    if(model.subset == FALSE) {
      moransI.name<- paste(directory, date, scenario, "moransI.csv", sep = "")
      #get.lock(moransI.name)
      append<- file.exists(moransI.name)
      write.table(morans.i, file = moransI.name, append = append,
                  row.names = FALSE, col.names = !append, sep = ",")
      #return.lock(moransI.name)
    }
    
    ## Write out model selection results
    boot.globalmodresults<- paste(directory, date, scenario, "globalmodresults.csv", sep = "")
    #get.lock(boot.globalmodresults)
    append<- file.exists(boot.globalmodresults)
    write.table(results, file = boot.globalmodresults, append = append,
                row.names = FALSE, col.names = !append, sep = ",")
    #return.lock(boot.globalmodresults)
    rm(results, morans.i)
  }

  ### End Fit glmmPQL
  
  ### Fit GEE 
  if(model.fit == "gee.fixed" || model.fit == "gee.custom") {
    print("gee")
    suppressWarnings(library(ncf))
    suppressWarnings(library(gee))
    
    # Switch name from AIC to QIC, because GEE uses quasilikelihood
    names(results)[which(colnames(results) == "AICc")]<- "QICu"
    
    for (k in seq(along=datasets)) {
    
#       if(k == 10 | k == 12) {
#         print(k)
#         next
#       }
      
      boot.id<- boot.id.sequence[k]
      dataset.temp<- datasets[[k]]
      coords <- cbind(dataset.temp$x, dataset.temp$y)
      
      # Gee models
      if(model.fit == "gee.fixed") {
        sample.size<- nrow(dataset.temp)
        dataset.temp.split<- sample(dataset.temp$id, size = sample.size/2)
        training1<- dataset.temp[dataset.temp.split, ]
        coords.t1 <- cbind(training1$x, training1$y)
        training2<- dataset.temp[-dataset.temp.split, ]
        coords.t2 <- cbind(training2$x, training2$y)
        
        # Fit of autocorrelation for gee fixed model
        mod.glm <- glm(model.formula, family = binomial, data = dataset.temp)
        residuals.glm <- resid(mod.glm, type = "response")
        cor.glm <- correlog(dataset.temp$x, dataset.temp$y, residuals.glm, na.rm = TRUE, increment = 1, resamp = 1)
        alpha <- cor.glm$correlation[1]
        idn<- rep(1,nrow(dataset.temp))
        D <- as.matrix(dist(coords))
        R <- alpha^D
        
        #Fit gee fixed model
        options(warn = 2)
        attach(dataset.temp)
        mod <- try(gee(as.formula(model.formula), family= binomial, data = dataset.temp, id = idn, R = R, corstr = "fixed"), silent = TRUE)
        detach(dataset.temp)
        
        # Cross validated datasets and model building
        #T1
        mod.glm.t1 <- glm(model.formula, family = binomial, data = training1)
        residuals.glm.t1 <- resid(mod.glm.t1, type = "response")
        cor.glm.t1 <- correlog(training1$x, training1$y, residuals.glm.t1, na.rm = TRUE, increment = 1, resamp = 1)
        alpha.t1 <- cor.glm.t1$correlation[1]
        idn.t1 <- rep(1,nrow(training1))
        D.t1 <- as.matrix(dist(coords.t1))
        R.t1 <- alpha.t1^D.t1
        
        options(warn = 2)
        attach(training1)
        mod.train1 <- try(gee(as.formula(model.formula), family= binomial, data = training1, id = idn.t1, R = R.t1, corstr = "fixed"), silent = TRUE)
        detach(training1)
        
        #T2
        mod.glm.t2 <- glm(model.formula, family = binomial, data = training2)
        residuals.glm.t2 <- resid(mod.glm.t2, type = "response")
        cor.glm.t2 <- correlog(training2$x, training2$y, residuals.glm.t2, na.rm = TRUE, increment = 1, resamp = 1)
        alpha.t2 <- cor.glm.t2$correlation[1]
        idn.t2 <- rep(1,nrow(training2))
        D.t2 <- as.matrix(dist(coords.t2))
        R.t2 <- alpha.t1^D.t2
        
        options(warn = 2)
        attach(training2)
        mod.train2 <- try(gee(as.formula(model.formula), family= binomial, data = training2, id = idn.t2, R = R.t2, corstr = "fixed"), silent = TRUE)
        detach(training2)

        if(class(mod) == "try-error" || class(mod.train1) == "try-error" || class(mod.train2)== "try-error") {
          #print(k)
          #readline("Press <Enter> to continue")
          next
        }
        
        # Check for bad fits
        mod.summary<- data.frame(summary(mod)[[7]])
        mod.se<- mod.summary$Naive.S.E.
        if(any(as.numeric(mod$s.e) > 25)) {
          print(k)
          next
        }
      }
      
      if(model.fit == "gee.custom") {
        dataset.custom<- dataset.temp
        sample.size<- nrow(dataset.custom)
        dataset.custom$time<- rep(1, nrow(dataset.custom))
        dataset.temp.split<- sample(dataset.custom$id, size = sample.size/2)
        training1<- dataset.custom[dataset.temp.split, ]
        training2<- dataset.custom[-dataset.temp.split, ]
        coords<-as.matrix(cbind(dataset.custom$x, dataset.custom$y))
        coords.t1 <- cbind(training1$x, training1$y)
        coords.t2 <- cbind(training2$x, training2$y)
        
        #### Full model
        dist.mat <- as.matrix(dist(coords))
        diag(wt.mat)<- 0
        
        if(neg.exp == TRUE) {
          wt.mat<- exp(-neg.exp.param*dist.mat)
        }
        if(gauss == TRUE) {
        wt.mat<- (1/(sqrt(2*pi*sigma^2)))*exp(-(dist.mat - mu)^2/(sqrt(2*sigma^2)))
        }
        
        # zcor stuff
        zcor.use<- fixed2Zcor(wt.mat, id=dataset.custom$id, waves = dataset.custom$time)
        
        # Fit model
        mod<- geeglm(as.formula(model.formula), id=id, data=dataset.custom, corst ="fixed", zcor=zcor.use)
        
        #### Training1
        dist.mat.t1 <- as.matrix(dist(coords.t1))
        diag(wt.mat.t1)<- 0
        
        if(neg.exp == TRUE) {
          wt.mat.t1<- exp(-neg.exp.param*dist.mat.t1)
        }
        if(gauss == TRUE) {
          wt.mat.t1<- (1/(sqrt(2*pi*sigma^2)))*exp(-(dist.mat.t1 - mu)^2/(sqrt(2*sigma^2)))
        }
        
        # zcor stuff
        zcor.use.t1<- fixed2Zcor(wt.mat.t1, id=training1$id, waves = training1$time)
        
        # Fit model
        mod.t1<- geeglm(as.formula(model.formula), id=id, data=training1, corst ="fixed", zcor=zcor.use.t1)
        
        #### Training2
        dist.mat.t2 <- as.matrix(dist(coords.t2))
        diag(wt.mat.t2)<- 0
        
        if(neg.exp == TRUE) {
          wt.mat.t2<- exp(-neg.exp.param*dist.mat.t2)
        }
        if(gauss == TRUE) {
          wt.mat.t2<- (1/(sqrt(2*pi*sigma^2)))*exp(-(dist.mat.t2 - mu)^2/(sqrt(2*sigma^2)))
        }
        
        # zcor stuff
        zcor.use.t2<- fixed2Zcor(wt.mat.t2, id=training2$id, waves = training2$time)
        
        # Fit model
        mod.t2<- geeglm(as.formula(model.formula), id=id, data=training2, corst ="fixed", zcor=zcor.use.t2)
      }

      if(model.subset == TRUE) {
        quasi.like<- sum(dataset.temp$zero.one * log(mod$fitted)/(1-log(mod$fitted)) + log(1-mod$fitted))
        results$QICu[k]<- -2*(quasi.like - n.predictors)
        print(k)
        #readline(prompt = "Pause. Press <Enter> to continue...")
        next
      }

      if(model.subset == FALSE) {
        ## Storing results
        mod.summary<- data.frame(summary(mod)[[7]])
        mod.params<- mod.summary$Estimate
        mod.se<- mod.summary$Naive.S.E.
        mod.z<- mod.summary$Naive.z
        results$intercept[k]<- as.numeric(mod.params[1])
        results$intercept.z[k]<- as.numeric(mod.z[1])
        results$intercept.p[k]<- NA
        results$intercept.se[k]<- as.numeric(mod.se[1])
        results$x1[k]<- as.numeric(mod.params[2])
        results$x1.z[k]<- as.numeric(mod.z[2])
        results$x1.p[k]<- NA
        results$x1.se[k]<- as.numeric(mod.se[2])
        if(n.predictors == 2) {
          results$random[k]<- as.numeric(mod.params[3])
          results$random.z[k]<- as.numeric(mod.z[3])
          results$random.p[k]<- NA
          results$random.se[k]<- as.numeric(mod.se[3])
        }
        if(n.predictors == 3) {
          results$x2[k]<- as.numeric(mod.params[3])
          results$x2.z[k]<- as.numeric(mod.z[3])
          results$x2.p[k]<- NA
          results$x2.se[k]<- as.numeric(mod.se[3])
          results$random[k]<- as.numeric(mod.params[4])
          results$random.z[k]<-as.numeric(mod.z[4])
          results$random.p[k]<- NA
          results$random.se[k]<- as.numeric(mod.se[4])
        }
        if(n.predictors == 4) {
          results$x2[k]<- as.numeric(mod.params[3])
          results$x2.z[k]<- as.numeric(mod.z[3])
          results$x2.p[k]<- NA
          results$x2.se[k]<- as.numeric(mod.se[3])
          results$dslope[k]<- as.numeric(mod.params[4])
          results$dslope.z[k]<- as.numeric(mod.z[4])
          results$dslope.p[k]<- NA
          results$dslope.se[k]<- as.numeric(mod.se[4])
          results$random[k]<- as.numeric(mod.params[5])
          results$random.z[k]<- as.numeric(mod.z[5])
          results$random.p[k]<- NA
          results$random.se[k]<- as.numeric(mod.se[5])
        }
        quasi.like<- sum(dataset.temp$zero.one * log(mod$fitted)/(1-log(mod$fitted)) + log(1-mod$fitted))
        results$QICu[k]<- -2*(quasi.like - n.predictors)
        
        if(n.predictors == 3) {
          fullmod.predictions<- plogis(as.numeric(mod.params[1]) + as.numeric(mod.params[2])*dataset.temp$x1 + as.numeric(mod.params[3])*dataset.temp$x2 + as.numeric(mod.params[4])*dataset.temp$x.random)
          
          mod.summary.t2<- data.frame(summary(mod.train2)[[7]])
          mod.params.t2<- mod.summary.t2$Estimate
        
          mod.summary.t1<- data.frame(summary(mod.train1)[[7]])
          mod.params.t1<- mod.summary.t1$Estimate
          
          crossvalid.predictions<- c(plogis(as.numeric(mod.params.t2[1]) + as.numeric(mod.params.t2[2])*training1$x1 + as.numeric(mod.params.t2[3])*training1$x2 + as.numeric(mod.params.t2[4])*training1$x.random), 
                                     plogis(as.numeric(mod.params.t1[1]) + as.numeric(mod.params.t1[2])*training2$x1 + as.numeric(mod.params.t1[3])*training2$x2 + as.numeric(mod.params.t1[4])*training2$x.random))
        }
        
        if(n.predictors == 2) {
          fullmod.predictions<- plogis(as.numeric(mod.params[1]) + as.numeric(mod.params[2])*dataset.temp$x1 + as.numeric(mod.params[3])*dataset.temp$x.random)
          
          mod.summary.t2<- data.frame(summary(mod.train2)[[7]])
          mod.params.t2<- mod.summary.t2$Estimate
          
          mod.summary.t1<- data.frame(summary(mod.train1)[[7]])
          mod.params.t1<- mod.summary.t1$Estimate
          
          crossvalid.predictions<- c(plogis(as.numeric(mod.params.t2[1]) + as.numeric(mod.params.t2[2])*training1$x1 + as.numeric(mod.params.t2[3])*training1$x.random), 
                                     plogis(as.numeric(mod.params.t1[1]) + as.numeric(mod.params.t1[2])*training2$x1 + as.numeric(mod.params.t1[3])*training2$x.random))
        }

        dat.for.kappa.full<- data.frame(cbind(dataset.temp$id, dataset.temp$zero.one, fullmod.predictions))
        opt.thresh.full<- optimal.thresholds(DATA = dat.for.kappa.full, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.full<- opt.thresh.full[,2]
        confusion.mat.full<- cmx(DATA = dat.for.kappa.full, threshold = thresh.use.full)
        kappa.full<- Kappa(CMX = confusion.mat.full, st.dev = TRUE)
        results$full.kappa[k]<- kappa.full[1,1]
        results$full.auc[k]<- auc(DATA = dat.for.kappa.full, st.dev = FALSE)
        predictions.full.binned<- data.frame(summary(cut(fullmod.predictions, breaks = bins)))[,1]
        prop.full.presences<- suppressWarnings(aggregate(dataset.temp$zero.one, by = list(cut(fullmod.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.full.binned)
      
        # Occassionally have 0 obs in a bin, set to 0 instead of "Inf"
        prop.full.presences[prop.full.presences == "Inf"]<- 0
        ccc.results.full<- epi.ccc(prop.full.presences, midpts)
        results$full.ccc.mean[k]<- ccc.results.full$rho.c$est
        results$full.ccc.low[k]<- ccc.results.full$rho.c$lower
        results$full.ccc.up[k]<- ccc.results.full$rho.c$upper
        results$omission.err.full[k]<- confusion.mat.full[2,1]/sum(confusion.mat.full[,1])
        results$commission.err.full[k]<- confusion.mat.full[1,2]/sum(confusion.mat.full[,2])

        # Two-fold cross validation
        crossvalid.fulldataset.temp<- rbind(training1, training2)
        dat.for.kappa.crossvalid<- data.frame(cbind(crossvalid.fulldataset.temp$id, crossvalid.fulldataset.temp$zero.one, crossvalid.predictions))
        opt.thresh.crossvalid<- optimal.thresholds(DATA = dat.for.kappa.crossvalid, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.crossvalid<- opt.thresh.crossvalid[,2]
        confusion.mat.crossvalid<- cmx(DATA = dat.for.kappa.crossvalid, threshold = thresh.use.crossvalid)
        kappa.crossvalid<- Kappa(CMX = confusion.mat.crossvalid, st.dev = TRUE)
        results$split.kappa[k]<- kappa.crossvalid[1,1]
        results$split.auc[k]<- auc(DATA = dat.for.kappa.crossvalid, st.dev = FALSE)
        predictions.split.binned<- data.frame(summary(cut(crossvalid.predictions, breaks = bins)))[,1]
        prop.split.presences<- suppressWarnings(aggregate(crossvalid.fulldataset.temp$zero.one, by = list(cut(crossvalid.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.split.binned)
        prop.split.presences[prop.split.presences == "Inf"] <- 0
        ccc.results.split<- epi.ccc(prop.split.presences, midpts)
        results$split.ccc.mean[k]<- ccc.results.split$rho.c$est
        results$split.ccc.low[k]<- ccc.results.split$rho.c$lower
        results$split.ccc.up[k]<- ccc.results.split$rho.c$upper
        results$omission.err.split[k]<- confusion.mat.crossvalid[2,1]/sum(confusion.mat.crossvalid[,1])
        results$commission.err.split[k]<- confusion.mat.crossvalid[1,2]/sum(confusion.mat.crossvalid[,2])
                                 
        # Morans I 
        attach(dataset.temp)
        res.gee.fitresid<- suppressWarnings(res.gee.carl(as.formula(model.formula), family = "binomial", data = dataset.temp, n = nrow(dataset.temp), b = mod$coeff, R=R)) # Pearson resid, include working corr mat
        fullmod.resids<- res.gee.fitresid$resid
        detach(dataset.temp)
        
        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
          mat<- dataset.temp[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
          } else {
            dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
          }
          if(sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
          } else {
            morani.result<- morani(fullmod.resids, dist.binary)
          }
          morans.i$boot.iter[si]<- boot.id
          morans.i$dist.bin[si]<- morans.bins[j]
          morans.i$moransI[si]<- morani.result
          si<- si+1
        }
      }
      print(k)
    }
    
    ## Morans I residuals
    if(model.subset == FALSE) {
      moransI.name<- paste(directory, date, scenario, "moransI.csv", sep = "")
      #get.lock(moransI.name)
      append<- file.exists(moransI.name)
      write.table(morans.i, file = moransI.name, append = append,
            row.names = FALSE, col.names = !append, sep = ",")
      #return.lock(moransI.name)
    }

    ## Write out model selection results
    boot.globalmodresults<- paste(directory, date, scenario, "globalmodresults.csv", sep = "")
    #get.lock(boot.globalmodresults)
    append<- file.exists(boot.globalmodresults)
    write.table(results, file = boot.globalmodresults, append = append,
            row.names = FALSE, col.names = !append, sep = ",")
    #return.lock(boot.globalmodresults)
    rm(results, morans.i)
  }

  ### End Fit GEE
  
  ### Fit GLM wavelet
  if(model.fit == "glm.wavelet") {
    print("glm.wavelet")
   
    for (k in seq(along=datasets)) {
      
      boot.id<- boot.id.sequence[k]
      dataset.temp<- datasets[[k]]
      sample.size<- nrow(dataset.temp)
      
      # Full model
      mod<- try(suppressWarnings(WRM(as.formula(model.formula), family = "binomial", data = dataset.temp, coord = cbind(dataset.temp$x, dataset.temp$y), level = 1)), silent = TRUE)
      
      # Two-fold cross validated wavelet set up and model fit
      dataset.temp.split<- sample(dataset.temp$id, size = sample.size/2)
      training1<- dataset.temp[dataset.temp.split, ]
      training2<- dataset.temp[-dataset.temp.split, ]
      
      mod.train1<- try(suppressWarnings(WRM(as.formula(model.formula), family = "binomial", data = training1, coord = cbind(training1$x, training1$y), level = 1)), silent = TRUE)
      mod.train2<- try(suppressWarnings(WRM(as.formula(model.formula), family = "binomial", data = training2, coord = cbind(training2$x, training2$y), level = 1)), silent = TRUE)
      
      if(class(mod) == "try-error" || class(mod.train1) == "try-error" || class(mod.train2) == "try-error") {
        print("skip")
        next
      }
      
      # Calculating and storing results
      if(model.subset == FALSE) {
        nullmod <- glm(zero.one ~ 1, data=dataset.temp, family=binomial)
        results.summary<- data.frame(mod[2:5])
        
        # Check fit
        if(results.summary[1,2] > 25) {
          next
        }
        
        results$intercept[k]<- results.summary[1,1]
        results$intercept.z[k]<- results.summary[1,3]
        results$intercept.p[k]<- results.summary[1,4]
        results$intercept.se[k]<- results.summary[1,2]
        results$x1[k]<- results.summary[2,1]
        results$x1.z[k]<- results.summary[2,3]
        results$x1.p[k]<- results.summary[2,4]
        results$x1.se[k]<- results.summary[2,2]
        
        if(n.predictors == 2) {
          results$random[k]<- results.summary[3,1]
          results$random.z[k]<- results.summary[3,3]
          results$random.p[k]<- results.summary[3,4]
          results$random.se[k]<- results.summary[3,2]
        }
        
        if(n.predictors == 3) {
          results$x2[k]<- results.summary[3,1]
          results$x2.z[k]<- results.summary[3,3]
          results$x2.p[k]<- results.summary[3,4]
          results$x2.se[k]<- results.summary[3,2]
          results$random[k]<- results.summary[4,1]
          results$random.z[k]<- results.summary[4,3]
          results$random.p[k]<- results.summary[4,4]
          results$random.se[k]<- results.summary[4,2]
        }
        
        if(n.predictors == 4) {
          results$x2[k]<- results.summary[3,1]
          results$x2.z[k]<- results.summary[3,3]
          results$x2.p[k]<- results.summary[3,4]
          results$x2.se[k]<- results.summary[3,2]
          results$dslope[k]<- results.summary[4,1]
          results$dslope.z[k]<- results.summary[4,3]
          results$dslope.p[k]<- results.summary[4,4]
          results$dslope.se[k]<- results.summary[4,2]
          results$random[k]<- results.summary[5,1]
          results$random.z[k]<- results.summary[5,3]
          results$random.p[k]<- results.summary[5,4]
          results$random.se[k]<- results.summary[5,2]
        }

        if(n.predictors == 3) {
          fullmod.predictions<- as.numeric(mod$fitted)
          
          crossvalid.predictions<- c(plogis(as.numeric(mod.train2$b[1]) + as.numeric(mod.train2$b[2])*training1$x1 + as.numeric(mod.train2$b[3])*training1$x2 + as.numeric(mod.train2$b[4])*training1$x.random), 
                                   plogis(as.numeric(mod.train1$b[1]) + as.numeric(mod.train1$b[2])*training2$x1 + as.numeric(mod.train1$b[3])*training2$x2 + as.numeric(mod.train1$b[4])*training2$x.random))
        }
      
        if(n.predictors == 2) {
          fullmod.predictions<- as.numeric(mod$fitted)
        
          crossvalid.predictions<- c(plogis(as.numeric(mod.train2$b[1]) + as.numeric(mod.train2$b[2])*training1$x1 + as.numeric(mod.train2$b[3])*training1$x.random),
                                   plogis(as.numeric(mod.train1$b[1]) + as.numeric(mod.train1$b[2])*training2$x1 + as.numeric(mod.train1$b[3])*training2$x.random))
        }
      
        dat.for.kappa.full<- data.frame(cbind(dataset.temp$id, dataset.temp$zero.one, fullmod.predictions))
        opt.thresh.full<- optimal.thresholds(DATA = dat.for.kappa.full, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.full<- opt.thresh.full[,2]
        confusion.mat.full<- cmx(DATA = dat.for.kappa.full, threshold = thresh.use.full)
        kappa.full<- Kappa(CMX = confusion.mat.full, st.dev = TRUE)
        results$full.kappa[k]<- kappa.full[1,1]
        results$full.auc[k]<- auc(DATA = dat.for.kappa.full, st.dev = FALSE)
        predictions.full.binned<- data.frame(summary(cut(fullmod.predictions, breaks = bins)))[,1]
        prop.full.presences<- suppressWarnings(aggregate(dataset.temp$zero.one, by = list(cut(fullmod.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.full.binned)
      
        # Occassionally have 0 obs in a bin, set to 0 instead of "Inf"
        prop.full.presences[prop.full.presences == "Inf"]<- 0
        ccc.results.full<- epi.ccc(prop.full.presences, midpts)
        results$full.ccc.mean[k]<- ccc.results.full$rho.c$est
        results$full.ccc.low[k]<- ccc.results.full$rho.c$lower
        results$full.ccc.up[k]<- ccc.results.full$rho.c$upper
        results$omission.err.full[k]<- confusion.mat.full[2,1]/sum(confusion.mat.full[,1])
        results$commission.err.full[k]<- confusion.mat.full[1,2]/sum(confusion.mat.full[,2])
        
        # Two-fold cross validation
        crossvalid.fulldataset.temp<- rbind(training1, training2)
        dat.for.kappa.crossvalid<- data.frame(cbind(crossvalid.fulldataset.temp$id, crossvalid.fulldataset.temp$zero.one, crossvalid.predictions))
        opt.thresh.crossvalid<- optimal.thresholds(DATA = dat.for.kappa.crossvalid, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.crossvalid<- opt.thresh.crossvalid[,2]
        confusion.mat.crossvalid<- cmx(DATA = dat.for.kappa.crossvalid, threshold = thresh.use.crossvalid)
        kappa.crossvalid<- Kappa(CMX = confusion.mat.crossvalid, st.dev = TRUE)
        results$split.kappa[k]<- kappa.crossvalid[1,1]
        results$split.auc[k]<- auc(DATA = dat.for.kappa.crossvalid, st.dev = FALSE)
        predictions.split.binned<- data.frame(summary(cut(crossvalid.predictions, breaks = bins)))[,1]
        prop.split.presences<- suppressWarnings(aggregate(crossvalid.fulldataset.temp$zero.one, by = list(cut(crossvalid.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.split.binned)
        prop.split.presences[prop.split.presences == "Inf"] <- 0
        ccc.results.split<- epi.ccc(prop.split.presences, midpts)
        results$split.ccc.mean[k]<- ccc.results.split$rho.c$est
        results$split.ccc.low[k]<- ccc.results.split$rho.c$lower
        results$split.ccc.up[k]<- ccc.results.split$rho.c$upper
        results$omission.err.split[k]<- confusion.mat.crossvalid[2,1]/sum(confusion.mat.crossvalid[,1])
        results$commission.err.split[k]<- confusion.mat.crossvalid[1,2]/sum(confusion.mat.crossvalid[,2])
      
        # Morans I
        fullmod.resids<- mod$resid # Pearson residuals, include working corr mat
        
        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
          mat<- dataset.temp[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
          } else {
            dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
          }
          if(sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
          } else {
            morani.result<- morani(fullmod.resids, dist.binary)
          }
          morans.i$boot.iter[si]<- boot.id
          morans.i$dist.bin[si]<- morans.bins[j]
          morans.i$moransI[si]<- morani.result
          si<- si+1
        }
      }
      print(k)
    }
    
    ## Morans I residuals
    if(model.subset == FALSE) {
      moransI.name<- paste(directory, date, scenario, "moransI.csv", sep = "")
      #get.lock(moransI.name)
      append<- file.exists(moransI.name)
      write.table(morans.i, file = moransI.name, append = append,
                row.names = FALSE, col.names = !append, sep = ",")
      #return.lock(moransI.name)
    }
    
    ## Write out model selection results
    boot.globalmodresults<- paste(directory, date, scenario, "globalmodresults.csv", sep = "")
    #get.lock(boot.globalmodresults)
    append<- file.exists(boot.globalmodresults)
    write.table(results, file = boot.globalmodresults, append = append,
            row.names = FALSE, col.names = !append, sep = ",")
    #return.lock(boot.globalmodresults)
    rm(results, morans.i)
  }
  
  ### End Fit GLM wavelet
  
  ### Fit GLM custom autocovariate
  if(model.fit == "custom") {
    print("custom")
    suppressWarnings(library(fields))
    n.predictors<- length(attr(terms(as.formula(model.formula)), "term.labels"))

    for (k in seq(along=datasets)) {
      
      boot.id<- boot.id.sequence[k]
      dataset.temp<- datasets[[k]]
      coords<-as.matrix(cbind(dataset.temp$x, dataset.temp$y))
      dist.mat <- as.matrix(dist(coords))
      sample.size<- nrow(dataset.temp)
      dataset.temp.split<- sample(dataset.temp$id, size = sample.size/2)
  
      if(custom.fit == "neg.exp.free.full" || custom.fit == "neg.exp.free.red"){
        
        ## Full model
        # Fit glm
        glm.mod<- glm(model.formula, data = dataset.temp, family = binomial(link=logit), maxit = 1000)
        glm.resids<- residuals.glm(glm.mod, type = resids.type)
        
        # Calculate Moran's I at distance bins
        morans.i.temp<- data.frame(matrix(nrow = length(morans.bins), ncol = 2))
        names(morans.i.temp)[1:2]<- c("dist.bin", "moransI")
        
        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
          mat<- dataset.temp[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<morans.bins[j], 1, 0)
          } else {
            dist.binary<- ifelse(dist.mat>=morans.bins[j-1] & dist.mat<morans.bins[j], 1, 0)
          }
          if(sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
          } else {
            morani.result<- morani(glm.resids, dist.binary)
          }
          morans.i.temp$dist.bin[j]<- morans.bins[j]
          morans.i.temp$moransI[j]<- morani.result
        }
        
        # Remove NAs and fit NLS to MoransI data
        morans.i.nls<- na.omit(morans.i.temp)
        nls.fit<- try(nls(moransI ~ exp(-neg.exp.param*dist.bin), 
                      data = morans.i.nls, 
                      start = list(neg.exp.param = start.params$neg.exp)), silent = TRUE)
        
        if(class(nls.fit) == "try-error") {
          print("skip")
          next
        }
        
        # Recalculate neighborhood intensity values
        mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
        mat<- dataset.temp[,c(2:3)] 
        dist.mat<- rdist(as.matrix(mat))
        wt.mat<- exp(-coef(nls.fit)[1]*dist.mat)
        diag(wt.mat)<- 0
        ind.vec<- ifelse(dataset.temp$zero.one == 0, -1, 1)
        dataset.temp$x.nbintensity.nls<- as.numeric(wt.mat%*%ind.vec)
        
        # Refit with neighborhood intensity autocovariate
        model.formula.full<- c(paste(model.formula, "+", "x.nbintensity.nls"))
        options(warn = 2)
        mod<- try(glm(as.formula(model.formula.full), 
                      data = dataset.temp, family = binomial), silent = TRUE)
        
        ## Training Data1
        training1<- dataset.temp[dataset.temp.split, ]
        
        glm.mod.t1<- try(glm(model.formula, data = training1, family = binomial(link=logit), maxit = 1000), silent = TRUE)
        
        if(any(class(glm.mod.t1) == "try-error")) {
          print("skip")
          next
        }
        
        glm.resids.t1<- residuals.glm(glm.mod.t1, type = resids.type)
        
        # Calculate Moran's I at distance bins
        morans.i.temp.t1<- data.frame(matrix(nrow = length(morans.bins), ncol = 2))
        names(morans.i.temp.t1)[1:2]<- c("dist.bin", "moransI")
        
        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(training1), ncol = 2)
          mat<- training1[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<morans.bins[j], 1, 0)
          } else {
            dist.binary<- ifelse(dist.mat>=morans.bins[j-1] & dist.mat<morans.bins[j], 1, 0)
          }
          if(sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
          } else {
            morani.result<- morani(glm.resids.t1, dist.binary)
          }
          morans.i.temp.t1$dist.bin[j]<- morans.bins[j]
          morans.i.temp.t1$moransI[j]<- morani.result
        }
        
        # Remove NAs and fit NLS to MoransI data
        morans.i.nls.t1<- na.omit(morans.i.temp.t1)
        nls.fit.t1<- try(nls(moransI ~ exp(-neg.exp.param*dist.bin), 
                      data = morans.i.nls.t1, 
                      start = list(neg.exp.param = start.params$neg.exp)), silent = TRUE)
        
        if(class(nls.fit.t1) == "try-error") {
          print("skip")
          next
        }
        
        # Recalculate neighborhood intensity values
        mat<- matrix(nrow = nrow(training1), ncol = 2)
        mat<- training1[,c(2:3)] 
        dist.mat<- rdist(as.matrix(mat))
        wt.mat<- exp(-coef(nls.fit.t1)[1]*dist.mat)
        diag(wt.mat)<- 0
        ind.vec<- ifelse(training1$zero.one == 0, -1, 1)
        training1$x.nbintensity.nls.t<- as.numeric(wt.mat%*%ind.vec)
        
        model.formula.full.t1<- c(paste(model.formula, "+", "x.nbintensity.nls.t"))
        options(warn = 2)
        mod.train1<- try(glm(as.formula(model.formula.full.t1), 
                             data = training1, family = binomial), silent = TRUE)
        
        ## Training Data2
        training2<- dataset.temp[-dataset.temp.split, ]
        
        glm.mod.t2<- try(glm(model.formula, data = training2, family = binomial(link=logit), maxit = 1000), silent = TRUE)
        
        if(any(class(glm.mod.t2) == "try-error")) {
          print("skip")
          next
        }
        
        glm.resids.t2<- residuals.glm(glm.mod.t2, type = resids.type)
        
        # Calculate Moran's I at distance bins
        morans.i.temp.t2<- data.frame(matrix(nrow = length(morans.bins), ncol = 2))
        names(morans.i.temp.t2)[1:2]<- c("dist.bin", "moransI")
        
        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(training2), ncol = 2)
          mat<- training2[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
          } else {
            dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
          }
          if(sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
          } else {
            morani.result<- morani(glm.resids.t2, dist.binary)
          }
          morans.i.temp.t2$dist.bin[j]<- morans.bins[j]
          morans.i.temp.t2$moransI[j]<- morani.result
        }
        
        # Remove NAs and fit NLS to MoransI data
        morans.i.nls.t2<- na.omit(morans.i.temp.t2)
        nls.fit.t2<- try(nls(moransI ~ exp(-neg.exp.param*dist.bin), 
                         data = morans.i.nls.t2, 
                         start = list(neg.exp.param = start.params$neg.exp)), silent = TRUE)
        
        if(class(nls.fit.t2) == "try-error") {
          print("skip")
          next
        }
        
        # Recalculate neighborhood intensity values
        mat<- matrix(nrow = nrow(training2), ncol = 2)
        mat<- training2[,c(2:3)] 
        dist.mat<- rdist(as.matrix(mat))
        wt.mat<- exp(-coef(nls.fit.t2)[1]*dist.mat)
        diag(wt.mat)<- 0
        ind.vec<- ifelse(training2$zero.one == 0, -1, 1)
        training2$x.nbintensity.nls.t<- as.numeric(wt.mat%*%ind.vec)
        
        model.formula.full.t2<- c(paste(model.formula, "+", "x.nbintensity.nls.t"))
        options(warn = 2)
        mod.train2<- try(glm(as.formula(model.formula.full.t2), 
                            data = training2, family = binomial), silent = TRUE)
      }

      if(custom.fit == "gauss.free.full" || custom.fit == "gauss.free.red"){
        
        # Fit glm
        glm.mod<- try(glm(model.formula, data = dataset.temp, family = binomial(link=logit), maxit = 1000), silent = TRUE)
        
        if(any(class(glm.mod) == "try-error")) {
          print("skip")
          next
        }
        
        glm.resids<- residuals.glm(glm.mod, type = resids.type)

        # Calculate Moran's I at distance bins
        morans.i.temp<- data.frame(matrix(nrow = length(morans.bins), ncol = 2))
        names(morans.i.temp)[1:2]<- c("dist.bin", "moransI")

        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
          mat<- dataset.temp[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
            } else {
              dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
            }
          if(sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
            } else {
              morani.result<- morani(glm.resids, dist.binary)
            }
          morans.i.temp$dist.bin[j]<- morans.bins[j]
          morans.i.temp$moransI[j]<- morani.result
        }
        
        # Remove NAs and fit NLS to MoransI data
        morans.i.nls<- na.omit(morans.i.temp)
        nls.fit<- try(nls(moransI ~ ((1/(sigma.param*sqrt(2*pi)))*exp(-(((dist.bin-0)^2)/(2*sigma.param^2)))), 
              data = morans.i.nls, 
              start = list(sigma.param = start.params$sigma)), silent = TRUE)
        
        if(class(nls.fit) == "try-error") {
          print("skip")
          next
        }
        
        # Recalculate neighborhood intensity values
        mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
        mat<- dataset.temp[,c(2:3)] 
        dist.mat<- rdist(as.matrix(mat))
        wt.mat<- ((1/(coef(nls.fit)[1]*sqrt(2*pi)))*exp(-(((dist.mat-0)^2)/(2*coef(nls.fit)[1]^2))))
        diag(wt.mat)<- 0
        ind.vec<- ifelse(dataset.temp$zero.one == 0, -1, 1)
        dataset.temp$x.nbintensity.nls<- as.numeric(wt.mat%*%ind.vec)

        # Refit with neighborhood intensity autocovariate
        model.formula.full<- c(paste(model.formula, "+", "x.nbintensity.nls"))
        options(warn = 2)
        mod<- try(glm(as.formula(model.formula.full), data = dataset.temp, 
                      family = binomial), silent = TRUE)
        
        ## Training Data1
        training1<- dataset.temp[dataset.temp.split, ]
        
        glm.mod.t1<- try(glm(model.formula, data = training1, family = binomial(link=logit), maxit = 1000), silent = TRUE)
        
        if(any(class(glm.mod.t1) == "try-error")) {
          print("skip")
          next
        }
        
        glm.resids.t1<- residuals.glm(glm.mod.t1, type = resids.type)
        
        # Calculate Moran's I at distance bins
        morans.i.temp.t1<- data.frame(matrix(nrow = length(morans.bins), ncol = 2))
        names(morans.i.temp.t1)[1:2]<- c("dist.bin", "moransI")
        
        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(training1), ncol = 2)
          mat<- training1[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
          } else {
            dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
          }
          if(sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
          } else {
            morani.result<- morani(glm.resids.t1, dist.binary)
          }
          morans.i.temp.t1$dist.bin[j]<- morans.bins[j]
          morans.i.temp.t1$moransI[j]<- morani.result
        }
        
        # Remove NAs and fit NLS to MoransI data
        morans.i.nls.t1<- na.omit(morans.i.temp.t1)
        nls.fit.t1<- try(nls(moransI ~ ((1/(sigma.param*sqrt(2*pi)))*exp(-(((dist.bin-0)^2)/(2*sigma.param^2)))), 
                         data = morans.i.nls.t1, 
                         start = list(sigma.param = start.params$sigma)), silent = TRUE)
        
        if(class(nls.fit.t1) == "try-error") {
          print("skip")
          next
        }
        
        # Recalculate neighborhood intensity values
        mat<- matrix(nrow = nrow(training1), ncol = 2)
        mat<- training1[,c(2:3)] 
        dist.mat<- rdist(as.matrix(mat))
        wt.mat<- ((1/(coef(nls.fit.t1)[1]*sqrt(2*pi)))*exp(-(((dist.mat-0)^2)/(2*coef(nls.fit.t1)[1]^2))))
        diag(wt.mat)<- 0
        ind.vec<- ifelse(training1$zero.one == 0, -1, 1)
        training1$x.nbintensity.nls.t<- as.numeric(wt.mat%*%ind.vec)
        
        model.formula.full.t1<- c(paste(model.formula, "+", "x.nbintensity.nls.t"))
        options(warn = 2)
        mod.train1<- try(glm(as.formula(model.formula.full.t1), 
                             data = training1, family = binomial), silent = TRUE)
        
        ## Training Data2
        training2<- dataset.temp[-dataset.temp.split, ]
        
        glm.mod.t2<- try(glm(model.formula, data = training2, family = binomial(link=logit), maxit = 1000), silent = TRUE)
        
        if(any(class(glm.mod.t2) == "try-error")) {
          print("skip")
          next
        }
        
        glm.resids.t2<- residuals.glm(glm.mod.t2, type = resids.type)
        
        # Calculate Moran's I at distance bins
        morans.i.temp.t2<- data.frame(matrix(nrow = length(morans.bins), ncol = 2))
        names(morans.i.temp.t2)[1:2]<- c("dist.bin", "moransI")
        
        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(training2), ncol = 2)
          mat<- training2[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
          } else {
            dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
          }
          if(sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
          } else {
            morani.result<- morani(glm.resids.t2, dist.binary)
          }
          morans.i.temp.t2$dist.bin[j]<- morans.bins[j]
          morans.i.temp.t2$moransI[j]<- morani.result
        }
        
        # Remove NAs and fit NLS to MoransI data
        morans.i.nls.t2<- na.omit(morans.i.temp.t2)
        nls.fit.t2<- try(nls(moransI ~ ((1/(sigma.param*sqrt(2*pi)))*exp(-(((dist.bin-0)^2)/(2*sigma.param^2)))), 
                         data = morans.i.nls.t2, 
                         start = list(sigma.param = start.params$sigma)), silent = TRUE)
        
        if(class(nls.fit.t2) == "try-error") {
          print("skip")
          next
        }
        
        # Recalculate neighborhood intensity values
        mat<- matrix(nrow = nrow(training2), ncol = 2)
        mat<- training2[,c(2:3)] 
        dist.mat<- rdist(as.matrix(mat))
        wt.mat<- ((1/(coef(nls.fit.t2)[1]*sqrt(2*pi)))*exp(-(((dist.mat-0)^2)/(2*coef(nls.fit.t2)[1]^2))))
        diag(wt.mat)<- 0
        ind.vec<- ifelse(training2$zero.one == 0, -1, 1)
        training2$x.nbintensity.nls.t<- as.numeric(wt.mat%*%ind.vec)
        
        model.formula.full.t2<- c(paste(model.formula, "+", "x.nbintensity.nls.t"))
        options(warn = 2)
        mod.train2<- try(glm(as.formula(model.formula.full.t2), 
                             data = training2, family = binomial), silent = TRUE)
      }
      
      if(any(class(mod) == "try-error") || any(class(mod.train1) == "try-error") || any(class(mod.train2) == "try-error") || as.numeric(as.character(coef(summary(mod))[1,2])) > 25) {
        print("skip")
        next
      } else {
        
        # Storing statistical inference results
        nullmod <- glm(zero.one ~ 1, data=dataset.temp, family=binomial)
        results$intercept[k]<- as.numeric(as.character(coefficients(mod)[1]))
        results$intercept.z[k]<- as.numeric(as.character(coef(summary(mod))[1,3]))
        results$intercept.p[k]<- as.numeric(as.character(coef(summary(mod))[1,4]))
        results$intercept.se[k]<- as.numeric(as.character(coef(summary(mod))[1,2]))
        results$x1[k]<- as.numeric(as.character(coefficients(mod)[2]))
        results$x1.z[k]<- as.numeric(as.character(coef(summary(mod))[2,3]))
        results$x1.p[k]<- as.numeric(as.character(coef(summary(mod))[2,4]))
        results$x1.se[k]<- as.numeric(as.character(coef(summary(mod))[2,2]))
        
        if(custom.fit == "neg.exp.free.full") {
          results$x2[k]<- as.numeric(as.character(coefficients(mod)[3]))
          results$x2.z[k]<- as.numeric(as.character(coef(summary(mod))[3,3]))
          results$x2.p[k]<- as.numeric(as.character(coef(summary(mod))[3,4]))
          results$x2.se[k]<- as.numeric(as.character(coef(summary(mod))[3,2]))
          results$random[k]<- as.numeric(as.character(coefficients(mod)[4]))
          results$random.z[k]<- as.numeric(as.character(coef(summary(mod))[4,3]))
          results$random.p[k]<- as.numeric(as.character(coef(summary(mod))[4,4]))
          results$random.se[k]<- as.numeric(as.character(coef(summary(mod))[4,2]))
          results$dslope[k]<- as.numeric(as.character(coef(summary(mod))[5]))
          results$dslope.z[k]<- as.numeric(as.character(coef(summary(mod))[5,3]))
          results$dslope.p[k]<- as.numeric(as.character(coef(summary(mod))[5,4]))
          results$dslope.se[k]<- as.numeric(as.character(coef(summary(mod))[5,2]))
          results$neg.exp.param[k]<- coef(nls.fit)[1]
        }
        
        if(custom.fit == "neg.exp.free.red") {
          results$random[k]<- as.numeric(as.character(coefficients(mod)[3]))
          results$random.z[k]<- as.numeric(as.character(coef(summary(mod))[3,3]))
          results$random.p[k]<- as.numeric(as.character(coef(summary(mod))[3,4]))
          results$random.se[k]<- as.numeric(as.character(coef(summary(mod))[3,2]))
          results$dslope[k]<- as.numeric(as.character(coefficients(mod)[4]))
          results$dslope.z[k]<- as.numeric(as.character(coef(summary(mod))[4,3]))
          results$dslope.p[k]<- as.numeric(as.character(coef(summary(mod))[4,4]))
          results$dslope.se[k]<- as.numeric(as.character(coef(summary(mod))[4,2]))
          results$neg.exp.param[k]<- coef(nls.fit)[1]
        }
        
        if(custom.fit == "gauss.free.full") {
          results$x2[k]<- as.numeric(as.character(coefficients(mod)[3]))
          results$x2.z[k]<- as.numeric(as.character(coef(summary(mod))[3,3]))
          results$x2.p[k]<- as.numeric(as.character(coef(summary(mod))[3,4]))
          results$x2.se[k]<- as.numeric(as.character(coef(summary(mod))[3,2]))
          results$random[k]<- as.numeric(as.character(coefficients(mod)[4]))
          results$random.z[k]<- as.numeric(as.character(coef(summary(mod))[4,3]))
          results$random.p[k]<- as.numeric(as.character(coef(summary(mod))[4,4]))
          results$random.se[k]<- as.numeric(as.character(coef(summary(mod))[4,2]))
          results$dslope[k]<- as.numeric(as.character(coef(summary(mod))[5]))
          results$dslope.z[k]<- as.numeric(as.character(coef(summary(mod))[5,3]))
          results$dslope.p[k]<- as.numeric(as.character(coef(summary(mod))[5,4]))
          results$dslope.se[k]<- as.numeric(as.character(coef(summary(mod))[5,2]))
          results$sigma[k]<- coef(nls.fit)[1]
        }
        
        if(custom.fit == "gauss.free.red") {
          results$random[k]<- as.numeric(as.character(coefficients(mod)[3]))
          results$random.z[k]<- as.numeric(as.character(coef(summary(mod))[3,3]))
          results$random.p[k]<- as.numeric(as.character(coef(summary(mod))[3,4]))
          results$random.se[k]<- as.numeric(as.character(coef(summary(mod))[3,2]))
          results$dslope[k]<- as.numeric(as.character(coefficients(mod)[4]))
          results$dslope.z[k]<- as.numeric(as.character(coef(summary(mod))[4,3]))
          results$dslope.p[k]<- as.numeric(as.character(coef(summary(mod))[4,4]))
          results$dslope.se[k]<- as.numeric(as.character(coef(summary(mod))[4,2]))
          results$sigma[k]<- coef(nls.fit)[1]
        }
        
        # Calculating and storing model selection and prediction statistics
        results$deviance.explained[k]<- (mod$null - mod$deviance)/mod$null
        likelihood.ratio.results<- lrtest(mod, nullmod)
        results$mod.nll[k]<- -1*likelihood.ratio.results$LogLik[1]
        results$null.nll[k]<- -1*likelihood.ratio.results$LogLik[2]
        results$lrtest[k]<- likelihood.ratio.results$Pr[2]
        results$AICc[k]<- AICc(mod, nobs = sample.size)
        
        dataset.pred<- dataset.temp
        dataset.pred$x.nbintensity.nls<- 0
        fullmod.predictions<- predict.glm(mod, dataset.pred, type = "response")
  dat.for.kappa.full<- data.frame(cbind(dataset.temp$id, dataset.temp$zero.one, fullmod.predictions))
        opt.thresh.full<- optimal.thresholds(DATA = dat.for.kappa.full, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.full<- opt.thresh.full[,2]
        confusion.mat.full<- cmx(DATA = dat.for.kappa.full, threshold = thresh.use.full)
        kappa.full<- Kappa(CMX = confusion.mat.full, st.dev = TRUE)
        results$full.kappa[k]<- kappa.full[1,1]
        results$full.auc[k]<- auc(DATA = dat.for.kappa.full, st.dev = FALSE)
        predictions.full.binned<- data.frame(summary(cut(fullmod.predictions, breaks = bins)))[,1]
        prop.full.presences<- suppressWarnings(aggregate(dataset.temp$zero.one, by = list(cut(fullmod.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.full.binned)
        
        # Occassionally have 0 obs in a bin, set to 0 instead of "Inf"
        prop.full.presences[prop.full.presences == "Inf"]<- 0
        
        ccc.results.full<- epi.ccc(prop.full.presences, midpts)
        results$full.ccc.mean[k]<- ccc.results.full$rho.c$est
        results$full.ccc.low[k]<- ccc.results.full$rho.c$lower
        results$full.ccc.up[k]<- ccc.results.full$rho.c$upper
        results$omission.err.full[k]<- confusion.mat.full[2,1]/sum(confusion.mat.full[,1])
        results$commission.err.full[k]<- confusion.mat.full[1,2]/sum(confusion.mat.full[,2])
  
        # Two-fold cross validation
        training1.pred<- training1
        training1.pred$x.nbintensity.nls.t<- 0
  
        training2.pred<- training2
        training2.pred$x.nbintensity.nls.t<- 0
  
        crossvalid.predictions<- c(predict.glm(mod.train2, newdata = training1.pred, type= "response"), predict.glm(mod.train1, newdata = training2.pred, type= "response"))      
        crossvalid.fulldataset.temp<- rbind(training1, training2)
        dat.for.kappa.crossvalid<- data.frame(cbind(crossvalid.fulldataset.temp$id, crossvalid.fulldataset.temp$zero.one, crossvalid.predictions))
        opt.thresh.crossvalid<- optimal.thresholds(DATA = dat.for.kappa.crossvalid, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.crossvalid<- opt.thresh.crossvalid[,2]
        confusion.mat.crossvalid<- cmx(DATA = dat.for.kappa.crossvalid, threshold = thresh.use.crossvalid)
        kappa.crossvalid<- Kappa(CMX = confusion.mat.crossvalid, st.dev = TRUE)
        results$split.kappa[k]<- kappa.crossvalid[1,1]
        results$split.auc[k]<- auc(DATA = dat.for.kappa.crossvalid, st.dev = FALSE)
        predictions.split.binned<- data.frame(summary(cut(crossvalid.predictions, breaks = bins)))[,1]
        prop.split.presences<- suppressWarnings(aggregate(crossvalid.fulldataset.temp$zero.one, by = list(cut(crossvalid.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.split.binned)
        prop.split.presences[prop.split.presences == "Inf"] <- 0
        ccc.results.split<- epi.ccc(prop.split.presences, midpts)
        results$split.ccc.mean[k]<- ccc.results.split$rho.c$est
        results$split.ccc.low[k]<- ccc.results.split$rho.c$lower
        results$split.ccc.up[k]<- ccc.results.split$rho.c$upper
        results$omission.err.split[k]<- confusion.mat.crossvalid[2,1]/sum(confusion.mat.crossvalid[,1])
        results$commission.err.split[k]<- confusion.mat.crossvalid[1,2]/sum(confusion.mat.crossvalid[,2])
          
        # Morans I
        fullmod.resids<- residuals.glm(mod, dataset.temp, type = resids.type)
        
        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
          mat<- dataset.temp[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
          } else { 
            dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
          }
          if (sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
          } else {
            morani.result<- morani(fullmod.resids, dist.binary)
          }
          morans.i$boot.iter[si]<- boot.id
          morans.i$dist.bin[si]<- morans.bins[j]
          morans.i$moransI[si]<- morani.result
          si<- si+1
        }
        
        # Calculating model all subset information from biostats
        if(custom.fit == "neg.exp.free.full" || custom.fit == "neg.exp.free.red" 
           || custom.fit == "gauss.free.full" || custom.fit == "gauss.free.red") {
          response.index<- which(colnames(dataset.temp) == "zero.one")
          terms<- attr(terms(as.formula(model.formula)), "term.labels")
          x.index<- which(colnames(dataset.temp) %in% terms)
          all.subsets.list<- all.subsets.glm(y = dataset.temp[,response.index], x1 = dataset.temp[,x.index], x.force = data.frame(dataset.temp$x.nbintensity.nls), family = binomial(link=logit), maxp = length(x.index), select = "AIC", varimp = TRUE, delta.AIC = 10, coef.table = TRUE, barplot = FALSE)
          modstats.temp<- all.subsets.list$model.statistics
          modstats.temp$boot<- rep(boot.id, nrow(modstats.temp))
          names(modstats.temp)[1:8]<- c("id", "model", "d2", "AICc", "deltaAICc", "wgtAICc", "rank", "boot")
          exists.logical<- exists("modstats.results")
          if(exists.logical == FALSE) {
            modstats.results<- modstats.temp
            names(modstats.results)[1:8]<- c("id", "model", "d2", "AICc", "deltaAICc", "wgtAICc", "rank", "boot")
          } else {
            modstats.results<- data.frame(rbind(modstats.results, modstats.temp))
            names(modstats.results)[1:8]<- c("id", "model", "d2", "AICc", "deltaAICc", "wgtAICc", "rank", "boot")
          }
        }
      }
      print(k)
    }
    
    moransI.name<- paste(directory, date, scenario, "moransI.csv", sep = "")
    #get.lock(moransI.name)
    append<- file.exists(moransI.name)
    write.table(morans.i, file = moransI.name, append = append,
                row.names = FALSE, col.names = !append, sep = ",")
    #return.lock(moransI.name)
  
    # Output statistical inference and model selection results
    boot.globalmodresults<- paste(directory, date, scenario, "globalmodresults.csv", sep = "")
    #get.lock(boot.globalmodresults)
    append<- file.exists(boot.globalmodresults)
    write.table(results, file = boot.globalmodresults, append = append,
            row.names = FALSE, col.names = !append, sep = ",")
    #return.lock(boot.globalmodresults)
    if(n.predictors == 2 || n.predictors == 3 || n.predictors == 4) {
      modstats.name<- paste(directory, date, scenario, "modstats.csv", sep = "")
      #get.lock(modstats.name)
      append<- file.exists(modstats.name)
      write.table(modstats.results, file = modstats.name, append = append,
                  row.names = FALSE, col.names = !append, sep = ",")
      #return.lock(modstats.name)
    
      # Remove for cleaning up
      rm(results, morans.i, modstats.results)
      } else {
        rm(results, morans.i)
      }
  }
  
  ## End GLM custom autocovariate

  ### Fit spGLM 
  if(model.fit == "spglm") {
    print("spGLM")
    suppressWarnings(library(spBayes))
    
    # Switch name from AIC to DIC, because spGLM uses Bayesian likelihood+prior
    names(results)[which(colnames(results) == "AICc")]<- "DIC"
    
    for (k in seq(along=datasets)) {
      #       if(k == 35) {
      #         next
      #       }
      #Sys.time()
      boot.id<- boot.id.sequence[k]
      dataset.temp<- datasets[[k]]
      coords <- cbind(dataset.temp$x, dataset.temp$y)
      
      # Training datasets
      sample.size<- nrow(dataset.temp)
      dataset.temp.split<- sample(dataset.temp$id, size = sample.size/2)
      training1<- dataset.temp[dataset.temp.split, ]
      coords.t1<- cbind(training1$x, training1$y)
      training2<- dataset.temp[-dataset.temp.split, ]
      coords.t2<- cbind(training2$x, training2$y)
    
      # spGLM models
      weights <- rep(1, nrow(dataset.temp)) # Each site has a trial size of 1 (p or a)
      
      # Step 1: Fit non-spatial GLM to get parameter starting and tuning values
      glm.nonsp <- glm(model.formula, family = "binomial", data = dataset.temp)
      beta.starting <- coefficients(glm.nonsp)
      beta.tuning <- t(chol(vcov(glm.nonsp))) 

      ## Fit spGLM for full dataset
      # Calculate max distance, used for phi (starting if not specified) and priors 
      d.max <- max(iDist(coords))
      
      if(is.null(spGLM.phi.start)) {
        spGLM.phi.start<- 3/(0.5*d.max)
      }
      
      # Knots?
      if(!is.null(spGLM.knots)) {
                 knots<- kmeans(coords, centers = spGLM.knots, iter.max = 20)$centers
                 } else { 
                   knots <- NULL
                 }

      # spGLM set up
      n.batch <- spGLM.nbatch
      batch.length <- spGLM.batchlength
      n.samples <- n.batch * batch.length
      
      # Chains
      #Sys.time()
      spGLM.c1 <- spGLM(as.formula(model.formula), data = dataset.temp, family = "binomial", weights = weights, coords = coords, knots = knots,
                        starting = list(beta = beta.starting,
                                        phi = 3/(0.5*d.max),
                                        sigma.sq = 1,
                                        w = 0),
                        tuning = list(beta = beta.tuning,
                                      phi = spGLM.phi.tune,
                                      sigma.sq = spGLM.sigmasq.tune,
                                      w = 0.1),
                        priors = list("beta.Flat",
                                      phi.Unif = c(3/d.max, 3/(0.1*d.max)),
                                      sigma.sq.IG = c(2, 1)),
                        amcmc = list(n.batch = n.batch,
                                     batch.length = batch.length,
                                     accept.rate = spGLM.accept),
                        cov.model = spGLM.covmodel, verbose = FALSE)
      #Sys.time()
#       spGLM.c2 <- spGLM(as.formula(model.formula), data = dataset.temp, family = "binomial", weights = weights, coords = coords, knots = knots,
#                         starting = list(beta = beta.starting,
#                                         phi = (5/(0.5*d.max)),
#                                         sigma.sq = 3,
#                                         w = 0.5),
#                         tuning = list(beta = beta.tuning, 
#                                       phi = spGLM.phi.tune,
#                                       sigma.sq = spGLM.sigmasq.tune,
#                                       w = 0.1),
#                         priors = list("beta.Flat",
#                                       phi.Unif = c(3/d.max, 3/(0.1*d.max)),
#                                       sigma.sq.IG = c(2, 1)),
#                         amcmc = list(n.batch = n.batch,
#                                      batch.length=batch.length,
#                                      accept.rate = spGLM.accept),
#                         cov.model = spGLM.covmodel, verbose = FALSE)
#       
#       spGLM.c3 <- spGLM(as.formula(model.formula), data = dataset.temp, family = "binomial", weights = weights, coords = coords, knots = knots,
#                         starting = list(beta = beta.starting,
#                                         phi = (6/(0.5*d.max)),
#                                         sigma.sq = 5,
#                                         w = 0.7),
#                         tuning = list(beta = beta.tuning, 
#                                       phi = spGLM.phi.tune,
#                                       sigma.sq = spGLM.sigmasq.tune,
#                                       w = 0.1),
#                         priors = list("beta.Flat",
#                                       phi.Unif = c(3/d.max, 3/(0.1*d.max)),
#                                       sigma.sq.IG = c(2, 1)),
#                         amcmc = list(n.batch = n.batch,
#                                      batch.length=batch.length,
#                                      accept.rate = spGLM.accept),
#                         cov.model = spGLM.covmodel, verbose = FALSE)
      #Sys.time()
      
      # Get samples
      samps <- mcmc.list(spGLM.c1$p.beta.theta.samples)
#       samps <- mcmc.list(spGLM.c1$p.beta.theta.samples, 
#                          spGLM.c2$p.beta.theta.samples,
#                          spGLM.c3$p.beta.theta.samples)
#       pdf(file = paste(directory, scenario, ".pdf", sep = ""))
#       traceplot(samps)
#       dev.off()

      # Gelman-Rubin statistic to check convergence
#       gr.stat<- gelman.diag(window(samps, start = spGLM.burnin*n.samples, end = n.samples, thin = 1))
#       write.table(data.frame(gr.stat[1]), file = paste(directory, scenario, "grstat.csv", sep = ""))
      
      if(model.subset == TRUE) {
        spDiag.scores<- spDiag(spGLM.c1, start = spGLM.burnin*n.samples, verbose = FALSE)
        results$DIC[k]<- spDiag.scores[[1]][4]
        print(k)
        next
        #Sys.time()
      }
      ## Fit spGLM for training1 dataset

      if(model.subset == FALSE) {
        weights.t1 <- rep(1, nrow(training1)) # Each site has a trial size of 1 (p or a)
      
        # Step 1: Fit non-spatial GLM to get parameter starting and tuning values
        glm.nonsp.t1 <- glm(model.formula, family = "binomial", data = training1)
        beta.starting.t1 <- coefficients(glm.nonsp.t1)
        beta.tuning.t1 <- t(chol(vcov(glm.nonsp.t1))) 
      
        # Calculate max distance, used for phi (starting if not specified) and priors 
        d.max.t1 <- max(iDist(coords.t1))
      
        if(is.null(spGLM.phi.start)) {
          spGLM.phi.start<- 3/(0.5*d.max.t1)
        }
      
        # spGLM set up
        n.batch <- spGLM.nbatch
        batch.length <- spGLM.batchlength
        n.samples <- n.batch * batch.length
      
        # Chains
        #Sys.time()
        spGLM.t1.c1 <- spGLM(as.formula(model.formula), data = training1, family = "binomial", weights = weights.t1, coords = coords.t1,
                        starting = list(beta = beta.starting.t1,
                                        phi = 3/(0.5*d.max.t1),
                                        sigma.sq = spGLM.sigmasq.start,
                                        w = 0),
                        tuning = list(beta = beta.tuning.t1, 
                                      phi = spGLM.phi.tune,
                                      sigma.sq = spGLM.sigmasq.tune,
                                      w = 0.1),
                        priors = list("beta.Flat",
                                      phi.Unif = c(3/d.max.t1, 3/(0.1*d.max.t1)),
                                      sigma.sq.IG = c(2, 1)),
                        amcmc = list(n.batch = n.batch,
                                     batch.length=batch.length,
                                     accept.rate = spGLM.accept),
                        cov.model = spGLM.covmodel, 
                        verbose = FALSE)
#       
#       spGLM.t1.c2 <- spGLM(as.formula(model.formula), data = training1, family = "binomial", weights = weights.t1, coords = coords.t1,
#                            starting = list(beta = beta.starting.t1,
#                                            phi = (3/(0.5*d.max.t1))+0.1,
#                                            sigma.sq = spGLM.sigmasq.start,
#                                            w = 0),
#                            tuning = list(beta = beta.tuning.t1, 
#                                          phi = spGLM.phi.tune,
#                                          sigma.sq = spGLM.sigmasq.tune,
#                                          w = 0.1),
#                            priors = list("beta.Flat",
#                                          phi.Unif = c(3/d.max.t1, 3/(0.1*d.max.t1)),
#                                          sigma.sq.IG = c(2, 1)),
#                            amcmc = list(n.batch = n.batch,
#                                         batch.length=batch.length,
#                                         accept.rate = spGLM.accept),
#                            cov.model = spGLM.covmodel, verbose = FALSE)
#       
#       spGLM.t1.c3 <- spGLM(as.formula(model.formula), data = training1, family = "binomial", weights = weights.t1, coords = coords.t1,
#                            starting = list(beta = beta.starting.t1,
#                                            phi = 0.01,
#                                            sigma.sq = spGLM.sigmasq.start,
#                                            w = 0),
#                            tuning = list(beta = beta.tuning.t1, 
#                                          phi = spGLM.phi.tune,
#                                          sigma.sq = spGLM.sigmasq.tune,
#                                          w = 0.1),
#                            priors = list("beta.Flat",
#                                          phi.Unif = c(3/d.max.t1, 3/(0.1*d.max.t1)),
#                                          sigma.sq.IG = c(2, 1)),
#                            amcmc = list(n.batch = n.batch,
#                                         batch.length=batch.length,
#                                         accept.rate = spGLM.accept),
#                            cov.model = spGLM.covmodel, verbose = FALSE)
#       #Sys.time()
        samps.t1 <- mcmc.list(spGLM.t1.c1$p.beta.theta.samples)
#       samps.t1 <- mcmc.list(spGLM.t1.c1$p.beta.theta.samples, 
#                          spGLM.t1.c2$p.beta.theta.samples,
#                          spGLM.t1.c3$p.beta.theta.samples)
#       
#       # Gelman-Rubin statistic to check convergence
#       gr.stat.t1<- gelman.diag(window(samps.t1, start = spGLM.burnin*n.samples, end = nrow(spGLM.t1.c1$p.beta.theta.samples), thin = 1))
#       
#       ## Fit spGLM for training2 dataset
        weights.t2 <- rep(1, nrow(training2)) # Each site has a trial size of 1 (p or a)
      
        # Step 1: Fit non-spatial GLM to get parameter starting and tuning values
        glm.nonsp.t2 <- glm(model.formula, family = "binomial", data = training2)
        beta.starting.t2 <- coefficients(glm.nonsp.t2)
        beta.tuning.t2 <- t(chol(vcov(glm.nonsp.t2))) 
      
        # Calculate max distance, used for phi (starting if not specified) and priors 
        d.max.t2 <- max(iDist(coords.t2))
      
        if(is.null(spGLM.phi.start)) {
          spGLM.phi.start<- 3/(0.5*d.max.t2)
        }
      
        # spGLM set up
        n.batch <- spGLM.nbatch
        batch.length <- spGLM.batchlength
        n.samples <- n.batch * batch.length
      
        # Chains
        spGLM.t2.c1 <- spGLM(as.formula(model.formula), data = training2, family = "binomial", weights = weights.t2, coords = coords.t2,
                           starting = list(beta = beta.starting.t2,
                                           phi = 3/(0.5*d.max.t2),
                                           sigma.sq = spGLM.sigmasq.start,
                                           w = 0),
                           tuning = list(beta = beta.tuning.t2, 
                                         phi = spGLM.phi.tune,
                                         sigma.sq = spGLM.sigmasq.tune,
                                         w = 0.1),
                           priors = list("beta.Flat",
                                         phi.Unif = c(3/d.max.t2, 3/(0.1*d.max.t2)),
                                         sigma.sq.IG = c(2, 1)),
                           amcmc = list(n.batch = n.batch,
                                        batch.length=batch.length,
                                        accept.rate = spGLM.accept),
                           cov.model = spGLM.covmodel, 
                           verbose = FALSE)
      #Sys.time()
#       spGLM.t2.c2 <- spGLM(model.formula, data = training2, family = "binomial", weights = weights.t2, coords = coords.t2, knots = NULL,
#                            starting = list(beta = beta.starting.t2,
#                                            phi = (3/(0.5*d.max.t1))+0.1,
#                                            sigma.sq = spGLM.sigmasq.start,
#                                            w = 0),
#                            tuning = list(beta = beta.tuning.t2, 
#                                          phi = spGLM.phi.tune,
#                                          sigma.sq = spGLM.sigmasq.tune,
#                                          w = 0.1),
#                            priors = list("beta.Flat",
#                                          phi.Unif = c(3/d.max.t2, 3/(0.1*d.max.t2)),
#                                          sigma.sq.IG = c(2, 1)),
#                            amcmc = list(n.batch = n.batch,
#                                         batch.length=batch.length,
#                                         accept.rate = spGLM.accept),
#                            cov.model = spGLM.covmodel)
#       
#       spGLM.t2.c3 <- spGLM(model.formula, data = training2, family = "binomial", weights = weights.t2, coords = coords.t2, knots = NULL,
#                            starting = list(beta = beta.starting.t2,
#                                            phi = (3/(0.5*d.max.t1))-0.1,
#                                            sigma.sq = spGLM.sigmasq.start,
#                                            w = 0),
#                            tuning = list(beta = beta.tuning.t2, 
#                                          phi = spGLM.phi.tune,
#                                          sigma.sq = spGLM.sigmasq.tune,
#                                          w = 0.1),
#                            priors = list("beta.Flat",
#                                          phi.Unif = c(3/d.max.t2, 3/(0.1*d.max.t2)),
#                                          sigma.sq.IG = c(2, 1)),
#                            amcmc = list(n.batch = n.batch,
#                                         batch.length=batch.length,
#                                         accept.rate = spGLM.accept),
#                            cov.model = spGLM.covmodel)
# 
        samps.t2 <- mcmc.list(spGLM.t2.c1$p.beta.theta.samples)
#       samps.t2 <- mcmc.list(spGLM.t2.c1$p.beta.theta.samples, 
#                             spGLM.t2.c2$p.beta.theta.samples,
#                             spGLM.t2.c3$p.beta.theta.samples)
#       
#       # Gelman-Rubin statistic to check convergence
#       gr.stat.t2<- gelman.diag(window(samps.t2, start = spGLM.burnin*n.samples, end = nrow(spGLM.t2.c1$p.beta.theta.samples), thin = 1))
#       
      # Check convergence of three models (full dataset, training1, training2) before moving on and storing results
#       if(any(gr.stat$Point est >= 1.3) || any(gr.stat.t1$Point est >= 1.3) || any(gr.stat.t2$Point est >= 1.3)) {
#         next
#       }
#       
        ## Storing results
        mod.summary<- data.frame(summary(window(samps, start = spGLM.burnin*n.samples))[[1]])
        mod.quantiles<- data.frame(summary(window(samps, start = spGLM.burnin*n.samples))[[2]])
        results$intercept[k]<- as.numeric(mod.summary$Mean[1])
        results$intercept.z[k]<- NA
        results$intercept.p[k]<- ifelse(mod.quantiles[1,1] < 0 && mod.quantiles[1,5] > 0, "T", "F")
        results$intercept.se[k]<- as.numeric(mod.summary$Time.series.SE[1])
        results$x1[k]<- as.numeric(mod.summary$Mean[2])
        results$x1.z[k]<- NA
        results$x1.p[k]<- ifelse(mod.quantiles[2,1] < 0 && mod.quantiles[2,5] > 0, "T", "F")
        results$x1.se[k]<- as.numeric(mod.summary$Time.series.SE[2])
        
        if(n.predictors == 2) {
          results$random[k]<- as.numeric(mod.summary$Mean[3])
          results$random.z[k]<- NA
          results$random.p[k]<- ifelse(mod.quantiles[3,1] < 0 && mod.quantiles[3,5] > 0, "T", "F")
          results$random.se[k]<- as.numeric(mod.summary$Time.series.SE[3])
        }
      
        if(n.predictors == 3) {
          results$x2[k]<- as.numeric(mod.summary$Mean[3])
          results$x2.z[k]<- NA
          results$x2.p[k]<- ifelse(mod.quantiles[3,1] < 0 && mod.quantiles[3,5] > 0, "T", "F")
          results$x2.se[k]<- as.numeric(mod.summary$Time.series.SE[3])
          results$random[k]<- as.numeric(mod.summary$Mean[4])
          results$random.z[k]<-NA
          results$random.p[k]<- ifelse(mod.quantiles[4,1] < 0 && mod.quantiles[4,5] > 0, "T", "F")
          results$random.se[k]<- as.numeric(mod.summary$Time.series.SE[4])
        }
      
        spDiag.scores<- data.frame(spDiag(spGLM.c1, start = spGLM.burnin*n.samples, verbose = FALSE))
        results$DIC[k]<- spDiag.scores[[1]][4]
      
        # Predictions - full dataset - predict for each MCMC chain then take mean
        # Get fitted values
        # Covariate values, model matrix
        X <- model.matrix(as.formula(model.formula), data = dataset.temp)
        
        # Predicted values
        # Chain 1
        B.c1<- as.matrix(window(samps[[1]]), start = spGLM.burnin*n.samples)
        B.hat.c1<- apply(B.c1[,1:ncol(X)], 2, mean)
        mu.c1<- X%*%(B.hat.c1) + (apply(spGLM.c1$p.w.samples[,(spGLM.burnin*n.samples):n.samples], 1, mean))
      
#       # Chain 2
#       B.c2<- as.matrix(window(samps[[2]]), start = spGLM.burnin*n.samples)
#       B.hat.c2<- apply(B.c2[,1:ncol(X)], 2, mean)
#       mu.c2<- X%*%(B.hat.c2) + (apply(spGLM.c2$p.w.samples[,(spGLM.burnin*n.samples):n.samples], 1, mean))
#       
#       # Chain 3
#       B.c3<- as.matrix(window(samps[[3]]), start = spGLM.burnin*n.samples)
#       B.hat.c3<- apply(B.c3[,1:ncol(X)], 2, mean)
#       mu.c3<- X%*%(B.hat.c3) + (apply(spGLM.c3$p.w.samples[,(spGLM.burnin*n.samples):n.samples], 1, mean))
     
#       # Fitted values -- multiple chains
#       mu.full<- cbind(mu.c1, mu.c2, mu.c3)
#       mu.samps<- apply(mu.full, 1, mean)
      
        # One chain
        mu.samps<- mu.c1

        # Backtransform fitted to get on 0-1 scale
        fullmod.predictions <- plogis(mu.samps) # The same as 1/(1+exp(-(mu.samps)))
      
        # AUC and CCC
        dat.for.kappa.full<- data.frame(cbind(dataset.temp$id, dataset.temp$zero.one, fullmod.predictions))
        opt.thresh.full<- optimal.thresholds(DATA = dat.for.kappa.full, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.full<- opt.thresh.full[,2]
        confusion.mat.full<- cmx(DATA = dat.for.kappa.full, threshold = thresh.use.full)
        kappa.full<- Kappa(CMX = confusion.mat.full, st.dev = TRUE)
        results$full.kappa[k]<- kappa.full[1,1]
        results$full.auc[k]<- auc(DATA = dat.for.kappa.full, st.dev = FALSE)
        predictions.full.binned<- data.frame(summary(cut(fullmod.predictions, breaks = bins)))[,1]
        prop.full.presences<- suppressWarnings(aggregate(dataset.temp$zero.one, by = list(cut(fullmod.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.full.binned)
      
        # Occassionally have 0 obs in a bin, set to 0 instead of "Inf"
        prop.full.presences[prop.full.presences == "Inf"]<- 0
        ccc.results.full<- epi.ccc(prop.full.presences, midpts)
        results$full.ccc.mean[k]<- ccc.results.full$rho.c$est
        results$full.ccc.low[k]<- ccc.results.full$rho.c$lower
        results$full.ccc.up[k]<- ccc.results.full$rho.c$upper
        results$omission.err.full[k]<- confusion.mat.full[2,1]/sum(confusion.mat.full[,1])
        results$commission.err.full[k]<- confusion.mat.full[1,2]/sum(confusion.mat.full[,2])
      
        # Crossvalidated predictions -- multiple chains
#       t1.y.pred<- cbind(spPredict(spGLM.t2.c1, pred.covars = X.t1, pred.coords = coords.t1)$p.y.predictive.samples,
#                          spPredict(spGLM.t2.c2, pred.covars = X.t1, pred.coords = coords.t1)$p.y.predictive.samples,
#                          spPredict(spGLM.t2.c3, pred.covars = X.t1, pred.coords = coords.t1)$p.y.predictive.samples)
#       t1.y.pred.med<- t(matrix(apply(t1.y.pred, 1, median, na.rm = T), ncol = nrow(training1), brow = T))
      
        # One chain
        # Covariate values, model matrix
        X.t1 <- model.matrix(as.formula(model.formula), data = training1)
        t1.y.pred<- spPredict(spGLM.t2.c1, pred.covars = X.t1, pred.coords = coords.t1, verbose = FALSE)$p.y.predictive.samples
        t1.y.pred.med<- t(matrix(apply(t1.y.pred, 1, median, na.rm = T), ncol = nrow(training1)))
#       
#       t2.y.pred<- cbind(spPredict(spGLM.t1.c1, pred.covars = X.t2, pred.coords = coords.t2)$p.y.predictive.samples,
#                         spPredict(spGLM.t1.c2, pred.covars = X.t2, pred.coords = coords.t2)$p.y.predictive.samples,
#                         spPredict(spGLM.t1.c3, pred.covars = X.t2, pred.coords = coords.t2)$p.y.predictive.samples)
#       t2.y.pred.med<- t(matrix(apply(t2.y.pred, 1, median, na.rm = T), ncol = nrow(training2))
      
        # One chain
        X.t2 <- model.matrix(as.formula(model.formula), data = training2)
        t2.y.pred<- spPredict(spGLM.t1.c1, pred.covars = X.t2, pred.coords = coords.t2, verbose = FALSE)$p.y.predictive.samples
        t2.y.pred.med<- t(matrix(apply(t2.y.pred, 1, median, na.rm = T), ncol = nrow(training2)))
        crossvalid.predictions<- c(t1.y.pred.med, t2.y.pred.med)
      
        crossvalid.fulldataset.temp<- rbind(training1, training2)
        dat.for.kappa.crossvalid<- data.frame(cbind(crossvalid.fulldataset.temp$id, crossvalid.fulldataset.temp$zero.one, crossvalid.predictions))
        opt.thresh.crossvalid<- optimal.thresholds(DATA = dat.for.kappa.crossvalid, threshold = 50, opt.methods = "MaxKappa")
        thresh.use.crossvalid<- opt.thresh.crossvalid[,2]
        confusion.mat.crossvalid<- cmx(DATA = dat.for.kappa.crossvalid, threshold = thresh.use.crossvalid)
        kappa.crossvalid<- Kappa(CMX = confusion.mat.crossvalid, st.dev = TRUE)
        results$split.kappa[k]<- kappa.crossvalid[1,1]
        results$split.auc[k]<- auc(DATA = dat.for.kappa.crossvalid, st.dev = FALSE)
        predictions.split.binned<- data.frame(summary(cut(crossvalid.predictions, breaks = bins)))[,1]
        prop.split.presences<- suppressWarnings(aggregate(crossvalid.fulldataset.temp$zero.one, by = list(cut(crossvalid.predictions, breaks = bins)), FUN = sum, simplify = TRUE)[,2]/predictions.split.binned)
        prop.split.presences[prop.split.presences == "Inf"] <- 0
        ccc.results.split<- epi.ccc(prop.split.presences, midpts)
        results$split.ccc.mean[k]<- ccc.results.split$rho.c$est
        results$split.ccc.low[k]<- ccc.results.split$rho.c$lower
        results$split.ccc.up[k]<- ccc.results.split$rho.c$upper
        results$omission.err.split[k]<- confusion.mat.crossvalid[2,1]/sum(confusion.mat.crossvalid[,1])
        results$commission.err.split[k]<- confusion.mat.crossvalid[1,2]/sum(confusion.mat.crossvalid[,2])
     
        # Morans I 
        # Calculate Pearson standardized residuals
        # Estimate SD for fitted values
        sd.est<- sqrt(fullmod.predictions*(1-fullmod.predictions))
      
        # Calculate Pearson std residuals 
        fullmod.resids<- (dataset.temp$zero.one - fullmod.predictions)/sd.est
      
        for (j in 1:length(morans.bins)) {
          mat<- matrix(nrow = nrow(dataset.temp), ncol = 2)
          mat<- dataset.temp[,c(2:3)] 
          dist.mat<- rdist(as.matrix(mat))
          diag(dist.mat)<- 0
          if(j == 1) {
            dist.binary<- ifelse(dist.mat>0 & dist.mat<=morans.bins[j], 1, 0)
          } else {
            dist.binary<- ifelse(dist.mat>morans.bins[j-1] & dist.mat<=morans.bins[j], 1, 0)
          }
          if(sum(dist.binary) <= moransi.minpairs) {
            morani.result <- NA
          } else {
            morani.result<- morani(fullmod.resids, dist.binary)
          }
          morans.i$boot.iter[si]<- boot.id
          morans.i$dist.bin[si]<- morans.bins[j]
          morans.i$moransI[si]<- morani.result
          si<- si+1
        }
      }
    #Sys.time()
    print(k)
  }
  
  ## Morans I residuals
  if(model.subset == FALSE) {
    moransI.name<- paste(directory, date, scenario, "moransI.csv", sep = "")
    #get.lock(moransI.name)
    append<- file.exists(moransI.name)
    write.table(morans.i, file = moransI.name, append = append,
                row.names = FALSE, col.names = !append, sep = ",")
    #return.lock(moransI.name)
  }
  
  ## Write out model selection results
  boot.globalmodresults<- paste(directory, date, scenario, "globalmodresults.csv", sep = "")
  #get.lock(boot.globalmodresults)
  append<- file.exists(boot.globalmodresults)
  write.table(results, file = boot.globalmodresults, append = append,
              row.names = FALSE, col.names = !append, sep = ",")
  #return.lock(boot.globalmodresults)
  rm(results, morans.i)
  }
  
  ### End Fit spGLM
}
###### End data modeling functions
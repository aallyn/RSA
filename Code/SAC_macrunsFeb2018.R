####### SAC mac runs
## Preliminaries
library(tidyverse)
library(foreach)
library(doParallel)
library(snow)
library(doSNOW)
library(tcltk)
source("./Code/SACfunctions_modelingandanalysis02252017_Mac.R")

# Load in input file...
model.params<- read_csv("./Inputs/MissingModelsJuly2018.csv") %>%
  data.frame() 
names(model.params)[2]<- "datasets.group"

### Parallel processing -- doesn't work...
{
sac_func_wrapper<- function(i, model.params) {
  
  mod.pars<- model.params[i,]
  
  source("./Code/SACfunctions_modelingandanalysis02252017_Mac.R")
  data.dir<- "./Data/simulated.data/"
  
  # Free to vary
  negexp.free<-function(a,b,c,d,e, neg.exponent, points, x1,x2,x.random, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ b*x1 + c*x2 + d*x.random + e*nb.intensity.kernel))/(1 + exp(a+ b*x1 + c*x2 + d*x.random + e*nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.free.nox2<-function(a,b,d,e, neg.exponent, points, x1,x.random, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ b*x1 + d*x.random + e*nb.intensity.kernel))/(1 + exp(a+ b*x1 + d*x.random + e*nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.free.x1x2<-function(a,b,c,e, neg.exponent, points, x1,x2, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ b*x1 + c*x2 + e*nb.intensity.kernel))/(1 + exp(a+ b*x1 + c*x2 + e*nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.free.nox1<-function(a,c,d,e, neg.exponent, points, x2,xrandom, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ c*x2 + d*x.random + e*nb.intensity.kernel))/(1 + exp(a+ c*x2 + d*x.random + e*nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.free.x1xnbi<-function(a,b,e, neg.exponent, points, x1, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ b*x1 + e*nb.intensity.kernel))/(1 + exp(a+ b*x1 + e*nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.free.x2xnbi<-function(a,c,e, neg.exponent, points, x2, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ c*x2 + e*nb.intensity.kernel))/(1 + exp(a+ c*x2 + e*nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.free.xrandxnbi<-function(a,d,e, neg.exponent, points, x.random, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ d*x.random + e*nb.intensity.kernel))/(1 + exp(a+ d*x.random + e*nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.free.xnbi<-function(a, e, neg.exponent, points, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ e*nb.intensity.kernel))/(1 + exp(a+ e*nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  # Fixed
  negexp.fix<-function(a,b,c,d, neg.exponent, points, x1,x2,x.random, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ b*x1 + c*x2 + d*x.random + nb.intensity.kernel))/(1 + exp(a+ b*x1 + c*x2 + d*x.random + nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.fix.nox2<-function(a,b,d, neg.exponent, points, x1,x.random, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ b*x1 + d*x.random + nb.intensity.kernel))/(1 + exp(a+ b*x1 + d*x.random + nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.fix.x1x2<-function(a,b,c, neg.exponent, points, x1,x2, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ b*x1 + c*x2 + nb.intensity.kernel))/(1 + exp(a+ b*x1 + c*x2 + nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.fix.nox1<-function(a,c,d, neg.exponent, points, x2,xrandom, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ c*x2 + d*x.random + nb.intensity.kernel))/(1 + exp(a+ c*x2 + d*x.random + nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.fix.x1xnbi<-function(a,b, neg.exponent, points, x1, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ b*x1 + nb.intensity.kernel))/(1 + exp(a+ b*x1 + nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.fix.x2xnbi<-function(a,c, neg.exponent, points, x2, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ c*x2 + nb.intensity.kernel))/(1 + exp(a+ c*x2 + nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.fix.xrandxnbi<-function(a,d, neg.exponent, points, x.random, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ d*x.random + nb.intensity.kernel))/(1 + exp(a+ d*x.random + nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.fix.xnbi<-function(a, neg.exponent, points, response) {
    dist.mat <- rdist(points)
    wt.mat<- exp(-neg.exponent*dist.mat)
    diag(wt.mat)<- 0
    ind.vec<- ifelse(response == 0, -1, 1)
    nb.intensity.kernel<- as.numeric(wt.mat%*%ind.vec)
    prob<-(exp(a+ nb.intensity.kernel))/(1 + exp(a+ nb.intensity.kernel))
    -sum(dbinom(response, size = 1, prob = prob, log = TRUE))
  }
  
  negexp.functions.list<- list(negexp.free, negexp.free.nox2, negexp.free.x1x2, negexp.free.nox1, negexp.free.x1xnbi, negexp.free.x2xnbi, negexp.free.xrandxnbi, negexp.free.xnbi, negexp.fix, negexp.fix.nox2, negexp.fix.x1x2, negexp.fix.nox1, negexp.fix.x1xnbi, negexp.fix.x2xnbi, negexp.fix.xrandxnbi, negexp.fix.xnbi)
  names(negexp.functions.list)<- c("negexp.free", "negexp.free.nox2", "negexp.free.x1x2", "negexp.free.nox1", "negexp.free.x1xnbi", "negexp.free.x2xnbi", "negexp.free.xrandxnbi", "negexp.free.xnbi", "negexp.fix", "negexp.fix.nox2", "negexp.fix.x1x2", "negexp.fix.nox1", "negexp.fix.x1xnbi", "negexp.fix.x2xnbi", "negexp.fix.xrandxnbi", "negexp.fix.xnbi")
  
  # Modeling stuff
  sac.model(nsim = mod.pars$nsim,
            model.fit = mod.pars$model.fit,
            resids.type = "pearson",
            model.subset = mod.pars$model.subset,
            mle2.method = mod.pars$mle2.method,
            datasets.list = get(load(paste(data.dir, mod.pars$batch, ".Rdata", sep = ""))),
            datasets.group = mod.pars$datasets.group,
            model.formula = mod.pars$model.formula,
            start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(mod.pars$neg.exp.param), mu = as.numeric(mod.pars$mu), sigma = as.numeric(mod.pars$sigma)))),
            grid.side = mod.pars$grid.side,
            cell.size = 1,
            directory = mod.pars$mac.directory,
            bin.size = mod.pars$bin.size,
            moransi.minpairs = mod.pars$moransi.minpairs,
            dnn.max = suppressWarnings(as.numeric(mod.pars$dnn.max)),
            custom.fit = mod.pars$custom.fit,
            spev.traditional = mod.pars$spev.traditional,
            spev.neg.exp = mod.pars$spev.neg.exp,
            spev.gauss = mod.pars$spev.gauss,
            neg.exp.param = suppressWarnings(as.numeric(mod.pars$neg.exp.param)),
            neg.exp.ev = suppressWarnings(as.numeric(mod.pars$neg.exp.ev)),
            mu.ev = suppressWarnings(as.numeric(mod.pars$mu.ev)),
            sigma.ev = suppressWarnings(as.numeric(mod.pars$sigma.ev)),
            spGLM.knots = suppressWarnings(as.numeric(mod.pars$spGLM.knots)),
            spGLM.phi.start = suppressWarnings(as.numeric(mod.pars$spGLM.phi.start)),
            spGLM.sigmasq.start = suppressWarnings(as.numeric(mod.pars$spGLM.sigmasq.start)),
            spGLM.phi.tune = suppressWarnings(as.numeric(mod.pars$spGLM.phi.tune)),
            spGLM.sigmasq.tune = suppressWarnings(as.numeric(mod.pars$spGLM.sigma.sq.tune)),
            spGLM.nbatch = suppressWarnings(as.numeric(mod.pars$spGLM.nbatch)),
            spGLM.batchlength = suppressWarnings(as.numeric(mod.pars$spGLM.batchlength)),
            spGLM.accept = suppressWarnings(as.numeric(mod.pars$spGLM.accept)),
            spGLM.covmodel = mod.pars$spGLM.covmodel,
            spGLM.burnin = suppressWarnings(as.numeric(mod.pars$spGLM.burnin)),
            date = mod.pars$date,
            scenario = mod.pars$scenario,
            negexp.functions.list = negexp.functions.list)
  print(paste("Model row ", i, " is done!", sep = ""))
}

## Set up cluster
# Set up cores
cores.avail<- detectCores() 

# get the cluster going
cl <- makeSOCKcluster(cores.avail-2)
registerDoSNOW(cl)

# Progress bar stuff
pb<- tkProgressBar(max = nrow(model.params))
progress<- function(n) setTkProgressBar(pb, n)
opts<- list(progress = progress)

# Run sdw_func_wrapper function in parallel

sac_fits<- foreach(i = 3:nrow(model.params), .packages = c("hier.part", "gtools", "lmtest", "bbmle", "PresenceAbsence", "epiR", "fields",  "tcltk", "ncf", "gee"), .combine = rbind.data.frame, .options.snow = opts) %dopar%
{
  sac_func_wrapper(i, model.params)
}
}

result.out<- NA

# Loop instead?
for(i in 3:nrow(model.params)) {
  mod.pars<- model.params[i,]
  
  data.dir<- "./Data/simulated.data/"
  
  out<- sac.model(nsim = mod.pars$nsim,
            model.fit = mod.pars$model.fit,
            resids.type = "pearson",
            model.subset = mod.pars$model.subset,
            mle2.method = mod.pars$mle2.method,
            datasets.list = get(load(paste(data.dir, mod.pars$batch, ".Rdata", sep = ""))),
            datasets.group = mod.pars$datasets.group,
            model.formula = mod.pars$model.formula,
            start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(mod.pars$neg.exp.param), mu = as.numeric(mod.pars$mu), sigma = as.numeric(mod.pars$sigma)))),
            grid.side = mod.pars$grid.side,
            cell.size = 1,
            directory = mod.pars$mac.directory,
            bin.size = mod.pars$bin.size,
            moransi.minpairs = mod.pars$moransi.minpairs,
            dnn.max = suppressWarnings(as.numeric(mod.pars$dnn.max)),
            custom.fit = mod.pars$custom.fit,
            spev.traditional = mod.pars$spev.traditional,
            spev.neg.exp = mod.pars$spev.neg.exp,
            spev.gauss = mod.pars$spev.gauss,
            neg.exp.param = suppressWarnings(as.numeric(mod.pars$neg.exp.param)),
            neg.exp.ev = suppressWarnings(as.numeric(mod.pars$neg.exp.ev)),
            mu.ev = suppressWarnings(as.numeric(mod.pars$mu.ev)),
            sigma.ev = suppressWarnings(as.numeric(mod.pars$sigma.ev)),
            spGLM.knots = suppressWarnings(as.numeric(mod.pars$spGLM.knots)),
            spGLM.phi.start = suppressWarnings(as.numeric(mod.pars$spGLM.phi.start)),
            spGLM.sigmasq.start = suppressWarnings(as.numeric(mod.pars$spGLM.sigmasq.start)),
            spGLM.phi.tune = suppressWarnings(as.numeric(mod.pars$spGLM.phi.tune)),
            spGLM.sigmasq.tune = suppressWarnings(as.numeric(mod.pars$spGLM.sigma.sq.tune)),
            spGLM.nbatch = suppressWarnings(as.numeric(mod.pars$spGLM.nbatch)),
            spGLM.batchlength = suppressWarnings(as.numeric(mod.pars$spGLM.batchlength)),
            spGLM.accept = suppressWarnings(as.numeric(mod.pars$spGLM.accept)),
            spGLM.covmodel = mod.pars$spGLM.covmodel,
            spGLM.burnin = suppressWarnings(as.numeric(mod.pars$spGLM.burnin)),
            date = mod.pars$date,
            scenario = paste(mod.pars$batch, ".", mod.pars$scenario, sep = ""),
            negexp.functions.list = negexp.functions.list)
  print(paste("Model row ", i, " is done!", sep = ""))
  result.out<- i
  write.table(result.out, file = "~/Desktop/missing.tracking.group1.csv", append = FALSE, row.names = FALSE, sep = ",")
}




####### Figuring out the custom method problem #######
## What is the problem? Looping over different datasets and fitting our custom modeling function to each of the datasets. This seems to work internally but not when I send things to Anthill with the function(containing the loop), wrapped into a model wrapper function to work with Anthill.

###############
### Starting with what works, running things manually
###############

## Remove everything to start fresh
rm(list = ls())

## Custom model functions, each stored in a list

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

## Some other functions needed
# Morans I function
morani <- function(variable, adj.mat) {
  zvar <- variable-mean(variable)
  morani.basic <- (t(zvar)%*%adj.mat%*%zvar) / sum(zvar^2)
  morani.stat <- (length(zvar)/sum(adj.mat)) * morani.basic
  return(morani.stat)
}

# Probability quantiles functions for morans I
p025 <- function(x) quantile(x, prob=.025, na.rm=TRUE)
p975 <- function(x) quantile(x, prob=.975, na.rm=TRUE)

## Now, get into the function.

# Set parameters
par.path<- "Z:/Users/Andrew/sac/Input/"
par.path<- "~/Desktop/rSACWinter2016/Input/"
model.par<- read.csv(paste(par.path, "SAC_batchlist_models_12122016_custom.csv", sep = ""), as.is=TRUE)
ri = 1
set.seed(ri)
nsim = model.par$nsim[ri]
model.fit = model.par$model.fit[ri]
resids.type = "pearson"
model.subset = model.par$model.subset[ri]
mle2.method = model.par$mle2.method[ri]
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
neg.exp.param = suppressWarnings(as.numeric(model.par$neg.exp.param[ri]))
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
spGLM.burnin = suppressWarnings(as.numeric(model.par$spGLM.burnin[ri]))
date = model.par$date[ri]
directory = model.par$directory[ri]
scenario = model.par$scenario[ri]

# Data
data.dir <- "Z:/Users/Andrew/sac/simulated.data/"
data.dir<- "~/Desktop/rSACWinter2016/SimulatedData/"
file=paste(data.dir, model.par$batch[ri], ".Rdata", sep = "")
load(file=file)
datasets.list = d
datasets.group = model.par$datasets.group[ri]
datasets = datasets.list[1:100]

# Libraries
suppressWarnings(library(hier.part))
suppressWarnings(library(gtools))
suppressWarnings(library(lmtest))
suppressWarnings(library(bbmle))
suppressWarnings(library(PresenceAbsence))
suppressWarnings(library(epiR))
suppressWarnings(library(fields))
suppressWarnings(library(fields))
suppressWarnings(library(bbmle))

# Results objects
results<- data.frame(matrix(nrow = length(datasets), ncol = 37))
names(results)[1:37]<- c("intercept", "intercept.z", "intercept.p", "intercept.se", "x1", "x1.z", "x1.p", "x1.se", "x2", "x2.z", "x2.p", "x2.se", "random", "random.z", "random.p", "random.se", "neg.exp.param","deviance.explained", "mod.nll", "null.nll", "lrtest", "full.kappa", "full.auc", "full.ccc.mean", "full.ccc.low", "full.ccc.up", "omission.err.full", "commission.err.full", "split.kappa", "split.auc", "split.ccc.mean", "split.ccc.low", "split.ccc.up", "omission.err.split", "commission.err.split", "AICc", "Time")
results$datasets.group<- rep(datasets.group, length(datasets))

# Lin's CCC
bins<- seq(from = 0, to = 1, by = 0.2)
midpts<- seq(from = 0.1, to = 1, by = 0.2)
  
# Morans I 
morans.bins<- seq(from = bin.size, to = ((grid.side*cell.size)*(1/2)), by = bin.size) 
morans.i<- data.frame(matrix(nrow = length(morans.bins)*length(datasets), ncol = 3))
names(morans.i)[1:3]<- c("boot.iter", "dist.bin", "moransI")
  
# Selection index and bootstrap id for help with tracking and storing results
si<- 1

# Bootstrap id
boot.id.sequence<- seq(from = 1, to = length(datasets), by = 1)

## Loop time
n.predictors<- length(attr(terms(as.formula(model.formula)), "term.labels"))

for (k in seq(along = datasets)) {
	syst.time.start<- Sys.time()
  boot.id<- boot.id.sequence[k]
  dataset.temp<- datasets[[k]]
  coords<-as.matrix(cbind(dataset.temp$x, dataset.temp$y))
  dist.mat <- as.matrix(dist(coords))
  sample.size<- nrow(dataset.temp)
  dataset.temp.split<- sample(dataset.temp$id, size = sample.size/2)
  training1<- dataset.temp[dataset.temp.split, ]
  coords.t1<-as.matrix(cbind(training1$x, training1$y))
  dist.mat.t1 <- as.matrix(dist(coords.t1))
  training2<- dataset.temp[-dataset.temp.split, ]
  coords.t2<-as.matrix(cbind(training2$x, training2$y))
  dist.mat.t2 <- as.matrix(dist(coords.t2))
  
  ## Full model
  # Fit glm
  glm.mod<- glm(model.formula, data = dataset.temp, family = binomial(link=logit), maxit = 1000)
  
  # Use glm as starting params, fit nll function
  nll.use<- match(custom.fit, names(negexp.functions.list))
  points.full<- cbind(dataset.temp$x, dataset.temp$x)
  mod<- try(mle2(minuslogl = negexp.functions.list[[nll.use]],
                 start = list(a = coef(glm.mod)[[1]], b = coef(glm.mod)[[2]], c = coef(glm.mod)[[3]], d = coef(glm.mod)[[4]], neg.exponent = neg.exp.param),
                 data = list(points = points.full, x1 = dataset.temp$x1, x2 = dataset.temp$x2, x.random = dataset.temp$x.random, response = dataset.temp$zero.one), method = "Nelder-Mead", control=list(maxit=5000,trace=F), skip.hessian=FALSE), silent = TRUE)
  
  # Training1
  points.t1<- cbind(training1$x, training1$y)
  mod.train1<- try(mle2(minuslogl = negexp.functions.list[[nll.use]],
                        start = list(a = coef(glm.mod)[[1]], b = coef(glm.mod)[[2]], c = coef(glm.mod)[[3]], d = coef(glm.mod)[[4]], neg.exponent = neg.exp.param),
                        data = list(points = points.t1, x1 = training1$x1, x2 = training1$x2, x.random = training1$x.random, response = training1$zero.one), method = "Nelder-Mead", control=list(maxit=5000,trace=F), skip.hessian=FALSE), silent = TRUE)
  mod.train1.summary<- summary(mod.train1)
  
  # Training2
  points.t2<- cbind(training2$x, training2$y)
  mod.train2<- try(mle2(minuslogl = negexp.functions.list[[nll.use]],
                        start = list(a = coef(glm.mod)[[1]], b = coef(glm.mod)[[2]], c = coef(glm.mod)[[3]], d = coef(glm.mod)[[4]], neg.exponent = neg.exp.param),
                        data = list(points = points.t2, x1 = training2$x1, x2 = training2$x2, x.random = training2$x.random, response = training2$zero.one), method = "Nelder-Mead", control=list(maxit=5000,trace=F), skip.hessian=FALSE), silent = TRUE)
  mod.train2.summary<- summary(mod.train2)

	# Exit if bad fit
  if(class(mod) == "try-error") {
    next
  }

	# If good, store results
	# Store results
	coeffs<- summary(mod)@coef
  nullmod <- glm(zero.one ~ 1, data=dataset.temp, family=binomial)
  results$intercept[k]<- coeffs[1,1]
  results$intercept.z[k]<- coeffs[1,3]
  results$intercept.p[k]<- coeffs[1,4]
  results$intercept.se[k]<- coeffs[1,2]
  results$x1[k]<- coeffs[2,1]
  results$x1.z[k]<- coeffs[2,3]
  results$x1.p[k]<- coeffs[2,4]
  results$x1.se[k]<- coeffs[2,2]
  results$x2[k]<- coeffs[3,1]
  results$x2.z[k]<- coeffs[3,3]
  results$x2.p[k]<- coeffs[3,4]
  results$x2.se[k]<- coeffs[3,2]
  results$random[k]<- coeffs[4,1]
  results$random.z[k]<- coeffs[4,3]
  results$random.p[k]<- coeffs[4,4]
  results$random.se[k]<- coeffs[4,2]
  results$neg.exp.param[k]<- coeffs[5,1]

	print(k)
}

## Okay, so that works. What about if we use the "sac.model" function.

###############
### Running sac.model function, which contains everything above.
###############

## Remove everything to start fresh
rm(list = ls())

## Source modeling function
source("Z:/Users/Andrew/sac/Code/SACfunctions_modelingandanalysis12212016_Anthill.R")
source("~/Desktop/rSACWinter2016/Code/SACfunctions_modelingandanalysis12292016_Anthill.R")

## Model parameters file, with arguments for sac.model function
par.path<- "~/Desktop/rSACWinter2016/Input/"
model.par<- read.csv(paste(par.path, "SAC_batchlist_models_12122016_custom.csv", sep = ""), as.is=TRUE)

# Keep first row
model.par<- model.par[1,]
row.names(model.par)<- seq(from = 1, to = nrow(model.par), by = 1)
model.par$id<- seq(from = 1, to = nrow(model.par), by = 1)

## Data Directory
data.dir <- "Z:/Users/Andrew/sac/simulated.data/"
data.dir<- "~/Desktop/rSACWinter2016/SimulatedData/"

## Run the sac.model function
ri<- 1
file=paste(data.dir, model.par$batch[ri], ".Rdata", sep = "")
load(file=file)
set.seed(ri)

# Find model formula
mod.name <- model.par$batch.model[ri]
  
# Run fitting function
fit <- sac.model(directory = "~/Desktop/", 
                 date = model.par$date[ri], 
                 scenario = model.par$scenario[ri], 
                 datasets.list = d, 
                 datasets.group = model.par$datasets.group[ri],
                 nsim = model.par$nsim[ri], 
                 model.fit = model.par$model.fit[ri],
                 resids.type = "pearson",
                 model.subset = model.par$model.subset[ri],
                 mle2.method = model.par$mle2.method[ri], 
                 model.formula = model.par$model.formula[ri], 
                 grid.side = model.par$grid.side[ri], 
                 bin.size = model.par$bin.size[ri], 
                 moransi.minpairs = model.par$moransi.minpairs[ri], 
                 dnn.max =  as.numeric(model.par$dnn.max[ri]), 
                 custom.fit = model.par$custom.fit[ri], 
                 start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(model.par$neg.exp.param[ri]), mu = as.numeric(model.par$mu[ri]), sigma = as.numeric(model.par$sigma[ri])))), 
                 spev.neg.exp = model.par$spev.neg.exp[ri], 
                 neg.exp.ev = as.numeric(model.par$neg.exp.ev[ri]), 
                 mu.ev = as.numeric(model.par$mu.ev[ri]), 
                 sigma.ev = as.numeric(model.par$sigma.ev[ri]), 
                 spev.gauss = model.par$spev.gauss[ri],
                 spev.traditional = model.par$spev.traditional[ri],
                 spGLM.knots = suppressWarnings(as.numeric(model.par$spGLM.knots[ri])),
                 spGLM.phi.start = suppressWarnings(as.numeric(model.par$spGLM.phi.start[ri])),
                 spGLM.sigmasq.start = suppressWarnings(as.numeric(model.par$spGLM.sigmasq.start[ri])),
                 spGLM.phi.tune = suppressWarnings(as.numeric(model.par$spGLM.phi.tune[ri])),
                 spGLM.sigmasq.tune = suppressWarnings(as.numeric(model.par$spGLM.sigma.sq.tune[ri])),
                 spGLM.nbatch = suppressWarnings(as.numeric(model.par$spGLM.nbatch[ri])),
                 spGLM.batchlength = suppressWarnings(as.numeric(model.par$spGLM.batchlength[ri])),
                 spGLM.accept = suppressWarnings(as.numeric(model.par$spGLM.accept[ri])),
                 spGLM.covmodel = model.par$spGLM.covmodel[ri],
                 spGLM.burnin = as.numeric(model.par$spGLM.burnin[ri]))

# Okay, that clearly did not work....why??? What if we run a different model, will that work?
## Model parameters file, with arguments for sac.model function
par.path<- "~/Desktop/rSACWinter2016/Input/"
model.par<- read.csv(paste(par.path, "SAC_batchlist_models_05042016_Anthill.csv", sep = ""), as.is=TRUE)

# Keep first row
model.par<- model.par[1,]
row.names(model.par)<- seq(from = 1, to = nrow(model.par), by = 1)
model.par$id<- seq(from = 1, to = nrow(model.par), by = 1)

# Run fitting function
fit <- sac.model(directory = "~/Desktop/", 
                 date = model.par$date[ri], 
                 scenario = model.par$scenario[ri], 
                 datasets.list = d, 
                 datasets.group = model.par$datasets.group[ri],
                 nsim = model.par$nsim[ri], 
                 model.fit = model.par$model.fit[ri],
                 resids.type = "pearson",
                 model.subset = model.par$model.subset[ri],
                 mle2.method = model.par$mle2.method[ri], 
                 model.formula = model.par$model.formula[ri], 
                 grid.side = model.par$grid.side[ri], 
                 bin.size = model.par$bin.size[ri], 
                 moransi.minpairs = model.par$moransi.minpairs[ri], 
                 dnn.max =  as.numeric(model.par$dnn.max[ri]), 
                 custom.fit = model.par$custom.fit[ri], 
                 start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(model.par$neg.exp.param[ri]), mu = as.numeric(model.par$mu[ri]), sigma = as.numeric(model.par$sigma[ri])))),
                 spev.neg.exp = model.par$spev.neg.exp[ri], 
                 neg.exp.ev = as.numeric(model.par$neg.exp.ev[ri]), 
                 mu.ev = as.numeric(model.par$mu.ev[ri]), 
                 sigma.ev = as.numeric(model.par$sigma.ev[ri]), 
                 spev.gauss = model.par$spev.gauss[ri],
                 spev.traditional = model.par$spev.traditional[ri],
                 spGLM.knots = suppressWarnings(as.numeric(model.par$spGLM.knots[ri])),
                 spGLM.phi.start = suppressWarnings(as.numeric(model.par$spGLM.phi.start[ri])),
                 spGLM.sigmasq.start = suppressWarnings(as.numeric(model.par$spGLM.sigmasq.start[ri])),
                 spGLM.phi.tune = suppressWarnings(as.numeric(model.par$spGLM.phi.tune[ri])),
                 spGLM.sigmasq.tune = suppressWarnings(as.numeric(model.par$spGLM.sigma.sq.tune[ri])),
                 spGLM.nbatch = suppressWarnings(as.numeric(model.par$spGLM.nbatch[ri])),
                 spGLM.batchlength = suppressWarnings(as.numeric(model.par$spGLM.batchlength[ri])),
                 spGLM.accept = suppressWarnings(as.numeric(model.par$spGLM.accept[ri])),
                 spGLM.covmodel = model.par$spGLM.covmodel[ri],
                 spGLM.burnin = as.numeric(model.par$spGLM.burnin[ri]))

### That works...
  






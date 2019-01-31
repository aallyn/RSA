#code.dir<- "~/Dropbox/Andrew/Work/R/Allyn"
code.dir<- "~/Dropbox/rSACWinter2016/Functions"
#code.dir <- "Z:/Users/Andrew/sac/Code"

source(paste(code.dir, "/biostats01292014.R", sep=""))
source(paste(code.dir, "/WaveletFunctions.R", sep=""))
source(paste(code.dir, "/geefunctions.R", sep = ""))
source(paste(code.dir, "/pearson.resids.R", sep = ""))


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

## Custom Negative Log Likelihood Functions
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

#### Run it
source("~/Dropbox/rSACWinter2016/Code/SACfunctions_modelingandanalysis01102017_Anthill.R")
model.par<- read.csv("~/Dropbox/rSACWinter2016/Input/models.newruns.02062017.csv", as.is = TRUE)
#model.par<- read.csv("~/Desktop/rSACWinter2016/Input/SAC_batchlist_models_05042016_anthill.csv", as.is = TRUE)
#model.par<- model.par[model.par$model.fit == "glm",]
#model.par<- model.par[1,]
options(warn = 1)

for(z in 1:nrow(model.par)) {
  ri<- z 
  set.seed(ri)
  d<- get(load(paste("~/Dropbox/rSACWinter2016/SimulatedData/", model.par$datasets[ri], ".Rdata", sep = "")))
  out<- sac.model(datasets.list = d, nsim = model.par$nsim[ri], model.fit = model.par$model.fit[ri], resids.type = "pearson", model.subset = model.par$model.subset[ri], mle2.method = model.par$mle2.method[ri], datasets.group = "i", model.formula = model.par$model.formula[ri], start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(model.par$neg.exp.param[ri]), mu = as.numeric(model.par$mu[ri]), sigma = as.numeric(model.par$sigma[ri])))), grid.side = model.par$grid.side[ri], cell.size = 1, bin.size = model.par$bin.size[ri], moransi.minpairs = model.par$moransi.minpairs[ri], dnn.max = suppressWarnings(as.numeric(model.par$dnn.max[ri])), custom.fit = model.par$custom.fit[ri], spev.traditional = model.par$spev.traditional[ri], spev.neg.exp = model.par$spev.neg.exp[ri], spev.gauss = model.par$spev.gauss[ri], neg.exp.param = suppressWarnings(as.numeric(model.par$neg.exp.param[ri])), neg.exp.ev = suppressWarnings(as.numeric(model.par$neg.exp.ev[ri])), mu.ev = suppressWarnings(as.numeric(model.par$mu.ev[ri])), sigma.ev = suppressWarnings(as.numeric(model.par$sigma.ev[ri])), spGLM.knots = suppressWarnings(as.numeric(model.par$spGLM.knots[ri])), spGLM.phi.start = suppressWarnings(as.numeric(model.par$spGLM.phi.start[ri])), spGLM.sigmasq.start = suppressWarnings(as.numeric(model.par$spGLM.sigmasq.start[ri])), spGLM.phi.tune = suppressWarnings(as.numeric(model.par$spGLM.phi.tune[ri])), spGLM.sigmasq.tune = suppressWarnings(as.numeric(model.par$spGLM.sigma.sq.tune[ri])), spGLM.nbatch = suppressWarnings(as.numeric(model.par$spGLM.nbatch[ri])), spGLM.batchlength = suppressWarnings(as.numeric(model.par$spGLM.batchlength[ri])), spGLM.accept = suppressWarnings(as.numeric(model.par$spGLM.accept[ri])), spGLM.covmodel = model.par$spGLM.covmodel[ri], spGLM.burnin = suppressWarnings(as.numeric(model.par$spGLM.burnin[ri])), date = model.par$date[ri], directory = model.par$directory[ri], scenario = model.par$scenario[ri])
}

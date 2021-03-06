code.dir<- "~/Desktop/SAC_finalcountdown/ForR/Functions"
data.dir<- "~/Desktop/SAC_finalcountdown/ForR/simulated.data/"
#code.dir <- "Z:/Users/Andrew/sac/Code"

source(paste(code.dir, "/biostats01292014.R", sep=""))
source(paste(code.dir, "/WaveletFunctions.R", sep=""))
source(paste(code.dir, "/geefunctions.R", sep = ""))
source("~/Desktop/SAC_finalcountdown/ForR/SACfunctions_modelingandanalysis03042016_Mac.R")

### Run GEE models
model.par<- read.csv("~/Desktop/SAC_finalcountdown/ForR/SAC_batchlist_models_03042016_gee.csv", as.is = TRUE)
model.par<- model.par[c(421:630),]

## Start loop
for (i in 1:nrow(model.par)) {
  ri<- i
  file=paste(data.dir, model.par$batch[ri], ".Rdata", sep = "")
  load(file=file)
  set.seed(ri+7)
  
  # Find model formula
  mod.name <- model.par$batch.model[ri]
  
  # Run fitting function
  fit <- sac.model(directory = model.par$directory[ri],, 
                   mod.name<- model.par$bath.model[ri],
                   nsim = model.par$nsim[ri], 
                   model.fit = model.par$model.fit[ri],
                   resids.type = "pearson",
                   model.subset = model.par$model.subset[ri],
                   mle2.method = model.par$mle2.method[ri], 
                   datasets = d, 
                   datasets.group = model.par$datasets.group[ri],
                   model.formula = model.par$model.formula[ri], 
                   start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(model.par$neg.exp.param[ri]), 
                                                                    mu = as.numeric(model.par$mu[ri]), 
                                                                    sigma = as.numeric(model.par$sigma[ri])))), 
                   grid.side = model.par$grid.side[ri],
                   cell.size = 1,
                   bin.size = model.par$bin.size[ri], 
                   moransi.minpairs = model.par$moransi.minpairs[ri], 
                   dnn.max =  suppressWarnings(as.numeric(model.par$dnn.max[ri])), 
                   custom.fit = model.par$custom.fit[ri], 
                   spev.neg.exp = model.par$spev.neg.exp[ri],
                   spev.gauss = model.par$spev.gauss[ri],
                   spev.traditional = model.par$spev.traditional[ri],
                   neg.exp.ev = as.numeric(model.par$neg.exp.ev[ri]), 
                   mu.ev = as.numeric(model.par$mu.ev[ri]), 
                   sigma.ev = as.numeric(model.par$sigma.ev[ri]), 
                   spGLM.knots = suppressWarnings(as.numeric(model.par$spGLM.knots[ri])),
                   spGLM.phi.start = suppressWarnings(as.numeric(model.par$spGLM.phi.start[ri])),
                   spGLM.sigmasq.start = suppressWarnings(as.numeric(model.par$spGLM.sigmasq.start[ri])),
                   spGLM.phi.tune = suppressWarnings(as.numeric(model.par$spGLM.phi.tune[ri])),
                   spGLM.sigmasq.tune = suppressWarnings(as.numeric(model.par$spGLM.sigma.sq.tune[ri])),
                   spGLM.nbatch = suppressWarnings(as.numeric(model.par$spGLM.nbatch[ri])),
                   spGLM.batchlength = suppressWarnings(as.numeric(model.par$spGLM.batchlength[ri])),
                   spGLM.accept = suppressWarnings(as.numeric(model.par$spGLM.accept[ri])),
                   spGLM.covmodel = model.par$spGLM.covmodel[ri],
                   spGLM.burnin = as.numeric(model.par$spGLM.burnin[ri]),
                   date = model.par$date[ri],
                   scenario = model.par$scenario[ri])
  print(paste("Model.", ri, sep = ""))
}  

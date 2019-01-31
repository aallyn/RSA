############################################################################################# Anthill shell for SAC simulation project ##################

rm(list=ls()) # clean workspace.  Caution!!!!

################ Source functions and libraries
source("Z:/Users/Andrew/sac/Code/SACfunctions_modelingandanalysis04162016_Anthill.R")

################ START

#### Data generation
{
## Load parameter file for data generation
par.path<- "Z:/Users/Andrew/sac/Input/"
data.par <- read.csv(paste(par.path, "SAC_batchlist_datageneration_04302016.csv", sep = ""), as.is=TRUE)

## Setting directories
data.dir <- "Z:/Users/Andrew/sac/simulated.data/"

## Wrapper function for data generation
data.gen.and.save <- function(id, data.par, data.dir) {
  
  # Setting row index for which dataset to simulate
  stopifnot(id %in% data.par$id)
  ri <- which(data.par$id %in% id)  
  set.seed(ri)
  d <- sac.stochastic.data(nsim = data.par$nsim[ri], 
                           method = data.par$method[ri], 
                           psill.use = data.par$psill.use[ri], 
                           range.env1 = as.numeric(data.par$range.env1[ri]), 
                           range.env2 = as.numeric(data.par$range.env2[ri]), 
                           correlated.env = data.par$correlated.env[ri], 
                           corr.param = data.par$corr.param[ri], 
                           model.use = data.par$model.use[ri], 
                           nugget.use = data.par$nugget.use[ri], 
                           nobs.use = data.par$nobs.use[ri], 
                           dist.use = data.par$dist.use[ri], 
                           rescale = data.par$rescale[ri], 
                           lbound.env = data.par$lbound.env[ri], 
                           ubound.env = data.par$ubound.env[ri], 
                           fractal.rand = data.par$fractal.rand[ri], 
                           fractal.env1 = data.par$fractal.env1[ri],
                           fractal.env2 = as.numeric(data.par$fractal.env2[ri]), 
                           p.rbinom = data.par$p.rbinom[ri],
                           sample.size = data.par$sample.size[ri],
                           census = data.par$census[ri],
                           grid.side = data.par$grid.side[ri], 
                           cell.size = data.par$cell.size[ri], 
                           logistic.env = data.frame(suppressWarnings(cbind(a = as.numeric(data.par$logistic.env.a[ri]), 
                                                                            b = as.numeric(data.par$logistic.env.b[ri]), 
                                                                            c = as.numeric(data.par$logistic.env.c[ri])))), 
                           exp.param = data.par$exp.param[ri])
  save(d, file=paste(data.dir, data.par$batch[ri], ".Rdata", sep = ""))
}

# End data gen function

## Set up and launching on Anthill

# Set up stuff
library(anthill)
config() # will only work on cluster

call <- "data.gen.and.save(data.par=data.par, data.dir=data.dir)" 

subtasks <- c(paste("range:", compact.range(data.par$id),  sep=""))
subtaskarg <- "id"


# Launch on Anthill
priority.use<- 0
maxthreads.use <- 5

simple.launch(call=call,
              subtaskarg=subtaskarg, 
              subtask=subtasks, 
              name = "sac_datagen", 
              owner="andrew", 
              priority=priority.use, 
              maxthreads=maxthreads.use)

# Update priority to start project
set.priority("sac_datagen", 5)

# Update threads
set.maxthreads("sac_datagen", 10)

#### End data generation
}
####################
##################

rm(list=ls()) # clean workspace.  Caution!!!!

################ Source functions and libraries
source("Z:/Users/Andrew/sac/Code/SACfunctions_modelingandanalysis04162016_Anthill.R")

#### Start data modeling
## Load parameter file for data generation
par.path<- "Z:/Users/Andrew/sac/Input/"
#model.par<- read.csv(paste(par.path, "SAC_batchlist_models_05042016_anthill.csv", sep = ""), as.is=TRUE)
model.par<- read.csv(paste(par.path, "models.newruns.07202016.csv", sep = ""), as.is=TRUE)

model.par<- model.par[sample(nrow(model.par)),]
row.names(model.par)<- seq(from = 1, to = nrow(model.par), by = 1)
model.par$id<- seq(from = 1, to = nrow(model.par), by = 1)

###### Added for error
# model.par.red<- data.frame(model.par[1500:nrow(model.par),])
# row.names(model.par.red)<- seq(from = 1, to = nrow(model.par.red), by = 1)
# model.par.red$id<- seq(from = 1, to = nrow(model.par.red), by = 1)
# 
# model.par<- model.par.red


## Setting directories
data.dir <- "Z:/Users/Andrew/sac/simulated.data/"

## Wrapper function for modeling
mod <- function(model.id, model.par, data.dir){
  
  # load data
  stopifnot(model.id %in% model.par$id)
  ri<- which(model.par$id %in% model.id)
  file=paste(data.dir, model.par$batch[ri], ".Rdata", sep = "")
  load(file=file)
  set.seed(ri)
  
  # Find model formula
  mod.name <- model.par$batch.model[ri]
  
  # Run fitting function
  fit <- sac.model(directory = model.par$directory[ri], 
                   date = model.par$date[ri], 
                   scenario = model.par$scenario[ri], 
                   datasets = d, 
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
                   start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(model.par$neg.exp.param[ri]), 
                                                                    mu = as.numeric(model.par$mu[ri]), 
                                                                    sigma = as.numeric(model.par$sigma[ri])))), 
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
  }  

# End mod function

## Set up and launching on Anthill
library(anthill)
config() # will only work on cluster

subtasks <- paste("range:", compact.range(model.par$id), sep="")
subtaskarg <- rep("model.id", length(subtasks))
call <- rep(paste("mod(model.par=model.par, data.dir=data.dir)"),
            length(subtasks)) 

priority.use<- 0
maxthreads.use <- 5

simple.launch(call=call,
              subtaskarg=subtaskarg, 
              subtask=subtasks, 
              name = "sac_0720_a", 
              owner="andrew", 
              priority=priority.use, 
              maxthreads=maxthreads.use)

# Update priority to start project
set.priority("sac_0720_a", 5)
set.maxthreads("sac_0720_a", 20)


# Custom only
rm(list = ls())

source("Z:/Users/Andrew/sac/Code/SACfunctions_modelingandanalysis04162016_Anthill.R")

par.path<- "Z:/Users/Andrew/sac/Input/"
model.par<- read.csv(paste(par.path, "SAC_batchlist_models_05042016_custom_anthill.csv", sep = ""), as.is=TRUE)
row.names(model.par)<- seq(from = 1, to = nrow(model.par), by = 1)
model.par$id<- seq(from = 1, to = nrow(model.par), by = 1)

data.dir <- "Z:/Users/Andrew/sac/simulated.data/"


## Wrapper function for modeling
mod.wrap <- function(model.id, model.par, data.dir){
  
  # load data
  stopifnot(model.id %in% model.par$id)
  ri<- which(model.par$id %in% model.id)
  file=paste(data.dir, model.par$batch[ri], ".Rdata", sep = "")
  load(file=file)
  set.seed(ri)
  
  # Find model formula
  mod.name <- model.par$batch.model[ri]
  
  # Run fitting function
  fit <- sac.model(directory = model.par$directory[ri], 
                   date = model.par$date[ri], 
                   scenario = model.par$scenario[ri], 
                   datasets = d, 
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
                   start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(model.par$neg.exp.param[ri]), 
                                                                    mu = as.numeric(model.par$mu[ri]), 
                                                                    sigma = as.numeric(model.par$sigma[ri])))), 
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
  }  

# End mod function

## Set up and launching on Anthill
library(anthill)
config() # will only work on cluster

subtasks <- paste("range:", compact.range(model.par$id), sep="")
subtaskarg <- rep("model.id", length(subtasks))
call <- rep(paste("mod.wrap(model.par=model.par, data.dir=data.dir)"),
            length(subtasks)) 

priority.use<- 0
maxthreads.use <- 1

simple.launch(call=call,
              subtaskarg=subtaskarg, 
              subtask=subtasks, 
              name = "sac_0523_h", 
              owner="andrew", 
              priority=priority.use, 
              maxthreads=maxthreads.use)

# Update priority to start project
set.priority("sac_0523_h", 5)
set.maxthreads("sac_0523_h", 10)










purge.project("sac_1108b")
set.maxthreads("sac_1108b", 100)


##### 3501:3831
model.par<- read.csv(file = paste("Z:/Users/Andrew/sac/Input/", "models.newruns.02022016.csv", sep = ""), as.is = TRUE)
row.names(model.par)<- seq(from = 1, to = nrow(model.par), by = 1)
model.par$id<- seq(from = 1, to = nrow(model.par), by = 1)

###### Added for error
model.par.red<- data.frame(model.par[3501:nrow(model.par),])
row.names(model.par.red)<- seq(from = 1, to = nrow(model.par.red), by = 1)
model.par.red$id<- seq(from = 1, to = nrow(model.par.red), by = 1)

model.par<- model.par.red


## Setting directories
data.dir <- "Z:/Users/Andrew/sac/simulated.data/"

## Wrapper function for modeling
mod <- function(model.id, model.par, data.dir){
  
  # load data
  stopifnot(model.id %in% model.par$id)
  ri<- which(model.par$id %in% model.id)
  file=paste(data.dir, model.par$batch[ri], ".Rdata", sep = "")
  load(file=file)
  set.seed(ri)
  
  # Find model formula
  mod.name <- model.par$batch.model[ri]
  
  # Run fitting function
  fit <- sac.model(directory = model.par$directory[ri], 
                   date = model.par$date[ri], 
                   scenario = model.par$scenario[ri], 
                   datasets = d, 
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
                   start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(model.par$neg.exp.param[ri]), 
                                                                    mu = as.numeric(model.par$mu[ri]), 
                                                                    sigma = as.numeric(model.par$sigma[ri])))), 
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
}  

# End mod function

## Set up and launching on Anthill
library(anthill)
config() # will only work on cluster

subtasks <- paste("range:", compact.range(model.par$id), sep="")
subtaskarg <- rep("model.id", length(subtasks))
call <- rep(paste("mod(model.par=model.par, data.dir=data.dir)"),
            length(subtasks)) 

priority.use<- 0
maxthreads.use <- 5

simple.launch(call=call,
              subtaskarg=subtaskarg, 
              subtask=subtasks, 
              name = "sac_02152016", 
              owner="andrew", 
              priority=priority.use, 
              maxthreads=maxthreads.use)

# Update priority to start project
set.priority("sac_02152016", 5)
set.maxthreads("sac_020616", 50)

purge.project("sac_1108b")
set.maxthreads("sac_1108b", 100)























# Model.par 324


####### Relaunch after debugging
update.workspace(obj = c("mod", "sac.model", "call"), "sac_2")
rerun("sac_2")

set.maxthreads("sac_2", 5)
set.priority("sac_2", 5)

#### None of that seems to be working, modifying input and rerunning as new project.
model.par.red<- data.frame(model.par[1500:nrow(model.par),])
row.names(model.par.red)<- seq(from = 1, to = nrow(model.par.red), by = 1)
model.par.red$id<- seq(from = 1, to = nrow(model.par.red), by = 1)

data.dir <- "Z:/Users/Andrew/sac/simulated.data/"

## Wrapper function for modeling
mod <- function(model.id, model.par.red, data.dir){
  
  # load data
  stopifnot(model.id %in% model.par.red$id)
  ri<- which(model.par.red$id %in% model.id)
  file=paste(data.dir, model.par.red$batch[ri], ".Rdata", sep = "")
  load(file=file)
  set.seed(ri)
  
  # Find model formula
  mod.name <- model.par.red$batch.model[ri]
  
  # Run fitting function
  fit <- sac.model(directory = model.par.red$directory[ri], 
                   date = model.par.red$date[ri], 
                   scenario = model.par.red$scenario[ri], 
                   datasets = d, 
                   datasets.group = model.par.red$datasets.group[ri],
                   nsim = model.par.red$nsim[ri], 
                   model.fit = model.par.red$model.fit[ri],
                   resids.type = "pearson",
                   model.subset = model.par.red$model.subset[ri],
                   mle2.method = model.par.red$mle2.method[ri], 
                   model.formula = model.par.red$model.formula[ri], 
                   grid.side = model.par.red$grid.side[ri], 
                   bin.size = model.par.red$bin.size[ri], 
                   moransi.minpairs = model.par.red$moransi.minpairs[ri], 
                   dnn.max =  as.numeric(model.par.red$dnn.max[ri]), 
                   custom.fit = model.par.red$custom.fit[ri], 
                   start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(model.par.red$neg.exp.param[ri]), 
                                                                    mu = as.numeric(model.par.red$mu[ri]), 
                                                                    sigma = as.numeric(model.par.red$sigma[ri])))), 
                   spev.neg.exp = model.par.red$spev.neg.exp[ri], 
                   neg.exp.ev = as.numeric(model.par.red$neg.exp.ev[ri]), 
                   mu.ev = as.numeric(model.par.red$mu.ev[ri]), 
                   sigma.ev = as.numeric(model.par.red$sigma.ev[ri]), 
                   spev.gauss = model.par.red$spev.gauss[ri],
                   spev.traditional = model.par.red$spev.traditional[ri],
                   spGLM.knots = suppressWarnings(as.numeric(model.par.red$spGLM.knots[ri])),
                   spGLM.phi.start = suppressWarnings(as.numeric(model.par.red$spGLM.phi.start[ri])),
                   spGLM.sigmasq.start = suppressWarnings(as.numeric(model.par.red$spGLM.sigmasq.start[ri])),
                   spGLM.phi.tune = suppressWarnings(as.numeric(model.par.red$spGLM.phi.tune[ri])),
                   spGLM.sigmasq.tune = suppressWarnings(as.numeric(model.par.red$spGLM.sigma.sq.tune[ri])),
                   spGLM.nbatch = suppressWarnings(as.numeric(model.par.red$spGLM.nbatch[ri])),
                   spGLM.batchlength = suppressWarnings(as.numeric(model.par.red$spGLM.batchlength[ri])),
                   spGLM.accept = suppressWarnings(as.numeric(model.par.red$spGLM.accept[ri])),
                   spGLM.covmodel = model.par.red$spGLM.covmodel[ri],
                   spGLM.burnin = as.numeric(model.par.red$spGLM.burnin[ri]))
}  

# End mod function

## Set up and launching on Anthill
library(anthill)
config() # will only work on cluster

subtasks <- paste("range:", compact.range(model.par.red$id), sep="")
subtaskarg <- rep("model.id", length(subtasks))
call <- rep(paste("mod(model.par=model.par, data.dir=data.dir)"),
            length(subtasks)) 

priority.use<- 0
maxthreads.use <- 5

simple.launch(call=call,
              subtaskarg=subtaskarg, 
              subtask=subtasks, 
              name = "sac_5", 
              owner="andrew", 
              priority=priority.use, 
              maxthreads=maxthreads.use)

# Update priority to start project
set.priority("sac_5", 5)
set.maxthreads("sac_5", 50)


# Trying to update workspace again.
# First, fix bug in function, then source
# Second, update.workspace()
set.maxthreads("sac_5", 5)
update.workspace(sac.model, "sac_5")
rerun("sac_5")

update.workspace(c("sac.model", "mod"), project = "sac_5")
rerun("sac_5")

######################
### Still no luck, running others
rm(list=ls()) # clean workspace.  Caution!!!!

################ Source functions and libraries
source("Z:/Users/Andrew/sac/Code/SACfunctions_modelingandanalysis10302015_Anthill.R")

model.par.red<- data.frame(model.par[1500:nrow(model.par),])
model.par.red2<- data.frame(model.par.red[1501:nrow(model.par.red),])
model.par.red<- model.par.red2
row.names(model.par.red)<- seq(from = 1, to = nrow(model.par.red), by = 1)
model.par.red$id<- seq(from = 1, to = nrow(model.par.red), by = 1)

data.dir <- "Z:/Users/Andrew/sac/simulated.data/"

## Wrapper function for modeling
mod <- function(model.id, model.par.red, data.dir){
  
  # load data
  stopifnot(model.id %in% model.par.red$id)
  ri<- which(model.par.red$id %in% model.id)
  file=paste(data.dir, model.par.red$batch[ri], ".Rdata", sep = "")
  load(file=file)
  set.seed(ri)
  
  # Find model formula
  mod.name <- model.par.red$batch.model[ri]
  
  # Run fitting function
  fit <- sac.model(directory = model.par.red$directory[ri], 
                   date = model.par.red$date[ri], 
                   scenario = model.par.red$scenario[ri], 
                   datasets = d, 
                   datasets.group = model.par.red$datasets.group[ri],
                   nsim = model.par.red$nsim[ri], 
                   model.fit = model.par.red$model.fit[ri],
                   resids.type = "pearson",
                   model.subset = model.par.red$model.subset[ri],
                   mle2.method = model.par.red$mle2.method[ri], 
                   model.formula = model.par.red$model.formula[ri], 
                   grid.side = model.par.red$grid.side[ri], 
                   bin.size = model.par.red$bin.size[ri], 
                   moransi.minpairs = model.par.red$moransi.minpairs[ri], 
                   dnn.max =  as.numeric(model.par.red$dnn.max[ri]), 
                   custom.fit = model.par.red$custom.fit[ri], 
                   start.params = data.frame(suppressWarnings(cbind(neg.exp.param = as.numeric(model.par.red$neg.exp.param[ri]), 
                                                                    mu = as.numeric(model.par.red$mu[ri]), 
                                                                    sigma = as.numeric(model.par.red$sigma[ri])))), 
                   spev.neg.exp = model.par.red$spev.neg.exp[ri], 
                   neg.exp.ev = as.numeric(model.par.red$neg.exp.ev[ri]), 
                   mu.ev = as.numeric(model.par.red$mu.ev[ri]), 
                   sigma.ev = as.numeric(model.par.red$sigma.ev[ri]), 
                   spev.gauss = model.par.red$spev.gauss[ri],
                   spev.traditional = model.par.red$spev.traditional[ri],
                   spGLM.knots = suppressWarnings(as.numeric(model.par.red$spGLM.knots[ri])),
                   spGLM.phi.start = suppressWarnings(as.numeric(model.par.red$spGLM.phi.start[ri])),
                   spGLM.sigmasq.start = suppressWarnings(as.numeric(model.par.red$spGLM.sigmasq.start[ri])),
                   spGLM.phi.tune = suppressWarnings(as.numeric(model.par.red$spGLM.phi.tune[ri])),
                   spGLM.sigmasq.tune = suppressWarnings(as.numeric(model.par.red$spGLM.sigma.sq.tune[ri])),
                   spGLM.nbatch = suppressWarnings(as.numeric(model.par.red$spGLM.nbatch[ri])),
                   spGLM.batchlength = suppressWarnings(as.numeric(model.par.red$spGLM.batchlength[ri])),
                   spGLM.accept = suppressWarnings(as.numeric(model.par.red$spGLM.accept[ri])),
                   spGLM.covmodel = model.par.red$spGLM.covmodel[ri],
                   spGLM.burnin = as.numeric(model.par.red$spGLM.burnin[ri]))
}  

# End mod function

## Set up and launching on Anthill
library(anthill)
config() # will only work on cluster

subtasks <- paste("range:", compact.range(model.par.red$id), sep="")
subtaskarg <- rep("model.id", length(subtasks))
call <- rep(paste("mod(model.par=model.par, data.dir=data.dir)"),
            length(subtasks)) 

priority.use<- 0
maxthreads.use <- 5

simple.launch(call=call,
              subtaskarg=subtaskarg, 
              subtask=subtasks, 
              name = "sac_6", 
              owner="andrew", 
              priority=priority.use, 
              maxthreads=maxthreads.use)

# Update priority to start project
set.priority("sac_6", 5)
set.maxthreads("sac_6", 0)


## Another error, model.id = 1500
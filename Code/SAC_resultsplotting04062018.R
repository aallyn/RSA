############## RESIDUAL SPATIAL CORRELATION RESULTS ######################

library(tidyverse)

##########################
###### Check for all model fits 
###########################
# Need to compare the full parameters model file to the finished fits, which are located in "~/Desktop/RSA_MergedFies_Winter2018" with subfolders for Mod/Strong/Weak.

# Lets make this list of files in R
# GLM -- all subsets are run, so no need for each model to be listed out
mod.frames<- c("glm")
data.sets<- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
mod.formulas<- c("zero.one ~ x1 + x2 + x.random", "zero.one ~ x1 + x.random")
batch<- c("emm.ewf.bn", "emm.ewm.bn", "emm.ewc.bn", "emm.emf.bn", "emm.emm.bn", "emm.emc.bn", "emm.esf.bn", "emm.esm.bn", "emm.esc.bn", "emm.ewf.bf", "emm.ewm.bm", "emm.ewc.bs", "emm.emf.bf", "emm.emm.bm", "emm.emc.bs", "emm.esf.bf", "emm.esm.bm", "emm.esc.bs")
glm<- expand.grid(mod.frames, data.sets, mod.formulas, batch)
names(glm)<- c("model.fit", "dataset.group", "model.formula", "batch")

# GLM and GLM autocov and GLM.spev -- all subsets are run, so no need for each model to be listed out
mod.frames<- c("glm.autocov", "glm.spev")
data.sets<- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
mod.formulas<- c("zero.one ~ x1 + x2 + x.random", "zero.one ~ x1 + x.random")
batch<- c("emm.ewf.bf", "emm.ewm.bm", "emm.ewc.bs", "emm.emf.bf", "emm.emm.bm", "emm.emc.bs", "emm.esf.bf", "emm.esm.bm", "emm.esc.bs")
glm.glmac<- expand.grid(mod.frames, data.sets, mod.formulas, batch)
names(glm.glmac)<- c("model.fit", "dataset.group", "model.formula", "batch")

# GAM
mod.frames<- c("gam.s")
data.sets<- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
mod.formulas<- c("zero.one ~ x1 + x2 + x.random + s(x,y)", "zero.one ~ x1 + x.random + s(x,y)", "zero.one ~ x2 + x.random + s(x,y)", "zero.one ~ x1 + x2 + s(x,y)", "zero.one ~ x1 + x2", "zero.one ~ x1 + x.random",  "zero.one ~ x2 + x.random", "zero.one ~ x1 + s(x,y)", "zero.one ~ x2 + s(x,y)", "zero.one ~ x.random + s(x,y)", "zero.one ~ s(x,y)", "zero.one ~ x1", "zero.one ~ x2", "zero.one ~ x.random")
batch<- c("emm.ewf.bf", "emm.ewm.bm", "emm.ewc.bs", "emm.emf.bf", "emm.emm.bm", "emm.emc.bs", "emm.esf.bf", "emm.esm.bm", "emm.esc.bs")
gam<- expand.grid(mod.frames, data.sets, mod.formulas, batch)
names(gam)<- c("model.fit", "dataset.group", "model.formula", "batch")

# Custom
mod.frames<- c("negexp.free")
data.sets<- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
mod.formulas<- c("zero.one ~ x1 + x2 + x.random", "zero.one ~ x1 + x.random", "zero.one ~ x2 + x.random", "zero.one ~ x1 + x2", "zero.one ~ x2", "zero.one ~ x1", "zero.one ~ x.random", "zero.one ~ xnbi")
batch<- c("emm.ewf.bf", "emm.ewm.bm", "emm.ewc.bs", "emm.emf.bf", "emm.emm.bm", "emm.emc.bs", "emm.esf.bf", "emm.esm.bm", "emm.esc.bs")
glm.cust<- expand.grid(mod.frames, data.sets, mod.formulas, batch)
names(glm.cust)<- c("model.fit", "dataset.group", "model.formula", "batch")

# GEE
mod.frames<- c("gee")
data.sets<- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
mod.formulas<- c("zero.one ~ x1 + x2 + x.random", "zero.one ~ x1 + x.random", "zero.one ~ x2 + x.random", "zero.one ~ x1 + x2", "zero.one ~ x2", "zero.one ~ x1", "zero.one ~ x.random")
batch<- c("emm.ewf.bf", "emm.ewm.bm", "emm.ewc.bs", "emm.emf.bf", "emm.emm.bm", "emm.emc.bs", "emm.esf.bf", "emm.esm.bm", "emm.esc.bs")
gee<- expand.grid(mod.frames, data.sets, mod.formulas, batch)
names(gee)<- c("model.fit", "dataset.group", "model.formula", "batch")

# GLM.wavelet
mod.frames<- c("glm.wavelet")
data.sets<- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
mod.formulas<- c("zero.one ~ x1 + x2 + x.random", "zero.one ~ x1 + x.random", "zero.one ~ x2 + x.random", "zero.one ~ x1 + x2", "zero.one ~ x2", "zero.one ~ x1", "zero.one ~ x.random")
batch<- c("emm.ewf.bf", "emm.ewm.bm", "emm.ewc.bs", "emm.emf.bf", "emm.emm.bm", "emm.emc.bs", "emm.esf.bf", "emm.esm.bm", "emm.esc.bs")
glm.wave<- expand.grid(mod.frames, data.sets, mod.formulas, batch)
names(glm.wave)<- c("model.fit", "dataset.group", "model.formula", "batch")

# spGLM
mod.frames<- c("spglm")
data.sets<- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
data.sets.num<- seq(from = 1, to = 20, by = 1)
mod.formulas<- c("zero.one ~ x1 + x2 + x.random", "zero.one ~ x1 + x.random", "zero.one ~ x2 + x.random", "zero.one ~ x1 + x2", "zero.one ~ x2", "zero.one ~ x1", "zero.one ~ x.random")
data.sets<- paste(rep(data.sets, each = 20), data.sets.num, sep = ".")
batch<- c("emm.ewf.bf", "emm.ewm.bm", "emm.ewc.bs", "emm.emf.bf", "emm.emm.bm", "emm.emc.bs", "emm.esf.bf", "emm.esm.bm", "emm.esc.bs")
glm.spglm<- expand.grid(mod.frames, data.sets, mod.formulas, batch)
names(glm.spglm)<- c("model.fit", "dataset.group", "model.formula", "batch")

# All mods
all.mods<- bind_rows(glm, glm.glmac, glm.cust, gam, gee, glm.spglm, glm.wave)
all.mods$date<- 11072015

# GLM, GLM.AC, GLM.SPEV scenario naming
all.mods$scenario<- ifelse(all.mods$model.fit == "glm" & all.mods$model.formula == "zero.one ~ x1 + x2 + x.random", "glm",
                           ifelse(all.mods$model.fit == "glm" & all.mods$model.formula == "zero.one ~ x1 + x.random", "glm.nox2",
                                  ifelse(all.mods$model.fit == "glm.autocov" & all.mods$model.formula == "zero.one ~ x1 + x2 + x.random", "autocov",
                                         ifelse(all.mods$model.fit == "glm.autocov" & all.mods$model.formula == "zero.one ~ x1 + x.random", "autocov.nox2",
                                                ifelse(all.mods$model.fit == "glm.spev" & all.mods$model.formula == "zero.one ~ x1 + x2 + x.random", "spev.negexp",
                                                       ifelse(all.mods$model.fit == "glm.spev" & all.mods$model.formula == "zero.one ~ x1 + x.random", "spev.negexp.nox2",
                                                              ifelse(all.mods$model.fit == "negexp.free" & all.mods$model.formula == "zero.one ~ x1 + x2 + x.random", "mle2.negexp.free",
                                                                     ifelse(all.mods$model.fit == "negexp.free" & all.mods$model.formula == "zero.one ~ x1 + x.random", "mle2.negexp.free.nox2",
                                                                            ifelse(all.mods$model.fit == "negexp.free" & all.mods$model.formula == "zero.one ~ x2 + x.random", "mle2.negexp.free.nox1",
                                                                                   ifelse(all.mods$model.fit == "negexp.free" & all.mods$model.formula == "zero.one ~ x1 + x2", "mle2.negexp.free.x1x2",
                                                                                          ifelse(all.mods$model.fit == "negexp.free" & all.mods$model.formula == "zero.one ~ x1", "mle2.negexp.free.x1xnbi",
                                                                                                 ifelse(all.mods$model.fit == "negexp.free" & all.mods$model.formula == "zero.one ~ x2", "mle2.negexp.free.x2xnbi",
                                                                                                        ifelse(all.mods$model.fit == "negexp.free" & all.mods$model.formula == "zero.one ~ x.random", "mle2.negexp.free.xrandxnbi",
                                                                                                               ifelse(all.mods$model.fit == "negexp.free" & all.mods$model.formula == "zero.one ~ xnbi", "mle2.negexp.free.xnbi", NA))))))))))))))

all.mods$scenario<- ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x1 + x2 + x.random + s(x,y)", "gams",
                           ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x1 + x.random + s(x,y)", "gams.nox2",
                                  ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x2 + x.random + s(x,y)", "gams.nox1",
                                         ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x1 + x2 + s(x,y)", "gams.x1x2smooth",
                                                ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x1 + s(x,y)", "gams.x1smooth",
                                                       ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x2 + s(x,y)", "gams.x2smooth",
                                                              ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x.random + s(x,y)", "gams.xrandsmooth",
                                                                     ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x1 + x2", "gams.x1x2",
                                                                            ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x1 + x.random", "gams.x1xrand",
                                                                                   ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x2 + x.random", "gams.x2xrand",
                                                                     ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x1", "gams.x1",
                                                                            ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x2", "gams.x2",
                                                                                   ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ x.random", "gams.xrand",
                                                                     ifelse(all.mods$model.fit == "gam.s" & all.mods$model.formula == "zero.one ~ s(x,y)", "gams.smooth", all.mods$scenario))))))))))))))


all.mods$scenario<- ifelse(all.mods$model.fit == "gee" & all.mods$model.formula == "zero.one ~ x1 + x2 + x.random", "gee",
                           ifelse(all.mods$model.fit == "gee" & all.mods$model.formula == "zero.one ~ x1 + x.random", "gee.nox2",
                                  ifelse(all.mods$model.fit == "gee" & all.mods$model.formula == "zero.one ~ x2 + x.random", "gee.x2xrand",
                                         ifelse(all.mods$model.fit == "gee" & all.mods$model.formula == "zero.one ~ x1 + x2", "gee.x1x2",
                                                ifelse(all.mods$model.fit == "gee" & all.mods$model.formula == "zero.one ~ x1", "gee.x1",
                                                       ifelse(all.mods$model.fit == "gee" & all.mods$model.formula == "zero.one ~ x2", "gee.x2",
                                                              ifelse(all.mods$model.fit == "gee" & all.mods$model.formula == "zero.one ~ x.random", "gee.xrand", all.mods$scenario)))))))

all.mods$scenario<- ifelse(all.mods$model.fit == "glm.wavelet" & all.mods$model.formula == "zero.one ~ x1 + x2 + x.random", "glm.wavelet",
                           ifelse(all.mods$model.fit == "glm.wavelet" & all.mods$model.formula == "zero.one ~ x1 + x.random", "glm.wavelet.nox2",
                                  ifelse(all.mods$model.fit == "glm.wavelet" & all.mods$model.formula == "zero.one ~ x2 + x.random", "glm.wavelet.x2xrand",
                                         ifelse(all.mods$model.fit == "glm.wavelet" & all.mods$model.formula == "zero.one ~ x1 + x2", "glm.wavelet.x1x2",
                                                ifelse(all.mods$model.fit == "glm.wavelet" & all.mods$model.formula == "zero.one ~ x1", "glm.wavelet.x1",
                                                       ifelse(all.mods$model.fit == "glm.wavelet" & all.mods$model.formula == "zero.one ~ x2", "glm.wavelet.x2", 
                                                              ifelse(all.mods$model.fit == "glm.wavelet" & all.mods$model.formula == "zero.one ~ x.random", "glm.wavelet.xrand", all.mods$scenario)))))))

all.mods$scenario<- ifelse(all.mods$model.fit == "spglm" & all.mods$model.formula == "zero.one ~ x1 + x2 + x.random", "spglm",
                           ifelse(all.mods$model.fit == "spglm" & all.mods$model.formula == "zero.one ~ x1 + x.random", "spglm.nox2",
                                  ifelse(all.mods$model.fit == "spglm" & all.mods$model.formula == "zero.one ~ x2 + x.random", "spglm.nox1",
                                         ifelse(all.mods$model.fit == "spglm" & all.mods$model.formula == "zero.one ~ x1 + x2", "spglm.noxrand",
                                                ifelse(all.mods$model.fit == "spglm" & all.mods$model.formula == "zero.one ~ x1", "spglm.x1",
                                                       ifelse(all.mods$model.fit == "spglm" & all.mods$model.formula == "zero.one ~ x2", "spglm.x2",
                                                              ifelse(all.mods$model.fit == "spglm" & all.mods$model.formula == "zero.one ~ x.random", "spglm.xrand", all.mods$scenario)))))))

# Check this quickly...
temp<- all.mods[is.na(all.mods$scenario),]
temp2<- temp[temp$model.fit == "gee",]

# Model par names
model.par.names<- paste(all.mods$date, all.mods$batch, ".", all.mods$scenario, "globalmodresults", ".", all.mods$dataset.group, sep = "")
model.check.df<- data.frame("id" = seq(1, length(model.par.names)), "file" = model.par.names, "complete" = FALSE)

# Need to look for a unique ID to check to see if the model run has completed.
folders<- c("Weak", "Mod", "Strong")
stem.dir<- "~/Desktop/RSA_MergedFiles_Winter2018/"

for (i in 1:length(folders)) {
  sfold<- folders[i]
  dir.temp<- paste(stem.dir, sfold, "/", sep = "")
  all.temp<- list.files(dir.temp)[grepl("globalmodresults", list.files(dir.temp))]
  
  for(j in 1:length(all.temp)) {
    file.temp<- read.csv(paste(dir.temp, all.temp[j], sep = ""))
    file.name.comp<- paste(gsub(".csv", "", all.temp[j]), ".", unique(file.temp$datasets.group), sep = "")
    model.check.df$complete[model.check.df$file %in% file.name.comp]<- TRUE
  }
}

# What's missing?
models.missing.index<- model.check.df[!model.check.df$complete,]
models.missing<- all.mods[models.missing.index$id,]
write.csv(models.missing, "~/Desktop/RSA_MergedFiles_Winter2018/MissingModels04102018.csv")

############
###### Running missing models....
############
# Need to have the model fit parameters file updated to account for these missing models
# Is there a chance I did this already??
models.miss.early<- read_csv("./Inputs/SAC_batchlist_models_missing_02282017.csv")
head(models.miss.early)

# Merging column
models.miss.early$Merge.Column<- paste(models.miss.early$model.fit, models.miss.early$batch, sep = "_")

# Same for models.missing
models.missing$Merge.Column<- paste(models.missing$model.fit, models.missing$batch, sep = "_")

# What columns do we not need to bring over? id, batch, batch.model, model.fit, datasets, datasets.group, scenario.model.dataset, model.formula, scenario, Check.Column
models.miss.early.merge<- models.miss.early %>%
  dplyr::select(., -id, -batch, -date, -batch.model, -model.fit, -datasets, -datasets.group, -scenario.model.dataset, -model.formula, -scenario)
models.miss.early.merge$Merge.Column<- gsub("custom", "negexp", models.miss.early.merge$Merge.Column)
models.miss.early.merge<- models.miss.early.merge[!duplicated(models.miss.early.merge$Merge.Column),]
models.missing<- models.missing %>%
  left_join(., models.miss.early.merge, by = "Merge.Column")

# Will need to do some work with the inputs sheet....
write.csv(models.missing, "./Inputs/MissingmodelsApril2018.csv")

# Need to check SAC modeling functions -- make some changes to account for the mac piece and update directories
# Run function to fit missing models
# Re-run missing
# Move on to plots


############## RESIDUAL SPATIAL CORRELATION RESULTS ###########################
library(MESS)
library(plyr)
library(reshape)
library(ggplot2)

######################## Results plotting ###################################
colors<- c("#252525", "#636363", "#969696", "#bdbdbd", "#238b45", "#3182bd", "#08519c", "#d7310f")
colors<- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d','#666666')
lty.use<- c(1,1,2,3,6,1,1,5,1)
lwd.use<- c(3.25, 3.25, 3.25, 3.25, 3.25, 3.25, 3.25, 3.25)
################################################################################################
######################## Overall fitting success: Table
########################
n.mods<- 9
fit.time<- data.frame(matrix(nrow = n.mods, ncol = 10))
names(fit.time)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
fit.time[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
fit.time.ind<- 2

succ.rate<- data.frame(matrix(nrow = n.mods, ncol = 10))
names(succ.rate)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
succ.rate[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
succ.rate.ind<- 2


## Weak
{
  dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/WeakSpEnv/"

  # Fine
  # Order for plotting
  files.list.use<- as.vector(list.files[["Weak"]]$Fine)
  
  for(i in 1:length(files.list.use)) {
    t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
    fit.time[i, fit.time.ind]<- round(mean(t1$Time, na.rm = T), 2)
    succ.rate[i, succ.rate.ind]<- round(sum(!is.na(t1$intercept))/nrow(t1), 2)
    print(i)
  }
  
  # Med
  fit.time.ind<- fit.time.ind+1
  succ.rate.ind<- succ.rate.ind+1
  
  # Order for plotting
  files.list.use<- as.vector(list.files[["Weak"]]$Mod)
  
  for(i in 1:length(files.list.use)) {
    t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
    fit.time[i, fit.time.ind]<- round(mean(t1$Time, na.rm = T), 2)
    succ.rate[i, succ.rate.ind]<- round(sum(!is.na(t1$intercept))/nrow(t1), 2)
    print(i)
  }
  
  # Coarse
  fit.time.ind<- fit.time.ind+1
  succ.rate.ind<- succ.rate.ind+1
  
  # Order for plotting
  files.list.use<- as.vector(list.files[["Weak"]]$Coarse)
  
  for(i in 1:length(files.list.use)) {
    t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
    fit.time[i, fit.time.ind]<- round(mean(t1$Time, na.rm = T), 2)
    succ.rate[i, succ.rate.ind]<- round(sum(!is.na(t1$intercept))/nrow(t1), 2)
    print(i)
  }
  
  fit.time.ind<- fit.time.ind+1
  succ.rate.ind<- succ.rate.ind+1
}

## Mod
{
  dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/ModSpEnv/"
  
  # Fine
  # Order for plotting
  files.list.use<- as.vector(list.files[["Mod"]]$Fine)
  
  for(i in 1:length(files.list.use)) {
    t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
    fit.time[i, fit.time.ind]<- round(mean(t1$Time, na.rm = T), 2)
    succ.rate[i, succ.rate.ind]<- round(sum(!is.na(t1$intercept))/nrow(t1), 2)
    print(i)
  }
  
  # Med
  fit.time.ind<- fit.time.ind+1
  succ.rate.ind<- succ.rate.ind+1
  
  # Order for plotting
  files.list.use<- as.vector(list.files[["Mod"]]$Mod)
  
  for(i in 1:length(files.list.use)) {
    t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
    fit.time[i, fit.time.ind]<- round(mean(t1$Time, na.rm = T), 2)
    succ.rate[i, succ.rate.ind]<- round(sum(!is.na(t1$intercept))/nrow(t1), 2)
    print(i)
  }
  
  # Coarse
  fit.time.ind<- fit.time.ind+1
  succ.rate.ind<- succ.rate.ind+1
  
  # Order for plotting
  files.list.use<- as.vector(list.files[["Mod"]]$Coarse)
  
  for(i in 1:length(files.list.use)) {
    t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
    fit.time[i, fit.time.ind]<- round(mean(t1$Time, na.rm = T), 2)
    succ.rate[i, succ.rate.ind]<- round(sum(!is.na(t1$intercept))/nrow(t1), 2)
    print(i)
  }
  
  fit.time.ind<- fit.time.ind+1
  succ.rate.ind<- succ.rate.ind+1
}

## Strong
{
  dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/StrongSpEnv/"

  # Fine
  # Order for plotting
  files.list.use<- as.vector(list.files[["Strong"]]$Fine)
  
  for(i in 1:length(files.list.use)) {
    t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
    fit.time[i, fit.time.ind]<- round(mean(t1$Time, na.rm = T), 2)
    succ.rate[i, succ.rate.ind]<- round(sum(!is.na(t1$intercept))/nrow(t1), 2)
    print(i)
  }
  
  # Med
  fit.time.ind<- fit.time.ind+1
  succ.rate.ind<- succ.rate.ind+1
  
  # Order for plotting
  files.list.use<- as.vector(list.files[["Strong"]]$Mod)
  
  for(i in 1:length(files.list.use)) {
    t1<- try(read.csv(paste(dir.use, files.list.use[i], sep = "")), silent = TRUE)
    if(class(t1) == "try-error"){
      print(i)
      next()
    } else {
      fit.time[i, fit.time.ind]<- round(mean(t1$Time, na.rm = T), 2)
      succ.rate[i, succ.rate.ind]<- round(sum(!is.na(t1$intercept))/nrow(t1), 2)
      print(i)
    }
  }
  
  # Coarse
  fit.time.ind<- fit.time.ind+1
  succ.rate.ind<- succ.rate.ind+1
  
  # Order for plotting
  files.list.use<- as.vector(list.files[["Strong"]]$Coarse)
  
  for(i in 1:length(files.list.use)) {
    t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
    fit.time[i, fit.time.ind]<- round(mean(t1$Time, na.rm = T), 2)
    succ.rate[i, succ.rate.ind]<- round(sum(!is.na(t1$intercept))/nrow(t1), 2)
    print(i)
  }
}


################################################################################################
######################## Accounting for/removing rSAC: Correlogram
########################

##### Causes of rSAC: Environment vs. Biological processes
##### Environment Only
{
  dir.use<- "~/R/Allyn/PhD/SAC/Anthill/Old2/"
  files.list<- list.files(dir.use, "moransI.csv$")
  
  result<- data.frame(matrix(nrow = 50, ncol = length(files.list)))
  names(result)[1]<- "dist.bin"
  result[,1]<- seq(from  = 1, to  = 50, by = 1)
  
  for (i in 1:length(files.list)) {
    t1<- read.csv(paste(dir.use, files.list[i], sep = ""))
    result[,(i+1)]<- t1$morans.i
    names(result)[(i+1)]<- paste(files.list[i])
  }
  
  # Order for plotting
  orderA<- c("dist.bin", "2182014ewf.enn.bnn.glm.fullmoransI.csv", "2182014ewf.enn.bnn.nox1moransI.csv", "2182014esc.enn.bnn.glm.fullmoransI.csv", "2182014esc.enn.bnn.glm.nox1moransI.csv") 
  # "2182014esm.enn.bnn.glm.fullmoransI.csv", "2182014esm.enn.bnn.glm.nox1moransI.csv", "2182014esf.enn.bnn.glm.fullmoransI.csv", "2182014esf.enn.bnn.glm.nox1moransI.csv", "2182014emc.enn.bnn.glm.fullmoransI.csv", "2182014emc.enn.bnn.glm.nox1moransI.csv", "2182014emm.enn.bnn.glm.fullmoransI.csv", "2182014emm.enn.bnn.glm.nox1moransI.csv", "2182014emf.enn.bnn.glm.fullmoransI.csv", "2182014emf.enn.bnn.glm.nox1moransI.csv", "2182014ewc.enn.bnn.glm.fullmoransI.csv", "2182014ewc.enn.bnn.glm.nox1moransI.csv", "2182014ewm.enn.bnn.glm.fullmoransI.csv", "2182014ewm.enn.bnn.glm.nox1moransI.csv"
  )

result.use<- result[orderA]

par(mar = c(4,5,3,2)+0.1)
par(mfrow = c(1,2))

# Subplot 1

#EWF
plot(x = result.use$dist.bin, 
     y = result.use[,2], 
     type = "l",
     las = 1,
     xlim = c(0,50), 
     xlab = "Distance bin (cells)",
     ylim = c(-0.1, 0.6), 
     ylab = "Moran's I", 
     main = paste("Weak", "\n", "Species-Environment Relationship"),
     lty = 1,
     col = "black")
lines(x = result.use$dist.bin, 
      y = result.use[,3], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 0.6), 
      ylab = "Moran's I", 
      lty = 2,
      col = "black")

#ESC
plot(x = result.use$dist.bin, 
     y = result.use[,4], 
     type = "l",
     las = 1,
     xlim = c(0,50), 
     xlab = "Distance bin (cells)",
     ylim = c(-0.1, 0.6), 
     ylab = "Moran's I", 
     main = paste("Strong", "\n", "Species-Environment Relationship"),
     lty = 1,
     col = "black")
lines(x = result.use$dist.bin, 
      y = result.use[,5], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 0.6), 
      ylab = "Moran's I", 
      lty = 2,
      col = "black")

# ESM
lines(x = result.use$dist.bin, 
      y = result.use[,4], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 1,
      col = "green")
lines(x = result.use$dist.bin, 
      y = result.use[,5], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 2,
      col = "green")

# ESF
lines(x = result.use$dist.bin, 
      y = result.use[,6], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 1,
      col = "blue")
lines(x = result.use$dist.bin, 
      y = result.use[,7], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 2,
      col = "blue")


# Subplot 2

#EMC
plot(x = result.use$dist.bin, 
     y = result.use[,8], 
     type = "l",
     las = 1,
     xlim = c(0,50), 
     xlab = "Distance bin (cells)",
     ylim = c(-0.1, 1), 
     ylab = "Moran's I", 
     main = paste("Moderate", "\n", "Species-Environment Relationship"),
     lty = 1,
     col = "red")
lines(x = result.use$dist.bin, 
      y = result.use[,9], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 2,
      col = "red")

# ESM
lines(x = result.use$dist.bin, 
      y = result.use[,10], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 1,
      col = "green")
lines(x = result.use$dist.bin, 
      y = result.use[,11], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 2,
      col = "green")

# ESF
lines(x = result.use$dist.bin, 
      y = result.use[,12], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 1,
      col = "blue")
lines(x = result.use$dist.bin, 
      y = result.use[,13], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 2,
      col = "blue")

# Subplot 3

#EWC
plot(x = result.use$dist.bin, 
     y = result.use[,14], 
     type = "l",
     las = 1,
     xlim = c(0,50), 
     xlab = "Distance bin (cells)",
     ylim = c(-0.1, 1), 
     ylab = "Moran's I", 
     main = paste("Weak", "\n", "Species-Environment Relationship"),
     lty = 1,
     col = "red")
lines(x = result.use$dist.bin, 
      y = result.use[,15], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 2,
      col = "red")

# EWM
lines(x = result.use$dist.bin, 
      y = result.use[,16], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 1,
      col = "green")
lines(x = result.use$dist.bin, 
      y = result.use[,17], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 2,
      col = "green")

# EWF
lines(x = result.use$dist.bin, 
      y = result.use[,18], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 1,
      col = "blue")
lines(x = result.use$dist.bin, 
      y = result.use[,19], 
      las = 1,
      xlim = c(0,50), 
      xlab = "Distance bin (cells)",
      ylim = c(-0.1, 1), 
      ylab = "Moran's I", 
      lty = 2,
      col = "blue")

plot.linetypes<- c(1,2,1,1,2)
plot.colors<- c("red", "red", "green", "green", "blue", "blue")
plot.text<- c("Coarse.full", "Coarse.reduced", "Medium.Full", "Medium.Reduced", "Fine.Full", "Fine.reduced")
}

##### Bio Only
{
  dir.use<- "~/R/Allyn/PhD/SAC/Anthill/Old2/"
  files.list<- list.files(dir.use, "moransI.csv$")
  
  result<- data.frame(matrix(nrow = 50, ncol = length(files.list)))
  names(result)[1]<- "dist.bin"
  result[,1]<- seq(from  = 1, to  = 50, by = 1)
  
  for (i in 1:length(files.list)) {
    t1<- read.csv(paste(dir.use, files.list[i], sep = ""))
    result[,(i+1)]<- t1$morans.i
    names(result)[(i+1)]<- paste(files.list[i])
  }
  
  # Order for plotting
  orderA<- c("dist.bin", "2182014enn.enn.bf.glmmoransI.csv", "2182014enn.enn.bs.glmmoransI.csv")
  
  result.use<- result[orderA]
  
  par(mar = c(4,5,3,2)+0.1)
  par(mfrow = c(1,2))
  
  plot(x = result.use$dist.bin, 
       y = result.use[,2], 
       type = "l",
       las = 1,
       xlim = c(0,50), 
       xlab = "Distance bin (cells)",
       ylim = c(-0.1, 0.6), 
       ylab = "Moran's I", 
       main = paste("Biological process only"),
       lty = 1,
       col = "darkblue")
  lines(x = result.use$dist.bin, 
        y = result.use[,3], 
        las = 1,
        xlim = c(0,50), 
        xlab = "Distance bin (cells)",
        ylim = c(-0.1, 0.6), 
        ylab = "Moran's I", 
        lty = 2,
        col = "darkgreen")
  lines(x = result.use$dist.bin, 
        y = result.use[,4], 
        las = 1,
        xlim = c(0,50), 
        xlab = "Distance bin (cells)",
        ylim = c(-0.1, 1), 
        ylab = "Moran's I", 
        lty = 1,
        col = "blue")
}  

##### Accounting for/Removing rSAC
##### Weak species-environment relationship, fine-medium-coarse scale rSAC
colors<- c("#252525", "#636363", "#969696", "#bdbdbd", "#238b45", "#3182bd", "#08519c", "#d7310f")
colors<- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d','#666666')
colors<- c("black", "#023858", "#045a8d", "#0570b0", "#3690c0", "#4daf4a", "#cb181d", "#fb6a4a", "#984ea3")
lty.use<- c(1,1,2,3,5,1,1,3,1)
lwd.use<- c(1.5, rep(2, 8))
out.dir<- "~/Desktop/RSA_MergedFiles_Winter2018/"
{
  dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/MoransI/Weak/"
  
  ## File selection
  files.sv<- c("11072015emm.ewf.bn.glmmoransI.csv", "11072015emm.ewf.bf.glmmoransI.csv", "11072015emm.ewf.bf.autocovmoransI.csv", "11072015emm.ewf.bf.spev.negexpmoransI.csv", "11072015emm.ewf.bf.mle2.negexp.freemoransI.csv", "11072015emm.ewf.bf.gamsmoransI.csv", "11072015emm.ewf.bf.geemoransI.csv", "11072015emm.ewf.bf.spglmmoransI.csv", "11072015emm.ewf.bf.glm.waveletmoransI.csv",  
               "11072015emm.ewf.bn.glm.nox2moransI.csv", "11072015emm.ewf.bf.glm.nox2moransI.csv", "11072015emm.ewf.bf.autocov.nox2moransI.csv", "11072015emm.ewf.bf.spev.negexp.nox2moransI.csv", "11072015emm.ewf.bf.mle2.negexp.free.nox2moransI.csv", "11072015emm.ewf.bf.gams.nox2moransI.csv", "11072015emm.ewf.bf.gee.nox2moransI.csv", "11072015emm.ewf.bf.spglm.nox2moransI.csv", "11072015emm.ewf.bf.glm.wavelet.nox2moransI.csv", 
               # Weak-Mod, Biological Med
               "11072015emm.ewm.bn.glmmoransI.csv", "11072015emm.ewm.bm.glmmoransI.csv", "11072015emm.ewm.bm.autocovmoransI.csv", "11072015emm.ewm.bm.spev.negexpmoransI.csv", 
               #"11072015emm.ewm.bm.mle2.negexp.freemoransI.csv", 
               "11072015emm.ewf.bf.mle2.negexp.freemoransI.csv", "11072015emm.ewm.bm.gamsmoransI.csv", "11072015emm.ewm.bm.geemoransI.csv", "11072015emm.ewm.bm.spglmmoransI.csv", "11072015emm.ewm.bm.glm.waveletmoransI.csv", 
               "11072015emm.ewm.bn.glm.nox2moransI.csv", "11072015emm.ewm.bm.glm.nox2moransI.csv", "11072015emm.ewm.bm.autocov.nox2moransI.csv", "11072015emm.ewm.bm.spev.negexp.nox2moransI.csv", "11072015emm.ewm.bm.mle2.negexp.free.nox2moransI.csv", "11072015emm.ewm.bm.gams.nox2moransI.csv", "11072015emm.ewm.bm.gee.nox2moransI.csv", "11072015emm.ewm.bm.spglm.nox2moransI.csv", "11072015emm.ewm.bm.glm.wavelet.nox2moransI.csv", 
               # Weak-Coarse, Biological Strong
               "11072015emm.ewc.bn.glmmoransI.csv", "11072015emm.ewc.bs.glmmoransI.csv", "11072015emm.ewc.bs.autocovmoransI.csv", "11072015emm.ewc.bs.spev.negexpmoransI.csv", "11072015emm.ewc.bs.mle2.negexp.freemoransI.csv", "11072015emm.ewc.bs.gamsmoransI.csv", "11072015emm.ewc.bs.geemoransI.csv", "11072015emm.ewc.bs.spglmmoransI.csv", "11072015emm.ewc.bs.glm.waveletmoransI.csv", 
               "11072015emm.ewc.bn.glm.nox2moransI.csv", "11072015emm.ewc.bs.glm.nox2moransI.csv", "11072015emm.ewc.bs.autocov.nox2moransI.csv", "11072015emm.ewc.bs.spev.negexp.nox2moransI.csv", "11072015emm.ewc.bs.mle2.negexp.free.nox2moransI.csv", "11072015emm.ewc.bs.gams.nox2moransI.csv", "11072015emm.ewc.bs.gee.nox2moransI.csv", "11072015emm.ewc.bs.spglm.nox2moransI.csv", "11072015emm.ewc.bs.glm.wavelet.nox2moransI.csv")
  
  ## Correlogram plot results
  result<- data.frame(matrix(nrow = 50, ncol = length(files.sv)+1))
  names(result)[1]<- "dist.bin"
  result[,1]<- seq(from  = 1, to  = 50, by = 1)
  
  ## Correlogram AUC results
  result.weak.auc<- data.frame(matrix(nrow = length(files.sv), ncol = 2))
  names(result.weak.auc)<- c("model", "auc")
  result.weak.auc[,1]<- paste(files.sv)
  
  for (i in 1:length(files.sv)) {
    t1<- read.csv(paste(dir.use, files.sv[i], sep = ""))
    
    # Correlogram plot
    result[,(i+1)]<- aggregate(t1$moransI, by = list(t1$dist.bin), function(x) mean(x, na.rm = T))[,2]
    names(result)[(i+1)]<- paste(files.sv[i])
    
    # Correlogram AUC
    result.weak.auc$auc[i]<- MESS::auc(result$dist.bin, abs(result[,(i+1)]), from = 2, to = 50, type = "spline")
    
    print(i)
  }
  
  # Order for plotting
  result.use<- result[c("dist.bin", files.sv[-25])]
  result.use<- result
  
  ## Plotting
  tiff(filename = paste(out.dir, "WeakFine_MoransI.tiff", sep = ""),
       width = 11, height = 8, units = "in", res = 250)
  
  yrange.use<- c(-0.2, 0.7)
  xrange.use<- c(0, 30)
  
  par(mar = c(4,5,3,2)+0.1)
  par(mfrow = c(1,2))
  
  
  # Subplot 1 - WEAK-FINE - FULL
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.ewf.bf.glmmoransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bn.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.autocovmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.spev.negexpmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.mle2.negexp.freemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.gamsmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.geemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.spglmmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.glm.waveletmoransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "A")
  
  
  # Subplot 2 - WEAK-FINE - NoX2
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.ewf.bf.glm.nox2moransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bn.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.autocov.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.spev.negexp.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.mle2.negexp.free.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.gams.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.gee.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.spglm.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.glm.wavelet.nox2moransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "B")
  dev.off()
  
  ####
  # Subplot 1 - WEAK-MED - FULL
  tiff(filename = paste(out.dir, "WeakMed_MoransI.tiff", sep = ""),
       width = 11, height = 8, units = "in", res = 250)
  
  yrange.use<- c(-0.2, 0.7)
  xrange.use<- c(0, 30)
  
  par(mar = c(4,5,3,2)+0.1)
  par(mfrow = c(1,2))
  
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.ewm.bm.glmmoransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bn.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.autocovmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.spev.negexpmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  # EDIT
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewf.bf.mle2.negexp.freemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.gamsmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.geemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.spglmmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.glm.waveletmoransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "A")
  
  # Subplot 2 - WEAK-MED - NoX2
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.ewm.bm.glm.nox2moransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bn.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.autocov.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.spev.negexp.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.mle2.negexp.free.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.gams.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.gee.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.spglm.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewm.bm.glm.wavelet.nox2moransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "B")
  dev.off()
  
  
  ####
  # Subplot 1 - WEAK-COARSE - FULL
  tiff(filename = paste(out.dir, "WeakCoarse_MoransI.tiff", sep = ""),
       width = 11, height = 8, units = "in", res = 250)
  
  yrange.use<- c(-0.2, 0.7)
  xrange.use<- c(0, 30)
  
  par(mar = c(4,5,3,2)+0.1)
  par(mfrow = c(1,2))
  
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.ewc.bs.glmmoransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bn.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.autocovmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.spev.negexpmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.mle2.negexp.freemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.gamsmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.geemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.spglmmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.glm.waveletmoransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "A")
  
  # Subplot 2 - WEAK-COARSE - NoX2
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.ewc.bs.glm.nox2moransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bn.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.autocov.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.spev.negexp.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.mle2.negexp.free.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.gams.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.gee.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.spglm.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.ewc.bs.glm.wavelet.nox2moransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "B")
  dev.off()
}

##### Mod species-environment relationship, fine-medium-coarse scale rSAC
{
  dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/MoransI/Mod/"
  
  ## File selection
  files.sv<- c("11072015emm.emf.bn.glmmoransI.csv", "11072015emm.emf.bf.glmmoransI.csv", "11072015emm.emf.bf.autocovmoransI.csv", "11072015emm.emf.bf.spev.negexpmoransI.csv", "11072015emm.emf.bf.mle2.negexp.freemoransI.csv", "11072015emm.emf.bf.gamsmoransI.csv", "11072015emm.emf.bf.geemoransI.csv", "11072015emm.emf.bf.spglmmoransI.csv", "11072015emm.emf.bf.glm.waveletmoransI.csv",  
               "11072015emm.emf.bn.glm.nox2moransI.csv", "11072015emm.emf.bf.glm.nox2moransI.csv", "11072015emm.emf.bf.autocov.nox2moransI.csv", "11072015emm.emf.bf.spev.negexp.nox2moransI.csv", "11072015emm.emf.bf.mle2.negexp.free.nox2moransI.csv", "11072015emm.emf.bf.gams.nox2moransI.csv", "11072015emm.emf.bf.gee.nox2moransI.csv", "11072015emm.emf.bf.spglm.nox2moransI.csv", "11072015emm.emf.bf.glm.wavelet.nox2moransI.csv", 
               # Mod-Mod, Biological Med
               "11072015emm.emm.bn.glmmoransI.csv", "11072015emm.emm.bm.glmmoransI.csv", "11072015emm.emm.bm.autocovmoransI.csv", "11072015emm.emm.bm.spev.negexpmoransI.csv", "11072015emm.emm.bm.mle2.negexp.freemoransI.csv", "11072015emm.emm.bm.gamsmoransI.csv", "11072015emm.emm.bm.geemoransI.csv", "11072015emm.emm.bm.spglmmoransI.csv", "11072015emm.emm.bm.glm.waveletmoransI.csv", 
               "11072015emm.emm.bn.glm.nox2moransI.csv", "11072015emm.emm.bm.glm.nox2moransI.csv", "11072015emm.emm.bm.autocov.nox2moransI.csv", "11072015emm.emm.bm.spev.negexp.nox2moransI.csv", "11072015emm.emm.bm.mle2.negexp.free.nox2moransI.csv", "11072015emm.emm.bm.gams.nox2moransI.csv", "11072015emm.emm.bm.gee.nox2moransI.csv", "11072015emm.emm.bm.spglm.nox2moransI.csv", "11072015emm.emm.bm.glm.wavelet.nox2moransI.csv", 
               # Mod-Coarse, Biological Strong
               "11072015emm.emc.bn.glmmoransI.csv", "11072015emm.emc.bs.glmmoransI.csv", "11072015emm.emc.bs.autocovmoransI.csv", "11072015emm.emc.bs.spev.negexpmoransI.csv", "11072015emm.emc.bs.mle2.negexp.freemoransI.csv", "11072015emm.emc.bs.gamsmoransI.csv", "11072015emm.emc.bs.geemoransI.csv", "11072015emm.emc.bs.spglmmoransI.csv", "11072015emm.emc.bs.glm.waveletmoransI.csv", 
               "11072015emm.emc.bn.glm.nox2moransI.csv", "11072015emm.emc.bs.glm.nox2moransI.csv", "11072015emm.emc.bs.autocov.nox2moransI.csv", "11072015emm.emc.bs.spev.negexp.nox2moransI.csv", "11072015emm.emc.bs.mle2.negexp.free.nox2moransI.csv", "11072015emm.emc.bs.gams.nox2moransI.csv", "11072015emm.emc.bs.gee.nox2moransI.csv", "11072015emm.emc.bs.spglm.nox2moransI.csv", "11072015emm.emc.bs.glm.wavelet.nox2moransI.csv")
  
  ## Correlogram plot results
  result<- data.frame(matrix(nrow = 50, ncol = length(files.sv)+1))
  names(result)[1]<- "dist.bin"
  result[,1]<- seq(from  = 1, to  = 50, by = 1)
  
  ## Correlogram AUC results
  result.mod.auc<- data.frame(matrix(nrow = length(files.sv), ncol = 2))
  names(result.mod.auc)<- c("model", "auc")
  result.mod.auc[,1]<- paste(files.sv)
  
  for (i in 1:length(files.sv)) {
    t1<- read.csv(paste(dir.use, files.sv[i], sep = ""))
    
    # Correlogram plot
    result[,(i+1)]<- aggregate(t1$moransI, by = list(t1$dist.bin), function(x) mean(x, na.rm = T))[,2]
    names(result)[(i+1)]<- paste(files.sv[i])
    
    # Correlogram AUC
    result.mod.auc$auc[i]<- MESS::auc(result$dist.bin, abs(result[,(i+1)]), from = 2, to = 50, type = "spline")
    
    print(i)
  }
  
  # Order for plotting
  result.use<- result[c("dist.bin", files.sv)]
  
  ## Plotting
  tiff(filename = paste(out.dir, "ModFine_MoransI.tiff", sep = ""),
       width = 11, height = 8, units = "in", res = 250)
  
  yrange.use<- c(-0.2, 0.7)
  xrange.use<- c(0, 30)
  
  par(mar = c(4,5,3,2)+0.1)
  par(mfrow = c(1,2))
  
  # Subplot 1 - MOD-FINE - FULL
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.emf.bf.glmmoransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bn.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.autocovmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.spev.negexpmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.mle2.negexp.freemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.gamsmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.geemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.spglmmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.glm.waveletmoransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "A")
  
  
  # Subplot 2 - MOD-FINE - NoX2
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.emf.bf.glm.nox2moransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bn.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.autocov.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.spev.negexp.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.mle2.negexp.free.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.gams.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.gee.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.spglm.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emf.bf.glm.wavelet.nox2moransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "B")
  dev.off()
  
  
  ####
  # Subplot 1 - MOD-MED - FULL
  tiff(filename = paste(out.dir, "ModMed_MoransI.tiff", sep = ""),
       width = 11, height = 8, units = "in", res = 250)
  
  yrange.use<- c(-0.2, 0.7)
  xrange.use<- c(0, 30)
  
  par(mar = c(4,5,3,2)+0.1)
  par(mfrow = c(1,2))
  
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.emm.bm.glmmoransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bn.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.autocovmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.spev.negexpmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.mle2.negexp.freemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.gamsmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.geemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.spglmmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.glm.waveletmoransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "A")
  
  # Subplot 2 - MOD-MED - NoX2
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.emm.bm.glm.nox2moransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bn.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.autocov.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.spev.negexp.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.mle2.negexp.free.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.gams.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.gee.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.spglm.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emm.bm.glm.wavelet.nox2moransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "B")
  dev.off()
  
  
  ####
  # Subplot 1 - MOD-COARSE - FULL
  tiff(filename = paste(out.dir, "ModCoarse_MoransI.tiff", sep = ""),
       width = 11, height = 8, units = "in", res = 250)
  
  yrange.use<- c(-0.2, 0.7)
  xrange.use<- c(0, 30)
  
  par(mar = c(4,5,3,2)+0.1)
  par(mfrow = c(1,2))
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.emc.bs.glmmoransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bn.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.autocovmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.spev.negexpmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.mle2.negexp.freemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.gamsmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.geemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.spglmmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.glm.waveletmoransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "A")
  
  # Subplot 2 - MOD-COARSE - NoX2
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.emc.bs.glm.nox2moransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bn.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.autocov.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.spev.negexp.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.mle2.negexp.free.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.gams.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.gee.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.spglm.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.emc.bs.glm.wavelet.nox2moransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "B")
  dev.off()
}

##### Strong species-environment relationship, fine-medium-coarse scale rSAC
{
  dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/MoransI/Strong/"
  
  ## File selection
  files.sv<- c("11072015emm.esf.bn.glmmoransI.csv", "11072015emm.esf.bf.glmmoransI.csv", "11072015emm.esf.bf.autocovmoransI.csv", "11072015emm.esf.bf.spev.negexpmoransI.csv", "11072015emm.esf.bf.mle2.negexp.freemoransI.csv", "11072015emm.esf.bf.gamsmoransI.csv", "11072015emm.esf.bf.geemoransI.csv", "11072015emm.esf.bf.spglmmoransI.csv", "11072015emm.esf.bf.glm.waveletmoransI.csv",  
               "11072015emm.esf.bn.glm.nox2moransI.csv", "11072015emm.esf.bf.glm.nox2moransI.csv", "11072015emm.esf.bf.autocov.nox2moransI.csv", "11072015emm.esf.bf.spev.negexp.nox2moransI.csv", "11072015emm.esf.bf.mle2.negexp.free.nox2moransI.csv", "11072015emm.esf.bf.gams.nox2moransI.csv", "11072015emm.esf.bf.gee.nox2moransI.csv", "11072015emm.esf.bf.spglm.nox2moransI.csv", "11072015emm.esf.bf.glm.wavelet.nox2moransI.csv", 
               # Strong-Mod, Biological Med
               "11072015emm.esm.bn.glmmoransI.csv", "11072015emm.esm.bm.glmmoransI.csv", "11072015emm.esm.bm.autocovmoransI.csv", "11072015emm.esm.bm.spev.negexpmoransI.csv", "11072015emm.esm.bm.mle2.negexp.freemoransI.csv", "11072015emm.esm.bm.gamsmoransI.csv", "11072015emm.esm.bm.geemoransI.csv", "11072015emm.esm.bm.spglmmoransI.csv", "11072015emm.esm.bm.glm.waveletmoransI.csv", 
               "11072015emm.esm.bn.glm.nox2moransI.csv", "11072015emm.esm.bm.glm.nox2moransI.csv", "11072015emm.esm.bm.autocov.nox2moransI.csv", "11072015emm.esm.bm.spev.negexp.nox2moransI.csv", "11072015emm.esm.bm.mle2.negexp.free.nox2moransI.csv", "11072015emm.esm.bm.gams.nox2moransI.csv", "11072015emm.esm.bm.gee.nox2moransI.csv", "11072015emm.esm.bm.spglm.nox2moransI.csv", "11072015emm.esm.bm.glm.wavelet.nox2moransI.csv", 
               # Strong-Coarse, Biological Strong
               "11072015emm.esc.bn.glmmoransI.csv", "11072015emm.esc.bs.glmmoransI.csv", "11072015emm.esc.bs.autocovmoransI.csv", "11072015emm.esc.bs.spev.negexpmoransI.csv", "11072015emm.esc.bs.mle2.negexp.freemoransI.csv", "11072015emm.esc.bs.gamsmoransI.csv", "11072015emm.esc.bs.geemoransI.csv", "11072015emm.esc.bs.spglmmoransI.csv", "11072015emm.esc.bs.glm.waveletmoransI.csv", 
               "11072015emm.esc.bn.glm.nox2moransI.csv", "11072015emm.esc.bs.glm.nox2moransI.csv", "11072015emm.esc.bs.autocov.nox2moransI.csv", "11072015emm.esc.bs.spev.negexp.nox2moransI.csv", "11072015emm.esc.bs.mle2.negexp.free.nox2moransI.csv", "11072015emm.esc.bs.gams.nox2moransI.csv", "11072015emm.esc.bs.gee.nox2moransI.csv", "11072015emm.esc.bs.spglm.nox2moransI.csv", "11072015emm.esc.bs.glm.wavelet.nox2moransI.csv")
  
  ## Correlogram plot results
  result<- data.frame(matrix(nrow = 50, ncol = length(files.sv)+1))
  names(result)[1]<- "dist.bin"
  result[,1]<- seq(from  = 1, to  = 50, by = 1)
  
  ## Correlogram AUC results
  result.strong.auc<- data.frame(matrix(nrow = length(files.sv), ncol = 2))
  names(result.strong.auc)<- c("model", "auc")
  result.strong.auc[,1]<- paste(files.sv)
  
  for (i in 1:length(files.sv)) {
    t1<- read.csv(paste(dir.use, files.sv[i], sep = ""))
    
    # Correlogram plot
    result[,(i+1)]<- aggregate(t1$moransI, by = list(t1$dist.bin), function(x) mean(x, na.rm = T))[,2]
    names(result)[(i+1)]<- paste(files.sv[i])
    
    # Correlogram AUC
    result.strong.auc$auc[i]<- MESS::auc(result$dist.bin, abs(result[,(i+1)]), from = 2, to = 50, type = "spline")
    
    print(i)
  }
  
  # Order for plotting
  result.use<- result[c("dist.bin", files.sv)]
  
  ## Plotting
  tiff(filename = paste(out.dir, "StrongFine_MoransI.tiff", sep = ""),
       width = 11, height = 8, units = "in", res = 250)
  
  yrange.use<- c(-0.2, 0.7)
  xrange.use<- c(0, 30)
  
  par(mar = c(4,5,3,2)+0.1)
  par(mfrow = c(1,2))
  
  # Subplot 1 - STRONG-FINE - FULL
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.esf.bf.glmmoransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bn.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.autocovmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.spev.negexpmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.mle2.negexp.freemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.gamsmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.geemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.spglmmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.glm.waveletmoransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "A")
  
  # Subplot 2 - STRONG-FINE - NoX2
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.esf.bf.glm.nox2moransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bn.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.autocov.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.spev.negexp.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.mle2.negexp.free.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.gams.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.gee.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.spglm.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esf.bf.glm.wavelet.nox2moransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "B")
  dev.off()
  
  ####
  # Subplot 1 - STRONG-MED - FULL
  tiff(filename = paste(out.dir, "StrongMed_MoransI.tiff", sep = ""),
       width = 11, height = 8, units = "in", res = 250)
  
  yrange.use<- c(-0.2, 0.7)
  xrange.use<- c(0, 30)
  
  par(mar = c(4,5,3,2)+0.1)
  par(mfrow = c(1,2))
  
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.esm.bm.glmmoransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bn.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.autocovmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.spev.negexpmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.mle2.negexp.freemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.gamsmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.geemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.spglmmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.glm.waveletmoransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "A")
  
  # Subplot 2 - STRONG-MED - NoX2
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.esm.bm.glm.nox2moransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bn.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.autocov.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.spev.negexp.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.mle2.negexp.free.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.gams.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.gee.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.spglm.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esm.bm.glm.wavelet.nox2moransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "B")
  dev.off()
  
  ####
  # Subplot 1 - STRONG-COARSE - FULL
  tiff(filename = paste(out.dir, "StrongCoarse_MoransI.tiff", sep = ""),
       width = 11, height = 8, units = "in", res = 250)
  
  yrange.use<- c(-0.2, 0.7)
  xrange.use<- c(0, 30)
  
  par(mar = c(4,5,3,2)+0.1)
  par(mfrow = c(1,2))
  
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.esc.bs.glmmoransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bn.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.glmmoransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.autocovmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.spev.negexpmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.mle2.negexp.freemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.gamsmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.geemoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.spglmmoransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.glm.waveletmoransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "A")
  
  # Subplot 2 - STRONG-COARSE - NoX2
  plot(x = result.use$dist.bin, 
       y = result.use[,"11072015emm.esc.bs.glm.nox2moransI.csv"], 
       xlim = xrange.use,
       ylim = yrange.use,
       xlab = "Distance bin (cells)",
       ylab = "Moran's I",
       las = 1,
       type = "n")
  
  # Plots
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
  
  # Add horizontal line
  abline(h = 0, col = "#969696")
  
  # Other lines
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bn.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[1],
        col = colors[1],
        lwd = lwd.use[1])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.glm.nox2moransI.csv"], 
        type = "l",
        lty = lty.use[2],
        col = colors[2],
        lwd = lwd.use[2])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.autocov.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylim = yrange.use, 
        ylab = "Moran's I", 
        lty = lty.use[3],
        col = colors[3],
        lwd = lwd.use[3])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.spev.negexp.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[4],
        col = colors[4],
        lwd = lwd.use[4])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.mle2.negexp.free.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[5],
        col = colors[5],
        lwd = lwd.use[5])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.gams.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[6],
        col = colors[6],
        lwd = lwd.use[6])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.gee.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[7],
        col = colors[7],
        lwd = lwd.use[7])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.spglm.nox2moransI.csv"], 
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[8],
        col = colors[8],
        lwd = lwd.use[8])
  lines(x = result.use$dist.bin, 
        y = result.use[,"11072015emm.esc.bs.glm.wavelet.nox2moransI.csv"], 
        type = "l",
        las = 1,
        xlim = xrange.use, 
        xlab = "Distance bin (cells)",
        ylab = "Moran's I", 
        lty = lty.use[9],
        col = colors[9],
        lwd = lwd.use[9])
  text(x = 0.15, y = 0.69, cex = 2.25, labels = "B")
  dev.off()
}

########## Consequeces - Parameter estimates

###### Density plots
{
  #### Weak
  {
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/WeakSpEnv/"
    
    # What parameter?
    param<- "x1" # Look at distribution of x1
    
    # Fine
    # Order for plotting
    {
      files.list.use<- c("11072015emm.ewf.bn.glmglobalmodresults.csv", "11072015emm.ewf.bf.glmglobalmodresults.csv", "11072015emm.ewf.bf.autocovglobalmodresults.csv", "11072015emm.ewf.bf.spev.negexpglobalmodresults.csv", "11072015emm.ewf.bf.mle2.negexp.freeglobalmodresults.csv", "11072015emm.ewf.bf.gamsglobalmodresults.csv", "11072015emm.ewf.bf.geeglobalmodresults.csv", "11072015emm.ewf.bf.spglmglobalmodresults.csv", "11072015emm.ewf.bf.glm.waveletglobalmodresults.csv",  
                         "11072015emm.ewf.bn.glm.nox2globalmodresults.csv", "11072015emm.ewf.bf.glm.nox2globalmodresults.csv", "11072015emm.ewf.bf.autocov.nox2globalmodresults.csv", "11072015emm.ewf.bf.spev.negexp.nox2globalmodresults.csv", "11072015emm.ewf.bf.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.ewf.bf.gams.nox2globalmodresults.csv", "11072015emm.ewf.bf.gee.nox2globalmodresults.csv", "11072015emm.ewf.bf.spglm.nox2globalmodresults.csv", "11072015emm.ewf.bf.glm.wavelet.nox2globalmodresults.csv")
      
      # Full models
      start<- 1
      tiff(filename = paste(out.dir, "WeakFine_X1Density.tiff", sep = ""),
           width = 11, height = 8, units = "in", res = 250)
      
      par(mfrow = c(1,2))
      par(mar = c(4,5,3,2)+0.1)
      
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 165), las = 1, xlab = "X1 Parameter estimate", main = "", ylab = "Density", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # Start index
      start<- 1
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, lty = "dashed", col = colors[1])
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
      
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "A")
      abline(v = 0.025, lty = 2)
      
      ## Reduced models
      new.start<- (length(files.list.use)/2)+1
      
      # Set up plot
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 165), las = 1, xlab = "X1 Parameter estimate", main = "", ylab = "Density", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[new.start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[1], lty = "dashed")
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[new.start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[new.start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
      
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[new.start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[new.start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "B")
      abline(v = 0.025, lty = 2)
      dev.off()
    }
    
    # Medium
    # Order for plotting
    {
      files.list.use<- c("11072015emm.ewm.bn.glmglobalmodresults.csv", "11072015emm.ewm.bm.glmglobalmodresults.csv", "11072015emm.ewm.bm.autocovglobalmodresults.csv", "11072015emm.ewm.bm.spev.negexpglobalmodresults.csv", "11072015emm.ewm.bm.mle2.negexp.freeglobalmodresults.csv", "11072015emm.ewm.bm.gamsglobalmodresults.csv", "11072015emm.ewm.bm.geeglobalmodresults.csv", "11072015emm.ewm.bm.spglmglobalmodresults.csv", "11072015emm.ewm.bm.glm.waveletglobalmodresults.csv", 
                         "11072015emm.ewm.bn.glm.nox2globalmodresults.csv", "11072015emm.ewm.bm.glm.nox2globalmodresults.csv", "11072015emm.ewm.bm.autocov.nox2globalmodresults.csv", "11072015emm.ewm.bm.spev.negexp.nox2globalmodresults.csv", "11072015emm.ewm.bm.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.ewm.bm.gams.nox2globalmodresults.csv", "11072015emm.ewm.bm.gee.nox2globalmodresults.csv", "11072015emm.ewm.bm.spglm.nox2globalmodresults.csv", "11072015emm.ewm.bm.glm.wavelet.nox2globalmodresults.csv")
      
      # Full models
      tiff(filename = paste(out.dir, "WeakMed_X1Density.tiff", sep = ""),
           width = 11, height = 8, units = "in", res = 250)
      
      par(mfrow = c(1,2))
      par(mar = c(4,5,3,2)+0.1)
      
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # Start index
      start<- 1
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, lty = "dashed", col = colors[1])
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
  
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "A")
      abline(v = 0.025, lty = 2)
      
      ## Reduced models
      new.start<- (length(files.list.use)/2)+1
      
      # Set up plot
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[new.start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[1], lty = "dashed")
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[new.start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[new.start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
      
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[new.start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[new.start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "B")
      abline(v = 0.025, lty = 2)
      dev.off()
    }
    
    # Coarse
    {
      files.list.use<- c("11072015emm.ewc.bn.glmglobalmodresults.csv", "11072015emm.ewc.bs.glmglobalmodresults.csv", "11072015emm.ewc.bs.autocovglobalmodresults.csv", "11072015emm.ewc.bs.spev.negexpglobalmodresults.csv", "11072015emm.ewc.bs.mle2.negexp.freeglobalmodresults.csv", "11072015emm.ewc.bs.gamsglobalmodresults.csv", "11072015emm.ewc.bs.geeglobalmodresults.csv", "11072015emm.ewc.bs.spglmglobalmodresults.csv", "11072015emm.ewc.bs.glm.waveletglobalmodresults.csv", 
                         "11072015emm.ewc.bn.glm.nox2globalmodresults.csv", "11072015emm.ewc.bs.glm.nox2globalmodresults.csv", "11072015emm.ewc.bs.autocov.nox2globalmodresults.csv", "11072015emm.ewc.bs.spev.negexp.nox2globalmodresults.csv", "11072015emm.ewc.bs.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.ewc.bs.gams.nox2globalmodresults.csv", "11072015emm.ewc.bs.gee.nox2globalmodresults.csv", "11072015emm.ewc.bs.spglm.nox2globalmodresults.csv", "11072015emm.ewc.bs.glm.wavelet.nox2globalmodresults.csv")
      
      # Full models
      tiff(filename = paste(out.dir, "WeakCoarse_X1Density.tiff", sep = ""),
           width = 11, height = 8, units = "in", res = 250)
      
      par(mfrow = c(1,2))
      par(mar = c(4,5,3,2)+0.1)
      
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # Start index
      start<- 1
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, lty = "dashed", col = colors[1])
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
  
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "A")
      abline(v = 0.025, lty = 2)
      
      ## Reduced models
      new.start<- (length(files.list.use)/2)+1
      
      # Set up plot
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[new.start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[1], lty = "dashed")
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[new.start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[new.start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
      
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[new.start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[new.start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "B")
      abline(v = 0.025, lty = 2)
      dev.off()
    }
  }
  
  #### Moderate
  {
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/ModSpEnv/"
    
    # What parameter?
    param<- "x1" # Look at distribution of x1
    
    # Fine
    # Order for plotting
    {
      files.list.use<- c("11072015emm.emf.bn.glmglobalmodresults.csv", "11072015emm.emf.bf.glmglobalmodresults.csv", "11072015emm.emf.bf.autocovglobalmodresults.csv", "11072015emm.emf.bf.spev.negexpglobalmodresults.csv", "11072015emm.emf.bf.mle2.negexp.freeglobalmodresults.csv", "11072015emm.emf.bf.gamsglobalmodresults.csv", "11072015emm.emf.bf.geeglobalmodresults.csv", "11072015emm.emf.bf.spglmglobalmodresults.csv", "11072015emm.emf.bf.glm.waveletglobalmodresults.csv", 
                         "11072015emm.emf.bn.glm.nox2globalmodresults.csv", "11072015emm.emf.bf.glm.nox2globalmodresults.csv", "11072015emm.emf.bf.autocov.nox2globalmodresults.csv", "11072015emm.emf.bf.spev.negexp.nox2globalmodresults.csv", "11072015emm.emf.bf.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.emf.bf.gams.nox2globalmodresults.csv", "11072015emm.emf.bf.gee.nox2globalmodresults.csv", "11072015emm.emf.bf.spglm.nox2globalmodresults.csv", "11072015emm.emf.bf.glm.wavelet.nox2globalmodresults.csv")
      
      # Full models
      tiff(filename = paste(out.dir, "ModFine_X1Density.tiff", sep = ""),
           width = 11, height = 8, units = "in", res = 250)
      
      par(mfrow = c(1,2))
      par(mar = c(4,5,3,2)+0.1)
      
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      plot(density(t1[,param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # Start index
      start<- 1
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, lty = "dashed", col = colors[1])
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
      
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "A")
      abline(v = 0.025, lty = 2)
      
      ## Reduced models
      new.start<- (length(files.list.use)/2)+1
      
      # Set up plot
      plot(density(t1[,param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[new.start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[1], lty = "dashed")
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[new.start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[new.start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
      
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[new.start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[new.start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "B")
      abline(v = 0.025, lty = 2)
      dev.off()
    }
    
    # Medium
    # Order for plotting
    {
      files.list.use<- c("11072015emm.emm.bn.glmglobalmodresults.csv", "11072015emm.emm.bm.glmglobalmodresults.csv", "11072015emm.emm.bm.autocovglobalmodresults.csv", "11072015emm.emm.bm.spev.negexpglobalmodresults.csv", "11072015emm.emm.bm.mle2.negexp.freeglobalmodresults.csv", "11072015emm.emm.bm.gamsglobalmodresults.csv", "11072015emm.emm.bm.geeglobalmodresults.csv", "11072015emm.emm.bm.spglmglobalmodresults.csv", "11072015emm.emm.bm.glm.waveletglobalmodresults.csv", 
                         "11072015emm.emm.bn.glm.nox2globalmodresults.csv", "11072015emm.emm.bm.glm.nox2globalmodresults.csv", "11072015emm.emm.bm.autocov.nox2globalmodresults.csv", "11072015emm.emm.bm.spev.negexp.nox2globalmodresults.csv", "11072015emm.emm.bm.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.emm.bm.gams.nox2globalmodresults.csv", "11072015emm.emm.bm.gee.nox2globalmodresults.csv", "11072015emm.emm.bm.spglm.nox2globalmodresults.csv", "11072015emm.emm.bm.glm.wavelet.nox2globalmodresults.csv")
      
      # Full models
      tiff(filename = paste(out.dir, "ModMed_X1Density.tiff", sep = ""),
           width = 11, height = 8, units = "in", res = 250)
      
      par(mfrow = c(1,2))
      par(mar = c(4,5,3,2)+0.1)
      
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # Start index
      start<- 1
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, lty = "dashed", col = colors[1])
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
      
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "A")
      abline(v = 0.025, lty = 2)
      
      ## Reduced models
      new.start<- (length(files.list.use)/2)+1
      
      # Set up plot
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[new.start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[1], lty = "dashed")
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[new.start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[new.start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
     
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[new.start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[new.start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "B")
      abline(v = 0.025, lty = 2)
      dev.off()
    }
    
    # Coarse
    {
      files.list.use<- c("11072015emm.emc.bn.glmglobalmodresults.csv", "11072015emm.emc.bs.glmglobalmodresults.csv", "11072015emm.emc.bs.autocovglobalmodresults.csv", "11072015emm.emc.bs.spev.negexpglobalmodresults.csv", "11072015emm.emc.bs.mle2.negexp.freeglobalmodresults.csv", "11072015emm.emc.bs.gamsglobalmodresults.csv", "11072015emm.emc.bs.geeglobalmodresults.csv", "11072015emm.emc.bs.spglmglobalmodresults.csv", "11072015emm.emc.bs.glm.waveletglobalmodresults.csv", 
                         "11072015emm.emc.bn.glm.nox2globalmodresults.csv", "11072015emm.emc.bs.glm.nox2globalmodresults.csv", "11072015emm.emc.bs.autocov.nox2globalmodresults.csv", "11072015emm.emc.bs.spev.negexp.nox2globalmodresults.csv", "11072015emm.emc.bs.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.emc.bs.gams.nox2globalmodresults.csv", "11072015emm.emc.bs.gee.nox2globalmodresults.csv", "11072015emm.emc.bs.spglm.nox2globalmodresults.csv", "11072015emm.emc.bs.glm.wavelet.nox2globalmodresults.csv")
      
      # Full models
      tiff(filename = paste(out.dir, "ModCoarse_X1Density.tiff", sep = ""),
           width = 11, height = 8, units = "in", res = 250)
      
      par(mfrow = c(1,2))
      par(mar = c(4,5,3,2)+0.1)
      
      
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # Start index
      start<- 1
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, lty = "dashed", col = colors[1])
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
  
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
    
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "A")
      abline(v = 0.025, lty = 2)
      
      ## Reduced models
      new.start<- (length(files.list.use)/2)+1
      
      # Set up plot
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[new.start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[1], lty = "dashed")
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[new.start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[new.start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
  
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[new.start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[new.start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "B")
      abline(v = 0.025, lty = 2)
      dev.off()
    }
  }
  
  #### Strong
  {
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/StrongSpEnv/"
    
    # What parameter?
    param<- "x1" # Look at distribution of x1
    
    # Fine
    # Order for plotting
    {
      files.list.use<- c("11072015emm.esf.bn.glmglobalmodresults.csv", "11072015emm.esf.bf.glmglobalmodresults.csv", "11072015emm.esf.bf.autocovglobalmodresults.csv", "11072015emm.esf.bf.spev.negexpglobalmodresults.csv", "11072015emm.esf.bf.mle2.negexp.freeglobalmodresults.csv", "11072015emm.esf.bf.gamsglobalmodresults.csv", "11072015emm.esf.bf.geeglobalmodresults.csv", "11072015emm.esf.bf.spglmglobalmodresults.csv", "11072015emm.esf.bf.glm.waveletglobalmodresults.csv",  
                         "11072015emm.esf.bn.glm.nox2globalmodresults.csv", "11072015emm.esf.bf.glm.nox2globalmodresults.csv", "11072015emm.esf.bf.autocov.nox2globalmodresults.csv", "11072015emm.esf.bf.spev.negexp.nox2globalmodresults.csv", "11072015emm.esf.bf.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.esf.bf.gams.nox2globalmodresults.csv", "11072015emm.esf.bf.gee.nox2globalmodresults.csv", "11072015emm.esf.bf.spglm.nox2globalmodresults.csv", "11072015emm.esf.bf.glm.wavelet.nox2globalmodresults.csv")
      
      # Full models
      tiff(filename = paste(out.dir, "StrongFine_X1Density.tiff", sep = ""),
           width = 11, height = 8, units = "in", res = 250)
      
      par(mfrow = c(1,2))
      par(mar = c(4,5,3,2)+0.1)
      
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # Start index
      start<- 1
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, lty = "dashed", col = colors[1])
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
    
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "A")
      abline(v = 0.025, lty = 2)
      
      ## Reduced models
      new.start<- (length(files.list.use)/2)+1
      
      # Set up plot
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[new.start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[1], lty = "dashed")
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[new.start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[new.start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
     
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[new.start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[new.start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "B")
      abline(v = 0.025, lty = 2)
      dev.off()
    }
    
    # Medium
    # Order for plotting
    {
      files.list.use<- c("11072015emm.esm.bn.glmglobalmodresults.csv", "11072015emm.esm.bm.glmglobalmodresults.csv", "11072015emm.esm.bm.autocovglobalmodresults.csv", "11072015emm.esm.bm.spev.negexpglobalmodresults.csv", "11072015emm.esm.bm.mle2.negexp.freeglobalmodresults.csv", "11072015emm.esm.bm.gamsglobalmodresults.csv", "11072015emm.esm.bm.geeglobalmodresults.csv", "11072015emm.esm.bm.spglmglobalmodresults.csv", "11072015emm.esm.bm.glm.waveletglobalmodresults.csv", 
                         "11072015emm.esm.bn.glm.nox2globalmodresults.csv", "11072015emm.esm.bm.glm.nox2globalmodresults.csv", "11072015emm.esm.bm.autocov.nox2globalmodresults.csv", "11072015emm.esm.bm.spev.negexp.nox2globalmodresults.csv", "11072015emm.esm.bm.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.esm.bm.gams.nox2globalmodresults.csv", "11072015emm.esm.bm.gee.nox2globalmodresults.csv", "11072015emm.esm.bm.spglm.nox2globalmodresults.csv", "11072015emm.esm.bm.glm.wavelet.nox2globalmodresults.csv")
      
      # Full models
      tiff(filename = paste(out.dir, "StrongMed_X1Density.tiff", sep = ""),
           width = 11, height = 8, units = "in", res = 250)
      
      par(mfrow = c(1,2))
      par(mar = c(4,5,3,2)+0.1)
      
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # Start index
      start<- 1
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, lty = "dashed", col = colors[1])
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
      
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "A")
      abline(v = 0.025, lty = 2)
      
      ## Reduced models
      new.start<- (length(files.list.use)/2)+1
      
      # Set up plot
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[new.start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[1], lty = "dashed")
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[new.start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[new.start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
  
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[new.start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)  
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[new.start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
      
      text(x = -0.01, y = 158, cex = 2.25, labels = "B")
      abline(v = 0.025, lty = 2)
      dev.off()
    }
    
    # Coarse
    {
      files.list.use<- c("11072015emm.esc.bn.glmglobalmodresults.csv", "11072015emm.esc.bs.glmglobalmodresults.csv", "11072015emm.esc.bs.autocovglobalmodresults.csv", "11072015emm.esc.bs.spev.negexpglobalmodresults.csv", "11072015emm.esc.bs.mle2.negexp.freeglobalmodresults.csv", "11072015emm.esc.bs.gamsglobalmodresults.csv", "11072015emm.esc.bs.geeglobalmodresults.csv", "11072015emm.esc.bs.spglmglobalmodresults.csv", "11072015emm.esc.bs.glm.waveletglobalmodresults.csv", 
                         "11072015emm.esc.bn.glm.nox2globalmodresults.csv", "11072015emm.esc.bs.glm.nox2globalmodresults.csv", "11072015emm.esc.bs.autocov.nox2globalmodresults.csv", "11072015emm.esc.bs.spev.negexp.nox2globalmodresults.csv", "11072015emm.esc.bs.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.esc.bs.gams.nox2globalmodresults.csv", "11072015emm.esc.bs.gee.nox2globalmodresults.csv", "11072015emm.esc.bs.spglm.nox2globalmodresults.csv", "11072015emm.esc.bs.glm.wavelet.nox2globalmodresults.csv")
      
      # Full models
      tiff(filename = paste(out.dir, "StrongCoarse_X1Density.tiff", sep = ""),
           width = 11, height = 8, units = "in", res = 250)
      
      par(mfrow = c(1,2))
      par(mar = c(4,5,3,2)+0.1)
      
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # Start index
      start<- 1
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, lty = "dashed", col = colors[1])
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
  
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)

      text(x = -0.01, y = 158, cex = 2.25, labels = "A")
      abline(v = 0.025, lty = 2)
      
      ## Reduced models
      new.start<- (length(files.list.use)/2)+1
      
      # Set up plot
      plot(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), xlim = c(-0.01, 0.06), ylim = c(0, 160), las = 1, xlab = "X1 Parameter estimate", ylab = "Density", main = "", type = "n")
      par.use <- par("usr")
      rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "#d9d9d9", border = "black")
      
      # GLM no added rSAC
      t1<- read.csv(paste(dir.use, files.list.use[new.start], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[1], lty = "dashed")
      
      # GLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+1], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), lwd = 2, col = colors[2])
      
      # GLM AC
      t1<- read.csv(paste(dir.use, files.list.use[new.start+2], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[3], lwd = 2)
      
      # SPEV
      t1<- read.csv(paste(dir.use, files.list.use[new.start+3], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[4], lwd = 2)
      
      # NB.NEGEXP.FREE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+4], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[5], lwd = 2)
 
      # GAM.S
      t1<- read.csv(paste(dir.use, files.list.use[new.start+5], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[6], lwd = 2)
      
      # GEE
      t1<- read.csv(paste(dir.use, files.list.use[new.start+6], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[7], lwd = 2)
      
      # SPGLM
      t1<- read.csv(paste(dir.use, files.list.use[new.start+7], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[8], lwd = 2)
      
      # WAVELET
      t1<- read.csv(paste(dir.use, files.list.use[new.start+8], sep = ""))
      lines(density(t1[,colnames(t1) %in% param], kernel = "gaussian", na.rm = TRUE), col = colors[9], lwd = 2)
     
      text(x = -0.01, y = 158, cex = 2.25, labels = "B")
      abline(v = 0.025, lty = 2)
      dev.off()
    }
  }
}

##### Mean bias 
{
  source("~/R/Allyn/ErrorBar.R")
  library(plyr)

  n.mods<- 9
  plotting.levels<- rev(c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE"))
  
  mean.diff.res<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(mean.diff.res)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  mean.diff.res[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  mean.ind<- 2
  
  low.res<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(low.res)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  low.res[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  low.ind<- 2
  
  high.res<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(high.res)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  high.res[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  high.ind<- 2
  
  obs.res<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(obs.res)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  obs.res[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  obs.ind<- 2
  
  weak.files<- data.frame("Fine" = c("11072015emm.ewf.bn.glmglobalmodresults.csv", "11072015emm.ewf.bf.glmglobalmodresults.csv", "11072015emm.ewf.bf.autocovglobalmodresults.csv", "11072015emm.ewf.bf.spev.negexpglobalmodresults.csv", "11072015emm.ewf.bf.mle2.negexp.freeglobalmodresults.csv", "11072015emm.ewf.bf.gamsglobalmodresults.csv", "11072015emm.ewf.bf.geeglobalmodresults.csv", "11072015emm.ewf.bf.spglmglobalmodresults.csv", "11072015emm.ewf.bf.glm.waveletglobalmodresults.csv"), "Mod" =  c("11072015emm.ewm.bn.glmglobalmodresults.csv", "11072015emm.ewm.bm.glmglobalmodresults.csv", "11072015emm.ewm.bm.autocovglobalmodresults.csv", "11072015emm.ewm.bm.spev.negexpglobalmodresults.csv", "11072015emm.ewm.bm.mle2.negexp.freeglobalmodresults.csv", "11072015emm.ewm.bm.gamsglobalmodresults.csv", "11072015emm.ewm.bm.geeglobalmodresults.csv", "11072015emm.ewm.bm.spglmglobalmodresults.csv", "11072015emm.ewm.bm.glm.waveletglobalmodresults.csv"), "Coarse" = c("11072015emm.ewc.bn.glmglobalmodresults.csv", "11072015emm.ewc.bs.glmglobalmodresults.csv", "11072015emm.ewc.bs.autocovglobalmodresults.csv", "11072015emm.ewc.bs.spev.negexpglobalmodresults.csv", "11072015emm.ewc.bs.mle2.negexp.freeglobalmodresults.csv", "11072015emm.ewc.bs.gamsglobalmodresults.csv", "11072015emm.ewc.bs.geeglobalmodresults.csv", "11072015emm.ewc.bs.spglmglobalmodresults.csv", "11072015emm.ewc.bs.glm.waveletglobalmodresults.csv"))
  mod.files<-  data.frame("Fine" = c("11072015emm.emf.bn.glmglobalmodresults.csv", "11072015emm.emf.bf.glmglobalmodresults.csv", "11072015emm.emf.bf.autocovglobalmodresults.csv", "11072015emm.emf.bf.spev.negexpglobalmodresults.csv", "11072015emm.emf.bf.mle2.negexp.freeglobalmodresults.csv", "11072015emm.emf.bf.gamsglobalmodresults.csv", "11072015emm.emf.bf.geeglobalmodresults.csv", "11072015emm.emf.bf.spglmglobalmodresults.csv", "11072015emm.emf.bf.glm.waveletglobalmodresults.csv"), "Mod" =  c("11072015emm.emm.bn.glmglobalmodresults.csv", "11072015emm.emm.bm.glmglobalmodresults.csv", "11072015emm.emm.bm.autocovglobalmodresults.csv", "11072015emm.emm.bm.spev.negexpglobalmodresults.csv", "11072015emm.emm.bm.mle2.negexp.freeglobalmodresults.csv", "11072015emm.emm.bm.gamsglobalmodresults.csv", "11072015emm.emm.bm.geeglobalmodresults.csv", "11072015emm.emm.bm.spglmglobalmodresults.csv", "11072015emm.emm.bm.glm.waveletglobalmodresults.csv"), "Coarse" = c("11072015emm.emc.bn.glmglobalmodresults.csv", "11072015emm.emc.bs.glmglobalmodresults.csv", "11072015emm.emc.bs.autocovglobalmodresults.csv", "11072015emm.emc.bs.spev.negexpglobalmodresults.csv", "11072015emm.emc.bs.mle2.negexp.freeglobalmodresults.csv", "11072015emm.emc.bs.gamsglobalmodresults.csv", "11072015emm.emc.bs.geeglobalmodresults.csv", "11072015emm.emc.bs.spglmglobalmodresults.csv", "11072015emm.emc.bs.glm.waveletglobalmodresults.csv"))
  strong.files<- data.frame("Fine" = c("11072015emm.esf.bn.glmglobalmodresults.csv", "11072015emm.esf.bf.glmglobalmodresults.csv", "11072015emm.esf.bf.autocovglobalmodresults.csv", "11072015emm.esf.bf.spev.negexpglobalmodresults.csv", "11072015emm.esf.bf.mle2.negexp.freeglobalmodresults.csv", "11072015emm.esf.bf.gamsglobalmodresults.csv", "11072015emm.esf.bf.geeglobalmodresults.csv", "11072015emm.esf.bf.spglmglobalmodresults.csv", "11072015emm.esf.bf.glm.waveletglobalmodresults.csv"), "Mod" =  c("11072015emm.esm.bn.glmglobalmodresults.csv", "11072015emm.esm.bm.glmglobalmodresults.csv", "11072015emm.esm.bm.autocovglobalmodresults.csv", "11072015emm.esm.bm.spev.negexpglobalmodresults.csv", "11072015emm.esm.bm.mle2.negexp.freeglobalmodresults.csv", "11072015emm.esm.bm.gamsglobalmodresults.csv", "11072015emm.esm.bm.geeglobalmodresults.csv", "11072015emm.esm.bm.spglmglobalmodresults.csv", "11072015emm.esm.bm.glm.waveletglobalmodresults.csv"), "Coarse" = c("11072015emm.esc.bn.glmglobalmodresults.csv", "11072015emm.esc.bs.glmglobalmodresults.csv", "11072015emm.esc.bs.autocovglobalmodresults.csv", "11072015emm.esc.bs.spev.negexpglobalmodresults.csv", "11072015emm.esc.bs.mle2.negexp.freeglobalmodresults.csv", "11072015emm.esc.bs.gamsglobalmodresults.csv", "11072015emm.esc.bs.geeglobalmodresults.csv", "11072015emm.esc.bs.spglmglobalmodresults.csv", "11072015emm.esc.bs.glm.waveletglobalmodresults.csv"))
  list.files<- list(weak.files, mod.files, strong.files)
  names(list.files)<- c("Weak", "Mod", "Strong")
  
  weak.nox2.files<- data.frame("Fine" = c("11072015emm.ewf.bn.glm.nox2globalmodresults.csv", "11072015emm.ewf.bf.glm.nox2globalmodresults.csv", "11072015emm.ewf.bf.autocov.nox2globalmodresults.csv", "11072015emm.ewf.bf.spev.negexp.nox2globalmodresults.csv", "11072015emm.ewf.bf.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.ewf.bf.gams.nox2globalmodresults.csv", "11072015emm.ewf.bf.gee.nox2globalmodresults.csv", "11072015emm.ewf.bf.spglm.nox2globalmodresults.csv", "11072015emm.ewf.bf.glm.wavelet.nox2globalmodresults.csv"), "Mod" = c("11072015emm.ewm.bn.glm.nox2globalmodresults.csv", "11072015emm.ewm.bm.glm.nox2globalmodresults.csv", "11072015emm.ewm.bm.autocov.nox2globalmodresults.csv", "11072015emm.ewm.bm.spev.negexp.nox2globalmodresults.csv", "11072015emm.ewm.bm.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.ewm.bm.gams.nox2globalmodresults.csv", "11072015emm.ewm.bm.gee.nox2globalmodresults.csv", "11072015emm.ewm.bm.spglm.nox2globalmodresults.csv", "11072015emm.ewm.bm.glm.wavelet.nox2globalmodresults.csv"), "Coarse" = c("11072015emm.ewc.bn.glm.nox2globalmodresults.csv", "11072015emm.ewc.bs.glm.nox2globalmodresults.csv", "11072015emm.ewc.bs.autocov.nox2globalmodresults.csv", "11072015emm.ewc.bs.spev.negexp.nox2globalmodresults.csv", "11072015emm.ewc.bs.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.ewc.bs.gams.nox2globalmodresults.csv", "11072015emm.ewc.bs.gee.nox2globalmodresults.csv", "11072015emm.ewc.bs.spglm.nox2globalmodresults.csv", "11072015emm.ewc.bs.glm.wavelet.nox2globalmodresults.csv"))
  mod.nox2.files<- data.frame("Fine" = c("11072015emm.emf.bn.glm.nox2globalmodresults.csv", "11072015emm.emf.bf.glm.nox2globalmodresults.csv", "11072015emm.emf.bf.autocov.nox2globalmodresults.csv", "11072015emm.emf.bf.spev.negexp.nox2globalmodresults.csv", "11072015emm.emf.bf.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.emf.bf.gams.nox2globalmodresults.csv", "11072015emm.emf.bf.gee.nox2globalmodresults.csv", "11072015emm.emf.bf.spglm.nox2globalmodresults.csv", "11072015emm.emf.bf.glm.wavelet.nox2globalmodresults.csv"), "Mod" = c("11072015emm.emm.bn.glm.nox2globalmodresults.csv", "11072015emm.emm.bm.glm.nox2globalmodresults.csv", "11072015emm.emm.bm.autocov.nox2globalmodresults.csv", "11072015emm.emm.bm.spev.negexp.nox2globalmodresults.csv", "11072015emm.emm.bm.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.emm.bm.gams.nox2globalmodresults.csv", "11072015emm.emm.bm.gee.nox2globalmodresults.csv", "11072015emm.emm.bm.spglm.nox2globalmodresults.csv", "11072015emm.emm.bm.glm.wavelet.nox2globalmodresults.csv"), "Coarse" = c("11072015emm.emc.bn.glm.nox2globalmodresults.csv", "11072015emm.emc.bs.glm.nox2globalmodresults.csv", "11072015emm.emc.bs.autocov.nox2globalmodresults.csv", "11072015emm.emc.bs.spev.negexp.nox2globalmodresults.csv", "11072015emm.emc.bs.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.emc.bs.gams.nox2globalmodresults.csv", "11072015emm.emc.bs.gee.nox2globalmodresults.csv", "11072015emm.emc.bs.spglm.nox2globalmodresults.csv", "11072015emm.emc.bs.glm.wavelet.nox2globalmodresults.csv"))
  strong.nox2.files<- data.frame("Fine" = c("11072015emm.esf.bn.glm.nox2globalmodresults.csv", "11072015emm.esf.bf.glm.nox2globalmodresults.csv", "11072015emm.esf.bf.autocov.nox2globalmodresults.csv", "11072015emm.esf.bf.spev.negexp.nox2globalmodresults.csv", "11072015emm.esf.bf.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.esf.bf.gams.nox2globalmodresults.csv", "11072015emm.esf.bf.gee.nox2globalmodresults.csv", "11072015emm.esf.bf.spglm.nox2globalmodresults.csv", "11072015emm.esf.bf.glm.wavelet.nox2globalmodresults.csv"), "Mod" = c("11072015emm.esm.bn.glm.nox2globalmodresults.csv", "11072015emm.esm.bm.glm.nox2globalmodresults.csv", "11072015emm.esm.bm.autocov.nox2globalmodresults.csv", "11072015emm.esm.bm.spev.negexp.nox2globalmodresults.csv", "11072015emm.esm.bm.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.esm.bm.gams.nox2globalmodresults.csv", "11072015emm.esm.bm.gee.nox2globalmodresults.csv", "11072015emm.esm.bm.spglm.nox2globalmodresults.csv", "11072015emm.esm.bm.glm.wavelet.nox2globalmodresults.csv"), "Coarse" = c("11072015emm.esc.bn.glm.nox2globalmodresults.csv", "11072015emm.esc.bs.glm.nox2globalmodresults.csv", "11072015emm.esc.bs.autocov.nox2globalmodresults.csv", "11072015emm.esc.bs.spev.negexp.nox2globalmodresults.csv", "11072015emm.esc.bs.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.esc.bs.gams.nox2globalmodresults.csv", "11072015emm.esc.bs.gee.nox2globalmodresults.csv", "11072015emm.esc.bs.spglm.nox2globalmodresults.csv", "11072015emm.esc.bs.glm.wavelet.nox2globalmodresults.csv"))
  list.nox2.files<- list(weak.nox2.files, mod.nox2.files, strong.nox2.files)
  names(list.nox2.files)<- c("Weak", "Mod", "Strong")
  
  ## Weak
  {
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/WeakSpEnv/"
    
    # What parameter?
    param.a<- "x1" 
    
    # True value
    true.value<- 0.025
    
    # Fine
    # Order for plotting
    files.list.use<- as.vector(list.files[["Weak"]]$Fine)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Med
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Weak"]]$Mod)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Coarse
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Weak"]]$Coarse)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    ## Plotting
    # Option 2 - Scales, then models
    mean.diff.weak<- mean.diff.res[,c(1, 2:4)]
    mean.diff.weak$Model<- factor(mean.diff.weak$Model)
    mean.diff.weak.m<- melt(mean.diff.weak, id.vars = "Model")
    mean.diff.weak.m$variable<- factor(mean.diff.weak.m$variable)
    mean.diff.weak.m$Model<- factor(mean.diff.weak.m$Model, levels = plotting.levels)
    
    low.weak<- low.res[,c(1, 2:4)]
    low.weak$Model<- factor(low.weak$Model)
    low.weak.m<- melt(low.weak, id.vars = "Model")
    low.weak.m$variable<- factor(low.weak.m$variable)
    low.weak.m$Model<- factor(low.weak.m$Model, levels = plotting.levels)
    
    high.weak<- high.res[,c(1, 2:4)]
    high.weak$Model<- factor(high.weak$Model)
    high.weak.m<- melt(high.weak, id.vars = "Model")
    high.weak.m$variable<- factor(high.weak.m$variable)
    high.weak.m$Model<- factor(high.weak.m$Model, levels = plotting.levels)
    
    limits <- aes(ymax = high.weak.m$value,
                  ymin = low.weak.m$value)
    
    out<- ggplot(mean.diff.weak.m, aes(variable, value, fill = Model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Coarse", "Med", "Fine")) +
      scale_fill_manual(name = "Model", values = rev(colors)) +
      scale_y_continuous(breaks = c(-0.05, -0.025, 0, 0.025, 0.05), limits = c(-0.05, 0.055)) +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      xlab("") + ylab("X1 Estimate Bias") +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, angle = 90, vjust = 0.5, hjust = 1)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, angle = 90, vjust = 1, hjust = 0.5)) + 
      coord_flip() +
      theme_bw() +
      guides(fill = guide_legend(reverse = TRUE))
    
    ggsave(paste(out.dir, "Weak_MeanBias.tiff", sep = ""), out)
  }
  
  ## Moderate
  {
    # Update indices
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/ModSpEnv/"
    
    # What parameter?
    param.a<- "x1" 
    
    # True value
    true.value<- 0.025
    
    # Fine
    # Order for plotting
    files.list.use<- as.vector(list.files[["Mod"]]$Fine)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Med
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Mod"]]$Mod)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Coarse
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Mod"]]$Coarse)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    ## Plotting
    mean.diff.mod<- mean.diff.res[,c(1, 5:7)]
    mean.diff.mod$Model<- factor(mean.diff.mod$Model)
    mean.diff.mod.m<- melt(mean.diff.mod, id.vars = "Model")
    mean.diff.mod.m$variable<- factor(mean.diff.mod.m$variable)
    mean.diff.mod.m$Model<- factor(mean.diff.mod.m$Model, levels = plotting.levels)
    
    low.mod<- low.res[,c(1, 5:7)]
    low.mod$Model<- factor(low.mod$Model)
    low.mod.m<- melt(low.mod, id.vars = "Model")
    low.mod.m$variable<- factor(low.mod.m$variable)
    low.mod.m$Model<- factor(low.mod.m$Model, levels = plotting.levels)
    
    high.mod<- high.res[,c(1, 5:7)]
    high.mod$Model<- factor(high.mod$Model)
    high.mod.m<- melt(high.mod, id.vars = "Model")
    high.mod.m$variable<- factor(high.mod.m$variable)
    high.mod.m$Model<- factor(high.mod.m$Model, levels = plotting.levels)
    
    limits <- aes(ymax = high.mod.m$value,
                  ymin = low.mod.m$value)
    
    out<- ggplot(mean.diff.mod.m, aes(variable, value, fill = Model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Coarse", "Med", "Fine")) +
      scale_fill_manual(name = "Model", values = rev(colors)) +
      scale_y_continuous(breaks = c(-0.05, -0.025, 0, 0.025, 0.05), limits = c(-0.05, 0.055)) +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      xlab("") + ylab("X1 Estimate Bias") +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, angle = 90, vjust = 0.5, hjust = 1)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, angle = 90, vjust = 1, hjust = 0.5)) +
      coord_flip() +
      theme_bw() +
      guides(fill = guide_legend(reverse = TRUE))
    ggsave(paste(out.dir, "Mod_MeanBias.tiff", sep = ""), out)
    
  }
  
  ## Strong
  {
    # Update indices
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/StrongSpEnv/"
    
    # What parameter?
    param.a<- "x1" 
    
    # True value
    true.value<- 0.025
    
    # Fine
    # Order for plotting
    files.list.use<- as.vector(list.files[["Strong"]]$Fine)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Med
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Strong"]]$Mod)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Coarse
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Strong"]]$Coarse)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    ## Plotting
    mean.diff.strong<- mean.diff.res[,c(1, 8:10)]
    mean.diff.strong$Model<- factor(mean.diff.strong$Model)
    mean.diff.strong.m<- melt(mean.diff.strong, id.vars = "Model")
    mean.diff.strong.m$variable<- factor(mean.diff.strong.m$variable)
    mean.diff.strong.m$Model<- factor(mean.diff.strong.m$Model, levels = plotting.levels)
    
    low.strong<- low.res[,c(1, 8:10)]
    low.strong$Model<- factor(low.strong$Model)
    low.strong.m<- melt(low.strong, id.vars = "Model")
    low.strong.m$variable<- factor(low.strong.m$variable)
    low.strong.m$Model<- factor(low.strong.m$Model, levels = plotting.levels)
    
    high.strong<- high.res[,c(1, 8:10)]
    high.strong$Model<- factor(high.strong$Model)
    high.strong.m<- melt(high.strong, id.vars = "Model")
    high.strong.m$variable<- factor(high.strong.m$variable)
    high.strong.m$Model<- factor(high.strong.m$Model, levels = plotting.levels)
    
    limits <- aes(ymax = high.strong.m$value,
                  ymin = low.strong.m$value)
    
    out<- ggplot(mean.diff.strong.m, aes(variable, value, fill = Model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Coarse", "Med", "Strong")) +
      scale_fill_manual(name = "Model", values = rev(colors)) +
      scale_y_continuous(breaks = c(-0.05, -0.025, 0, 0.025, 0.05), limits = c(-0.05, 0.05)) +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      xlab("") + ylab("X1 Estimate Bias") +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, angle = 90, vjust = 0.5, hjust = 1)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, angle = 90, vjust = 1, hjust = 0.5)) +
      coord_flip() +
      theme_bw() +
      guides(fill = guide_legend(reverse = TRUE))
    ggsave(paste(out.dir, "Strong_MeanBias.tiff", sep = ""), out)
  }
  
  #################################
  #### No X2 
  mean.diff.res<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(mean.diff.res)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  mean.diff.res[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  mean.ind<- 2
  
  low.res<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(low.res)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  low.res[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  low.ind<- 2
  
  high.res<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(high.res)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  high.res[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  high.ind<- 2
  
  obs.res<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(obs.res)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  obs.res[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  obs.ind<- 2
  
  ## Weak
  {
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/WeakSpEnv/"
    
    # What parameter?
    param.a<- "x1" 
    
    # True value
    true.value<- 0.025
    
    # Fine
    # Order for plotting
    files.list.use<- as.vector(list.nox2.files[["Weak"]]$Fine)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Med
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.nox2.files[["Weak"]]$Mod)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Coarse
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.nox2.files[["Weak"]]$Coarse)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    ## Plotting
    # Option 1 - Models, then scales
    mean.diff.weak<- mean.diff.res[,c(1, 2:4)]
    mean.diff.weak$Model<- factor(mean.diff.weak$Model)
    mean.diff.weak.m<- melt(mean.diff.weak, id.vars = "Model")
    mean.diff.weak.m$variable<- factor(mean.diff.weak.m$variable)
    
    low.weak<- low.res[,c(1, 2:4)]
    low.weak$Model<- factor(low.weak$Model)
    low.weak.m<- melt(low.weak, id.vars = "Model")
    low.weak.m$variable<- factor(low.weak.m$variable)
    
    high.weak<- high.res[,c(1, 2:4)]
    high.weak$Model<- factor(high.weak$Model)
    high.weak.m<- melt(high.weak, id.vars = "Model")
    high.weak.m$variable<- factor(high.weak.m$variable)
    
    
    limits <- aes(ymax = high.weak.m$value,
                  ymin = low.weak.m$value)
    
    # ggplot(mean.diff.weak.m, aes(Model, value, fill = variable)) +
    #   geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
    #   scale_x_discrete(limits = mean.diff.weak$Model) +
    #   scale_y_continuous(breaks = c(-0.03, -0.015, 0, 0.015, 0.03), limits = c(-0.03, 0.03)) +
    #   scale_fill_discrete(name = "rSAC Scale/Strength") +
    #   geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
    #   xlab("") + ylab("X1 Estimate Bias") +
    #   theme(axis.title.y = element_text(colour = "black", size = 16)) +
    #   theme(axis.text.x = element_text(colour = "black", size=14, angle = 90, vjust = 0.5, hjust = 1)) +
    #   theme(axis.text.y = element_text(colour = "black", size = 14, angle = 90, vjust = 1, hjust = 0.5))
    
    # Option 2 - Scales, then models
    mean.diff.weak<- mean.diff.res[,c(1, 2:4)]
    mean.diff.weak$Model<- factor(mean.diff.weak$Model)
    mean.diff.weak.m<- melt(mean.diff.weak, id.vars = "Model")
    mean.diff.weak.m$variable<- factor(mean.diff.weak.m$variable)
    mean.diff.weak.m$Model<- factor(mean.diff.weak.m$Model, levels = plotting.levels)
    
    low.weak<- low.res[,c(1, 2:4)]
    low.weak$Model<- factor(low.weak$Model)
    low.weak.m<- melt(low.weak, id.vars = "Model")
    low.weak.m$variable<- factor(low.weak.m$variable)
    low.weak.m$Model<- factor(low.weak.m$Model, levels = plotting.levels)
    
    high.weak<- high.res[,c(1, 2:4)]
    high.weak$Model<- factor(high.weak$Model)
    high.weak.m<- melt(high.weak, id.vars = "Model")
    high.weak.m$variable<- factor(high.weak.m$variable)
    high.weak.m$Model<- factor(high.weak.m$Model, levels = plotting.levels)
    
    limits <- aes(ymax = high.weak.m$value,
                  ymin = low.weak.m$value)
    
    out<- ggplot(mean.diff.weak.m, aes(variable, value, fill = Model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Coarse", "Med", "Fine")) +
      scale_fill_manual(name = "Model", values = rev(colors)) +
      scale_y_continuous(breaks = c(-0.05, -0.025, 0, 0.025, 0.05), limits = c(-0.05, 0.05)) +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      xlab("") + ylab("X1 Estimate Bias") +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, angle = 90, vjust = 0.5, hjust = 1)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, angle = 90, vjust = 1, hjust = 0.5)) + 
      coord_flip() +
      theme_bw() +
      guides(fill = guide_legend(reverse = TRUE))
    ggsave(paste(out.dir, "WeakNoX2_MeanBias.tiff", sep = ""), out)
  }
  
  ## Moderate
  {
    # Update indices
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/ModSpEnv/"
    
    # What parameter?
    param.a<- "x1" 
    
    # True value
    true.value<- 0.025
    
    # Fine
    # Order for plotting
    files.list.use<- as.vector(list.nox2.files[["Mod"]]$Fine)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Med
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.nox2.files[["Mod"]]$Mod)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Coarse
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.nox2.files[["Mod"]]$Coarse)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    ## Plotting
    mean.diff.mod<- mean.diff.res[,c(1, 5:7)]
    mean.diff.mod$Model<- factor(mean.diff.mod$Model)
    mean.diff.mod.m<- melt(mean.diff.mod, id.vars = "Model")
    mean.diff.mod.m$variable<- factor(mean.diff.mod.m$variable)
    mean.diff.mod.m$Model<- factor(mean.diff.mod.m$Model, levels = plotting.levels)
    
    low.mod<- low.res[,c(1, 5:7)]
    low.mod$Model<- factor(low.mod$Model)
    low.mod.m<- melt(low.mod, id.vars = "Model")
    low.mod.m$variable<- factor(low.mod.m$variable)
    low.mod.m$Model<- factor(low.mod.m$Model, levels = plotting.levels)
    
    high.mod<- high.res[,c(1, 5:7)]
    high.mod$Model<- factor(high.mod$Model)
    high.mod.m<- melt(high.mod, id.vars = "Model")
    high.mod.m$variable<- factor(high.mod.m$variable)
    high.mod.m$Model<- factor(high.mod.m$Model, levels = plotting.levels)
    
    limits <- aes(ymax = high.mod.m$value,
                  ymin = low.mod.m$value)
    
    out<- ggplot(mean.diff.mod.m, aes(variable, value, fill = Model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Coarse", "Med", "Fine")) +
      scale_fill_manual(name = "Model", values = rev(colors)) +
      scale_y_continuous(breaks = c(-0.05, -0.025, 0, 0.025, 0.05), limits = c(-0.05, 0.05)) +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      xlab("") + ylab("X1 Estimate Bias") +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, angle = 90, vjust = 0.5, hjust = 1)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, angle = 90, vjust = 1, hjust = 0.5)) + 
      coord_flip() +
      theme_bw() +
      guides(fill = guide_legend(reverse = TRUE))
    ggsave(paste(out.dir, "ModNoX2_MeanBias.tiff", sep = ""), out)
  }
  
  ## Strong
  {
    # Update indices
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    dir.use<-  "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/StrongSpEnv/"
    
    # What parameter?
    param.a<- "x1" 
    
    # True value
    true.value<- 0.025
    
    # Fine
    # Order for plotting
    files.list.use<- as.vector(list.nox2.files[["Strong"]]$Fine)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Med
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.nox2.files[["Strong"]]$Mod)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Coarse
    mean.ind<- mean.ind+1
    low.ind<- low.ind+1
    high.ind<- high.ind+1
    obs.ind<- obs.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.nox2.files[["Strong"]]$Coarse)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- t1[,x1.index] - true.value
      mean.diff.res[i, mean.ind]<- mean(x1.diff, na.rm = TRUE)
      low.res[i, low.ind]<- quantile(x1.diff, probs = 0.025, na.rm = TRUE)
      high.res[i, high.ind]<- quantile(x1.diff, probs = 0.975, na.rm = TRUE)
      obs.res[i, obs.ind]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    ## Plotting
    mean.diff.strong<- mean.diff.res[,c(1, 8:10)]
    mean.diff.strong$Model<- factor(mean.diff.strong$Model)
    mean.diff.strong.m<- melt(mean.diff.strong, id.vars = "Model")
    mean.diff.strong.m$variable<- factor(mean.diff.strong.m$variable)
    mean.diff.strong.m$Model<- factor(mean.diff.strong.m$Model, levels = plotting.levels)
    
    low.strong<- low.res[,c(1, 8:10)]
    low.strong$Model<- factor(low.strong$Model)
    low.strong.m<- melt(low.strong, id.vars = "Model")
    low.strong.m$variable<- factor(low.strong.m$variable)
    low.strong.m$Model<- factor(low.strong.m$Model, levels = plotting.levels)
    
    high.strong<- high.res[,c(1, 8:10)]
    high.strong$Model<- factor(high.strong$Model)
    high.strong.m<- melt(high.strong, id.vars = "Model")
    high.strong.m$variable<- factor(high.strong.m$variable)
    high.strong.m$Model<- factor(high.strong.m$Model, levels = plotting.levels)
    
    limits <- aes(ymax = high.strong.m$value,
                  ymin = low.strong.m$value)
    
    out<- ggplot(mean.diff.strong.m, aes(variable, value, fill = Model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Coarse", "Med", "Fine")) +
      scale_fill_manual(name = "Model", values = rev(colors)) +
      scale_y_continuous(breaks = c(-0.05, -0.025, 0, 0.025, 0.05), limits = c(-0.05, 0.05)) +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      xlab("") + ylab("X1 Estimate Bias") +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, angle = 90, vjust = 0.5, hjust = 1)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, angle = 90, vjust = 1, hjust = 0.5)) +
      coord_flip() +
      theme_bw() +
      guides(fill = guide_legend(reverse = TRUE))
    ggsave(paste(out.dir, "StrongNoX2_MeanBias.tiff", sep = ""), out)
  }
  
}

##### Hypothesis Testing Percent of Type I and Type II errors
{
  type2.err<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(type2.err)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  type2.err[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  type2.ind<- 2
  
  type1.err<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(type1.err)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  type1.err[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  type1.ind<- 2
  
  
  ## Weak
  {
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/WeakSpEnv/"
    
    # What parameter?
    param.a<- "x1.p" # Type II errors
    param.b<- "random.p" # Type I errors
    
    # Fine
    # Order for plotting
    files.list.use<- as.vector(list.files[["Weak"]]$Fine)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      type2.err[i, type2.ind]<- (sum((t1[,x1.index]/2) > 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x1.index]))))*100
      x.random.index<- match(param.b, colnames(t1))
      type1.err[i, type1.ind]<- (sum((t1[,x.random.index]/2) < 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x.random.index]))))*100
      print(i)
    }
    
    # Med
    type2.ind<- type2.ind+1
    type1.ind<- type1.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Weak"]]$Mod)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      type2.err[i, type2.ind]<- (sum((t1[,x1.index]/2) > 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x1.index]))))*100
      x.random.index<- match(param.b, colnames(t1))
      type1.err[i, type1.ind]<- (sum((t1[,x.random.index]/2) < 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x.random.index]))))*100
      print(i)
    }
    
    # Coarse
    type2.ind<- type2.ind+1
    type1.ind<- type1.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Weak"]]$Coarse)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      type2.err[i, type2.ind]<- (sum((t1[,x1.index]/2) > 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x1.index]))))*100
      x.random.index<- match(param.b, colnames(t1))
      type1.err[i, type1.ind]<- (sum((t1[,x.random.index]/2) < 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x.random.index]))))*100
      print(i)
    }
    
    type2.ind<- type2.ind+1
    type1.ind<- type1.ind+1
  }
  
  ## Mod
  {
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/ModSpEnv/"
    
    # What parameter?
    param.a<- "x1.p" # Type II errors
    param.b<- "random.p" # Type I errors
    
    # Fine
    # Order for plotting
    files.list.use<- as.vector(list.files[["Mod"]]$Fine)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      type2.err[i, type2.ind]<- (sum((t1[,x1.index]/2) > 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x1.index]))))*100
      x.random.index<- match(param.b, colnames(t1))
      type1.err[i, type1.ind]<- (sum((t1[,x.random.index]/2) < 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x.random.index]))))*100
      print(i)
    }
    
    # Med
    type2.ind<- type2.ind+1
    type1.ind<- type1.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Mod"]]$Mod)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      type2.err[i, type2.ind]<- (sum((t1[,x1.index]/2) > 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x1.index]))))*100
      x.random.index<- match(param.b, colnames(t1))
      type1.err[i, type1.ind]<- (sum((t1[,x.random.index]/2) < 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x.random.index]))))*100
      print(i)
    }
    
    # Coarse
    type2.ind<- type2.ind+1
    type1.ind<- type1.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Mod"]]$Coarse)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      type2.err[i, type2.ind]<- (sum((t1[,x1.index]/2) > 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x1.index]))))*100
      x.random.index<- match(param.b, colnames(t1))
      type1.err[i, type1.ind]<- (sum((t1[,x.random.index]/2) < 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x.random.index]))))*100
      print(i)
    }
    
    type2.ind<- type2.ind+1
    type1.ind<- type1.ind+1
  }
  
  ## Strong
  {
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/ParamHypoPrediction/StrongSpEnv/"
    
    # What parameter?
    param.a<- "x1.p" # Type II errors
    param.b<- "random.p" # Type I errors
    
    # Fine
    # Order for plotting
    files.list.use<- as.vector(list.files[["Strong"]]$Fine)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      type2.err[i, type2.ind]<- (sum((t1[,x1.index]/2) > 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x1.index]))))*100
      x.random.index<- match(param.b, colnames(t1))
      type1.err[i, type1.ind]<- (sum((t1[,x.random.index]/2) < 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x.random.index]))))*100
      print(i)
    }
    
    # Med
    type2.ind<- type2.ind+1
    type1.ind<- type1.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Strong"]]$Mod)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      type2.err[i, type2.ind]<- (sum((t1[,x1.index]/2) > 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x1.index]))))*100
      x.random.index<- match(param.b, colnames(t1))
      type1.err[i, type1.ind]<- (sum((t1[,x.random.index]/2) < 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x.random.index]))))*100
      print(i)
    }
    
    # Coarse
    type2.ind<- type2.ind+1
    type1.ind<- type1.ind+1
    
    # Order for plotting
    files.list.use<- as.vector(list.files[["Strong"]]$Coarse)
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      type2.err[i, type2.ind]<- (sum((t1[,x1.index]/2) > 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x1.index]))))*100
      x.random.index<- match(param.b, colnames(t1))
      type1.err[i, type1.ind]<- (sum((t1[,x.random.index]/2) < 0.05, na.rm = TRUE)/(1000-sum(is.na(t1[,x.random.index]))))*100
      print(i)
    }
  }
  
  ## Plotting
  {
    # Weak
    {
      type1.err.plot<- type1.err[,c(1, 2:4)]
      type1.err.plot$Model<- factor(type1.err.plot$Model)
      type1.err.plot.m<- melt(type1.err.plot, id.vars = "Model")
      type1.err.plot.m$variable<- factor(type1.err.plot.m$variable)
      type1.err.plot.m$Model<- factor(type1.err.plot.m$Model, levels = type1.err.plot$Model)
      
      out<- ggplot(type1.err.plot.m, aes(variable, value, fill = Model)) +
        geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
        scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
        scale_fill_manual(name = "Model", values = colors) +
        scale_y_continuous(limits = c(0, 15)) +
        xlab("") + ylab("Type I Error") +
        theme(axis.title.y = element_text(colour = "black", size = 16)) +
        theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
        theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5)) +
        theme_bw()
      ggsave(paste(out.dir, "Weak_TypeIErr.tiff", sep = ""), out)
      
      
      type2.err.plot<- type2.err[,c(1, 2:4)]
      type2.err.plot$Model<- factor(type2.err.plot$Model)
      type2.err.plot.m<- melt(type2.err.plot, id.vars = "Model")
      type2.err.plot.m$variable<- factor(type2.err.plot.m$variable)
      type2.err.plot.m$Model<- factor(type2.err.plot.m$Model, levels = type2.err.plot$Model)
      
      
      out<- ggplot(type2.err.plot.m, aes(variable, value, fill = Model)) +
        geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
        scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
        scale_fill_manual(name = "Model", values = colors) +
        scale_y_continuous(limits = c(0, 45)) +
        xlab("") + ylab("Type II Error") +
        theme(axis.title.y = element_text(colour = "black", size = 16)) +
        theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
        theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5)) +
        theme_bw()
      ggsave(paste(out.dir, "Weak_TypeIIErr.tiff", sep = ""), out)
    }
    
    # Mod
    {
      type1.err.plot<- type1.err[,c(1, 5:7)]
      type1.err.plot[6,2:4]<- NA
      type1.err.plot$Model<- factor(type1.err.plot$Model)
      type1.err.plot.m<- melt(type1.err.plot, id.vars = "Model")
      type1.err.plot.m$variable<- factor(type1.err.plot.m$variable)
      type1.err.plot.m$Model<- factor(type1.err.plot.m$Model, levels = type1.err.plot$Model)
      
      out<- ggplot(type1.err.plot.m, aes(variable, value, fill = Model)) +
        geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
        scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
        scale_fill_manual(name = "Model", values = colors) +
        scale_y_continuous(limits = c(0, 15)) +
        xlab("") + ylab("Type I Error") +
        theme(axis.title.y = element_text(colour = "black", size = 16)) +
        theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
        theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5)) +
        theme_bw()
      ggsave(paste(out.dir, "Mod_TypeIErr.tiff", sep = ""), out)
      
      type2.err.plot<- type2.err[,c(1, 5:7)]
      type2.err.plot[6,2:4]<- NA
      type2.err.plot$Model<- factor(type2.err.plot$Model)
      type2.err.plot.m<- melt(type2.err.plot, id.vars = "Model")
      type2.err.plot.m$variable<- factor(type2.err.plot.m$variable)
      type2.err.plot.m$Model<- factor(type2.err.plot.m$Model, levels = type2.err.plot$Model)
      
      out<- ggplot(type2.err.plot.m, aes(variable, value, fill = Model)) +
        geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
        scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
        scale_fill_manual(name = "Model", values = colors) +
        scale_y_continuous(limits = c(0, 45)) +
        xlab("") + ylab("Type II Error") +
        theme(axis.title.y = element_text(colour = "black", size = 16)) +
        theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
        theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5)) +
        theme_bw()
      ggsave(paste(out.dir, "Mod_TypeIIErr.tiff", sep = ""), out)
    }
    
    # Strong
    {
      type1.err.plot<- type1.err[,c(1, 8:10)]
      type1.err.plot[6,2:4]<- NA
      type1.err.plot$Model<- factor(type1.err.plot$Model)
      type1.err.plot.m<- melt(type1.err.plot, id.vars = "Model")
      type1.err.plot.m$variable<- factor(type1.err.plot.m$variable)
      type1.err.plot.m$Model<- factor(type1.err.plot.m$Model, levels = type1.err.plot$Model)
      
      out<- ggplot(type1.err.plot.m, aes(variable, value, fill = Model)) +
        geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
        scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
        scale_fill_manual(name = "Model", values = colors) +
        scale_y_continuous(limits = c(0, 15)) +
        xlab("") + ylab("Type I Error") +
        theme(axis.title.y = element_text(colour = "black", size = 16)) +
        theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
        theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5)) +
        theme_bw()
      ggsave(paste(out.dir, "Strong_TypeIErr.tiff", sep = ""), out)
      
      type2.err.plot<- type2.err[,c(1, 8:10)]
      type2.err.plot[6,2:4]<- NA
      type2.err.plot$Model<- factor(type2.err.plot$Model)
      type2.err.plot.m<- melt(type2.err.plot, id.vars = "Model")
      type2.err.plot.m$variable<- factor(type2.err.plot.m$variable)
      type2.err.plot.m$Model<- factor(type2.err.plot.m$Model, levels = type2.err.plot$Model)
      
      out<- ggplot(type2.err.plot.m, aes(variable, value, fill = Model)) +
        geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
        scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
        scale_fill_manual(name = "Model", values = colors) +
        scale_y_continuous(limits = c(0, 45)) +
        xlab("") + ylab("Type II Error") +
        theme(axis.title.y = element_text(colour = "black", size = 16)) +
        theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
        theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5)) +
        theme_bw()
      ggsave(paste(out.dir, "Strong_TypeIIErr.tiff", sep = ""), out)
    }
  }
}

##### Model Selection - How many times did we select the best model?
library(plyr)
n.mods<- 9

{
  x1x2best<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(x1x2best)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  x1x2best[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  x1x2best.ind<- 2
  
  xrandbest<- data.frame(matrix(nrow = n.mods, ncol = 10))
  names(xrandbest)<- c("Model", rep(c("Fine", "Med", "Coarse"), 3))
  xrandbest[,1]<- c("GLM.NO.rSAC", "GLM", "GLM+AC", "GLM.SPEV", "GLM+NBI.AC.FREE", "GAM", "GEE", "SPGLM", "WAVE")
  xrandbest.ind<- 2
  
  result.ind<- 1
  
  ## Weak
  {
    dir.use<- "~/Desktop/RSA_MergedFiles_Winter2018/Fits/Weak/"
    
    # Fine
    # Order for plotting
    files.list.use<- c("11072015emm.ewf.bn.glmmodstats.csv", "11072015emm.ewf.bf.glmmodstats.csv", "11072015emm.ewf.bf.autocovmodstats.csv", "11072015emm.ewf.bf.spev.negexpmodstats.csv", 
                       
                       # NBI Custom Free
                       "11072015emm.ewf.bf.mle2.negexp.freeglobalmodresults.csv", "11072015emm.ewf.bf.mle2.negexp.free.x1x2globalmodresults.csv", "11072015emm.ewf.bf.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.ewf.bf.mle2.negexp.free.nox1globalmodresults.csv", "11072015emm.ewf.bf.mle2.negexp.free.x1xnbiglobalmodresults.csv", "11072015emm.ewf.bf.mle2.negexp.free.x2xnbiglobalmodresults.csv", "11072015emm.ewf.bf.mle2.negexp.free.xrandxnbiglobalmodresults.csv", "11072015emm.ewf.bf.mle2.negexp.free.xnbiglobalmodresults.csv", "11072015emm.ewf.bf.glmmodstats.csv",
                       # GAMS 
                       "11072015emm.ewf.bf.gamsglobalmodresults.csv", "11072015emm.ewf.bf.gams.nox2globalmodresults.csv", "11072015emm.ewf.bf.gams.nox1globalmodresults.csv", "11072015emm.ewf.bf.gams.x1smoothglobalmodresults.csv", "11072015emm.ewf.bf.gams.x1x2smoothglobalmodresults.csv", "11072015emm.ewf.bf.gams.x2smoothglobalmodresults.csv", "11072015emm.ewf.bf.gams.x2xrandglobalmodresults.csv", "11072015emm.ewf.bf.gams.x1x2globalmodresults.csv", "11072015emm.ewf.bf.gams.x1xrandglobalmodresults.csv", "11072015emm.ewf.bf.gams.xrandsmoothglobalmodresults.csv", "11072015emm.ewf.bf.gams.smoothglobalmodresults.csv", "11072015emm.ewf.bf.gams.x1globalmodresults.csv", "11072015emm.ewf.bf.gams.x2globalmodresults.csv", "11072015emm.ewf.bf.gams.xrandglobalmodresults.csv",
                       
                       # GEE
                       "11072015emm.ewf.bf.geeglobalmodresults.csv", "11072015emm.ewf.bf.gee.nox2globalmodresults.csv", "11072015emm.ewf.bf.gee.x1globalmodresults.csv", "11072015emm.ewf.bf.gee.x1x2globalmodresults.csv", "11072015emm.ewf.bf.gee.x2globalmodresults.csv", "11072015emm.ewf.bf.gee.x2xrandglobalmodresults.csv", "11072015emm.ewf.bf.gee.xrandglobalmodresults.csv",
                       
                       # spGLM
                       "11072015emm.ewf.bf.spglmglobalmodresults.csv", "11072015emm.ewf.bf.spglm.nox2globalmodresults.csv", "11072015emm.ewf.bf.spglm.x1globalmodresults.csv", "11072015emm.ewf.bf.spglm.noxrandglobalmodresults.csv", "11072015emm.ewf.bf.spglm.x2globalmodresults.csv", "11072015emm.ewf.bf.spglm.nox1globalmodresults.csv", "11072015emm.ewf.bf.spglm.xrandglobalmodresults.csv",
                       
                       # WAVE - no model selection option
                       "no.file")
    
    for(i in 1:length(files.list.use)) {
      
      # GLM no RSAC
      if(i == 1) {
        print(i)
        t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # GLM, GLM+AC, GLM+SPEV
      if(i >= 2 && i < 5) {
        print(i)
        t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ]) 
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2+", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # NEG.EXP.FREE
      if(i == 5) {
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+xnbi+x.random"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2+xnbi"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+xnbi+x.random"
        
        nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        nox1$boot<- seq(from = 1, to = 1000)
        nox1$model<- "x2+xnbi+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x1$boot<- seq(from = 1, to = 1000)
        x1$model<- "x1+xnbi"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2+xnbi"
        
        xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xnbixrand$boot<- seq(from = 1, to = 1000)
        xnbixrand$model<- "xnbi+x.random"
        
        xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
        xnbi$boot<- seq(from = 1, to = 1000)
        xnbi$model<- "xnbi"
        
        glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
        glm.model$model<- as.character(glm.model$model)
        glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
        
        t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
        
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # Update
      if(i > 5 && i < 14) {
        print(i)
        next
      }
      
      # GAM
      if(i == 14){
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+smooth+x.random"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+smooth+x.random"
        
        x2xrand<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        x2xrand$boot<- seq(from = 1, to = 1000)
        x2xrand$model<- "x2+smooth+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        x1$boot<- seq(from = 1, to = 1000)
        x1$model<- "x1+smooth"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2+smooth"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2+smooth"
        
        x2xrandnos<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        x2xrandnos$boot<- seq(from = 1, to = 1000)
        x2xrandnos$model<- "x2+x.random"
        
        x1x2nos<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
        x1x2nos$boot<- seq(from = 1, to = 1000)
        x1x2nos$model<- "x1+x2"
        
        x1xrandnos<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
        x1xrandnos$boot<- seq(from = 1, to = 1000)
        x1xrandnos$model<- "x1+x.random"
        
        xrand<- read.csv(paste(dir.use, files.list.use[i+9], sep = ""))
        xrand$boot<- seq(from = 1, to = 1000)
        xrand$model<- "smooth+x.random"
        
        xsmooth<- read.csv(paste(dir.use, files.list.use[i+10], sep = ""))
        xsmooth$boot<- seq(from = 1, to = 1000)
        xsmooth$model<- "smooth"
        
        x1nos<- read.csv(paste(dir.use, files.list.use[i+11], sep = ""))
        x1nos$boot<- seq(from = 1, to = 1000)
        x1nos$model<- "x1"
        
        x2nos<- read.csv(paste(dir.use, files.list.use[i+12], sep = ""))
        x2nos$boot<- seq(from = 1, to = 1000)
        x2nos$model<- "x2"
        
        xrandnos<- read.csv(paste(dir.use, files.list.use[i+13], sep = ""))
        xrandnos$boot<- seq(from = 1, to = 1000)
        xrandnos$model<- "x.random"
        
        t1.working<- data.frame(model = c(global$model, nox2$model, x2xrand$model, x1$model, x1x2$model, x2$model, x2xrandnos$model, x1x2nos$model, x1xrandnos$model, xrand$model, xsmooth$model, x1nos$model, x2nos$model, xrandnos$model), 
                                AICc = c(global$AICc, nox2$AICc, x2xrand$AICc, x1$AICc, x1x2$AICc, x2$AICc, x2xrandnos$AICc, x1x2nos$AICc, x1xrandnos$AICc, xrand$AICc, xsmooth$AICc, x1nos$AICc, x2nos$AICc, xrandnos$AICc), 
                                boot = c(global$boot, nox2$boot, x2xrand$boot, x1$boot, x1x2$boot, x2$boot, x2xrandnos$boot, x1x2nos$boot, x1xrandnos$boot, xrand$boot, xsmooth$boot, x1nos$boot, x2nos$boot, xrandnos$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # Update
      if(i > 14 && i < 28) {
        print(i)
        next
      }
      
      # GEE
      if(i == 28) {
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+x.random"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        x1$boot<- seq(from = 1, to = 1000)
        x1$model<- "x1"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2"
        
        x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2xrand$boot<- seq(from = 1, to = 1000)
        x2xrand$model<- "x2+x.random"
        
        xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xrand$boot<- seq(from = 1, to = 1000)
        xrand$model<- "x.random"
        
        t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), QICu = c(global$QICu, nox2$QICu, x1$QICu, x1x2$QICu, x2$QICu, x2xrand$QICu, xrand$QICu), boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(QICu, ties.method = "first"))
        
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$QICu != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # Update
      if(i > 28 && i < 35) {
        print(i)
        next
      }
      
      # spGLM
      if(i == 35) {
        data.sets<- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
        data.sets.num<- seq(from = 1, to = 20, by = 1)
        dat.check<- rep(paste(rep(data.sets, each = 20), data.sets.num, sep = "."), each = 5)
        dat.check<- paste(dat.check[order(dat.check)], seq(from = 1, to = 1000), sep = "_")
        
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global<- global[order(global$datasets.group),]
        global$boot<- seq(from = 1, to = nrow(global))
        global$check<- paste(global$datasets.group, global$boot, sep = "_")
        global<- global[global$check == dat.check,]
        global$model<- "x1+x2+x.random"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        nox2<- nox2[order(nox2$datasets.group),]
        nox2$boot<- seq(from = 1, to = nrow(nox2))
        nox2$check<- paste(nox2$datasets.group, nox2$boot, sep = "_")
        nox2<- nox2[nox2$check == dat.check,]
        nox2$model<- "x1+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        x1<- x1[order(x1$datasets.group),]
        x1$boot<- seq(from = 1, to = nrow(x1))
        x1$check<- paste(x1$datasets.group, x1$boot, sep = "_")
        x1<- x1[x1$check == dat.check,]
        x1$model<- "x1"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        x1x2<- x1x2[order(x1x2$datasets.group),]
        x1x2$boot<- seq(from = 1, to = nrow(x1x2))
        x1x2$check<- paste(x1x2$datasets.group, x1x2$boot, sep = "_")
        x1x2<- x1x2[x1x2$check == dat.check,]
        x1x2$model<- "x1+x2"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x2<- x2[order(x2$datasets.group),]
        x2$boot<- seq(from = 1, to = nrow(x2))
        x2$check<- paste(x2$datasets.group, x2$boot, sep = "_")
        x2<- x2[x2$check == dat.check,]
        x2$model<- "x2"
        
        x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2xrand<- x2xrand[order(x2xrand$datasets.group),]
        x2xrand$boot<- seq(from = 1, to = nrow(x2xrand))
        x2xrand$check<- paste(x2xrand$datasets.group, x2xrand$boot, sep = "_")
        x2xrand<- x2xrand[x2xrand$check == dat.check,]
        x2xrand$model<- "x2+x.random"
        
        xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xrand<- xrand[order(xrand$datasets.group),]
        xrand$boot<- seq(from = 1, to = nrow(xrand))
        xrand$check<- paste(xrand$datasets.group, xrand$boot, sep = "_")
        xrand<- xrand[xrand$check == dat.check,]
        xrand$model<- "x.random"
        
        t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), 
                                DIC = c(global$DIC, nox2$DIC, x1$DIC, x1x2$DIC, x2$DIC, x2xrand$DIC, xrand$DIC), 
                                boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(DIC, ties.method = "first"))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$DIC != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      if(i > 38 && i < 46) {
        print(i)
        next
      }
      
      if(i == 46) {
        print(i)
        x1x2best[result.ind, x1x2best.ind]<- NA
        xrandbest[result.ind, xrandbest.ind]<- NA
        
        result.ind<- result.ind+1
      }
    }
    
    # Med
    result.ind<- 1
    x1x2best.ind<- 3
    xrandbest.ind<- 3
    
    # Update file list
    temp<- gsub("emm.ewf.bf", "emm.ewm.bm", files.list.use)
    files.list.use<- gsub("emm.ewf.bn", "emm.ewm.bn", temp)
    
    for(i in 1:length(files.list.use)) {
      
      # GLM no RSAC
      if(i == 1 || i == 2) {
        print(i)
        t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # GLM, GLM+AC, GLM+SPEV
      if(i > 2 && i < 5) {
        print(i)
        t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ]) 
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2+", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # NEG.EXP.FREE
      if(i == 5) {
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+xnbi+x.random"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2+xnbi"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+xnbi+x.random"
        
        nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        nox1$boot<- seq(from = 1, to = 1000)
        nox1$model<- "x2+xnbi+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x1$boot<- seq(from = 1, to = 100)
        x1$model<- "x1+xnbi"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2+xnbi"
        
        xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xnbixrand$boot<- seq(from = 1, to = 1000)
        xnbixrand$model<- "xnbi+x.random"
        
        xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
        xnbi$boot<- seq(from = 1, to = 100)
        xnbi$model<- "xnbi"
        
        glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
        glm.model$model<- as.character(glm.model$model)
        glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
        
        t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
        
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # Update
      if(i > 5 && i < 14) {
        print(i)
        next
      }
      
      # NEG.EXP.FIX
      if(i == 14){
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+x.nbi+x.random"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2+xnbi"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+xnbi+x.random"
        
        nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        nox1$boot<- seq(from = 1, to = 1000)
        nox1$model<- "x2+xnbi+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x1$boot<- seq(from = 1, to = 1000)
        x1$model<- "x1+xnbi"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2+xnbi"
        
        xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xnbixrand$boot<- seq(from = 1, to = 1000)
        xnbixrand$model<- "xnbi+x.random"
        
        xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
        #xnbi$boot<- seq(from = 1, to = 1000)
        xnbi$boot<- seq(from = 1, to = 1000)
        xnbi$model<- "xnbi"
        
        glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
        glm.model$model<- as.character(glm.model$model)
        glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
        
        t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
        
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # Update
      if(i > 14 && i < 23) {
        print(i)
        next
      }
      
      # GAM
      if(i == 23){
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+smooth+x.random"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+smooth+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        x1$boot<- seq(from = 1, to = 1000)
        x1$model<- "x1+smooth"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2+smooth"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2+smooth"
        
        x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2xrand$boot<- seq(from = 1, to = 1000)
        x2xrand$model<- "x2+smooth+x.random"
        
        xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xrand$boot<- seq(from = 1, to = 1000)
        xrand$model<- "smooth+x.random"
        
        xsmooth<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
        xsmooth$boot<- seq(from = 1, to = 1000)
        xsmooth$model<- "smooth"
        
        glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
        glm.model$model<- as.character(glm.model$model)
        glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
        
        t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model, xsmooth$model, glm.model.df$model), 
                                AICc = c(global$AICc, nox2$AICc, x1$AICc, x1x2$AICc, x2$AICc, x2xrand$AICc, xrand$AICc, xsmooth$AICc, glm.model.df$AICc), 
                                boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot, xsmooth$boot, glm.model.df$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # Update
      if(i > 23 && i < 32) {
        print(i)
        next
      }
      
      # GEE
      if(i == 32) {
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+x.random"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        x1$boot<- seq(from = 1, to = 1000)
        x1$model<- "x1"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2"
        
        x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2xrand$boot<- seq(from = 1, to = 1000)
        x2xrand$model<- "x2+x.random"
        
        xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xrand$boot<- seq(from = 1, to = 1000)
        xrand$model<- "x.random"
        
        t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), QICu = c(global$QICu, nox2$QICu, x1$QICu, x1x2$QICu, x2$QICu, x2xrand$QICu, xrand$QICu), boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(QICu, ties.method = "first"))
        
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$QICu != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # Update
      if(i > 31 && i < 39) {
        print(i)
        next
      }
      
      # spGLM
      if(i == 39) {
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+x.random"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        x1$boot<- seq(from = 1, to = 1000)
        x1$model<- "x1"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2"
        
        x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2xrand$boot<- seq(from = 1, to = 1000)
        x2xrand$model<- "x2+x.random"
        
        xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xrand$boot<- seq(from = 1, to = 1000)
        xrand$model<- "x.random"
        
        t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), 
                                DIC = c(global$DIC, nox2$DIC, x1$DIC, x1x2$DIC, x2$DIC, x2xrand$DIC, xrand$DIC), 
                                boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(DIC, ties.method = "first"))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$DIC != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      if(i > 38 && i < 46) {
        print(i)
        next
      }
      
      if(i == 46) {
        print(i)
        x1x2best[result.ind, x1x2best.ind]<- NA
        xrandbest[result.ind, xrandbest.ind]<- NA
        
        result.ind<- result.ind+1
      }
    }
    
    # Coarse
    result.ind<- 1
    x1x2best.ind<- 4
    xrandbest.ind<- 4 
    
    # Update file list
    temp<- gsub("emm.ewm.bm", "emm.ewc.bs", files.list.use)
    files.list.use<- gsub("emm.ewm.bn", "emm.ewc.bn", temp)
    
    for(i in 1:length(files.list.use)) {
      
      # GLM no RSAC
      if(i == 1 || i == 2) {
        print(i)
        t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # GLM, GLM+AC, GLM+SPEV
      if(i > 2 && i < 5) {
        print(i)
        t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ]) 
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2+", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # NEG.EXP.FREE
      if(i == 5) {
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+xnbi+x.random"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2+xnbi"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+xnbi+x.random"
        
        nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        nox1$boot<- seq(from = 1, to = 1000)
        nox1$model<- "x2+xnbi+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x1$boot<- seq(from = 1, to = 100)
        x1$model<- "x1+xnbi"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2+xnbi"
        
        xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xnbixrand$boot<- seq(from = 1, to = 1000)
        xnbixrand$model<- "xnbi+x.random"
        
        xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
        xnbi$boot<- seq(from = 1, to = 100)
        xnbi$model<- "xnbi"
        
        glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
        glm.model$model<- as.character(glm.model$model)
        glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
        
        t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
        
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # Update
      if(i > 5 && i < 14) {
        print(i)
        next
      }
      
      # NEG.EXP.FIX
      if(i == 14){
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+x.nbi+x.random"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2+xnbi"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+xnbi+x.random"
        
        nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        nox1$boot<- seq(from = 1, to = 1000)
        nox1$model<- "x2+xnbi+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x1$boot<- seq(from = 1, to = 1000)
        x1$model<- "x1+xnbi"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2+xnbi"
        
        xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xnbixrand$boot<- seq(from = 1, to = 1000)
        xnbixrand$model<- "xnbi+x.random"
        
        xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
        #xnbi$boot<- seq(from = 1, to = 1000)
        xnbi$boot<- seq(from = 1, to = 1000)
        xnbi$model<- "xnbi"
        
        glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
        glm.model$model<- as.character(glm.model$model)
        glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
        
        t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
        
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # Update
      if(i > 14 && i < 23) {
        print(i)
        next
      }
      
      # GAM
      if(i == 23){
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+smooth+x.random"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+smooth+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        x1$boot<- seq(from = 1, to = 1000)
        x1$model<- "x1+smooth"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2+smooth"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2+smooth"
        
        x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2xrand$boot<- seq(from = 1, to = 1000)
        x2xrand$model<- "x2+smooth+x.random"
        
        xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xrand$boot<- seq(from = 1, to = 1000)
        xrand$model<- "smooth+x.random"
        
        xsmooth<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
        xsmooth$boot<- seq(from = 1, to = 1000)
        xsmooth$model<- "smooth"
        
        glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
        glm.model$model<- as.character(glm.model$model)
        glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
        
        t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model, xsmooth$model, glm.model.df$model), 
                                AICc = c(global$AICc, nox2$AICc, x1$AICc, x1x2$AICc, x2$AICc, x2xrand$AICc, xrand$AICc, xsmooth$AICc, glm.model.df$AICc), 
                                boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot, xsmooth$boot, glm.model.df$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$AICc != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # Update
      if(i > 23 && i < 32) {
        print(i)
        next
      }
      
      # GEE
      if(i == 32) {
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+x.random"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        x1$boot<- seq(from = 1, to = 1000)
        x1$model<- "x1"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2"
        
        x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2xrand$boot<- seq(from = 1, to = 1000)
        x2xrand$model<- "x2+x.random"
        
        xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xrand$boot<- seq(from = 1, to = 1000)
        xrand$model<- "x.random"
        
        t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), QICu = c(global$QICu, nox2$QICu, x1$QICu, x1x2$QICu, x2$QICu, x2xrand$QICu, xrand$QICu), boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(QICu, ties.method = "first"))
        
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$QICu != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      # Update
      if(i > 31 && i < 39) {
        print(i)
        next
      }
      
      # spGLM
      if(i == 39) {
        print(i)
        global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
        global$boot<- seq(from = 1, to = 1000)
        global$model<- "x1+x2+x.random"
        
        nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
        nox2$boot<- seq(from = 1, to = 1000)
        nox2$model<- "x1+x.random"
        
        x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
        x1$boot<- seq(from = 1, to = 1000)
        x1$model<- "x1"
        
        x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
        x1x2$boot<- seq(from = 1, to = 1000)
        x1x2$model<- "x1+x2"
        
        x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
        x2$boot<- seq(from = 1, to = 1000)
        x2$model<- "x2"
        
        x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
        x2xrand$boot<- seq(from = 1, to = 1000)
        x2xrand$model<- "x2+x.random"
        
        xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
        xrand$boot<- seq(from = 1, to = 1000)
        xrand$model<- "x.random"
        
        t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), 
                                DIC = c(global$DIC, nox2$DIC, x1$DIC, x1x2$DIC, x2$DIC, x2xrand$DIC, xrand$DIC), 
                                boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
        t1<- ddply(t1.working, .(boot), transform, rank = rank(DIC, ties.method = "first"))
        min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
        
        # x1 and x2 in best
        all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        # xrand in best
        all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
        
        #x1 an x2, no xrand
        n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
        #n.correct<- length(all.x1x2)
        n.tries<- length(which(min.aicc.df$DIC != "NA"))
        x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
        
        # x rand in best
        n.wrong<- length(all.xrand)
        xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
        
        result.ind<- result.ind+1
      }
      
      if(i > 38 && i < 46) {
        print(i)
        next
      }
      
      if(i == 46) {
        print(i)
        x1x2best[result.ind, x1x2best.ind]<- NA
        xrandbest[result.ind, xrandbest.ind]<- NA
        
        result.ind<- result.ind+1
      }
    }
   
  }
  
  ## Mod
  {
    result.ind<- 1
    x1x2best.ind<- 5
    xrandbest.ind<- 5
    
    # Weak
    {
      dir.use<- "~/Dropbox/rSACWinter2016/Fits/Mod/"
      
      # Fine
      # Order for plotting
      temp<- gsub("emm.ewc.bs", "emm.emf.bf", files.list.use)
      files.list.use<- gsub("emm.ewc.bn", "emm.emf.bn", temp)
  
      for(i in 1:length(files.list.use)) {
        
        # GLM no RSAC
        if(i == 1 || i == 2) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # GLM, GLM+AC, GLM+SPEV
        if(i > 2 && i < 5) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ]) 
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2+", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # NEG.EXP.FREE
        if(i == 5) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+xnbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 100)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xnbi$boot<- seq(from = 1, to = 100)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 5 && i < 14) {
          print(i)
          next
        }
        
        # NEG.EXP.FIX
        if(i == 14){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.nbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          #xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 14 && i < 23) {
          print(i)
          next
        }
        
        # GAM
        if(i == 23){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+smooth+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+smooth+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+smooth"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+smooth"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+smooth"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+smooth+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "smooth+x.random"
          
          xsmooth<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xsmooth$boot<- seq(from = 1, to = 1000)
          xsmooth$model<- "smooth"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model, xsmooth$model, glm.model.df$model), 
                                  AICc = c(global$AICc, nox2$AICc, x1$AICc, x1x2$AICc, x2$AICc, x2xrand$AICc, xrand$AICc, xsmooth$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot, xsmooth$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 23 && i < 32) {
          print(i)
          next
        }
        
        # GEE
        if(i == 32) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), QICu = c(global$QICu, nox2$QICu, x1$QICu, x1x2$QICu, x2$QICu, x2xrand$QICu, xrand$QICu), boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(QICu, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$QICu != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 31 && i < 39) {
          print(i)
          next
        }
        
        # spGLM
        if(i == 39) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), 
                                  DIC = c(global$DIC, nox2$DIC, x1$DIC, x1x2$DIC, x2$DIC, x2xrand$DIC, xrand$DIC), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(DIC, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$DIC != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        if(i > 38 && i < 46) {
          print(i)
          next
        }
        
        if(i == 46) {
          print(i)
          x1x2best[result.ind, x1x2best.ind]<- NA
          xrandbest[result.ind, xrandbest.ind]<- NA
          
          result.ind<- result.ind+1
        }
      }
    }
    
    # Med
    {
      result.ind<- 1
      x1x2best.ind<- 3
      xrandbest.ind<- 3
      
      # Update file list
      temp<- gsub("emm.emf.bf", "emm.emm.bm", files.list.use)
      files.list.use<- gsub("emm.emf.bn", "emm.emm.bn", temp)
      
      for(i in 1:length(files.list.use)) {
        
        # GLM no RSAC
        if(i == 1 || i == 2) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # GLM, GLM+AC, GLM+SPEV
        if(i > 2 && i < 5) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ]) 
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2+", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # NEG.EXP.FREE
        if(i == 5) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+xnbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 5 && i < 14) {
          print(i)
          next
        }
        
        # NEG.EXP.FIX
        if(i == 14){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.nbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          #xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 14 && i < 23) {
          print(i)
          next
        }
        
        # GAM
        if(i == 23){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+smooth+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+smooth+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+smooth"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+smooth"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+smooth"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+smooth+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "smooth+x.random"
          
          xsmooth<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xsmooth$boot<- seq(from = 1, to = 1000)
          xsmooth$model<- "smooth"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model, xsmooth$model, glm.model.df$model), 
                                  AICc = c(global$AICc, nox2$AICc, x1$AICc, x1x2$AICc, x2$AICc, x2xrand$AICc, xrand$AICc, xsmooth$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot, xsmooth$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 23 && i < 32) {
          print(i)
          next
        }
        
        # GEE
        if(i == 32) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), QICu = c(global$QICu, nox2$QICu, x1$QICu, x1x2$QICu, x2$QICu, x2xrand$QICu, xrand$QICu), boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(QICu, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$QICu != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 31 && i < 39) {
          print(i)
          next
        }
        
        # spGLM
        if(i == 39) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), 
                                  DIC = c(global$DIC, nox2$DIC, x1$DIC, x1x2$DIC, x2$DIC, x2xrand$DIC, xrand$DIC), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(DIC, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$DIC != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        if(i > 38 && i < 46) {
          print(i)
          next
        }
        
        if(i == 46) {
          print(i)
          x1x2best[result.ind, x1x2best.ind]<- NA
          xrandbest[result.ind, xrandbest.ind]<- NA
          
          result.ind<- result.ind+1
        }
      }
    }
    
    # Coarse
    {
      result.ind<- 1
      x1x2best.ind<- 4
      xrandbest.ind<- 4 
        
      # Update file list
      temp<- gsub("emm.emm.bm", "emm.emc.bs", files.list.use)
      files.list.use<- gsub("emm.emm.bn", "emm.emc.bn", temp)
      
      for(i in 1:length(files.list.use)) {
        
        # GLM no RSAC
        if(i == 1 || i == 2) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # GLM, GLM+AC, GLM+SPEV
        if(i > 2 && i < 5) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ]) 
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2+", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # NEG.EXP.FREE
        if(i == 5) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+xnbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 5 && i < 14) {
          print(i)
          next
        }
        
        # NEG.EXP.FIX
        if(i == 14){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.nbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          #xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 14 && i < 23) {
          print(i)
          next
        }
        
        # GAM
        if(i == 23){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+smooth+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+smooth+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+smooth"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+smooth"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+smooth"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+smooth+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "smooth+x.random"
          
          xsmooth<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xsmooth$boot<- seq(from = 1, to = 1000)
          xsmooth$model<- "smooth"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model, xsmooth$model, glm.model.df$model), 
                                  AICc = c(global$AICc, nox2$AICc, x1$AICc, x1x2$AICc, x2$AICc, x2xrand$AICc, xrand$AICc, xsmooth$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot, xsmooth$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 23 && i < 32) {
          print(i)
          next
        }
        
        # GEE
        if(i == 32) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), QICu = c(global$QICu, nox2$QICu, x1$QICu, x1x2$QICu, x2$QICu, x2xrand$QICu, xrand$QICu), boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(QICu, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$QICu != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 31 && i < 39) {
          print(i)
          next
        }
        
        # spGLM
        if(i == 39) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), 
                                  DIC = c(global$DIC, nox2$DIC, x1$DIC, x1x2$DIC, x2$DIC, x2xrand$DIC, xrand$DIC), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(DIC, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$DIC != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        if(i > 38 && i < 46) {
          print(i)
          next
        }
        
        if(i == 46) {
          print(i)
          x1x2best[result.ind, x1x2best.ind]<- NA
          xrandbest[result.ind, xrandbest.ind]<- NA
          
          result.ind<- result.ind+1
        }
      }
        
        
      }
  }
  
  ## Strong
  {
    result.ind<- 1
    x1x2best.ind<- 8
    xrandbest.ind<- 8
    
    dir.use<- "~/Dropbox/rSACWinter2016/Fits/Strong/"
    
    # Fine
    {
      # Order for plotting
      temp<- gsub("emm.emc.bs", "emm.esf.bw", files.list.use)
      files.list.use<- gsub("emm.emc.bn", "emm.esf.bn", temp)
      
      for(i in 1:length(files.list.use)) {
        
        # GLM no RSAC
        if(i == 1 || i == 2) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # GLM, GLM+AC, GLM+SPEV
        if(i > 2 && i < 5) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ]) 
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2+", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # NEG.EXP.FREE
        if(i == 5) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+xnbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 5 && i < 14) {
          print(i)
          next
        }
        
        # NEG.EXP.FIX
        if(i == 14){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.nbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          #xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 14 && i < 23) {
          print(i)
          next
        }
        
        # GAM
        if(i == 23){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+smooth+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+smooth+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+smooth"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+smooth"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+smooth"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+smooth+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "smooth+x.random"
          
          xsmooth<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xsmooth$boot<- seq(from = 1, to = 1000)
          xsmooth$model<- "smooth"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model, xsmooth$model, glm.model.df$model), 
                                  AICc = c(global$AICc, nox2$AICc, x1$AICc, x1x2$AICc, x2$AICc, x2xrand$AICc, xrand$AICc, xsmooth$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot, xsmooth$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 23 && i < 32) {
          print(i)
          next
        }
        
        # GEE
        if(i == 32) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), QICu = c(global$QICu, nox2$QICu, x1$QICu, x1x2$QICu, x2$QICu, x2xrand$QICu, xrand$QICu), boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(QICu, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$QICu != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 31 && i < 39) {
          print(i)
          next
        }
        
        # spGLM
        if(i == 39) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), 
                                  DIC = c(global$DIC, nox2$DIC, x1$DIC, x1x2$DIC, x2$DIC, x2xrand$DIC, xrand$DIC), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(DIC, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$DIC != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        if(i > 38 && i < 46) {
          print(i)
          next
        }
        
        if(i == 46) {
          print(i)
          x1x2best[result.ind, x1x2best.ind]<- NA
          xrandbest[result.ind, xrandbest.ind]<- NA
          
          result.ind<- result.ind+1
        }
      }
    }
    
    # Med
    {
      result.ind<- 1
      x1x2best.ind<- 3
      xrandbest.ind<- 3
      
      # Update file list
      temp<- gsub("emm.esf.bf", "emm.esm.bm", files.list.use)
      files.list.use<- gsub("emm.esf.bn", "emm.esm.bn", temp)
      
      for(i in 1:length(files.list.use)) {
        
        # GLM no RSAC
        if(i == 1 || i == 2) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # GLM, GLM+AC, GLM+SPEV
        if(i > 2 && i < 5) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ]) 
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2+", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # NEG.EXP.FREE
        if(i == 5) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+xnbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 5 && i < 14) {
          print(i)
          next
        }
        
        # NEG.EXP.FIX
        if(i == 14){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.nbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          #xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 14 && i < 23) {
          print(i)
          next
        }
        
        # GAM
        if(i == 23){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+smooth+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+smooth+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+smooth"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+smooth"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+smooth"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+smooth+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "smooth+x.random"
          
          xsmooth<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xsmooth$boot<- seq(from = 1, to = 1000)
          xsmooth$model<- "smooth"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model, xsmooth$model, glm.model.df$model), 
                                  AICc = c(global$AICc, nox2$AICc, x1$AICc, x1x2$AICc, x2$AICc, x2xrand$AICc, xrand$AICc, xsmooth$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot, xsmooth$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 23 && i < 32) {
          print(i)
          next
        }
        
        # GEE
        if(i == 32) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), QICu = c(global$QICu, nox2$QICu, x1$QICu, x1x2$QICu, x2$QICu, x2xrand$QICu, xrand$QICu), boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(QICu, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$QICu != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 31 && i < 39) {
          print(i)
          next
        }
        
        # spGLM
        if(i == 39) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), 
                                  DIC = c(global$DIC, nox2$DIC, x1$DIC, x1x2$DIC, x2$DIC, x2xrand$DIC, xrand$DIC), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(DIC, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$DIC != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        if(i > 38 && i < 46) {
          print(i)
          next
        }
        
        if(i == 46) {
          print(i)
          x1x2best[result.ind, x1x2best.ind]<- NA
          xrandbest[result.ind, xrandbest.ind]<- NA
          
          result.ind<- result.ind+1
        }
      }
    }
    
    # Coarse
    {
      result.ind<- 1
      x1x2best.ind<- 4
      xrandbest.ind<- 4 
      
      # Update file list
      temp<- gsub("emm.esm.bm", "emm.esc.bs", files.list.use)
      files.list.use<- gsub("emm.esm.bn", "emm.esc.bn", temp)
      
      for(i in 1:length(files.list.use)) {
        
        # GLM no RSAC
        if(i == 1 || i == 2) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # GLM, GLM+AC, GLM+SPEV
        if(i > 2 && i < 5) {
          print(i)
          t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ]) 
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2+", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # NEG.EXP.FREE
        if(i == 5) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+xnbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 5 && i < 14) {
          print(i)
          next
        }
        
        # NEG.EXP.FIX
        if(i == 14){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.nbi+x.random"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+xnbi"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+xnbi+x.random"
          
          nox1<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          nox1$boot<- seq(from = 1, to = 1000)
          nox1$model<- "x2+xnbi+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+xnbi"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+xnbi"
          
          xnbixrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xnbixrand$boot<- seq(from = 1, to = 1000)
          xnbixrand$model<- "xnbi+x.random"
          
          xnbi<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          #xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$boot<- seq(from = 1, to = 1000)
          xnbi$model<- "xnbi"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, x1x2$model, nox2$model, nox1$model, x1$model, x2$model, xnbixrand$model, xnbi$model, glm.model.df$model), 
                                  AICc = c(global$AICc, x1x2$AICc, nox2$AICc, nox1$AICc, x1$AICc, x2$AICc, xnbixrand$AICc, xnbi$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, x1x2$boot, nox2$boot, nox1$boot, x1$boot, x2$boot, xnbixrand$boot, xnbi$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 14 && i < 23) {
          print(i)
          next
        }
        
        # GAM
        if(i == 23){
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+smooth+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+smooth+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1+smooth"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2+smooth"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2+smooth"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+smooth+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "smooth+x.random"
          
          xsmooth<- read.csv(paste(dir.use, files.list.use[i+7], sep = ""))
          xsmooth$boot<- seq(from = 1, to = 1000)
          xsmooth$model<- "smooth"
          
          glm.model<- read.csv(paste(dir.use, files.list.use[i+8], sep = ""))
          glm.model$model<- as.character(glm.model$model)
          glm.model.df<- ddply(glm.model, .(boot), function(x) x[x$rank == 1, ]) 
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model, xsmooth$model, glm.model.df$model), 
                                  AICc = c(global$AICc, nox2$AICc, x1$AICc, x1x2$AICc, x2$AICc, x2xrand$AICc, xrand$AICc, xsmooth$AICc, glm.model.df$AICc), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot, xsmooth$boot, glm.model.df$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(AICc, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$AICc != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 23 && i < 32) {
          print(i)
          next
        }
        
        # GEE
        if(i == 32) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), QICu = c(global$QICu, nox2$QICu, x1$QICu, x1x2$QICu, x2$QICu, x2xrand$QICu, xrand$QICu), boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(QICu, ties.method = "first"))
          
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$QICu != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        # Update
        if(i > 31 && i < 39) {
          print(i)
          next
        }
        
        # spGLM
        if(i == 39) {
          print(i)
          global<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
          global$boot<- seq(from = 1, to = 1000)
          global$model<- "x1+x2+x.random"
          
          nox2<- read.csv(paste(dir.use, files.list.use[i+1], sep = ""))
          nox2$boot<- seq(from = 1, to = 1000)
          nox2$model<- "x1+x.random"
          
          x1<- read.csv(paste(dir.use, files.list.use[i+2], sep = ""))
          x1$boot<- seq(from = 1, to = 1000)
          x1$model<- "x1"
          
          x1x2<- read.csv(paste(dir.use, files.list.use[i+3], sep = ""))
          x1x2$boot<- seq(from = 1, to = 1000)
          x1x2$model<- "x1+x2"
          
          x2<- read.csv(paste(dir.use, files.list.use[i+4], sep = ""))
          x2$boot<- seq(from = 1, to = 1000)
          x2$model<- "x2"
          
          x2xrand<- read.csv(paste(dir.use, files.list.use[i+5], sep = ""))
          x2xrand$boot<- seq(from = 1, to = 1000)
          x2xrand$model<- "x2+x.random"
          
          xrand<- read.csv(paste(dir.use, files.list.use[i+6], sep = ""))
          xrand$boot<- seq(from = 1, to = 1000)
          xrand$model<- "x.random"
          
          t1.working<- data.frame(model = c(global$model, nox2$model, x1$model, x1x2$model, x2$model, x2xrand$model, xrand$model), 
                                  DIC = c(global$DIC, nox2$DIC, x1$DIC, x1x2$DIC, x2$DIC, x2xrand$DIC, xrand$DIC), 
                                  boot = c(global$boot, nox2$boot, x1$boot, x1x2$boot, x2$boot, x2xrand$boot, xrand$boot))
          t1<- ddply(t1.working, .(boot), transform, rank = rank(DIC, ties.method = "first"))
          min.aicc.df<- ddply(t1, .(boot), function(x) x[x$rank == 1, ])
          
          # x1 and x2 in best
          all.x1x2<- grep("x1+x2", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          # xrand in best
          all.xrand<- grep("x.random", as.character(min.aicc.df$model), ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = TRUE)
          
          #x1 an x2, no xrand
          n.correct<- length(all.x1x2[!(all.x1x2 %in% all.xrand)])
          #n.correct<- length(all.x1x2)
          n.tries<- length(which(min.aicc.df$DIC != "NA"))
          x1x2best[result.ind, x1x2best.ind]<- (n.correct/n.tries)*100
          
          # x rand in best
          n.wrong<- length(all.xrand)
          xrandbest[result.ind, xrandbest.ind]<- (n.wrong/n.tries)*100
          
          result.ind<- result.ind+1
        }
        
        if(i > 38 && i < 46) {
          print(i)
          next
        }
        
        if(i == 46) {
          print(i)
          x1x2best[result.ind, x1x2best.ind]<- NA
          xrandbest[result.ind, xrandbest.ind]<- NA
          
          result.ind<- result.ind+1
        }
      }
      
      
    }
  }
  
  ## Plotting
  # Weak X1X2Best
  x1x2best.plot<- x1x2best[,c(1, 2:4)]
  x1x2best.plot$Model<- factor(x1x2best$Model)
  x1x2best.plot.m<- melt(x1x2best.plot, id.vars = "Model")
  x1x2best.plot.m$variable<- factor(x1x2best.plot.m$variable)
  x1x2best.plot.m$Model<- factor(x1x2best.plot.m$Model, levels = x1x2best.plot$Model)
  
  ggplot(x1x2best.plot.m, aes(variable, value, fill = Model)) +
    geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
    scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
    scale_fill_manual(name = "Model", values = colors) +
    scale_y_continuous(limits = c(0, 100)) +
    xlab("") + ylab("X1 and X2 Best Model") +
    theme(axis.title.y = element_text(colour = "black", size = 16)) +
    theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
  
  xrandbest.plot<- xrandbest[,c(1, 2:4)]
  xrandbest.plot$Model<- factor(xrandbest$Model)
  xrandbest.plot.m<- melt(xrandbest.plot, id.vars = "Model")
  xrandbest.plot.m$variable<- factor(xrandbest.plot.m$variable)
  xrandbest.plot.m$Model<- factor(xrandbest.plot.m$Model, levels = xrandbest.plot$Model)
  
  ggplot(xrandbest.plot.m, aes(variable, value, fill = Model)) +
    geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
    scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
    scale_fill_manual(name = "Model", values = colors) +
    scale_y_continuous(limits = c(0, 100)) +
    xlab("") + ylab("Xrand in Best Model") +
    theme(axis.title.y = element_text(colour = "black", size = 16)) +
    theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
  
  
  # Mod X1X2Best
  x1x2best.plot<- x1x2best[,c(1, 5:7)]
  x1x2best.plot$Model<- factor(x1x2best$Model)
  x1x2best.plot.m<- melt(x1x2best.plot, id.vars = "Model")
  x1x2best.plot.m$variable<- factor(x1x2best.plot.m$variable)
  x1x2best.plot.m$Model<- factor(x1x2best.plot.m$Model, levels = x1x2best.plot$Model)
  
  ggplot(x1x2best.plot.m, aes(variable, value, fill = Model)) +
    geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
    scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
    scale_fill_manual(name = "Model", values = colors) +
    scale_y_continuous(limits = c(0, 100)) +
    xlab("") + ylab("X1 and X2 Best Model") +
    theme(axis.title.y = element_text(colour = "black", size = 16)) +
    theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
  
  xrandbest.plot<- xrandbest[,c(1, 5:7)]
  xrandbest.plot$Model<- factor(xrandbest$Model)
  xrandbest.plot.m<- melt(xrandbest.plot, id.vars = "Model")
  xrandbest.plot.m$variable<- factor(xrandbest.plot.m$variable)
  xrandbest.plot.m$Model<- factor(xrandbest.plot.m$Model, levels = xrandbest.plot$Model)
  
  ggplot(xrandbest.plot.m, aes(variable, value, fill = Model)) +
    geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
    scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
    scale_fill_manual(name = "Model", values = colors) +
    scale_y_continuous(limits = c(0, 100)) +
    xlab("") + ylab("Xrand in Best Model") +
    theme(axis.title.y = element_text(colour = "black", size = 16)) +
    theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
  
  # Strong X1X2Best
  x1x2best.plot<- x1x2best[,c(1, 8:10)]
  x1x2best.plot$Model<- factor(x1x2best$Model)
  x1x2best.plot.m<- melt(x1x2best.plot, id.vars = "Model")
  x1x2best.plot.m$variable<- factor(x1x2best.plot.m$variable)
  x1x2best.plot.m$Model<- factor(x1x2best.plot.m$Model, levels = x1x2best.plot$Model)
  
  ggplot(x1x2best.plot.m, aes(variable, value, fill = Model)) +
    geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
    scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
    scale_fill_manual(name = "Model", values = colors) +
    scale_y_continuous(limits = c(0, 100)) +
    xlab("") + ylab("X1 and X2 Best Model") +
    theme(axis.title.y = element_text(colour = "black", size = 16)) +
    theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
  
  xrandbest.plot<- xrandbest[,c(1, 8:10)]
  xrandbest.plot$Model<- factor(xrandbest$Model)
  xrandbest.plot.m<- melt(xrandbest.plot, id.vars = "Model")
  xrandbest.plot.m$variable<- factor(xrandbest.plot.m$variable)
  xrandbest.plot.m$Model<- factor(xrandbest.plot.m$Model, levels = xrandbest.plot$Model)
  
  ggplot(xrandbest.plot.m, aes(variable, value, fill = Model)) +
    geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
    scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
    scale_fill_manual(name = "Model", values = colors) +
    scale_y_continuous(limits = c(0, 100)) +
    xlab("") + ylab("Xrand in Best Model") +
    theme(axis.title.y = element_text(colour = "black", size = 16)) +
    theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
    theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
}

##### Model prediction -- AUC 2-fold cross validation.
{
  ### Weak - Full
  {
    dir.use<- "~/Desktop/SAC_finalcountdown/ForR/ResultsPlotting/ModelSelection/WeakSpEnv/"
    
    # Order for plotting
    files.list.use<- c("11072015emm.ewf.bf.glmglobalmodresults.csv", "11072015emm.ewf.bf.autocovglobalmodresults.csv",  "11072015emm.ewf.bf.spev.negexpglobalmodresults.csv",  "11072015emm.ewf.bf.mle2.negexp.freeglobalmodresults.csv", "11072015emm.ewf.bf.gamsglobalmodresults.csv",   "11072015emm.ewf.bf.geeglobalmodresults.csv", "11072015emm.ewf.bf.spglmglobalmodresults.csv", "11072015emm.ewf.bf.glm.waveletglobalmodresults.csv", 
                       "11072015emm.ewm.bm.glmglobalmodresults.csv", "11072015emm.ewm.bm.autocovglobalmodresults.csv", "11072015emm.ewm.bm.spev.negexpglobalmodresults.csv", "11072015emm.ewm.bm.mle2.negexp.freeglobalmodresults.csv",  "11072015emm.ewm.bm.gamsglobalmodresults.csv",  "11072015emm.ewm.bm.geeglobalmodresults.csv",   "11072015emm.ewm.bm.spglmglobalmodresults.csv",  "11072015emm.ewm.bm.glm.waveletglobalmodresults.csv", 
                       "11072015emm.ewc.bs.glmglobalmodresults.csv", "11072015emm.ewc.bs.autocovglobalmodresults.csv", "11072015emm.ewc.bs.spev.negexpglobalmodresults.csv", "11072015emm.ewc.bs.mle2.negexp.freeglobalmodresults.csv", "11072015emm.ewc.bs.gamsglobalmodresults.csv", "11072015emm.ewc.bs.geeglobalmodresults.csv", "11072015emm.ewc.bs.spglmglobalmodresults.csv", "11072015emm.ewc.bs.glm.waveletglobalmodresults.csv")
    
    mod.vec<- c(rep(c("GLM", "GLM+AC", "GLM+SPEV", "GLM+NBI.AC", "GAM", "GEE", "SPGLM", "WAVE"), 3))
    scale.vec<- c(rep(c("Fine", "Med", "Coarse"), each = 8))
    
    # AUC two-fold cross validated dataset
    result.auc.split.w<- data.frame(matrix(nrow = length(files.list.use), ncol = 5))
    result.auc.split.w[,1]<- mod.vec
    result.auc.split.w[,2]<- scale.vec
    names(result.auc.split.w)<- c("model", "scale", "mean", "0.025", "0.975")
    
    for (i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      
      ## AUC
      # Split
      result.auc.split.w[i,3]<- mean(t1$split.auc, na.rm = TRUE)
      result.auc.split.w[i,4]<- quantile(t1$split.auc, probs = 0.025, na.rm = TRUE)
      result.auc.split.w[i,5]<- quantile(t1$split.auc, probs = 0.975, na.rm = TRUE)
    }
    
    ## Plotting
    dat.mean<- result.auc.split.w[,c(1:3)]
    dat.err<- result.auc.split.w[,c(1, c(4:5))]
    dat.mean$model<- factor(dat.mean$model, levels = unique(dat.mean$model))
    dat.mean$scale<- factor(dat.mean$scale, levels = c("Fine", "Med", "Coarse"))
    
    limits <- aes(ymax = dat.err[,3],
                  ymin = dat.err[,2])
    
    ggplot(dat.mean, aes(scale, mean, fill = model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
      scale_fill_manual(name = "Model", values = colors) +
      scale_y_continuous(limits = c(0, 1)) +
      xlab("") + ylab("Cross-validated AUC") +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
  }
  
  ### Mod - Full
  {
    dir.use<- "~/Desktop/SAC_finalcountdown/ForR/ResultsPlotting/ModelSelection/ModSpEnv/"
    
    # Order for plotting
    files.list.use<- c("11072015emm.emf.bf.glmglobalmodresults.csv", "11072015emm.emf.bf.autocovglobalmodresults.csv",  "11072015emm.emf.bf.spev.negexpglobalmodresults.csv",  "11072015emm.emf.bf.mle2.negexp.freeglobalmodresults.csv", "11072015emm.emf.bf.gamsglobalmodresults.csv",   "11072015emm.emf.bf.geeglobalmodresults.csv", "11072015emm.emf.bf.spglmglobalmodresults.csv", "11072015emm.emf.bf.glm.waveletglobalmodresults.csv", 
                       "11072015emm.emm.bm.glmglobalmodresults.csv", "11072015emm.emm.bm.autocovglobalmodresults.csv", "11072015emm.emm.bm.spev.negexpglobalmodresults.csv", "11072015emm.emm.bm.mle2.negexp.freeglobalmodresults.csv",  "11072015emm.emm.bm.gamsglobalmodresults.csv",  "11072015emm.emm.bm.geeglobalmodresults.csv",   "11072015emm.emm.bm.spglmglobalmodresults.csv",  "11072015emm.emm.bm.glm.waveletglobalmodresults.csv", 
                       "11072015emm.emc.bs.glmglobalmodresults.csv", "11072015emm.emc.bs.autocovglobalmodresults.csv", "11072015emm.emc.bs.spev.negexpglobalmodresults.csv", "11072015emm.emc.bs.mle2.negexp.freeglobalmodresults.csv", "11072015emm.emc.bs.gamsglobalmodresults.csv", "11072015emm.emc.bs.geeglobalmodresults.csv", "11072015emm.emc.bs.spglmglobalmodresults.csv", "11072015emm.emc.bs.glm.waveletglobalmodresults.csv")
    
    mod.vec<- c(rep(c("GLM", "GLM+AC", "GLM+SPEV", "GLM+NBI.AC", "GAM", "GEE", "SPGLM", "WAVE"), 3))
    scale.vec<- c(rep(c("Fine", "Med", "Coarse"), each = 8))
    
    # AUC two-fold cross validated dataset
    result.auc.split.m<- data.frame(matrix(nrow = length(files.list.use), ncol = 5))
    result.auc.split.m[,1]<- mod.vec
    result.auc.split.m[,2]<- scale.vec
    names(result.auc.split.m)<- c("model", "scale", "mean", "0.025", "0.975")
    
    for (i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      
      ## AUC
      # Split
      result.auc.split.m[i,3]<- mean(t1$split.auc, na.rm = TRUE)
      result.auc.split.m[i,4]<- quantile(t1$split.auc, probs = 0.025, na.rm = TRUE)
      result.auc.split.m[i,5]<- quantile(t1$split.auc, probs = 0.975, na.rm = TRUE)
    }
    
    ## Plotting
    dat.mean<- result.auc.split.m[,c(1:3)]
    dat.err<- result.auc.split.m[,c(1, c(4:5))]
    dat.mean$model<- factor(dat.mean$model, levels = unique(dat.mean$model))
    dat.mean$scale<- factor(dat.mean$scale, levels = c("Fine", "Med", "Coarse"))
    
    limits <- aes(ymax = dat.err[,3],
                  ymin = dat.err[,2])
    
    ggplot(dat.mean, aes(scale, mean, fill = model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
      scale_fill_manual(name = "Model", values = colors) +
      scale_y_continuous(limits = c(0, 1)) +
      xlab("") + ylab("Cross-validated AUC") +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
  }
  
  ## Strong - Full
  {
    dir.use<- "~/Desktop/SAC_finalcountdown/ForR/ResultsPlotting/ModelSelection/StrongSpEnv/"
    
    # Order for plotting
    files.list.use<- c("11072015emm.esf.bf.glmglobalmodresults.csv", "11072015emm.esf.bf.autocovglobalmodresults.csv",  "11072015emm.esf.bf.spev.negexpglobalmodresults.csv",  "11072015emm.esf.bf.mle2.negexp.freeglobalmodresults.csv", "11072015emm.esf.bf.gamsglobalmodresults.csv",   "11072015emm.esf.bf.geeglobalmodresults.csv", "11072015emm.esf.bf.spglmglobalmodresults.csv", "11072015emm.esf.bf.glm.waveletglobalmodresults.csv", 
                       "11072015emm.esm.bm.glmglobalmodresults.csv", "11072015emm.esm.bm.autocovglobalmodresults.csv", "11072015emm.esm.bm.spev.negexpglobalmodresults.csv", "11072015emm.esm.bm.mle2.negexp.freeglobalmodresults.csv",  "11072015emm.esm.bm.gamsglobalmodresults.csv",  "11072015emm.esm.bm.geeglobalmodresults.csv",   "11072015emm.esm.bm.spglmglobalmodresults.csv",  "11072015emm.esm.bm.glm.waveletglobalmodresults.csv", 
                       "11072015emm.esc.bs.glmglobalmodresults.csv", "11072015emm.esc.bs.autocovglobalmodresults.csv", "11072015emm.esc.bs.spev.negexpglobalmodresults.csv", "11072015emm.esc.bs.mle2.negexp.freeglobalmodresults.csv", "11072015emm.esc.bs.gamsglobalmodresults.csv", "11072015emm.esc.bs.geeglobalmodresults.csv", "11072015emm.esc.bs.spglmglobalmodresults.csv", "11072015emm.esc.bs.glm.waveletglobalmodresults.csv")
    
    mod.vec<- c(rep(c("GLM", "GLM+AC", "GLM+SPEV", "GLM+NBI.AC", "GAM", "GEE", "SPGLM", "WAVE"), 3))
    scale.vec<- c(rep(c("Fine", "Med", "Coarse"), each = 8))
    
    # AUC two-fold cross validated dataset
    result.auc.split.s<- data.frame(matrix(nrow = length(files.list.use), ncol = 5))
    result.auc.split.s[,1]<- mod.vec
    result.auc.split.s[,2]<- scale.vec
    names(result.auc.split.s)<- c("model", "scale", "mean", "0.025", "0.975")
    
    for (i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      
      ## AUC
      # Split
      result.auc.split.s[i,3]<- mean(t1$split.auc, na.rm = TRUE)
      result.auc.split.s[i,4]<- quantile(t1$split.auc, probs = 0.025, na.rm = TRUE)
      result.auc.split.s[i,5]<- quantile(t1$split.auc, probs = 0.975, na.rm = TRUE)
    }
    
    ## Plotting
    dat.mean<- result.auc.split.s[,c(1:3)]
    dat.err<- result.auc.split.s[,c(1, c(4:5))]
    dat.mean$model<- factor(dat.mean$model, levels = unique(dat.mean$model))
    dat.mean$scale<- factor(dat.mean$scale, levels = c("Fine", "Med", "Coarse"))
    
    limits <- aes(ymax = dat.err[,3],
                  ymin = dat.err[,2])
    
    ggplot(dat.mean, aes(scale, mean, fill = model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
      scale_fill_manual(name = "Model", values = colors) +
      scale_y_continuous(limits = c(0, 1)) +
      xlab("") + ylab("Cross-validated AUC") +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
  }
  
  ### Weak - NoX2
  {
    dir.use<- "~/Desktop/SAC_finalcountdown/ForR/ResultsPlotting/ModelSelection/WeakSpEnv/"
    
    # Order for plotting
    files.list.use<- c("11072015emm.ewf.bf.glm.nox2globalmodresults.csv", "11072015emm.ewf.bf.autocov.nox2globalmodresults.csv",  "11072015emm.ewf.bf.spev.negexp.nox2globalmodresults.csv",  "11072015emm.ewf.bf.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.ewf.bf.gams.nox2globalmodresults.csv",   "11072015emm.ewf.bf.gee.nox2globalmodresults.csv", "11072015emm.ewf.bf.spglm.nox2globalmodresults.csv", "11072015emm.ewf.bf.glm.wavelet.nox2globalmodresults.csv", 
                       "11072015emm.ewm.bm.glm.nox2globalmodresults.csv", "11072015emm.ewm.bm.autocov.nox2globalmodresults.csv", "11072015emm.ewm.bm.spev.negexp.nox2globalmodresults.csv", "11072015emm.ewm.bm.mle2.negexp.free.nox2globalmodresults.csv",  "11072015emm.ewm.bm.gams.nox2globalmodresults.csv",  "11072015emm.ewm.bm.gee.nox2globalmodresults.csv",   "11072015emm.ewm.bm.spglm.nox2globalmodresults.csv",  "11072015emm.ewm.bm.glm.wavelet.nox2globalmodresults.csv", 
                       "11072015emm.ewc.bs.glm.nox2globalmodresults.csv", "11072015emm.ewc.bs.autocov.nox2globalmodresults.csv", "11072015emm.ewc.bs.spev.negexp.nox2globalmodresults.csv", "11072015emm.ewc.bs.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.ewc.bs.gams.nox2globalmodresults.csv", "11072015emm.ewc.bs.gee.nox2globalmodresults.csv", "11072015emm.ewc.bs.spglm.nox2globalmodresults.csv", "11072015emm.ewc.bs.glm.wavelet.nox2globalmodresults.csv")
    mod.vec<- c(rep(c("GLM", "GLM+AC", "GLM+SPEV", "GLM+NBI.AC", "GAM", "GEE", "SPGLM", "WAVE"), 3))
    scale.vec<- c(rep(c("Fine", "Med", "Coarse"), each = 8))
    
    # AUC two-fold cross validated dataset
    result.auc.split.w<- data.frame(matrix(nrow = length(files.list.use), ncol = 5))
    result.auc.split.w[,1]<- mod.vec
    result.auc.split.w[,2]<- scale.vec
    names(result.auc.split.w)<- c("model", "scale", "mean", "0.025", "0.975")
    
    for (i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      
      ## AUC
      # Split
      result.auc.split.w[i,3]<- mean(t1$split.auc, na.rm = TRUE)
      result.auc.split.w[i,4]<- quantile(t1$split.auc, probs = 0.025, na.rm = TRUE)
      result.auc.split.w[i,5]<- quantile(t1$split.auc, probs = 0.975, na.rm = TRUE)
    }
    
    ## Plotting
    dat.mean<- result.auc.split.w[,c(1:3)]
    dat.err<- result.auc.split.w[,c(1, c(4:5))]
    dat.mean$model<- factor(dat.mean$model, levels = unique(dat.mean$model))
    dat.mean$scale<- factor(dat.mean$scale, levels = c("Fine", "Med", "Coarse"))
    
    limits <- aes(ymax = dat.err[,3],
                  ymin = dat.err[,2])
    
    ggplot(dat.mean, aes(scale, mean, fill = model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
      scale_fill_manual(name = "Model", values = colors) +
      scale_y_continuous(limits = c(0, 1)) +
      xlab("") + ylab("Cross-validated AUC") +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
  }
  
  ### Mod - NoX2
  {
    dir.use<- "~/Desktop/SAC_finalcountdown/ForR/ResultsPlotting/ModelSelection/ModSpEnv/"
    
    # Order for plotting
    files.list.use<- c("11072015emm.emf.bf.glm.nox2globalmodresults.csv", "11072015emm.emf.bf.autocov.nox2globalmodresults.csv",  "11072015emm.emf.bf.spev.negexp.nox2globalmodresults.csv",  "11072015emm.emf.bf.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.emf.bf.gams.nox2globalmodresults.csv",   "11072015emm.emf.bf.gee.nox2globalmodresults.csv", "11072015emm.emf.bf.spglm.nox2globalmodresults.csv", "11072015emm.emf.bf.glm.wavelet.nox2globalmodresults.csv", 
                       "11072015emm.emm.bm.glm.nox2globalmodresults.csv", "11072015emm.emm.bm.autocov.nox2globalmodresults.csv", "11072015emm.emm.bm.spev.negexp.nox2globalmodresults.csv", "11072015emm.emm.bm.mle2.negexp.free.nox2globalmodresults.csv",  "11072015emm.emm.bm.gams.nox2globalmodresults.csv",  "11072015emm.emm.bm.gee.nox2globalmodresults.csv",   "11072015emm.emm.bm.spglm.nox2globalmodresults.csv",  "11072015emm.emm.bm.glm.wavelet.nox2globalmodresults.csv", 
                       "11072015emm.emc.bs.glm.nox2globalmodresults.csv", "11072015emm.emc.bs.autocov.nox2globalmodresults.csv", "11072015emm.emc.bs.spev.negexp.nox2globalmodresults.csv", "11072015emm.emc.bs.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.emc.bs.gams.nox2globalmodresults.csv", "11072015emm.emc.bs.gee.nox2globalmodresults.csv", "11072015emm.emc.bs.spglm.nox2globalmodresults.csv", "11072015emm.emc.bs.glm.wavelet.nox2globalmodresults.csv")
    
    mod.vec<- c(rep(c("GLM", "GLM+AC", "GLM+SPEV", "GLM+NBI.AC", "GAM", "GEE", "SPGLM", "WAVE"), 3))
    scale.vec<- c(rep(c("Fine", "Med", "Coarse"), each = 8))
    
    # AUC two-fold cross validated dataset
    result.auc.split.m<- data.frame(matrix(nrow = length(files.list.use), ncol = 5))
    result.auc.split.m[,1]<- mod.vec
    result.auc.split.m[,2]<- scale.vec
    names(result.auc.split.m)<- c("model", "scale", "mean", "0.025", "0.975")
    
    for (i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      
      ## AUC
      # Split
      result.auc.split.m[i,3]<- mean(t1$split.auc, na.rm = TRUE)
      result.auc.split.m[i,4]<- quantile(t1$split.auc, probs = 0.025, na.rm = TRUE)
      result.auc.split.m[i,5]<- quantile(t1$split.auc, probs = 0.975, na.rm = TRUE)
    }
    
    ## Plotting
    dat.mean<- result.auc.split.m[,c(1:3)]
    dat.err<- result.auc.split.m[,c(1, c(4:5))]
    dat.mean$model<- factor(dat.mean$model, levels = unique(dat.mean$model))
    dat.mean$scale<- factor(dat.mean$scale, levels = c("Fine", "Med", "Coarse"))
    
    limits <- aes(ymax = dat.err[,3],
                  ymin = dat.err[,2])
    
    ggplot(dat.mean, aes(scale, mean, fill = model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
      scale_fill_manual(name = "Model", values = colors) +
      scale_y_continuous(limits = c(0, 1)) +
      xlab("") + ylab("Cross-validated AUC") +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
  }
  
  ## Strong - NoX2
  {
    dir.use<- "~/Desktop/SAC_finalcountdown/ForR/ResultsPlotting/ModelSelection/StrongSpEnv/"
    
    # Order for plotting
    files.list.use<- c("11072015emm.esf.bf.glm.nox2globalmodresults.csv", "11072015emm.esf.bf.autocov.nox2globalmodresults.csv",  "11072015emm.esf.bf.spev.negexp.nox2globalmodresults.csv",  "11072015emm.esf.bf.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.esf.bf.gams.nox2globalmodresults.csv",   "11072015emm.esf.bf.gee.nox2globalmodresults.csv", "11072015emm.esf.bf.spglm.nox2globalmodresults.csv", "11072015emm.esf.bf.glm.wavelet.nox2globalmodresults.csv", 
                       "11072015emm.esm.bm.glm.nox2globalmodresults.csv", "11072015emm.esm.bm.autocov.nox2globalmodresults.csv", "11072015emm.esm.bm.spev.negexp.nox2globalmodresults.csv", "11072015emm.esm.bm.mle2.negexp.free.nox2globalmodresults.csv",  "11072015emm.esm.bm.gams.nox2globalmodresults.csv",  "11072015emm.esm.bm.gee.nox2globalmodresults.csv",   "11072015emm.esm.bm.spglm.nox2globalmodresults.csv",  "11072015emm.esm.bm.glm.wavelet.nox2globalmodresults.csv", 
                       "11072015emm.esc.bs.glm.nox2globalmodresults.csv", "11072015emm.esc.bs.autocov.nox2globalmodresults.csv", "11072015emm.esc.bs.spev.negexp.nox2globalmodresults.csv", "11072015emm.esc.bs.mle2.negexp.free.nox2globalmodresults.csv", "11072015emm.esc.bs.gams.nox2globalmodresults.csv", "11072015emm.esc.bs.gee.nox2globalmodresults.csv", "11072015emm.esc.bs.spglm.nox2globalmodresults.csv", "11072015emm.esc.bs.glm.wavelet.nox2globalmodresults.csv")
    
    mod.vec<- c(rep(c("GLM", "GLM+AC", "GLM+SPEV", "GLM+NBI.AC", "GAM", "GEE", "SPGLM", "WAVE"), 3))
    scale.vec<- c(rep(c("Fine", "Med", "Coarse"), each = 8))
    
    # AUC two-fold cross validated dataset
    result.auc.split.s<- data.frame(matrix(nrow = length(files.list.use), ncol = 5))
    result.auc.split.s[,1]<- mod.vec
    result.auc.split.s[,2]<- scale.vec
    names(result.auc.split.s)<- c("model", "scale", "mean", "0.025", "0.975")
    
    for (i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      
      ## AUC
      # Split
      result.auc.split.s[i,3]<- mean(t1$split.auc, na.rm = TRUE)
      result.auc.split.s[i,4]<- quantile(t1$split.auc, probs = 0.025, na.rm = TRUE)
      result.auc.split.s[i,5]<- quantile(t1$split.auc, probs = 0.975, na.rm = TRUE)
    }
    
    ## Plotting
    dat.mean<- result.auc.split.s[,c(1:3)]
    dat.err<- result.auc.split.s[,c(1, c(4:5))]
    dat.mean$model<- factor(dat.mean$model, levels = unique(dat.mean$model))
    dat.mean$scale<- factor(dat.mean$scale, levels = c("Fine", "Med", "Coarse"))
    
    limits <- aes(ymax = dat.err[,3],
                  ymin = dat.err[,2])
    
    ggplot(dat.mean, aes(scale, mean, fill = model)) +
      geom_bar(colour = "black", stat = "identity", position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("Fine", "Med", "Coarse")) +
      scale_fill_manual(name = "Model", values = colors) +
      scale_y_continuous(limits = c(0, 1)) +
      xlab("") + ylab("Cross-validated AUC") +
      geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
      theme(axis.title.y = element_text(colour = "black", size = 16)) +
      theme(axis.text.x = element_text(colour = "black", size=14, vjust = 0.5, hjust = 0.5)) +
      theme(axis.text.y = element_text(colour = "black", size = 14, vjust = 1, hjust = 0.5))
  }
  
  
  
  
  
  
  
  
  
  
  plot(dat$mean[1:3], type="n", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, pch = 0, col = "black")
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "white", border = "black")
  
  # GLM
  lines(dat$mean[1:3], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, lty = lty.use[1], col = colors[1], lwd = lwd.use[1])
  
  # GLM AC
  lines(dat$mean[4:6], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, lty = lty.use[2], col = colors[2], lwd = lwd.use[2])
  
  # SPEV
  lines(dat$mean[7:9], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, lty = lty.use[3], col = colors[3], lwd = lwd.use[3])
  
  # NBI AC
  lines(dat$mean[10:12], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, lty = lty.use[4], col = colors[4], lwd = lwd.use[4])
  
  # GAM
  lines(dat$mean[13:15], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, lty = lty.use[5], col = colors[5], lwd = lwd.use[5])
  
  # GEE
  lines(dat$mean[16:18], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, lty = lty.use[6], col = colors[6], lwd = lwd.use[6])
  
  # SPGLM
  lines(dat$mean[19:21], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, lty = lty.use[7], col = colors[7], lwd = lwd.use[7])
  
  # WAVE
  lines(dat$mean[22:24], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, lty.use[8], col = colors[8], lwd = lwd.use[8])
  
  axis(1, at=1:3, lab=c("Fine", "Medium", "Coarse"))
}














#### Other
###### Box and whisker parameter estimate plots 
{
  #### Weak
  {
    dir.use<- "~/R/Allyn/PhD/SAC/Results07222014/ParamHypoPrediction/Weak/"
    
    files.list<- list.files(dir.use, c("modresults.csv$"))
    
    # What parameter?
    param<- 5 # Look at distribution of x1
    
    # Fine
    # Order for plotting
    files.list.use<- c("5082014emm.ewf.bf.glmglobalmodresults.csv", "5082014emm.ewf.bf.autocovglobalmodresults.csv", "5082014emm.ewf.bf.spev.negexpglobalmodresults.csv", "5082014emm.ewf.bf.mle2.negexp.freeglobalmodresults.csv", "5082014emm.ewf.bf.mle2.gauss.freeglobalmodresults.csv", "5082014emm.ewf.bf.gamsglobalmodresults.csv", "5082014emm.ewf.bf.gamteglobalmodresults.csv", "6282014emm.ewf.bf.geeglobalmodresults.csv", 
                       #"5082014emm.ewf.bf.glmmglobalmodresults.csv", 
                       "5082014emm.ewf.bf.glm.waveletglobalmodresults.csv",
                       "5082014emm.ewf.bf.glm.nox2globalmodresults.csv", "5082014emm.ewf.bf.autocov.nox2globalmodresults.csv", "5082014emm.ewf.bf.spev.negexp.nox2globalmodresults.csv", "5082014emm.ewf.bf.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.ewf.bf.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.ewf.bf.gams.nox2globalmodresults.csv", "5082014emm.ewf.bf.gamte.nox2globalmodresults.csv",  "6282014emm.ewf.bf.gee.nox2globalmodresults.csv", 
                       #"5082014emm.ewf.bf.glmm.nox2globalmodresults.csv", 
                       "5082014emm.ewf.bf.glm.wavelet.nox2globalmodresults.csv")
    
    result<- data.frame(matrix(nrow = 1000*length(files.list.use), ncol = 2))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result[,1]<- c(rep(mod.vec, each = 1000), rep(mod.vec, each = 1000))
    
    start<- 1
    end.fixed<- 1000
    
    for(i in 1:length(files.list.use)) {
      end<- end.fixed*i
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      if(nrow(t1) < 1000) {
        result[c(start:end), 2]<- c(t1[,param], rep(NA, 1000-nrow(t1)))
      } else {
        result[c(start:end), 2]<- t1[,param]
      }
      start <- end+1
      print(i)
    }
    
    
    # Box plot
    full<- result[1:(nrow(result)/2),]
    names(full)<- c("model", "parameter")
    full$model<- factor(full$model, unique(full$model))
    
    par(mar = c(10.5,4,1.5,0.5))
    par(mfrow = c(1,2))
    boxplot(parameter ~ model, data = full, boxwex = 0.5, col = "grey", main = "Weak.Fine.Full", xlab = "", ylim = c(-0.13, 0.25), ylab = "", horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    reduced<- result[(nrow(result)/2)+1:nrow(result),]
    names(reduced)<- c("model", "parameter")
    reduced$model<- factor(reduced$model, unique(reduced$model))
    
    boxplot(parameter ~ model, data = reduced, boxwex = 0.5, col = "grey", main = "Weak.Fine.Red", xlab = "", ylab = "", ylim = c(-0.13, 0.25), horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    # Medium
    # Order for plotting
    files.list.use<- c("5082014emm.ewm.bm.glmglobalmodresults.csv", "5082014emm.ewm.bm.autocovglobalmodresults.csv", "5082014emm.ewm.bm.spev.negexpglobalmodresults.csv", "5082014emm.ewm.bm.mle2.negexp.freeglobalmodresults.csv", "5082014emm.ewm.bm.mle2.gauss.freeglobalmodresults.csv", "5082014emm.ewm.bm.gamsglobalmodresults.csv", "5082014emm.ewm.bm.gamteglobalmodresults.csv", "6282014emm.ewm.bm.geeglobalmodresults.csv", 
                       #"5082014emm.ewm.bm.glmmglobalmodresults.csv", 
                       "5082014emm.ewm.bm.glm.waveletglobalmodresults.csv",
                       "5082014emm.ewm.bm.glm.nox2globalmodresults.csv", "5082014emm.ewm.bm.autocov.nox2globalmodresults.csv", "5082014emm.ewm.bm.spev.negexp.nox2globalmodresults.csv", "5082014emm.ewm.bm.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.ewm.bm.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.ewm.bm.gams.nox2globalmodresults.csv", "5082014emm.ewm.bm.gamte.nox2globalmodresults.csv",  "6282014emm.ewm.bm.gee.nox2globalmodresults.csv", 
                       #"5082014emm.ewm.bm.glmm.nox2globalmodresults.csv", 
                       "5082014emm.ewm.bm.glm.wavelet.nox2globalmodresults.csv")
    
    
    result<- data.frame(matrix(nrow = 1000*length(files.list.use), ncol = 2))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result[,1]<- c(rep(mod.vec, each = 1000), rep(mod.vec, each = 1000))
    
    start<- 1
    end.fixed<- 1000
    
    for(i in 1:length(files.list.use)) {
      end<- end.fixed*i
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      if(nrow(t1) < 1000) {
        result[c(start:end), 2]<- c(t1[,param], rep(NA, 1000-nrow(t1)))
      } else {
        result[c(start:end), 2]<- t1[,param]
      }
      start <- end+1
      print(i)
    }
    
    
    # Box plot
    full<- result[1:(nrow(result)/2),]
    names(full)<- c("model", "parameter")
    full$model<- factor(full$model, unique(full$model))
    
    par(mar = c(10.5,4,1.5,0.5))
    par(mfrow = c(1,2))
    boxplot(parameter ~ model, data = full, boxwex = 0.5, col = "grey", main = "Weak.Med.Full", xlab = "", ylim = c(-0.13, 0.25), ylab = "", horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    reduced<- result[(nrow(result)/2)+1:nrow(result),]
    names(reduced)<- c("model", "parameter")
    reduced$model<- factor(reduced$model, unique(reduced$model))
    
    boxplot(parameter ~ model, data = reduced, boxwex = 0.5, col = "grey", main = "Weak.Med.Red", xlab = "", ylab = "", ylim = c(-0.13, 0.25), horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    
    # Coarse
    # Order for plotting
    files.list.use<- c("5082014emm.ewc.bs.glmglobalmodresults.csv", "5082014emm.ewc.bs.autocovglobalmodresults.csv", "5082014emm.ewc.bs.spev.negexpglobalmodresults.csv", "5082014emm.ewc.bs.mle2.negexp.freeglobalmodresults.csv", "5082014emm.ewc.bs.mle2.gauss.freeglobalmodresults.csv", "5082014emm.ewc.bs.gamsglobalmodresults.csv", "5082014emm.ewc.bs.gamteglobalmodresults.csv", "6282014emm.ewc.bs.geeglobalmodresults.csv", 
                       #"5082014emm.ewc.bs.glmmglobalmodresults.csv", 
                       "5082014emm.ewc.bs.glm.waveletglobalmodresults.csv",
                       "5082014emm.ewc.bs.glm.nox2globalmodresults.csv", "5082014emm.ewc.bs.autocov.nox2globalmodresults.csv", "5082014emm.ewc.bs.spev.negexp.nox2globalmodresults.csv", "5082014emm.ewc.bs.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.ewc.bs.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.ewc.bs.gams.nox2globalmodresults.csv", "5082014emm.ewc.bs.gamte.nox2globalmodresults.csv",  "6282014emm.ewc.bs.gee.nox2globalmodresults.csv", 
                       #"5082014emm.ewc.bs.glmm.nox2globalmodresults.csv", 
                       "5082014emm.ewc.bs.glm.wavelet.nox2globalmodresults.csv")
    
    result<- data.frame(matrix(nrow = 1000*length(files.list.use), ncol = 2))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result[,1]<- c(rep(mod.vec, each = 1000), rep(mod.vec, each = 1000))
    
    start<- 1
    end.fixed<- 1000
    
    for(i in 1:length(files.list.use)) {
      end<- end.fixed*i
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      if(nrow(t1) < 1000) {
        result[c(start:end), 2]<- c(t1[,param], rep(NA, 1000-nrow(t1)))
      } else {
        result[c(start:end), 2]<- t1[,param]
      }
      start <- end+1
      print(i)
    }
    
    
    # Box plot
    full<- result[1:(nrow(result)/2),]
    names(full)<- c("model", "parameter")
    full$model<- factor(full$model, unique(full$model))
    
    par(mar = c(10.5,4,1.5,0.5))
    par(mfrow = c(1,2))
    boxplot(parameter ~ model, data = full, boxwex = 0.5, col = "grey", main = "Weak.Coarse.Full", xlab = "", ylim = c(-0.13, 0.25), ylab = "", horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    reduced<- result[(nrow(result)/2)+1:nrow(result),]
    names(reduced)<- c("model", "parameter")
    reduced$model<- factor(reduced$model, unique(reduced$model))
    
    boxplot(parameter ~ model, data = reduced, boxwex = 0.5, col = "grey", main = "Weak.Coarse.Red", xlab = "", ylab = "", ylim = c(-0.13, 0.25), horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
  }
  
  #### Moderate
  {
    dir.use<- "~/R/Allyn/PhD/SAC/Results07222014/ParamHypoPrediction/Mod/"
    
    files.list<- list.files(dir.use, c("modresults.csv$"))
    
    # What parameter?
    param<- 5 # Look at distribution of x1
    
    # Fine
    # Order for plotting
    files.list.use<- c("5082014emm.emf.bf.glmglobalmodresults.csv", "5082014emm.emf.bf.autocovglobalmodresults.csv", "5082014emm.emf.bf.spev.negexpglobalmodresults.csv", "5082014emm.emf.bf.mle2.negexp.freeglobalmodresults.csv", "5082014emm.emf.bf.mle2.gauss.freeglobalmodresults.csv", "5082014emm.emf.bf.gamsglobalmodresults.csv", "5082014emm.emf.bf.gamteglobalmodresults.csv", "6282014emm.emf.bf.geeglobalmodresults.csv", 
                       #"5082014emm.emf.bf.glmmglobalmodresults.csv", 
                       "5082014emm.emf.bf.glm.waveletglobalmodresults.csv",
                       "5082014emm.emf.bf.glm.nox2globalmodresults.csv", "5082014emm.emf.bf.autocov.nox2globalmodresults.csv", "5082014emm.emf.bf.spev.negexp.nox2globalmodresults.csv", "5082014emm.emf.bf.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.emf.bf.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.emf.bf.gams.nox2globalmodresults.csv", "5082014emm.emf.bf.gamte.nox2globalmodresults.csv",  "6282014emm.emf.bf.gee.nox2globalmodresults.csv", 
                       #"5082014emm.emf.bf.glmm.nox2globalmodresults.csv", 
                       "5082014emm.emf.bf.glm.wavelet.nox2globalmodresults.csv")
    
    
    result<- data.frame(matrix(nrow = 1000*length(files.list.use), ncol = 2))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result[,1]<- c(rep(mod.vec, each = 1000), rep(mod.vec, each = 1000))
    
    start<- 1
    end.fixed<- 1000
    
    for(i in 1:length(files.list.use)) {
      end<- end.fixed*i
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      if(nrow(t1) < 1000) {
        result[c(start:end), 2]<- c(t1[,param], rep(NA, 1000-nrow(t1)))
      } else {
        result[c(start:end), 2]<- t1[,param]
      }
      start <- end+1
      print(i)
    }
    
    
    # Box plot
    full<- result[1:(nrow(result)/2),]
    names(full)<- c("model", "parameter")
    full$model<- factor(full$model, unique(full$model))
    
    par(mar = c(10.5,4,1.5,0.5))
    par(mfrow = c(1,2))
    boxplot(parameter ~ model, data = full, boxwex = 0.5, col = "grey", main = "Mod.Fine.Full", xlab = "", ylim = c(-0.13, 0.25), ylab = "", horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    reduced<- result[(nrow(result)/2)+1:nrow(result),]
    names(reduced)<- c("model", "parameter")
    reduced$model<- factor(reduced$model, unique(reduced$model))
    
    boxplot(parameter ~ model, data = reduced, boxwex = 0.5, col = "grey", main = "Mod.Fine.Red", xlab = "", ylab = "", ylim = c(-0.13, 0.25), horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    # Medium
    # Order for plotting
    files.list.use<- c("5082014emm.emm.bm.glmglobalmodresults.csv", "5082014emm.emm.bm.autocovglobalmodresults.csv", "5082014emm.emm.bm.spev.negexpglobalmodresults.csv", "5082014emm.emm.bm.mle2.negexp.freeglobalmodresults.csv", "5082014emm.emm.bm.mle2.gauss.freeglobalmodresults.csv", "5082014emm.emm.bm.gamsglobalmodresults.csv", "5082014emm.emm.bm.gamteglobalmodresults.csv", "6282014emm.emm.bm.geeglobalmodresults.csv", 
                       #"5082014emm.emm.bm.glmmglobalmodresults.csv", 
                       "5082014emm.emm.bm.glm.waveletglobalmodresults.csv",
                       "5082014emm.emm.bm.glm.nox2globalmodresults.csv", "5082014emm.emm.bm.autocov.nox2globalmodresults.csv", "5082014emm.emm.bm.spev.negexp.nox2globalmodresults.csv", "5082014emm.emm.bm.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.emm.bm.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.emm.bm.gams.nox2globalmodresults.csv", "5082014emm.emm.bm.gamte.nox2globalmodresults.csv",  "6282014emm.emm.bm.gee.nox2globalmodresults.csv", 
                       #"5082014emm.emm.bm.glmm.nox2globalmodresults.csv", 
                       "5082014emm.emm.bm.glm.wavelet.nox2globalmodresults.csv")
    
    
    result<- data.frame(matrix(nrow = 1000*length(files.list.use), ncol = 2))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result[,1]<- c(rep(mod.vec, each = 1000), rep(mod.vec, each = 1000))
    
    start<- 1
    end.fixed<- 1000
    
    for(i in 1:length(files.list.use)) {
      end<- end.fixed*i
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      if(nrow(t1) < 1000) {
        result[c(start:end), 2]<- c(t1[,param], rep(NA, 1000-nrow(t1)))
      } else {
        result[c(start:end), 2]<- t1[,param]
      }
      start <- end+1
      print(i)
    }
    
    
    # Box plot
    full<- result[1:(nrow(result)/2),]
    names(full)<- c("model", "parameter")
    full$model<- factor(full$model, unique(full$model))
    
    par(mar = c(10.5,4,1.5,0.5))
    par(mfrow = c(1,2))
    boxplot(parameter ~ model, data = full, boxwex = 0.5, col = "grey", main = "Mod.Med.Full", xlab = "", ylim = c(-0.13, 0.25), ylab = "", horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    reduced<- result[(nrow(result)/2)+1:nrow(result),]
    names(reduced)<- c("model", "parameter")
    reduced$model<- factor(reduced$model, unique(reduced$model))
    
    boxplot(parameter ~ model, data = reduced, boxwex = 0.5, col = "grey", main = "Mod.Med.Red", xlab = "", ylab = "", ylim = c(-0.13, 0.25), horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    
    # Coarse
    # Order for plotting
    files.list.use<- c("5082014emm.emc.bs.glmglobalmodresults.csv", "5082014emm.emc.bs.autocovglobalmodresults.csv", "5082014emm.emc.bs.spev.negexpglobalmodresults.csv", "5082014emm.emc.bs.mle2.negexp.freeglobalmodresults.csv", "5082014emm.emc.bs.mle2.gauss.freeglobalmodresults.csv", "5082014emm.emc.bs.gamsglobalmodresults.csv", "5082014emm.emc.bs.gamteglobalmodresults.csv", "6282014emm.emc.bs.geeglobalmodresults.csv", 
                       #"5082014emm.emc.bs.glmmglobalmodresults.csv", 
                       "5082014emm.emc.bs.glm.waveletglobalmodresults.csv",
                       "5082014emm.emc.bs.glm.nox2globalmodresults.csv", "5082014emm.emc.bs.autocov.nox2globalmodresults.csv", "5082014emm.emc.bs.spev.negexp.nox2globalmodresults.csv", "5082014emm.emc.bs.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.emc.bs.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.emc.bs.gams.nox2globalmodresults.csv", "5082014emm.emc.bs.gamte.nox2globalmodresults.csv",  "6282014emm.emc.bs.gee.nox2globalmodresults.csv", 
                       #"5082014emm.emc.bs.glmm.nox2globalmodresults.csv", 
                       "5082014emm.emc.bs.glm.wavelet.nox2globalmodresults.csv")
    
    
    result<- data.frame(matrix(nrow = 1000*length(files.list.use), ncol = 2))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result[,1]<- c(rep(mod.vec, each = 1000), rep(mod.vec, each = 1000))
    
    start<- 1
    end.fixed<- 1000
    
    for(i in 1:length(files.list.use)) {
      end<- end.fixed*i
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      if(nrow(t1) < 1000) {
        result[c(start:end), 2]<- c(t1[,param], rep(NA, 1000-nrow(t1)))
      } else {
        result[c(start:end), 2]<- t1[,param]
      }
      start <- end+1
      print(i)
    }
    
    
    # Box plot
    full<- result[1:(nrow(result)/2),]
    names(full)<- c("model", "parameter")
    full$model<- factor(full$model, unique(full$model))
    
    par(mar = c(10.5,4,1.5,0.5))
    par(mfrow = c(1,2))
    boxplot(parameter ~ model, data = full, boxwex = 0.5, col = "grey", main = "Mod.Coarse.Full", xlab = "", ylim = c(-0.13, 0.25), ylab = "", horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    reduced<- result[(nrow(result)/2)+1:nrow(result),]
    names(reduced)<- c("model", "parameter")
    reduced$model<- factor(reduced$model, unique(reduced$model))
    
    boxplot(parameter ~ model, data = reduced, boxwex = 0.5, col = "grey", main = "Mod.Coarse.Red", xlab = "", ylab = "", ylim = c(-0.13, 0.25), horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
  }
  
  #### Strong
  {
    dir.use<- "~/R/Allyn/PhD/SAC/Results07222014/ParamHypoPrediction/Strong/"
    
    files.list<- list.files(dir.use, c("modresults.csv$"))
    
    # What parameter?
    param<- 5 # Look at distribution of x1
    
    # Fine
    # Order for plotting
    files.list.use<- c("5082014emm.esf.bf.glmglobalmodresults.csv", "5082014emm.esf.bf.autocovglobalmodresults.csv", "5082014emm.esf.bf.spev.negexpglobalmodresults.csv", "5082014emm.esf.bf.mle2.negexp.freeglobalmodresults.csv", "5082014emm.esf.bf.mle2.gauss.freeglobalmodresults.csv", "5082014emm.esf.bf.gamsglobalmodresults.csv", "5082014emm.esf.bf.gamteglobalmodresults.csv", "6282014emm.esf.bf.geeglobalmodresults.csv", 
                       #"5082014emm.esf.bf.glmmglobalmodresults.csv", 
                       "5082014emm.esf.bf.glm.waveletglobalmodresults.csv",
                       "5082014emm.esf.bf.glm.nox2globalmodresults.csv", "5082014emm.esf.bf.autocov.nox2globalmodresults.csv", "5082014emm.esf.bf.spev.negexp.nox2globalmodresults.csv", "5082014emm.esf.bf.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.esf.bf.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.esf.bf.gams.nox2globalmodresults.csv", "5082014emm.esf.bf.gamte.nox2globalmodresults.csv",  "6282014emm.esf.bf.gee.nox2globalmodresults.csv", 
                       #"5082014emm.esf.bf.glmm.nox2globalmodresults.csv", 
                       "5082014emm.esf.bf.glm.wavelet.nox2globalmodresults.csv")
    
    
    result<- data.frame(matrix(nrow = 1000*length(files.list.use), ncol = 2))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result[,1]<- c(rep(mod.vec, each = 1000), rep(mod.vec, each = 1000))
    
    start<- 1
    end.fixed<- 1000
    
    for(i in 1:length(files.list.use)) {
      end<- end.fixed*i
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      if(nrow(t1) < 1000) {
        result[c(start:end), 2]<- c(t1[,param], rep(NA, 1000-nrow(t1)))
      } else {
        result[c(start:end), 2]<- t1[,param]
      }
      start <- end+1
      print(i)
    }
    
    
    # Box plot
    full<- result[1:(nrow(result)/2),]
    names(full)<- c("model", "parameter")
    full$model<- factor(full$model, unique(full$model))
    
    par(mar = c(10.5,4,1.5,0.5))
    par(mfrow = c(1,2))
    boxplot(parameter ~ model, data = full, boxwex = 0.5, col = "grey", main = "Strong.Fine.Full", xlab = "", ylim = c(-0.13, 0.25), ylab = "", horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    reduced<- result[(nrow(result)/2)+1:nrow(result),]
    names(reduced)<- c("model", "parameter")
    reduced$model<- factor(reduced$model, unique(reduced$model))
    
    boxplot(parameter ~ model, data = reduced, boxwex = 0.5, col = "grey", main = "Strong.Fine.Red", xlab = "", ylab = "", ylim = c(-0.13, 0.25), horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    # Medium
    # Order for plotting
    files.list.use<- c("5082014emm.esm.bm.glmglobalmodresults.csv", "5082014emm.esm.bm.autocovglobalmodresults.csv", "5082014emm.esm.bm.spev.negexpglobalmodresults.csv", "5082014emm.esm.bm.mle2.negexp.freeglobalmodresults.csv", "5082014emm.esm.bm.mle2.gauss.freeglobalmodresults.csv", "5082014emm.esm.bm.gamsglobalmodresults.csv", "5082014emm.esm.bm.gamteglobalmodresults.csv", "6282014emm.esm.bm.geeglobalmodresults.csv", 
                       #"5082014emm.esm.bm.glmmglobalmodresults.csv", 
                       "5082014emm.esm.bm.glm.waveletglobalmodresults.csv",
                       "5082014emm.esm.bm.glm.nox2globalmodresults.csv", "5082014emm.esm.bm.autocov.nox2globalmodresults.csv", "5082014emm.esm.bm.spev.negexp.nox2globalmodresults.csv", "5082014emm.esm.bm.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.esm.bm.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.esm.bm.gams.nox2globalmodresults.csv", "5082014emm.esm.bm.gamte.nox2globalmodresults.csv",  "6282014emm.esm.bm.gee.nox2globalmodresults.csv", 
                       #"5082014emm.esm.bm.glmm.nox2globalmodresults.csv", 
                       "5082014emm.esm.bm.glm.wavelet.nox2globalmodresults.csv")
    
    result<- data.frame(matrix(nrow = 1000*length(files.list.use), ncol = 2))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result[,1]<- c(rep(mod.vec, each = 1000), rep(mod.vec, each = 1000))
    
    start<- 1
    end.fixed<- 1000
    
    for(i in 1:length(files.list.use)) {
      end<- end.fixed*i
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      if(nrow(t1) < 1000) {
        result[c(start:end), 2]<- c(t1[,param], rep(NA, 1000-nrow(t1)))
      } else {
        result[c(start:end), 2]<- t1[,param]
      }
      start <- end+1
      print(i)
    }
    
    
    # Box plot
    full<- result[1:(nrow(result)/2),]
    names(full)<- c("model", "parameter")
    full$model<- factor(full$model, unique(full$model))
    
    par(mar = c(10.5,4,1.5,0.5))
    par(mfrow = c(1,2))
    boxplot(parameter ~ model, data = full, boxwex = 0.5, col = "grey", main = "Strong.Med.Full", xlab = "", ylim = c(-0.13, 0.25), ylab = "", horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    reduced<- result[(nrow(result)/2)+1:nrow(result),]
    names(reduced)<- c("model", "parameter")
    reduced$model<- factor(reduced$model, unique(reduced$model))
    
    boxplot(parameter ~ model, data = reduced, boxwex = 0.5, col = "grey", main = "Strong.Med.Red", xlab = "", ylab = "", ylim = c(-0.13, 0.25), horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    
    # Coarse
    # Order for plotting
    files.list.use<- c("5082014emm.esc.bs.glmglobalmodresults.csv", "5082014emm.esc.bs.autocovglobalmodresults.csv", "5082014emm.esc.bs.spev.negexpglobalmodresults.csv", "5082014emm.esc.bs.mle2.negexp.freeglobalmodresults.csv", "5082014emm.esc.bs.mle2.gauss.freeglobalmodresults.csv", "5082014emm.esc.bs.gamsglobalmodresults.csv", "5082014emm.esc.bs.gamteglobalmodresults.csv", "6282014emm.esc.bs.geeglobalmodresults.csv", 
                       #"5082014emm.esc.bs.glmmglobalmodresults.csv", 
                       "5082014emm.esc.bs.glm.waveletglobalmodresults.csv",
                       "5082014emm.esc.bs.glm.nox2globalmodresults.csv", "5082014emm.esc.bs.autocov.nox2globalmodresults.csv", "5082014emm.esc.bs.spev.negexp.nox2globalmodresults.csv", "5082014emm.esc.bs.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.esc.bs.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.esc.bs.gams.nox2globalmodresults.csv", "5082014emm.esc.bs.gamte.nox2globalmodresults.csv",  "6282014emm.esc.bs.gee.nox2globalmodresults.csv", 
                       #"5082014emm.esc.bs.glmm.nox2globalmodresults.csv", 
                       "5082014emm.esc.bs.glm.wavelet.nox2globalmodresults.csv")
    
    result<- data.frame(matrix(nrow = 1000*length(files.list.use), ncol = 2))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result[,1]<- c(rep(mod.vec, each = 1000), rep(mod.vec, each = 1000))
    
    start<- 1
    end.fixed<- 1000
    
    for(i in 1:length(files.list.use)) {
      end<- end.fixed*i
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      if(nrow(t1) < 1000) {
        result[c(start:end), 2]<- c(t1[,param], rep(NA, 1000-nrow(t1)))
      } else {
        result[c(start:end), 2]<- t1[,param]
      }
      start <- end+1
      print(i)
    }
    
    
    # Box plot
    full<- result[1:(nrow(result)/2),]
    names(full)<- c("model", "parameter")
    full$model<- factor(full$model, unique(full$model))
    
    par(mar = c(10.5,4,1.5,0.5))
    par(mfrow = c(1,2))
    boxplot(parameter ~ model, data = full, boxwex = 0.5, col = "grey", main = "Strong.Coarse.Full", xlab = "", ylim = c(-0.14, 0.25), ylab = "", horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
    
    reduced<- result[(nrow(result)/2)+1:nrow(result),]
    names(reduced)<- c("model", "parameter")
    reduced$model<- factor(reduced$model, unique(reduced$model))
    
    boxplot(parameter ~ model, data = reduced, boxwex = 0.5, col = "grey", main = "Strong.Coarse.Red", xlab = "", ylab = "", ylim = c(-0.14, 0.25), horizontal = FALSE, las = 2)
    abline(h = 0.025, lty = 2)
  }
}

###### Bias and Stdev vs. AUC or Correlogram, doesn't seem to work.
#### Weak
{
  dir.use<- "~/R/Allyn/PhD/SAC/Results07222014/ParamHypoPrediction/Weak/"
  
  files.list<- list.files(dir.use, c("modresults.csv$"))
  
  # Order for plotting
  files.list.use<- c(
    "5082014emm.ewf.bf.glmglobalmodresults.csv", "5082014emm.ewm.bm.glmglobalmodresults.csv", "5082014emm.ewc.bs.glmglobalmodresults.csv", 
    "5082014emm.ewf.bf.autocovglobalmodresults.csv", "5082014emm.ewm.bm.autocovglobalmodresults.csv", "5082014emm.ewc.bs.autocovglobalmodresults.csv",
    "5082014emm.ewf.bf.spev.negexpglobalmodresults.csv", "5082014emm.ewm.bm.spev.negexpglobalmodresults.csv", "5082014emm.ewc.bs.spev.negexpglobalmodresults.csv",
    "5082014emm.ewf.bf.mle2.negexp.freeglobalmodresults.csv",  "5082014emm.ewm.bm.mle2.negexp.freeglobalmodresults.csv", "5082014emm.ewc.bs.mle2.negexp.freeglobalmodresults.csv", 
    "5082014emm.ewf.bf.mle2.gauss.freeglobalmodresults.csv", "5082014emm.ewm.bm.mle2.gauss.freeglobalmodresults.csv", "5082014emm.ewc.bs.mle2.gauss.freeglobalmodresults.csv", 
    "5082014emm.ewf.bf.gamsglobalmodresults.csv", "5082014emm.ewm.bm.gamsglobalmodresults.csv",  "5082014emm.ewc.bs.gamsglobalmodresults.csv", 
    "5082014emm.ewf.bf.gamteglobalmodresults.csv",  "5082014emm.ewm.bm.gamteglobalmodresults.csv", "5082014emm.ewc.bs.gamteglobalmodresults.csv", 
    "6282014emm.ewf.bf.geeglobalmodresults.csv", "6282014emm.ewm.bm.geeglobalmodresults.csv", "6282014emm.ewc.bs.geeglobalmodresults.csv", 
    #"5082014emm.ewf.bf.glmmglobalmodresults.csv", #"5082014emm.ewm.bm.glmmglobalmodresults.csv", #"5082014emm.ewc.bs.glmmglobalmodresults.csv",
    "5082014emm.ewf.bf.glm.waveletglobalmodresults.csv", "5082014emm.ewm.bm.glm.waveletglobalmodresults.csv", "5082014emm.ewc.bs.glm.waveletglobalmodresults.csv")
  
  result.bias.w<- data.frame(matrix(nrow = length(files.list.use), ncol = 3))
  mod.vec<- c(rep("GLM", 3), rep("GLM.AC", 3), rep("GLM.SPEV", 3), rep("GLM.NB.NEGEXP", 3), rep("GLM.NB.GAUSS", 3), rep("GAM.S", 3), rep("GAM.TE", 3), rep( "GEE.FIX", 3),
              #rep("ICAR", 3),
              #rep("GLMM", 3),
              rep("WAVE", 3))
  scale.vec<- c(rep(c("F", "M", "C"), length(mod.vec)/3))
  mod.scale.vec<- paste(mod.vec, ".", scale.vec, sep = "")
  
  # Bias full dataset
  result.bias.w[,1]<- mod.scale.vec
  names(result.bias.w)<- c("model", "mean", "sd")
  
  # What parameter?
  param.a<- "x1" 
  
  # True value
  true.value<- 0.025
  
  for (i in 1:length(files.list.use)) {
    t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
    
    x1.index<- match(param.a, colnames(t1))
    x1.diff<- t1[,x1.index] - true.value
    
    # Store result
    result.bias.w[i,2]<- mean(x1.diff, na.rm = TRUE)
    result.bias.w[i,3]<- sd(x1.diff, na.rm = TRUE)
  }
  
  ## Plotting
  dat<- result.bias.w
  dat.auc<- result.weak.auc
  
  # Set up
  par(mar = c(5, 5, 1.5, 1)+0.1)
  plot(dat$mean[1:3], type="n", xlim = c(0, 4), ylim=c(-0.025, 0.01), ylab = "Mean Bias", xlab = "Correlogram AUC", las = 1, pch = 0, col = "black")
  
  par.use <- par("usr")
  rect(par.use[1], par.use[3], par.use[2], par.use[4], col = "gray87", border = "black")
  
  # GLM
  x.vec.start<- 1
  diff<- 18
  x.vec.plot<- c(dat.auc$auc[x.vec.start], dat.auc$auc[x.vec.start+diff], dat.auc$auc[x.vec.start+(diff*2)]) 
  
  lines(x.vec.plot, dat$mean[1:3], type="o", pch = 0, col = "black")
  
  # GLM AC
  x.vec.start<- 4
  diff<- 18
  x.vec.plot<- c(dat.auc$auc[x.vec.start], dat.auc$auc[x.vec.start+diff], dat.auc$auc[x.vec.start+(diff*2)]) 
  lines(x.vec.plot, dat$mean[4:6], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, pch = 1, col = "#e41a1c")
  
  # SPEV
  x.vec.start<- 5
  diff<- 18
  x.vec.plot<- c(dat.auc$auc[x.vec.start], dat.auc$auc[x.vec.start+diff], dat.auc$auc[x.vec.start+(diff*2)]) 
  lines(x.vec.plot, dat$mean[7:9], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, pch = 2, col = "#377eb8")
  
  # NB.NEG EXP
  x.vec.start<- 8
  diff<- 18
  x.vec.plot<- c(dat.auc$auc[x.vec.start], dat.auc$auc[x.vec.start+diff], dat.auc$auc[x.vec.start+(diff*2)]) 
  lines(x.vec.plot, dat$mean[10:12], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, pch = 3, col = "#4def4a")
  
  # NB.GAUSS
  x.vec.start<- 9
  diff<- 18
  x.vec.plot<- c(dat.auc$auc[x.vec.start], dat.auc$auc[x.vec.start+diff], dat.auc$auc[x.vec.start+(diff*2)]) 
  lines(x.vec.plot, dat$mean[13:15], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, pch = 4, col = "#984ea3")
  
  # GAM.S
  x.vec.start<- 2
  diff<- 18
  x.vec.plot<- c(dat.auc$auc[x.vec.start], dat.auc$auc[x.vec.start+diff], dat.auc$auc[x.vec.start+(diff*2)])
  lines(x.vec.plot, dat$mean[16:18], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, pch = 5, col = "#ff7f00")
  
  # GAM.TE
  x.vec.start<- 3
  diff<- 18
  x.vec.plot<- c(dat.auc$auc[x.vec.start], dat.auc$auc[x.vec.start+diff], dat.auc$auc[x.vec.start+(diff*2)])
  lines(x.vec.plot, dat$mean[19:21], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, pch = 6, col = "#ffff33")
  
  # GEE
  x.vec.start<- 6
  diff<- 18
  x.vec.plot<- c(dat.auc$auc[x.vec.start], dat.auc$auc[x.vec.start+diff], dat.auc$auc[x.vec.start+(diff*2)])
  lines(x.vec.plot, dat$mean[22:24], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, pch = 6, col = "#a65628")
  
  # iCAR
  # lines(dat$mean[19:21], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, pch = 6, col = "#ffff33")
  
  # WAVE
  x.vec.start<- 7
  diff<- 18
  x.vec.plot<- c(dat.auc$auc[x.vec.start], dat.auc$auc[x.vec.start+diff], dat.auc$auc[x.vec.start+(diff*2)])
  lines(x.vec.plot, dat$mean[25:27], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, pch = 6, col = "#f781bf")
  
  # # GLMM
  # lines(dat$mean[19:21], type="o", xaxt = "n", ylim=c(0.5,1), ylab = "AUC", las = 1, pch = 6, col = "#ffff33")
}

##### Difference in Odds Ratio 
{
  ## Weak
  {
    dir.use<- "~/R/Allyn/PhD/SAC/Results07222014/ParamHypoPrediction/Weak/"
    
    files.list<- list.files(dir.use, c("modresults.csv$"))
    
    # What parameter?
    param.a<- "x1" 
    
    # True odds
    true.value<- exp(0.025)
    
    # Fine
    # Order for plotting
    files.list.use<- c("5082014emm.ewf.bf.glmglobalmodresults.csv", "5082014emm.ewf.bf.autocovglobalmodresults.csv", "5082014emm.ewf.bf.spev.negexpglobalmodresults.csv", "5082014emm.ewf.bf.mle2.negexp.freeglobalmodresults.csv", "5082014emm.ewf.bf.mle2.gauss.freeglobalmodresults.csv", "5082014emm.ewf.bf.gamsglobalmodresults.csv", "5082014emm.ewf.bf.gamteglobalmodresults.csv", "6282014emm.ewf.bf.geeglobalmodresults.csv", 
                       #"5082014emm.ewf.bf.glmmglobalmodresults.csv", 
                       "5082014emm.ewf.bf.glm.waveletglobalmodresults.csv",
                       "5082014emm.ewf.bf.glm.nox2globalmodresults.csv", "5082014emm.ewf.bf.autocov.nox2globalmodresults.csv", "5082014emm.ewf.bf.spev.negexp.nox2globalmodresults.csv", "5082014emm.ewf.bf.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.ewf.bf.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.ewf.bf.gams.nox2globalmodresults.csv", "5082014emm.ewf.bf.gamte.nox2globalmodresults.csv",  "6282014emm.ewf.bf.gee.nox2globalmodresults.csv", 
                       #"5082014emm.ewf.bf.glmm.nox2globalmodresults.csv", 
                       "5082014emm.ewf.bf.glm.wavelet.nox2globalmodresults.csv")
    
    result.fine<- data.frame(matrix(nrow = length(files.list.use), ncol = 4))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result.fine[,1]<- c(rep(mod.vec, each = 1), rep(mod.vec, each = 1))
    names(result.fine)<- c("model", "mean", "sd", "obs") 
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- exp(t1[,x1.index]) - true.value
      result.fine[i, 2]<- mean(x1.diff, na.rm = TRUE)
      result.fine[i, 3]<- sd(x1.diff, na.rm = TRUE)
      result.fine[i, 4]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Med
    # Order for plotting
    files.list.use<- c("5082014emm.ewm.bm.glmglobalmodresults.csv", "5082014emm.ewm.bm.autocovglobalmodresults.csv", "5082014emm.ewm.bm.spev.negexpglobalmodresults.csv", "5082014emm.ewm.bm.mle2.negexp.freeglobalmodresults.csv", "5082014emm.ewm.bm.mle2.gauss.freeglobalmodresults.csv", "5082014emm.ewm.bm.gamsglobalmodresults.csv", "5082014emm.ewm.bm.gamteglobalmodresults.csv", "6282014emm.ewm.bm.geeglobalmodresults.csv", 
                       #"5082014emm.ewm.bm.glmmglobalmodresults.csv", 
                       "5082014emm.ewm.bm.glm.waveletglobalmodresults.csv",
                       "5082014emm.ewm.bm.glm.nox2globalmodresults.csv", "5082014emm.ewm.bm.autocov.nox2globalmodresults.csv", "5082014emm.ewm.bm.spev.negexp.nox2globalmodresults.csv", "5082014emm.ewm.bm.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.ewm.bm.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.ewm.bm.gams.nox2globalmodresults.csv", "5082014emm.ewm.bm.gamte.nox2globalmodresults.csv",  "6282014emm.ewm.bm.gee.nox2globalmodresults.csv", 
                       #"5082014emm.ewm.bm.glmm.nox2globalmodresults.csv", 
                       "5082014emm.ewm.bm.glm.wavelet.nox2globalmodresults.csv")
    
    
    result.med<- data.frame(matrix(nrow = length(files.list.use), ncol = 4))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result.med[,1]<- c(rep(mod.vec, each = 1), rep(mod.vec, each = 1))
    names(result.med)<- c("model", "mean", "sd", "obs") 
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- exp(t1[,x1.index]) - true.value 
      result.med[i, 2]<- mean(x1.diff, na.rm = TRUE)
      result.med[i, 3]<- sd(x1.diff, na.rm = TRUE)
      result.med[i, 4]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Coarse
    # Order for plotting
    files.list.use<- c("5082014emm.ewc.bs.glmglobalmodresults.csv", "5082014emm.ewc.bs.autocovglobalmodresults.csv", "5082014emm.ewc.bs.spev.negexpglobalmodresults.csv", "5082014emm.ewc.bs.mle2.negexp.freeglobalmodresults.csv", "5082014emm.ewc.bs.mle2.gauss.freeglobalmodresults.csv", "5082014emm.ewc.bs.gamsglobalmodresults.csv", "5082014emm.ewc.bs.gamteglobalmodresults.csv", "6282014emm.ewc.bs.geeglobalmodresults.csv", 
                       #"5082014emm.ewc.bs.glmmglobalmodresults.csv", 
                       "5082014emm.ewc.bs.glm.waveletglobalmodresults.csv",
                       "5082014emm.ewc.bs.glm.nox2globalmodresults.csv", "5082014emm.ewc.bs.autocov.nox2globalmodresults.csv", "5082014emm.ewc.bs.spev.negexp.nox2globalmodresults.csv", "5082014emm.ewc.bs.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.ewc.bs.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.ewc.bs.gams.nox2globalmodresults.csv", "5082014emm.ewc.bs.gamte.nox2globalmodresults.csv",  "6282014emm.ewc.bs.gee.nox2globalmodresults.csv", 
                       #"5082014emm.ewc.bs.glmm.nox2globalmodresults.csv", 
                       "5082014emm.ewc.bs.glm.wavelet.nox2globalmodresults.csv")
    
    result.coarse<- data.frame(matrix(nrow = length(files.list.use), ncol = 4))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result.coarse[,1]<- c(rep(mod.vec, each = 1), rep(mod.vec, each = 1))
    names(result.coarse)<- c("model", "mean", "sd", "obs") 
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- exp(t1[,x1.index]) - true.value 
      result.coarse[i, 2]<- mean(x1.diff, na.rm = TRUE)
      result.coarse[i, 3]<- sd(x1.diff, na.rm = TRUE)
      result.coarse[i, 4]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    ## Plotting
    # Full
    n.mods<- length(mod.vec)
    par(mar = c(10.5, 8, 1.5, 1))
    dat.full.x1<- data.frame(fine = result.fine[1:n.mods, 2], med = result.med[1:n.mods, 2], coarse = result.coarse[1:n.mods, 2])
    names.new<- rep(mod.vec, 3)
    bp<- barplot(as.matrix(dat.full.x1), space = c(0.05, 0.75), main="Weak full models", ylab= "", beside=TRUE, horiz = FALSE, width = 3.5, xlim = c(0,105), col = gray.colors(n.mods), ylim = c(-0.03, 0.03), names.arg = names.new, las = 2)
    title(ylab = "Difference in odds ratio", line = 4)
    sd.vec<- c(result.fine[1:n.mods, 3], result.med[1:n.mods,3], result.coarse[1:n.mods, 3])
    obs.vec<- c(result.fine[1:n.mods, 4], result.med[1:n.mods,4], result.coarse[1:n.mods, 4])
    error.bar(bp, as.matrix(dat.full.x1), 1.96*sd.vec/sqrt(obs.vec))
    
    
    # Reduced
    n.mods<- length(mod.vec)
    par(mar = c(10.5, 8, 1.5, 1))
    dat.full.xred<- data.frame(fine = result.fine[(n.mods+1):(n.mods*2), 2], med = result.med[(n.mods+1):(n.mods*2), 2], coarse = result.coarse[(n.mods+1):(n.mods*2), 2])
    names.new<- rep(mod.vec, 3)
    bp<- barplot(as.matrix(dat.full.xred), space = c(0.05, 0.75), main="Weak red models", ylab= "", beside=TRUE, horiz = FALSE, width = 3.5, xlim = c(0,105), col = gray.colors(n.mods), ylim = c(-0.03, 0.03), names.arg = names.new, las = 2)
    title(ylab = "Difference in odds ratio", line = 4)
    sd.vec<- c(result.fine[(n.mods+1):(n.mods*2), 3], result.med[(n.mods+1):(n.mods*2),3], result.coarse[(n.mods+1):(n.mods*2), 3])
    obs.vec<- c(result.fine[(n.mods+1):(n.mods*2), 4], result.med[(n.mods+1):(n.mods*2),4], result.coarse[(n.mods+1):(n.mods*2), 4])
    error.bar(bp, as.matrix(dat.full.xred), 1.96*sd.vec/sqrt(obs.vec))
  }
  
  ## Moderate
  {
    dir.use<- "~/R/Allyn/PhD/SAC/Results07222014/ParamHypoPrediction/Mod/"
    
    files.list<- list.files(dir.use, c("modresults.csv$"))
    
    # What parameter?
    param.a<- "x1" 
    
    # True value
    true.value<- exp(0.025)
    
    # Fine
    # Order for plotting
    files.list.use<- c("5082014emm.emf.bf.glmglobalmodresults.csv", "5082014emm.emf.bf.autocovglobalmodresults.csv", "5082014emm.emf.bf.spev.negexpglobalmodresults.csv", "5082014emm.emf.bf.mle2.negexp.freeglobalmodresults.csv", "5082014emm.emf.bf.mle2.gauss.freeglobalmodresults.csv", "5082014emm.emf.bf.gamsglobalmodresults.csv", "5082014emm.emf.bf.gamteglobalmodresults.csv", "6282014emm.emf.bf.geeglobalmodresults.csv", 
                       #"5082014emm.emf.bf.glmmglobalmodresults.csv", 
                       "5082014emm.emf.bf.glm.waveletglobalmodresults.csv",
                       "5082014emm.emf.bf.glm.nox2globalmodresults.csv", "5082014emm.emf.bf.autocov.nox2globalmodresults.csv", "5082014emm.emf.bf.spev.negexp.nox2globalmodresults.csv", "5082014emm.emf.bf.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.emf.bf.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.emf.bf.gams.nox2globalmodresults.csv", "5082014emm.emf.bf.gamte.nox2globalmodresults.csv",  "6282014emm.emf.bf.gee.nox2globalmodresults.csv", 
                       #"5082014emm.emf.bf.glmm.nox2globalmodresults.csv", 
                       "5082014emm.emf.bf.glm.wavelet.nox2globalmodresults.csv")
    
    result.fine<- data.frame(matrix(nrow = length(files.list.use), ncol = 4))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    
    result.fine[,1]<- c(rep(mod.vec, each = 1), rep(mod.vec, each = 1))
    names(result.fine)<- c("model", "mean", "sd", "obs") 
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- exp(t1[,x1.index]) - true.value
      result.fine[i, 2]<- mean(x1.diff, na.rm = TRUE)
      result.fine[i, 3]<- sd(x1.diff, na.rm = TRUE)
      result.fine[i, 4]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Med
    # Order for plotting
    files.list.use<- c("5082014emm.emm.bm.glmglobalmodresults.csv", "5082014emm.emm.bm.autocovglobalmodresults.csv", "5082014emm.emm.bm.spev.negexpglobalmodresults.csv", "5082014emm.emm.bm.mle2.negexp.freeglobalmodresults.csv", "5082014emm.emm.bm.mle2.gauss.freeglobalmodresults.csv", "5082014emm.emm.bm.gamsglobalmodresults.csv", "5082014emm.emm.bm.gamteglobalmodresults.csv", "6282014emm.emm.bm.geeglobalmodresults.csv", 
                       #"5082014emm.emm.bm.glmmglobalmodresults.csv", 
                       "5082014emm.emm.bm.glm.waveletglobalmodresults.csv",
                       "5082014emm.emm.bm.glm.nox2globalmodresults.csv", "5082014emm.emm.bm.autocov.nox2globalmodresults.csv", "5082014emm.emm.bm.spev.negexp.nox2globalmodresults.csv", "5082014emm.emm.bm.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.emm.bm.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.emm.bm.gams.nox2globalmodresults.csv", "5082014emm.emm.bm.gamte.nox2globalmodresults.csv",  "6282014emm.emm.bm.gee.nox2globalmodresults.csv", 
                       #"5082014emm.emm.bm.glmm.nox2globalmodresults.csv", 
                       "5082014emm.emm.bm.glm.wavelet.nox2globalmodresults.csv")
    
    result.med<- data.frame(matrix(nrow = length(files.list.use), ncol = 4))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result.med[,1]<- c(rep(mod.vec, each = 1), rep(mod.vec, each = 1))
    names(result.med)<- c("model", "mean", "sd", "obs") 
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- exp(t1[,x1.index]) - true.value
      result.med[i, 2]<- mean(x1.diff, na.rm = TRUE)
      result.med[i, 3]<- sd(x1.diff, na.rm = TRUE)
      result.med[i, 4]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    
    # Coarse
    # Order for plotting
    files.list.use<- c("5082014emm.emc.bs.glmglobalmodresults.csv", "5082014emm.emc.bs.autocovglobalmodresults.csv", "5082014emm.emc.bs.spev.negexpglobalmodresults.csv", "5082014emm.emc.bs.mle2.negexp.freeglobalmodresults.csv", "5082014emm.emc.bs.mle2.gauss.freeglobalmodresults.csv", "5082014emm.emc.bs.gamsglobalmodresults.csv", "5082014emm.emc.bs.gamteglobalmodresults.csv", "6282014emm.emc.bs.geeglobalmodresults.csv", 
                       #"5082014emm.emc.bs.glmmglobalmodresults.csv", 
                       "5082014emm.emc.bs.glm.waveletglobalmodresults.csv",
                       "5082014emm.emc.bs.glm.nox2globalmodresults.csv", "5082014emm.emc.bs.autocov.nox2globalmodresults.csv", "5082014emm.emc.bs.spev.negexp.nox2globalmodresults.csv", "5082014emm.emc.bs.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.emc.bs.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.emc.bs.gams.nox2globalmodresults.csv", "5082014emm.emc.bs.gamte.nox2globalmodresults.csv",  "6282014emm.emc.bs.gee.nox2globalmodresults.csv", 
                       #"5082014emm.emc.bs.glmm.nox2globalmodresults.csv", 
                       "5082014emm.emc.bs.glm.wavelet.nox2globalmodresults.csv")
    
    
    result.coarse<- data.frame(matrix(nrow = length(files.list.use), ncol = 4))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result.coarse[,1]<- c(rep(mod.vec, each = 1), rep(mod.vec, each = 1))
    names(result.coarse)<- c("model", "mean", "sd", "obs") 
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- exp(t1[,x1.index]) - true.value
      result.coarse[i, 2]<- mean(x1.diff, na.rm = TRUE)
      result.coarse[i, 3]<- sd(x1.diff, na.rm = TRUE)
      result.coarse[i, 4]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    
    ## Plotting
    # Full
    n.mods<- length(mod.vec)
    par(mar = c(10.5, 8, 1.5, 1))
    dat.full.x1<- data.frame(fine = result.fine[1:n.mods, 2], med = result.med[1:n.mods, 2], coarse = result.coarse[1:n.mods, 2])
    names.new<- rep(mod.vec, 3)
    bp<- barplot(as.matrix(dat.full.x1), space = c(0.05, 0.75), main="Mod full models", ylab= "", beside=TRUE, horiz = FALSE, width = 3.5, xlim = c(0,105), col = gray.colors(n.mods), ylim = c(-0.03, 0.03), names.arg = names.new, las = 2)
    title(ylab = "Difference in odds ratio", line = 4)
    sd.vec<- c(result.fine[1:n.mods, 3], result.med[1:n.mods,3], result.coarse[1:n.mods, 3])
    obs.vec<- c(result.fine[1:n.mods, 4], result.med[1:n.mods,4], result.coarse[1:n.mods, 4])
    error.bar(bp, as.matrix(dat.full.x1), 1.96*sd.vec/sqrt(obs.vec))
    
    
    # Reduced
    n.mods<- length(mod.vec)
    par(mar = c(10.5, 8, 1.5, 1))
    dat.full.xred<- data.frame(fine = result.fine[(n.mods+1):(n.mods*2), 2], med = result.med[(n.mods+1):(n.mods*2), 2], coarse = result.coarse[(n.mods+1):(n.mods*2), 2])
    names.new<- rep(mod.vec, 3)
    bp<- barplot(as.matrix(dat.full.xred), space = c(0.05, 0.75), main="Mod red models", ylab= "", beside=TRUE, horiz = FALSE, width = 3.5, xlim = c(0,105), col = gray.colors(n.mods), ylim = c(-0.03, 0.03), names.arg = names.new, las = 2)
    title(ylab = "Difference in odds ratio", line = 4)
    sd.vec<- c(result.fine[(n.mods+1):(n.mods*2), 3], result.med[(n.mods+1):(n.mods*2),3], result.coarse[(n.mods+1):(n.mods*2), 3])
    obs.vec<- c(result.fine[(n.mods+1):(n.mods*2), 4], result.med[(n.mods+1):(n.mods*2),4], result.coarse[(n.mods+1):(n.mods*2), 4])
    error.bar(bp, as.matrix(dat.full.xred), 1.96*sd.vec/sqrt(obs.vec))
  }
  
  ## Strong
  {
    dir.use<- "~/R/Allyn/PhD/SAC/Results07222014/ParamHypoPrediction/Strong/"
    
    files.list<- list.files(dir.use, c("modresults.csv$"))
    
    # What parameter?
    param.a<- "x1" 
    
    # True value
    true.value<- exp(0.025)
    
    # Fine
    # Order for plotting
    files.list.use<- c("5082014emm.esf.bf.glmglobalmodresults.csv", "5082014emm.esf.bf.autocovglobalmodresults.csv", "5082014emm.esf.bf.spev.negexpglobalmodresults.csv", "5082014emm.esf.bf.mle2.negexp.freeglobalmodresults.csv", "5082014emm.esf.bf.mle2.gauss.freeglobalmodresults.csv", "5082014emm.esf.bf.gamsglobalmodresults.csv", "5082014emm.esf.bf.gamteglobalmodresults.csv", "6282014emm.esf.bf.geeglobalmodresults.csv", 
                       #"5082014emm.esf.bf.glmmglobalmodresults.csv", 
                       "5082014emm.esf.bf.glm.waveletglobalmodresults.csv",
                       "5082014emm.esf.bf.glm.nox2globalmodresults.csv", "5082014emm.esf.bf.autocov.nox2globalmodresults.csv", "5082014emm.esf.bf.spev.negexp.nox2globalmodresults.csv", "5082014emm.esf.bf.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.esf.bf.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.esf.bf.gams.nox2globalmodresults.csv", "5082014emm.esf.bf.gamte.nox2globalmodresults.csv",  "6282014emm.esf.bf.gee.nox2globalmodresults.csv", 
                       #"5082014emm.esf.bf.glmm.nox2globalmodresults.csv", 
                       "5082014emm.esf.bf.glm.wavelet.nox2globalmodresults.csv")
    
    
    result.fine[,1]<- c(rep(mod.vec, each = 1), rep(mod.vec, each = 1))
    names(result.fine)<- c("model", "mean", "sd", "obs") 
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- exp(t1[,x1.index]) - true.value
      result.fine[i, 2]<- mean(x1.diff, na.rm = TRUE)
      result.fine[i, 3]<- sd(x1.diff, na.rm = TRUE)
      result.fine[i, 4]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Med
    # Order for plotting
    files.list.use<- c("5082014emm.esm.bm.glmglobalmodresults.csv", "5082014emm.esm.bm.autocovglobalmodresults.csv", "5082014emm.esm.bm.spev.negexpglobalmodresults.csv", "5082014emm.esm.bm.mle2.negexp.freeglobalmodresults.csv", "5082014emm.esm.bm.mle2.gauss.freeglobalmodresults.csv", "5082014emm.esm.bm.gamsglobalmodresults.csv", "5082014emm.esm.bm.gamteglobalmodresults.csv", "6282014emm.esm.bm.geeglobalmodresults.csv", 
                       #"5082014emm.esm.bm.glmmglobalmodresults.csv", 
                       "5082014emm.esm.bm.glm.waveletglobalmodresults.csv",
                       "5082014emm.esm.bm.glm.nox2globalmodresults.csv", "5082014emm.esm.bm.autocov.nox2globalmodresults.csv", "5082014emm.esm.bm.spev.negexp.nox2globalmodresults.csv", "5082014emm.esm.bm.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.esm.bm.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.esm.bm.gams.nox2globalmodresults.csv", "5082014emm.esm.bm.gamte.nox2globalmodresults.csv",  "6282014emm.esm.bm.gee.nox2globalmodresults.csv", 
                       #"5082014emm.esm.bm.glmm.nox2globalmodresults.csv", 
                       "5082014emm.esm.bm.glm.wavelet.nox2globalmodresults.csv")
    
    result.med<- data.frame(matrix(nrow = length(files.list.use), ncol = 4))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result.med[,1]<- c(rep(mod.vec, each = 1), rep(mod.vec, each = 1))
    names(result.med)<- c("model", "mean", "sd", "obs") 
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- exp(t1[,x1.index]) - true.value
      result.med[i, 2]<- mean(x1.diff, na.rm = TRUE)
      result.med[i, 3]<- sd(x1.diff, na.rm = TRUE)
      result.med[i, 4]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    # Coarse
    # Order for plotting
    files.list.use<- c("5082014emm.esc.bs.glmglobalmodresults.csv", "5082014emm.esc.bs.autocovglobalmodresults.csv", "5082014emm.esc.bs.spev.negexpglobalmodresults.csv", "5082014emm.esc.bs.mle2.negexp.freeglobalmodresults.csv", "5082014emm.esc.bs.mle2.gauss.freeglobalmodresults.csv", "5082014emm.esc.bs.gamsglobalmodresults.csv", "5082014emm.esc.bs.gamteglobalmodresults.csv", "6282014emm.esc.bs.geeglobalmodresults.csv", 
                       #"5082014emm.esc.bs.glmmglobalmodresults.csv", 
                       "5082014emm.esc.bs.glm.waveletglobalmodresults.csv",
                       "5082014emm.esc.bs.glm.nox2globalmodresults.csv", "5082014emm.esc.bs.autocov.nox2globalmodresults.csv", "5082014emm.esc.bs.spev.negexp.nox2globalmodresults.csv", "5082014emm.esc.bs.mle2.negexp.free.nox2globalmodresults.csv", "5082014emm.esc.bs.mle2.gauss.free.nox2globalmodresults.csv", "5082014emm.esc.bs.gams.nox2globalmodresults.csv", "5082014emm.esc.bs.gamte.nox2globalmodresults.csv",  "6282014emm.esc.bs.gee.nox2globalmodresults.csv", 
                       #"5082014emm.esc.bs.glmm.nox2globalmodresults.csv", 
                       "5082014emm.esc.bs.glm.wavelet.nox2globalmodresults.csv")
    
    result.coarse<- data.frame(matrix(nrow = length(files.list.use), ncol = 4))
    mod.vec<- c("GLM", "GLM.AC", "GLM.SPEV", "GLM.NB.NEGEXP", "GLM.NB.GAUSS", "GAM.S", "GAM.TE", "GEE.FIX",
                #"ICAR",
                #"GLMM",
                "WAVE")
    result.coarse[,1]<- c(rep(mod.vec, each = 1), rep(mod.vec, each = 1))
    names(result.coarse)<- c("model", "mean", "sd", "obs") 
    
    for(i in 1:length(files.list.use)) {
      t1<- read.csv(paste(dir.use, files.list.use[i], sep = ""))
      x1.index<- match(param.a, colnames(t1))
      x1.diff<- exp(t1[,x1.index]) - true.value
      result.coarse[i, 2]<- mean(x1.diff, na.rm = TRUE)
      result.coarse[i, 3]<- sd(x1.diff, na.rm = TRUE)
      result.coarse[i, 4]<- 1000-(sum(is.na(t1[,x1.index])))
      print(i)
    }
    
    ## Plotting
    # Full
    n.mods<- length(mod.vec)
    par(mar = c(10.5, 8, 1.5, 1))
    dat.full.x1<- data.frame(fine = result.fine[1:n.mods, 2], med = result.med[1:n.mods, 2], coarse = result.coarse[1:n.mods, 2])
    names.new<- rep(mod.vec, 3)
    bp<- barplot(as.matrix(dat.full.x1), space = c(0.05, 0.75), main="Strong full models", ylab= "", beside=TRUE, horiz = FALSE, width = 3.5, xlim = c(0,105), col = gray.colors(n.mods), ylim = c(-0.03, 0.03), names.arg = names.new, las = 2)
    title(ylab = "Difference in odds ratio", line = 4)
    sd.vec<- c(result.fine[1:n.mods, 3], result.med[1:n.mods,3], result.coarse[1:n.mods, 3])
    obs.vec<- c(result.fine[1:n.mods, 4], result.med[1:n.mods,4], result.coarse[1:n.mods, 4])
    error.bar(bp, as.matrix(dat.full.x1), 1.96*sd.vec/sqrt(obs.vec))
    
    
    # Reduced
    n.mods<- length(mod.vec)
    par(mar = c(10.5, 8, 1.5, 1))
    dat.full.xred<- data.frame(fine = result.fine[(n.mods+1):(n.mods*2), 2], med = result.med[(n.mods+1):(n.mods*2), 2], coarse = result.coarse[(n.mods+1):(n.mods*2), 2])
    names.new<- rep(mod.vec, 3)
    bp<- barplot(as.matrix(dat.full.xred), space = c(0.05, 0.75), main="Strong red models", ylab= "", beside=TRUE, horiz = FALSE, width = 3.5, xlim = c(0,105), col = gray.colors(n.mods), ylim = c(-0.03, 0.03), names.arg = names.new, las = 2)
    title(ylab = "Difference in odds ratio", line = 4)
    sd.vec<- c(result.fine[(n.mods+1):(n.mods*2), 3], result.med[(n.mods+1):(n.mods*2),3], result.coarse[(n.mods+1):(n.mods*2), 3])
    obs.vec<- c(result.fine[(n.mods+1):(n.mods*2), 4], result.med[(n.mods+1):(n.mods*2),4], result.coarse[(n.mods+1):(n.mods*2), 4])
    error.bar(bp, as.matrix(dat.full.xred), 1.96*sd.vec/sqrt(obs.vec))
  }
}


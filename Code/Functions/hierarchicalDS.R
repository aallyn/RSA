# This example simulates and analyzes data from 10 transects and one study site; however # the time to run is still about ~1 day, so it is recommended NOT to actually run it but do use "data(sim.data)" instead 

S<-10
set.seed(12345)
n.real.transects<- S
n.transects<- S #each transect spans two cells

#generate Observers matrix
Observers<- matrix(NA,2,n.transects)
Obs.cov<- array(0,dim=c(2,n.transects,1))
Obs.cov[1,,]<-1
n.obs.cov<- 0

for(i in 1:n.real.transects)

Observers[,i]=sample(c(1,2),size=2,replace=FALSE)

# provide levels of for each covariate of type "Factor"
Levels<- list(Observer=c("1","2"), Distance=c("1","2","3","4","5"), Species=c("1","2"))
n.species<-1
Out<- simulate_data(S=S, Observers=Observers, misID=FALSE, n.species=1, Beta.hab=matrix(c(log(20),1),1,2), X.site=cbin)
Dat<- Out$Dat
Mapping<- c(1:S)
Area.trans<- rep(1,S)
n.bins<- length(unique(Dat[,"Distance"]))
Area.hab<- rep(1,S)
Bin.length<- rep(1,n.bins)
Adj<- linear_adj(S)

Hab.cov<- data.frame(matrix(log(c(1:S)/S),ncol=1))

#covariate on abundance intensity
colnames(Hab.cov)<- c("Cov1")
Hab.formula<- list(~Cov1)
Det.formula<- ~Observer+Distance+Group
misID.mat<- NULL
misID.models<- NULL
   
#set priors for individual covariates
Cov.prior.parms<- array(0,dim=c(n.species,2,1))
Cov.prior.parms[1,,1]<- c(1.1,1)
Cov.prior.fixed<- matrix(0,n.species,dim(Cov.prior.parms)[3])
Cov.prior.pdf<- Cov.prior.fixed
Cov.prior.pdf[1]="pois1"
Cov.prior.n=matrix(2,1,1)

pol.eff<- NULL #not currently used since using distance bins
point.ind<- TRUE
spat.ind<- TRUE #dont’ include spatial dependence unles there really is spatial structure!
fix.tau.nu<- FALSE
srr<- FALSE
srr.tol<- 0.2
misID<- FALSE
grps<- TRUE

#set initial values for M (max number of groups allowed in a given transect)
M<- t(Out$G.true*3)
M[which(M<20)]<- 20
Control<- list(iter=30100, burnin=100, thin=10, MH.cor=0.2, MH.nu=matrix(.1,n.species,n.transects), MH.misID=NULL, RJ.N 
linear_adj 11
#provide initial values for habitat parameters to improve stability
hab<- matrix(0,n.species,2) #covariates are intercept, index,
hab[1,]<- c(log(20),0)
Inits=list(hab=hab,tau.nu=c(100))

#additional prior parameters
Prior.pars=list(a.eta=1,b.eta=.01,a.nu=1,b.nu=.01,beta.sd=100) #(1,.01) prior makes it closer to a uniform distrib adapt=TRUE
set.seed(8327329) #chain1
#set.seed(8327330) #chain2 

# CAUTION: this next line takes several hours to run! Recommend using data(sim_data) instead
Out<- hierarchical_DS(Dat=Dat, Adj=Adj, Area.hab=Area.hab, Mapping=Mapping, Area.trans=Area.trans, Observers=Observer data(simdata)

#look at some plots and summary statistics
plot_obs_pred(simdata)

#table(simdata$MCMC,a=0.025)
dens.plot(simdata$MCMC) 

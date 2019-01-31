cv.concordance<-function(use.data,avail.data,formula,n.vars,paired=FALSE,
  v=10,bins=NULL,plot=TRUE){

#load library dependencies
library(epiR) 

##################################################################
cc.bins<-function(bins){
  
  #create storage objects
  bin.number<-as.data.frame(seq(1,bins))
  names(bin.number)<-'bin.id'
  bin.mids<-seq(1/(bins*2),1-(1/(bins*2)),by=1/(bins))
  bin.ranges<-seq(0,1,by=1/bins)
  
  #tally validation points by bin
  bin.id<-cut(valid.out,breaks=bin.ranges,labels=FALSE)    
  temp<-as.data.frame(table(bin.id))
  temp$bin.id<-as.numeric(as.character(temp$bin.id))
  temp<-merge(temp,bin.number,all=TRUE)
  temp$Freq[is.na(temp$Freq)]<-0
  N.obs<-temp$Freq
  
  #tally available points by bin
  bin.id<-cut(avail.out,breaks=bin.ranges,labels=FALSE)    
  temp<-as.data.frame(table(bin.id))
  temp$bin.id<-as.numeric(as.character(temp$bin.id))
  temp<-merge(temp,bin.number,all=TRUE)
  temp$Freq[is.na(temp$Freq)]<-0
  N.avail<-temp$Freq
  
  #compute expected counts by bin
  denom<-sum(bin.mids*N.avail)
  N.exp<-((bin.mids*N.avail)/denom)*sum(N.obs)
  
  #check sums
  stopifnot(sum(N.obs)==nrow(use.data))
  stopifnot(round(sum(N.exp),0)==sum(N.obs))
  
  #compute coefficient of concordance
  cc<-epi.ccc(N.exp,N.obs,ci="z-transform",conf.level=0.95)$rho.c
  
  #create list object
  z<-list(v,bins,bin.mids,N.obs,N.avail,N.exp,cc)
  names(z)<-c('v-folds','number of bins','bin midpoint probabilities',
    'observed counts','available counts','expected counts',
    'coefficient of concordance')
  return(z)
  
}
##################################################################

#shuffle data
use.data<-use.data[sample(1:nrow(use.data),replace=FALSE),]
avail.data<-avail.data[sample(1:nrow(avail.data),replace=FALSE),]

#add subset indictor to datasets
if(v>1) use.data$v<-cut(1:nrow(use.data),breaks=v,labels=FALSE)
  else use.data$v<-1
if(v>1) avail.data$v<-cut(1:nrow(avail.data),breaks=v,labels=FALSE)
  else avail.data$v<-1

#create storage objects
if(paired==TRUE){
  coefs<-matrix(NA,nrow=v,ncol=n.vars)
  se.coefs<-matrix(NA,nrow=v,ncol=n.vars)
  }
else{
  coefs<-matrix(NA,nrow=v,ncol=n.vars+1)
  se.coefs<-matrix(NA,nrow=v,ncol=n.vars+1)
  }
valid.out<-NULL
avail.out<-NULL

#conduct v-fold cross-validation
for(i in 1:v){ 
  
  #for paired logistic regression
  if(paired==TRUE){
    
    #create training data subset
    if(v>1) train.use<-use.data[use.data$v!=i,]    
    else train.use<-use.data
    
    #fit glm model to training data subset
    fit<-glm(model.form,data=train.use,family='binomial')  
    
    #save coefficients and standard errors
    coefs[i,]<-coef(fit)
    Vcov<-vcov(fit,useScale=F)
    se.coefs[i,]<-sqrt(diag(Vcov))
    
    #create validation dataset
    if(v>1) valid.use<-use.data[use.data$v==i,]
    else valid.use<-use.data
    
    #predict response for validation use points
    mm<-model.matrix(terms(fit),valid.use)
    newdata=mm %*% coef(fit)
    valid.out<-c(valid.out,plogis(newdata))
    
    #predict response for available points
    mm<-model.matrix(terms(fit),avail.data)
    newdata=mm %*% coef(fit)
    avail.out<-c(avail.out,plogis(newdata))
        
    }
  
  #for unpaired logistic regression
  else{
    
    #create training datasets
    if(v>1){
      train.use<-use.data[use.data$v!=i,]
      train.avail<-avail.data[avail.data$v!=i,]
      train<-rbind(train.use,train.avail)
      }
    else{
      train.use<-use.data
      train.avail<-avail.data
      train<-rbind(train.use,train.avail)
      }
    
    #fit glm model
    fit<-glm(model.form,data=train,family='binomial')  
    
    #save coefficients and standard errors
    coefs[i,]<-coef(fit)
    Vcov<-vcov(fit,useScale=F)
    se.coefs[i,]<-sqrt(diag(Vcov))
    
    #create validation dataset
    if(v>1) valid.use<-use.data[use.data$v==i,]
    else valid.use<-use.data
    
    #predict response for validation use points
    mm<-model.matrix(terms(fit),valid.use)
    newdata=mm %*% coef(fit)
    valid.out<-c(valid.out,plogis(newdata))
    
    #predict response for available points
    mm<-model.matrix(terms(fit),avail.data)
    newdata=mm %*% coef(fit)
    avail.out<-c(avail.out,plogis(newdata))
        
    }
  }  

#brute force optimization of bin number
if(is.null(bins)){
  
  #create objects
  bin.seq<-seq(5,20)
  cc.out<-matrix(NA,nrow=length(bin.seq),ncol=3)
  cc.results<-vector('list',length=length(bin.seq))

  #loop over bins
  for(i in 1:length(bin.seq)){
    cc.results[[i]]<-cc.bins(bins=bin.seq[i])
    cc.out[i,]<-as.numeric(cc.results[[i]][[7]])
    }

  #find bin number for max cc
  cc.out<-as.data.frame(cbind(bin.seq,cc.out))
  names(cc.out)<-c('bins','est','lower','upper')
  
  #create output
  z<-cc.results[cc.out$est==max(cc.out$est)]
  z<-z[[1]]
  
  }

#user specified bin number
else{

  z<-cc.bins(bins=bins)  
  
  }

#plot observed vs expected counts
if(plot==TRUE){
  N.exp<-as.numeric(z[[6]])
  N.obs<-as.numeric(z[[4]])
  plot(N.exp,N.obs,pch=19,main='Observed vs Expected Counts',
    xlab='Expected frequency',ylab='Observed frequency')
  abline(0,1,lty=2,lwd=2)
  text(mean(N.exp),max(N.obs),
    paste("Concord. coef. = ",round(z[[7]][1],3),'; ',
      v,'-fold; ',bins,' bins',sep=''))
  }

return(z)
}

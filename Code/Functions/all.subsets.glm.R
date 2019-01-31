# all.subsets function for generalized linear models using glm
# Kevin McGarigal 26 March 2008 original script
# Kevin McGarigal 20 Sept 2011 added 'force' argument for forcing terms in all models
# Kevin McGarigal 1 Oct 2011 added option for ranking models based on false positive error rate
#  and including squared terms as predictors
# Kevin McGarigal 9 Oct 2011 added option for ranking models based on Kappa and included option
#  for including squared terms as forced predictors
# Ethan Plunkett Nov 3 2011  
#	- added argument paired.quadratic.x which cause squared and linear terms
#  for each vairalbe to be included or exclueded as a pair. 
#	-  changed the suffix for squared variables to ".sq" 
#	-  entire storage objects are now  allocated prior to the loop rather than 
#   incrementally exteneded each time through the loop (which copies the whole object each iteration)


all.subsets.glm <-
  function(y,x,force=NULL,family=binomial(link=logit),offset=NULL,weights=NULL,
           quadratic.x=FALSE,quadratic.force=FALSE,maxp=5,select='AIC',sensitivity=.95,delta.AIC=7,
           delta.FP=.1,delta.Kappa=.1,rank=10,varimp=FALSE,gof='logLik',coef.table=FALSE, paired.quadratic.x=FALSE,...){
    
    owarn <- options("warn")
    on.exit(options(warn=owarn$warn))
    options(warn=-1)
    
    
    ################################################################################
    #functions
    ################################################################################
    
    #binary function (copied from wle library)
    binary<-function(x,dim){
      if(x==0){
        pos<-1
      } else {
        pos<-floor(log(x,2))+1
      }
      if(!missing(dim)){
        if(pos<=dim){
          pos<-dim
        } else {
          warning("the value of `dim` is too small")
        }
      }
      bin<-rep(0,pos)
      dicotomy<-rep(FALSE,pos)
      for(i in pos:1){
        bin[i]<-floor(x/2^(i-1))
        dicotomy[i]<-bin[i]==1
        x<-x-((2^(i-1))*bin[i])
      }
      return(dicotomy)
    } # end binary function
    
    #Kappa function
    cohen.kappa<-function(y){
      N<-sum(y)
      ccr<-sum(diag(y))/sum(y)
      p<-apply(y,1,sum)/N
      q<-apply(y,2,sum)/N
      num<-ccr-sum(p*q)
      den<-1-sum(p*q)
      k<-num/den
      k[k<0]<-0
      return(k)		
    }
    
    #Kappa optimization function
    kappa.opt<-function(threshold){
      obs<-out[,1]>0
      if(threshold==0){
        pred<-out[,2]>=threshold
      } else {
        pred<-out[,2]>threshold
      }
      temp<-c(sum(pred&obs),sum(!pred&obs),sum(pred&!obs),sum(!pred&!obs))
      temp<-as.table(matrix(temp,nrow=2))
      cohen.kappa(temp)
    }
    
    ################################################################################
    
    #add quadratic x terms and define the svars, the selection variable vector
    if(paired.quadratic.x){
      # selection variables will not include quadratic terms
      svars <- names(x) 
    }
    if(quadratic.x==TRUE || paired.quadratic.x){
      nvars<-ncol(x)
      for(i in 1:nvars){
        temp<-temp<-x[,i]^2
        x<-cbind(x,temp)
        names(x)[nvars+i]<-paste(names(x[i]),'.sq',sep='')
      }
    }
    if(!paired.quadratic.x){
      svars <- names(x) # selection variables will include quadratic
    }
    p <- length(svars)
    
    #add quadratic forced terms
    if(quadratic.force==TRUE){
      nvars<-ncol(force)
      for(i in 1:nvars){
        temp<-temp<-force[,i]^2
        force<-cbind(force,temp)
        names(force)[nvars+i]<-paste(names(force[i]),'.sq',sep='')
      }
      q<-ncol(force)
    }
    
    if(is.null(force)) q <- 0 else q <- ncol(force)
    
    # Calculate the final dimension of the storage objects
    ns <- 0 # number of subsets we will consider (if maxp == p this is equal to m)
    nc <- 0 # number of coefficients
    
    
    f <- ifelse(paired.quadratic.x, 2, 1)
    for(i in 1:min(maxp, p)){
      a <- choose(p, i)
      ns <- ns + a
      nc <- nc + a * (i * f + 1 + q)
    }
    
    #create parameters and pre-allocate storage objects
    m<-(2^p)-1  # number of subsets (disregarding maxp)
    N<-nrow(x)
    
    coefficients<- data.frame(character(nc), numeric(nc), numeric(nc), numeric(nc), numeric(nc), numeric(nc), stringsAsFactors=FALSE)
    names(coefficients) <- c("Term", "Estimate", "Std. Error", "z value", "Pr(>|z|)","id")
    
    model<- character(ns)
    id<- numeric(ns)
    AICc <-numeric(ns)
    D2 <-numeric(ns)
    cutpoint<-numeric(ns)
    if(select=='commission') FP <- numeric(ns)
    if(select == "Kappa") Kappa<- numeric(ns)
    fvars <- names(force)  # forced variables  (including squared forced variables if any)
    
    # assemble all the data in one dataframe
    d <- if(is.null(force)) d <- cbind(data.frame(y=y), x) else
      d <- cbind(data.frame(y=y), x, force) 
    
    svars.sq <- paste(svars, ".sq", sep="")
    
    #loop thru all subsets models
    oi <- 0 # output index
    cr <- 1 # the index of the first empty Coefficient table Row 
    for(i in 1:m){
      bin<-binary(i,p)
      if(sum(bin)> maxp) next
      oi <- oi+1 
      
      # Create a model formula	
      if(paired.quadratic.x){
        terms <- c(svars[bin], svars.sq[bin], fvars)
      } else {
        terms <- c(svars[bin], fvars)
      }
      formula <- as.formula(paste("y~", paste(terms, collapse="+"), sep=""))
      model.glm <- glm(formula, family=family,offset=offset,weights=weights,data=d)
      
      
      #save coefficient stats 
      if(varimp==TRUE | coef.table==TRUE){
        temp<-as.data.frame(summary(model.glm)$coefficients)
        Term<-row.names(temp)
        temp<-cbind(Term,temp, stringsAsFactors=FALSE)
        temp$id<-i
        coefficients[cr:(cr+nrow(temp)-1), ] <- temp[,]
        cr <- cr + nrow(temp)
      }
      
      #save model aic stats (always)
      model[oi]<- paste(terms,collapse='+')
      id[oi] <- i
      AIC <-round(model.glm$aic,3)
      K<-model.glm$rank
      AICc[oi]<- round(AIC+(2*K*(K+1)/(N-K-1)),3)
      d2<-(model.glm$null.deviance-model.glm$deviance)/model.glm$null.deviance
      D2[oi]<- round(d2,3)
      
      #compute errors of commission
      if(select=='commission'){
        if(length(unique(y))>2) stop('y must be binary (0,1) for select=commission')
        out<-as.data.frame(cbind(model.glm$y,model.glm$fitted.values))
        names(out)<-c('observed','fitted')
        cut<-quantile(out$fitted[out$observed==1],prob=1-sensitivity)
        cutpoint[oi]< cut
        FP[oi]<- round(nrow(out[out$fitted>=cut & out$observed==0,])/
                         nrow(out[out$observed==0,]),3)
      }
      
      #compute Kappa
      if(select=='Kappa'){
        if(length(unique(y))>2) stop('y must be binary (0,1) response for select=Kappa')
        out<-as.data.frame(cbind(model.glm$y,model.glm$fitted.values))
        names(out)<-c('observed','fitted')
        temp<-optimize(kappa.opt,interval=c(min(out$fitted),max(out$fitted)),maximum=TRUE)
        cutpoint[oi]<- temp$maximum
        Kappa[oi] <-round(temp$objective,3)
      }	
    } # end for loop
    
    #create model stats table
    if(select=='commission'){
      model.stats<-data.frame(id,model,D2,AICc,cutpoint,FP)
      model.stats$deltaAICc<-round(model.stats$AICc-min(model.stats$AICc),3)
      wgtAICc<-exp(-0.5*model.stats$deltaAICc)
      model.stats$wgtAICc<-round(wgtAICc/sum(wgtAICc),3)
      model.stats$deltaFP<-model.stats$FP-min(model.stats$FP)
      model.stats<-model.stats[order(model.stats$FP),]
      model.stats$rank<-seq(1,nrow(model.stats))
      model.stats<-model.stats[model.stats$deltaFP<=delta.FP & model.stats$rank<=rank,]
      model.stats<-model.stats[,c(1,2,3,4,7,8,5,6,9,10)]
    } else if(select=='Kappa'){
      model.stats<-data.frame(id,model,D2,AICc,cutpoint,Kappa)
      model.stats$deltaAICc<-round(model.stats$AICc-min(model.stats$AICc),3)
      wgtAICc<-exp(-0.5*model.stats$deltaAICc)
      model.stats$wgtAICc<-round(wgtAICc/sum(wgtAICc),3)
      model.stats$deltaKappa<-max(model.stats$Kappa)-model.stats$Kappa
      model.stats<-model.stats[order(model.stats$Kappa,decreasing=TRUE),]
      model.stats$rank<-seq(1,nrow(model.stats))
      model.stats<-model.stats[model.stats$deltaKappa<=delta.Kappa & model.stats$rank<=rank,]
      model.stats<-model.stats[,c(1,2,3,4,7,8,5,6,9,10)]
    } else{
      model.stats<-data.frame(id,model,D2,AICc)
      model.stats$deltaAICc<-model.stats$AICc-min(model.stats$AICc)
      wgtAICc<-exp(-0.5*model.stats$deltaAICc)
      model.stats$wgtAICc<-round(wgtAICc/sum(wgtAICc),3)
      model.stats<-model.stats[order(model.stats$AICc),]
      model.stats$rank<-seq(1,nrow(model.stats))
      model.stats<-model.stats[model.stats$deltaAICc<=delta.AIC & model.stats$rank<=rank,]
    }
    
    #create coefficients table
    if(varimp==TRUE | coef.table==TRUE){
      sv <- coefficients$id %in% model.stats$id
      coefficients<-coefficients[sv,]
      coefficients<-coefficients[order(coefficients$Term),]
      coefficients<-coefficients[coefficients$Term!='(Intercept)',]
    }
    
    #create variable importance table
    if(varimp==TRUE){
      z<-merge(coefficients,model.stats,by='id')
      N<-aggregate(rep(1, nrow(z)),list(z$Term),sum)
      names(N)<-c('Term','N')
      AICc<-aggregate(z$wgt,list(z$Term),sum)
      names(AICc)<-c('Term','AICc')
      varimport<-merge(N,AICc,by='Term')
      if(p<=12){
        require(hier.part)
        HP<-round(hier.part(y,x,family=family,gof=gof)$I.perc/100,3)
        HP$Term<-row.names(HP)
        names(HP)<-c('HP','Term')
        varimport<-merge(varimport,HP,by='Term')
      }
    }
    
    #final clean up of tables
    model.stats<-subset(model.stats,select=-id)
    if (varimp==TRUE | coef.table==TRUE) coefficients<-subset(coefficients,select=-id)
    
    rownames(coefficients) <- 1:nrow(coefficients)
    
    #save results to list
    if(coef.table==TRUE & varimp==TRUE)
      return(list(model.statistics=model.stats,coefficients=coefficients,
                  variable.importance=varimport))
    
    if(coef.table==TRUE & varimp==FALSE)
      return(list(model.statistics=model.stats,coefficients=coefficients))
    
    if(coef.table==FALSE & varimp==TRUE)
      return(list(model.statistics=model.stats,variable.importance=varimport))
    
    return(list(model.statistics=model.stats))
    
  } #end function

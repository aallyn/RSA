#########################################################################
#                                   WRMcode.txt                         #         
######################################################################### 
#
# Carl & Dormann & Kühn: A wavelet-based method to remove spatial 
# autocorrelation in the analysis of species distributional data
# Electronic supplementary material
# Functions used to do the analyses
# Codes are written for R (www.r-project.org)
# using package waveslim
# Gudrun Carl, 2012
#
# Autocorrelation correcting code using wavelets
# to calculate regression models
# for responses: "gaussian", "binomial"(binary) or "poisson"
# for spatial (2-dimensional) autocorrelation 
# in macroecological data (regular gridded datasets,
# grid cells are assumed to be square)
# 
#########################################################################



#########################################################################
WRM<-function(formula,family,data,coord,
              b.ini=NULL,eps=0.001,denom.eps=1e-20,itmax=200,
              level=1,wavelet="haar",plot=FALSE,graph=FALSE){
  #########################################################################
  # Arguments:
  # formula  with specified notation according to names in data frame
  # family   "gaussian", "binomial"(binary) or "poisson"
  # data     data frame 
  # coord    corresponding coordinates which have to be integer
  # level    0 for GLM
  #          1 for best autocorrelation removal
  #          higher integers possible (limit depends on sample size)
  # wavelet  "haar" is recommended
  # value:   WRM returns a list containing the following elements
  #          b          estimate of regression parameters
  #          s.e.       standard errors
  #          z          z values (or corresponding values for statistics)
  #          p          probabilities
  #          fitted     fitted values
  #          resid      Pearson residuals
  #          if plot or graph is true:
  #          ac.glm     autocorrelation of glm.residuals
  #          ac         autocorrelation of wavelet.residuals
  #########################################################################
  require(waveslim)
  n<-dim(data)[1]
  l<-dim(data)[2]
  x<-coord[,1]
  y<-coord[,2]
  if(length(x)!=n) stop("error in dimension")
  logic1<-identical(as.numeric(x),round(x,0))
  logic2<-identical(as.numeric(y),round(y,0))
  if(!logic1 | !logic2) stop("coordinates not integer")
  
  X<-model.matrix(formula,data)
  nvar<-dim(X)[2]
  
  if(is.vector(model.frame(formula,data)[[1]])){
    yold<-model.frame(formula,data)[[1]]
    ntr<-1
  }
  if(family=="binomial" & is.matrix(model.frame(formula,data)[[1]])){
    yold<-model.frame(formula,data)[[1]][,1]
    ntr<-model.frame(formula,data)[[1]][,1] +
      model.frame(formula,data)[[1]][,2]
  }
  
  n.level<-level
  length.s<-3*n.level+1         
  s<-rep(1,length.s)          
  s[length.s]<-0  
  if(level==0) {s<-c(1,1,1,1) ; n.level<-1}
  beta<-matrix(NA,4,nvar)
  resi<-matrix(NA,4,n)
  ac<-matrix(NA,4,10)
  acp<-matrix(NA,4,10)
  se<-matrix(NA,4,nvar)
  
  pdim<- max(max(y)-min(y),max(x)-min(x))*3/2
  power<-0
  while(2^power<pdim) power<-power+1
  xmargin0<-as.integer((2^power-(max(x)-min(x)))/2)-min(x)+1
  ymargin0<-as.integer((2^power-(max(y)-min(y)))/2)-min(y)+1
  
  if(power<n.level) stop("level is too high")
  
  i4<-1
  while(i4<5){     #......................................................
    if(i4==1){xmargin<-xmargin0 ; ymargin<-ymargin0}
    if(i4==2){xmargin<-xmargin0+1 ; ymargin<-ymargin0}
    if(i4==3){xmargin<-xmargin0+1 ; ymargin<-ymargin0+1}
    if(i4==4){xmargin<-xmargin0 ; ymargin<-ymargin0+1}
    
    # GLM for comparison
    m0<-glm(formula,family,data)
    res0<-resid(m0,type="pearson")
    beta0<-m0$coeff
    
    #-----------------------------------------------------------------------
    
    if(is.null(b.ini)) { betaw<-rep(0,nvar)}
    else {betaw<-b.ini}
    lin<-X%*%betaw
    if(family=="gaussian") pi<-lin
    if(family=="binomial") pi<-exp(lin)/(1+exp(lin))
    if(family=="poisson")  pi<-exp(lin)
    
    #if(family=="gaussian") pi<-rep(0,n)
    #if(family=="binomial") pi<-rep(.5,n)
    #if(family=="poisson") pi<-rep(1,n)
    
    it<-0
    
    repeat{
      it<-it+1
      if(family=="gaussian") var<-rep(1,n)
      if(family=="binomial") var<-ntr*pi*(1-pi)
      if(family=="poisson")  var<-pi
      sigma<-as.vector(sqrt(var))
      W12<-diag(sigma)
      Am12<-diag(1/sigma)
      Xnew<-W12%*%X
      ynew<-W12%*%X%*%betaw+Am12%*%(yold-ntr*pi)
      
      F<-matrix(0,2^power,2^power)
      T<-array(0,c(2^power,2^power,nvar))
      for(ii in 1:n){
        kx<-x[ii]+xmargin
        ky<-y[ii]+ymargin
        F[ky,kx]<-ynew[ii]
        for (i3 in 1:nvar)
          T[ky,kx,i3]<-Xnew[ii,i3]
      }  # ii loop
      
      p<-2^power*2^power
      tt<-matrix(0,p,nvar)
      if(is.na(max(abs(F)))) {mdwt$coeff<-rep(NA,nvar);break}
      if(is.infinite(max(abs(F)))) {mdwt$coeff<-rep(NA,nvar);break}
      FT<-mra.2d(F,wavelet,n.level,method="dwt")
      FTS <- FT[[1]]
      for(is in 2:length(s))
        if(s[is]==1) FTS <- FTS + FT[[is]]
      ft<-as.vector(FTS)
      for (i3 in 1:nvar){
        TT<-mra.2d(T[,,i3],wavelet,n.level,method="dwt")
        TTS <- TT[[1]]
        for(is in 2:length(s))
          if(s[is]==1) TTS <- TTS + TT[[is]]
        tt[,i3]<-as.vector(TTS)
      }
      
      xnam<-paste("tt[,",1:nvar,"]",sep="")
      formula.dwt<-as.formula(paste("ft~",paste(xnam,collapse="+"),"-1"))
      mdwt<-lm(formula.dwt)
      if(max(abs(mdwt$coeff))>1e+10 ) {mdwt$coeff<-rep(NA,nvar);break}
      
      lin<-X%*%mdwt$coeff
      if(family=="gaussian") pi<-lin
      if(family=="binomial") pi<-exp(lin)/(1+exp(lin))
      if(family=="poisson") {pi<-exp(lin)
                             if(min(pi)<1e-10 |max(pi)>1e+10) {mdwt$coeff<-rep(NA,nvar);break}
      }
      
      if (max(abs(mdwt$coeff-betaw)/(abs(betaw)+denom.eps)) <= eps ) {i4<-4 ; break}
      if (i4==4 & it > itmax)  stop("too many iterations")
      if (it > itmax) break
      betaw<-mdwt$coeff
    }  # next step of iteration
    
    
    if(!is.na(mdwt$coeff[1])){
      Resmdwt<-matrix(resid(mdwt),2^power,2^power)
      resmdwt<-rep(0,n)
      for(i in 1:n) resmdwt[i]<-Resmdwt[y[i]+ymargin,x[i]+xmargin]
      if(plot | graph) {
        acw<-acpart(x,y,resmdwt,resmdwt)
        acpw<-acpart(x,y,resmdwt,res0)
      }
      if(!plot & !graph) {acw<-NA; acpw<-NA}
      var.b<-solve(t(tt)%*%tt) 
      if(family=="gaussian"){
        df<-n-nvar
        sigma2<-sum(resmdwt^2)/df
        var.b<-sigma2*var.b
      }
      s.e.<-rep(NA,nvar)
      for(i in 1:nvar){
        s.e.[i]<-sqrt(var.b[i,i])
      }
    }
    
    if(is.na(mdwt$coeff[1])) {acw<-NA; acpw<-NA; resmdwt<-NA; s.e.<-NA}
    beta[i4,1:nvar]<-mdwt$coeff[1:nvar]
    resi[i4,1:n ]<-resmdwt[1:n]
    ac[i4,1:10]<-acw[1:10]
    acp[i4,1:10]<-acpw[1:10]
    se[i4,1:nvar]<-s.e.[1:nvar]
    
    #--------------------------------------------------------------------
    i4<-i4+1
  } # i4 loop #...........................................................
  
  glm.beta<-beta0
  wavelet.beta<-apply(beta,2,mean,na.rm=TRUE)
  if(plot | graph) ac0<-acpart(x,y,res0,res0)
  if(!plot & !graph) ac0<-NA
  acw<-apply(ac,2,mean,na.rm=TRUE)
  acpw<-apply(acp,2,mean,na.rm=TRUE)
  resw<-apply(resi,2,mean,na.rm=TRUE)
  s.e.<-apply(se,2,mean,na.rm=TRUE)
  lin<-X%*%wavelet.beta
  if(family=="gaussian") pi<-lin
  if(family=="binomial") pi<-exp(lin)/(1+exp(lin))
  if(family=="poisson")  pi<-exp(lin)
  
  
  # test statistics 
  
  z.value<-rep(NA,nvar)
  pr<-rep(NA,nvar)
  if(!is.na(wavelet.beta[1])) {
    z.value<-wavelet.beta/s.e.
    for(i in 1:nvar){
      if(family=="gaussian"){
        if(z.value[i]<=0) pr[i]<-2*pt(z.value[i],df)
        if(z.value[i]>0)  pr[i]<-2*(1-pt(z.value[i],df))
      }
      if(family=="binomial" | family=="poisson"){
        if(z.value[i]<=0) pr[i]<-2*pnorm(z.value[i])
        if(z.value[i]>0)  pr[i]<-2*(1-pnorm(z.value[i]))
      }
    }
  }   
  
  beta<-cbind(glm.beta,wavelet.beta,s.e.,z.value,pr)
  beta<-beta[,2:5]
  if(family=="gaussian")
    colnames(beta) <- c("Estimate", "Std.Err", "t value", "Pr(>|t|)")
  if(family=="binomial" | family=="poisson")
    colnames(beta) <- c("Estimate", "Std.Err", "z value", "Pr(>|z|)")
  #cat("---","\n","Coefficients:","\n")
  #printCoefmat(beta)
  
  if(plot){
    cat("---","\n")
    cat("Autocorrelation for glm.residuals","\n")
    #print(ac0)
    cat("Autocorrelation for wavelet.residuals","\n")
    #print(acw)
  }
  
  if(graph){
    y1<-min(min(ac0),min(acw))-.1
    y2<-max(max(ac0),max(acw))+.1
    plot(ac0,type="b",ylim=c(y1,y2),
         ylab="Autocorrelation of residuals", xlab="Lag distance",
         main=paste("Autocorrelation for level = ", level))
    points(acw,pch=2,type="b")
    v<-1:2
    leg<-c("glm.residuals","wavelet.residuals")
    legend(6,y2-.1,leg,pch=v)
  }
  
  coef<-as.vector(wavelet.beta)
  names(coef)<-dimnames(X)[[2]]
  
  call<-match.call()
  fit<-list(call=call,b=coef,s.e.=s.e.,z=z.value,p=pr,fitted=pi,
            resid=resw,w.ac=acpw,ac.glm=ac0,ac=acw)
  class(fit)<-"wrm"
  fit
}

########################################################################




########################################################################
# Function to calculate the parts of autocorrelation for residuals 
acpart<-function(x,y,fpart,ftotal){
  ########################################################################
  if(!(length(x)==length(y) & length(x)==length(fpart) 
       & length(x)==length(ftotal))) stop("error in acpart")
  fpart<-fpart-mean(fpart)
  ftotal<-ftotal-mean(ftotal)
  mi<-max(x)-min(x)+1
  mk<-max(y)-min(y)+1
  n<-max(mi,mk)
  n2<-n*n
  P<-matrix(0,n,n)
  T<-matrix(0,n,n)
  mask<-matrix(0,n,n)
  for(i in 1:length(x)){
    kx<-x[i]-min(x)+1
    ky<-y[i]-min(y)+1
    P[ky,kx]<-fpart[i]
    T[ky,kx]<-ftotal[i]
    mask[ky,kx]<-1}
  filter1<-matrix(0,n,n)
  filter1[1,1]<-1
  leng<-length(ftotal)
  ne<-convolve(convolve(T,filter1),T)[1,1]/leng
  n3<-3*n
  P0<-matrix(0,n3,n3)
  P1<-matrix(0,n3,n3)
  n1<-n+1
  nn<-n+n
  P0[n1:nn,n1:nn]<-P[1:n,1:n]
  P1[1:n,1:n]<-P[1:n,1:n]
  maske0<-matrix(0,n3,n3)
  maske1<-matrix(0,n3,n3)
  maske0[n1:nn,n1:nn]<-mask[1:n,1:n]
  maske1[1:n,1:n]<-mask[1:n,1:n]
  nx<-rep(1:n3,n3)
  ny<-as.numeric(gl(n3,n3))
  
  gr<-1
  gr1<-2
  h<-n*n3+n+1
  corr<-rep(0,10)
  kk<-0
  while(kk<10){
    kk<-kk+1
    filter<-matrix(0,n3,n3)
    for(i in 1:(n3*n3)) {
      d<-sqrt((nx[h]-nx[i])^2+(ny[h]-ny[i])^2) 
      
      if(d>=gr & d<gr1) filter[ny[i],nx[i]]<-1} 
    
    sum<-convolve(convolve(maske0,filter),maske1)[1,1]
    za<-convolve(convolve(P0,filter),P1)[1,1]/sum
    corr[kk]<-za/ne
    gr<-gr+1
    gr1<-gr1+1
  }
  corr<-as.vector(corr)
} 

########################################################################

########################################################################

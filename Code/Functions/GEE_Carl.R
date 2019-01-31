#########################################################################
#                         GEEcode.txt                                   #
#########################################################################  

# Carl & Kühn: Analyzing Spatial Autocorrelation in Species Distributions
# using Gaussian and Logit Models
# Electronic supplementary material
# Functions used to do the analyses
# Codes are written for R (www.r-project.org)
# using packages gee and geepack
# Gudrun Carl, 2006

# Autocorrelation correcting code using GEE
# (Generalized Estimating Equations)
# for responses: "gaussian", "binomial"(binary) or "poisson"
# for spatial (2-dimensional) autocorrelation 
# in macroecological data (regular gridded datasets,
# grid cells are assumed to be square)
#########################################################################


#########################################################################
GEE<-function(formula,family,data,coord,corstr="independence",
              cluster=3,plot=FALSE,graph=FALSE){
  #########################################################################
  # Arguments:
  # formula  with specified notation according to names in data frame
  # family   "gaussian", "binomial"(binary) or "poisson"
  # data     data frame
  # coord    corresponding coordinates which have to be integer
  # corstr   autocorrelation structure: "independence", "fixed",
  #          "exchangeable", "quadratic"  are possible.
  # cluster  cluster size for cluster models: "exchangeable", "quadratic".
  #          cluster= 2,3,4 are allowed for size 2*2,3*3,4*4, respectively 
  #
  # value:   GEE returns a list containing the following elements
  #          b       estimate of regression parameters
  #          s.e.    Standard errors
  #          z       z values (or corresponding values for statistics)
  #          p       probabilities
  #          scale   scale parameter (dispersion parameter)
  #          fitted  fitted values
  #          resid   standardized Pearson residuals 
  #          w.ac       working autocorrelation parameters
  #          W.ac       working autocorrelation matrix
  #          if plot or graph is true:
  #          ac.glm     autocorrelation of glm.residuals
  #          ac         autocorrelation of gee.residuals
  #########################################################################
  require(gee)
  require(geepack)
  at<-intersect(names(data),all.vars(formula))
  if(length(at)==0) stop("formula: specified notation is missing") 
  nn<-nrow(data)
  x<-coord[,1]
  y<-coord[,2]
  if(length(x)!=nn) stop("error in dimension")
  logic1<-identical(as.numeric(x),round(x,0))
  logic2<-identical(as.numeric(y),round(y,0))
  if(!logic1 | !logic2) stop("coordinates not integer")
  
  m0<-glm(formula,family,data)
  res0<-resid(m0,type="pearson")
  
  if(corstr=="independence"){
    ashort<-0
    A<-0
    fitted<-fitted(m0)
    resid<-resid(m0,type="pearson")
    b<-summary(m0)$coefficients[,1]
    s.e.<-summary(m0)$coefficients[,2]
    z<-summary(m0)$coefficients[,3]
    p<-summary(m0)$coefficients[,4]
    scale<-summary(m0)$dispersion
    beta<-cbind(b,s.e.,z,p)
    if(family=="gaussian")
      colnames(beta) <- c("Estimate", "Std.Err", "t", "p")
    if(family=="binomial" | family=="poisson")
      colnames(beta) <- c("Estimate", "Std.Err", "z", "p")
    
    cat("---","\n","Coefficients:","\n")
    printCoefmat(beta,has.Pvalue=TRUE, P.values = TRUE)
    cat("---","\n")
    cat("Estimated scale parameter: ",scale,"\n")
    cat("Working correlation parameter(s): ",ashort,"\n" )
  }
  
  if(corstr=="fixed"){
    ac01<-acfft(x,y,res0,lim1=1,lim2=1.1,dmax=1)  
    ac05<-acfft(x,y,res0,lim1=5,lim2=5.1,dmax=1)
    if(ac05<=0) v<-1
    if(ac05>0) v<-log(log(ac05)/log(ac01))/log(5)        
    alpha<-ac01
    para0<-paste("n",nn,sep="=")
    para1<-paste(", alpha",round(alpha,3),sep="=")
    para2<-paste(", v",round(v,3),sep="=")
    A0<-paste(para0,para1,para2)
    cat("Parameters: ",A0,"\n" )
    id<-rep(1,nn)
    coord<-cbind(x,y)
    D<-as.matrix(dist(coord))
    R<-alpha^(D^v)
    data<-data.frame(data,id)
    mgee<-gee(formula=formula,family=family,data=data,id=id,R=R,corstr="fixed")
    para3<-"a=alpha^(d^v) , "
    ashort<-c(alpha,v)
    A<-paste(para3,para1,para2)
    b<-mgee$coeff
    res<-res.gee(formula,family,data,nn,b=mgee$coeff,R=R)
    fitted<-res$fitted
    resid<-res$resid
    s.e.=summary(mgee)$coefficients[,2]
    z<-summary(mgee)$coefficients[,3]
    p<-rep(NA,nrow(summary(mgee)$coefficients))
    for(ii in 1:nrow(summary(mgee)$coefficients)){
      if(z[ii]>=0) p[ii]<-2*(1-pnorm(z[ii]))
      if(z[ii]<0) p[ii]<-2*(pnorm(z[ii]))
    }
    scale<-summary(mgee)[[9]]
    beta<-cbind(b,s.e.,z,p)
    colnames(beta) <- c("Estimate", "Std.Err", "z", "p")
    cat("---","\n","Coefficients:","\n")
    printCoefmat(beta,has.Pvalue=TRUE, P.values = TRUE)
    cat("---","\n")
    cat("Estimated scale parameter: ",scale,"\n")
    cat("Working correlation parameter(s): ",A,"\n" )
  }
  
  if(corstr=="exchangeable") {
    dato<-dat.nn(data,coord,cluster)
    l<-dim(dato)[2]
    o<-dato[,l-2]
    id<-dato[,l-1]
    waves<-dato[,l]
    
    clusz<-clus.sz(id)
    zcor<-genZcor(clusz=clusz,waves=waves,"unstr")
    mgee<-gee(formula=formula,family=family,data=dato,id=id,corstr="exchangeable")
    
    ashort<-mgee$w[1,2]
    a<-a.gee(mgee$w,cluster,type="gee",corstr="exchangeable")
    A<-mgee$w
    b<-mgee$coeff
    res<-res.gee(formula,family,dato,cluster,clusz,zcor,a,b)
    fitted<-res$fitted[order(o)]
    resid<-res$resid[order(o)]
    s.e.=summary(mgee)$coefficients[,4]
    z<-summary(mgee)$coefficients[,5]
    p<-rep(NA,nrow(summary(mgee)$coefficients))
    for(ii in 1:nrow(summary(mgee)$coefficients)){
      if(z[ii]>=0) p[ii]<-2*(1-pnorm(z[ii]))
      if(z[ii]<0) p[ii]<-2*(pnorm(z[ii]))
    }
    scale<-summary(mgee)[[9]]
    beta<-cbind(b,s.e.,z,p)
    colnames(beta) <- c("Estimate", "Std.Err", "z", "p")
    cat("---","\n","Coefficients:","\n")
    printCoefmat(beta,has.Pvalue=TRUE, P.values = TRUE)
    cat("---","\n")
    cat("Estimated scale parameter: ",scale,"\n")
    cat("Working correlation parameter(s): ",ashort,"\n" )
  }
  
  if(corstr=="quadratic"){
    dato<-dat.nn(data,coord,cluster)
    l<-dim(dato)[2]
    o<-dato[,l-2]
    id<-dato[,l-1]
    waves<-dato[,l]
    
    clusz<-clus.sz(id)
    zcor<-genZcor(clusz=clusz,waves=waves,"unstr")
    zcorq<-zcor.quad(zcor,cluster,quad=T)
    mgeese<-geese(formula=formula,family=family,data=dato,id=id,corstr=
                    "userdefined",zcor=zcorq)
    
    ashort<-mgeese$a
    a<-a.gee(mgeese$a,cluster,type="geese",corstr="userdefined",quad=T)
    A<-cor.mat(cluster,a)
    b<-mgeese$b
    res<-res.gee(formula,family,dato,cluster,clusz,zcor,a,b)
    fitted<-res$fitted[order(o)]
    resid<-res$resid[order(o)]
    s.e.=summary(mgeese)$mean[,2]
    z<-summary(mgeese)$mean[,3]
    p<-summary(mgeese)$mean[,4]
    scale<-as.numeric(summary(mgeese)$scale[1])
    beta<-cbind(b,s.e.,z,p)
    colnames(beta) <- c("Estimate", "Std.Err", "wald", "p")
    cat("---","\n","Coefficients:","\n")
    printCoefmat(beta,has.Pvalue=TRUE, P.values = TRUE)
    cat("---","\n")
    cat("Estimated scale parameter: ",scale,"\n")
    cat("Working correlation parameter(s)", "\n")
    print(ashort)
  }
  
  if(!plot & !graph) {ac0<-NA; ac<-NA}
  if(plot | graph) {
    x<-coord[,1]
    y<-coord[,2]
    ac0<-acfft(x,y,res0)
    ac<-acfft(x,y,resid)
  }
  
  if(plot){
    cat("---","\n")
    cat("Autocorrelation for glm.residuals","\n")
    print(ac0)
    cat("Autocorrelation for gee.residuals","\n")
    print(ac)
  }
  
  if(graph){
    y1<-min(min(ac0),min(ac))-.1
    y2<-max(max(ac0),max(ac))+.1
    plot(ac0,type="b",ylim=c(y1,y2),
         ylab="Autocorrelation of residuals", xlab="Lag distance",
         main=paste("Autocorrelation for corstr = ", corstr))
    points(ac,pch=2,type="b")
    v<-1:2
    leg<-c("glm.residuals","gee.residuals")
    legend(6,y2-.1,leg,pch=v)
  }
  
  call<-match.call()
  list(call=call,b=b,s.e.=s.e.,z=z,p=p,
       scale=scale,fitted=fitted,resid=resid,w.ac=ashort,W.ac=A,
       ac.glm=ac0,ac=ac)
}

#########################################################################
#########################################################################



#########################################################################
dat.nn<-function(data,coord,n){
  #########################################################################
  # A function to generate clusters and order variables and
  # to produce a data frame with response, predictors, coordinates, and 
  # 3 new parameters:
  # o for order
  # id for identifying clusters and 
  # waves for identifying members of clusters
  #
  # Arguments
  # data      a data frame with response and predictors and 
  # coord     two columns with corresponding cartesian coordinates
  # n         for maximal cluster size  n*n 
  #########################################################################
  
  l<-dim(data)[2]
  OST<-coord[,1]
  NORD<-coord[,2]
  ko<-OST-min(OST)
  idx<-(ko-(ko%%(n)))/n+1
  ks<-NORD-min(NORD)
  idy<-(ks-(ks%%(n)))/n+1
  ie<-(idy-1)*max(idx)+idx
  idwx<-ko%%(n)+1
  idwy<-ks%%(n)+1
  wav<-(idwy-1)*n+idwx
  data<-as.matrix(data)
  o<-order(ie,wav)
  x<-OST[o]
  y<-NORD[o]
  id<-ie[o]
  waves<-wav[o]
  dat.new1<-data[o,]
  dat.new2<-cbind(dat.new1,x,y,o,id,waves)
  dat.new<-as.data.frame(dat.new2)
}


#########################################################################
a.gee<-function(mgee,n,type="glm",corstr="independence",quad=T) {
  #########################################################################
  # A function to order correlation parameters of Generalized Estimating 
  # Equation Models
  # Arguments
  # mgee       matrix or vector of correlation parameters according to model
  # n          for maximal cluster size n*n
  # type       type of model 
  #            "glm", "gee", "geese" are allowed
  # corstr     correlation structure
  #            "independence", "exchangeable", "userdefined" are allowed
  # quad       by default quadratic correlation structure
  #            for model "geese" and "userdefined" correlation only
  #########################################################################
  
  if(n==2)n3<-6
  if(n==3)n3<-36
  if(n==4)n3<-120
  a<-rep(0,n3)
  if(type=="glm") a<-a
  if(type=="gee"){ 
    if(corstr=="exchangeable") a[c(1:n3)]<-mgee[1,2]
    if(corstr=="independence") a<-a
  }
  a<-as.vector(a)
  
  if(type=="geese") {
    if(corstr=="userdefined"){
      if(quad) {
        if(n==2)  {
          a<-rep(0,6)
          a[c(1,2,5,6)]<-mgee[1]
          a[c(3,4)]<-mgee[2]
        }
        if(n==3)  {
          a<-rep(0,36)
          a[c(1,3,9,11,18,22,24,27,29,33,34,36)]<-mgee[1]
          a[c(2,6,14,21,23,35)]<-mgee[2]
          a[c(4,10,12,17,25,28,30,32)]<-mgee[3]
          a[c(5,7,13,15,16,20,26,31)]<-mgee[4]  
          a[c(8,19)]<-mgee[5]	
        }
        if(n==4)  {
          a<-rep(0,120)
          a[c(1,4,16,19,30,33,46,55,58,66,69,76,79,88,93,96,100,103,106,109,
              114,115,118,120)]<-mgee[1]
          a[c(2,8,17,23,37,50,56,62,67,73,83,92,94,101,116,119)]<-mgee[2]
          a[c(3,12,27,41,54,57,95,117)]<-mgee[3]
          a[c(5,18,20,32,34,45,59,68,70,78,80,87,97,102,104,108,110,113)]<-mgee[4]
          a[c(6,9,21,22,24,31,36,38,44,49,60,63,71,72,74,77,82,84,86,91,98,
              105,107,112)]<-mgee[5]
          a[c(7,13,26,28,40,42,43,53,61,85,99,111)]<-mgee[6]
          a[c(10,25,35,48,64,75,81,90)]<-mgee[7]
          a[c(11,14,29,39,47,52,65,89)]<-mgee[8]
          a[c(15,51)]<-mgee[9]
        }}
      if(!quad) a<-mgee 
    }
    if(corstr=="exchangeable") a[c(1:n3)]<-mgee
    if(corstr=="independence") a<-a
  }
  a<-as.vector(a)
}

#########################################################################
clus.sz<-function(id){
  #########################################################################
  # A function to calculate sizes of clusters
  # Argument
  # id     vector which identifies the clusters
  #########################################################################
  
  clus<-rep(0,length(id))
  k0<-0
  k1<-1
  for(i in 2:length(id)) { i1<-i-1
                           if(id[i]==id[i1]) {k1<-k1+1
                                              if(i==length(id)) {k0<-k0+1
                                                                 clus[k0]<-k1}}
                           if(id[i]!=id[i1]) {k0<-k0+1
                                              clus[k0]<-k1
                                              k1<-1
                                              if(i==length(id)) {k0<-k0+1
                                                                 clus[k0]<-k1 }}}
  clusz<-clus[clus>0]
}

#########################################################################  
zcor.quad<-function(zcor,n,quad=TRUE) {
  #########################################################################
  # A function to create a quadratic correlation structure 
  # zcor    an object of class "genZcor" (see: geepack)
  # n       for maximal cluster size n*n
  # quad    by default quadratic correlation structure
  #########################################################################
  
  if(quad) {
    if(n==2)  {
      zcorn<-matrix(0,dim(zcor)[1],2)
      zcorn[,1]<-zcor[,1]+zcor[,2]+zcor[,5]+zcor[,6]
      zcorn[,2]<-zcor[,3]+zcor[,4]
    }
    if(n==3)  {
      zcorn<-matrix(0,dim(zcor)[1],5)
      zcorn[,1]<-zcor[,1]+zcor[,3]+zcor[,9]+zcor[,11]+zcor[,18]+zcor[,22]+
        zcor[,24]+zcor[,27]+zcor[,29]+zcor[,33]+zcor[,34]+zcor[,36]
      zcorn[,2]<-zcor[,2]+zcor[,6]+zcor[,14]+zcor[,21]+zcor[,23]+zcor[,35]
      zcorn[,3]<-zcor[,4]+zcor[,10]+zcor[,12]+zcor[,17]+zcor[,25]+zcor[,28]+
        zcor[,30]+zcor[,32]
      zcorn[,4]<-zcor[,5]+zcor[,7]+zcor[,13]+zcor[,15]+zcor[,16]+zcor[,20]+
        zcor[,26]+zcor[,31]
      zcorn[,5]<-zcor[,8]+zcor[,19]
    }
    if(n==4)  {
      zcorn<-matrix(0,dim(zcor)[1],9)
      zcorn[,1]<-zcor[,1]+zcor[,4]+zcor[,16]+zcor[,19]+zcor[,30]+zcor[,33]+
        zcor[,46]+zcor[,55]+zcor[,58]+zcor[,66]+zcor[,69]+zcor[,76]+
        zcor[,79]+zcor[,88]+zcor[,93]+zcor[,96]+zcor[,100]+zcor[,103]+
        zcor[,106]+zcor[,109]+zcor[,114]+zcor[,115]+zcor[,118]+zcor[,120]
      zcorn[,2]<-zcor[,2]+zcor[,8]+zcor[,17]+zcor[,23]+zcor[,37]+zcor[,50]+
        zcor[,56]+zcor[,62]+zcor[,67]+zcor[,73]+zcor[,83]+zcor[,92]+
        zcor[,94]+zcor[,101]+zcor[,116]+zcor[,119]
      zcorn[,3]<-zcor[,3]+zcor[,12]+zcor[,27]+zcor[,41]+zcor[,54]+zcor[,57]+
        zcor[,95]+zcor[,117]
      zcorn[,4]<-zcor[,5]+zcor[,18]+zcor[,20]+zcor[,32]+zcor[,34]+zcor[,45]+
        zcor[,59]+zcor[,68]+zcor[,70]+zcor[,78]+zcor[,80]+zcor[,87]+
        zcor[,97]+zcor[,102]+zcor[,104]+zcor[,108]+zcor[,110]+zcor[,113]
      zcorn[,5]<-zcor[,6]+zcor[,9]+zcor[,21]+zcor[,22]+zcor[,24]+zcor[,31]+
        zcor[,36]+zcor[,38]+zcor[,44]+zcor[,49]+zcor[,60]+zcor[,63]+
        zcor[,71]+zcor[,72]+zcor[,74]+zcor[,77]+zcor[,82]+zcor[,84]+
        zcor[,86]+zcor[,91]+zcor[,98]+zcor[,105]+zcor[,107]+zcor[,112]
      zcorn[,6]<-zcor[,7]+zcor[,13]+zcor[,26]+zcor[,28]+zcor[,40]+zcor[,42]+
        zcor[,43]+zcor[,53]+zcor[,61]+zcor[,85]+zcor[,99]+zcor[,111]
      zcorn[,7]<-zcor[,10]+zcor[,25]+zcor[,35]+zcor[,48]+zcor[,64]+zcor[,75]+
        zcor[,81]+zcor[,90]
      zcorn[,8]<-zcor[,11]+zcor[,14]+zcor[,29]+zcor[,39]+zcor[,47]+zcor[,52]+
        zcor[,65]+zcor[,89]
      zcorn[,9]<-zcor[,15]+zcor[,51]
    }
  } 
  if(!quad) zcorn<-zcor
  zcorn<-as.matrix(zcorn)
}

#########################################################################  
cor.mat<-function(cluster,a) {
  #########################################################################
  # A function to create a correlation matrix 
  #########################################################################
  n<-cluster
  n2<-cluster*cluster
  z2<-a
  v1<-matrix(0,n2,n2)
  if(n==2)
  {  v1[1,2:4]<-z2[1:3]
     v1[2,3:4]<-z2[4:5]
     v1[3,4]<-z2[6]  }
  if(n==3)
  {  v1[1,2:9]<-z2[1:8]
     v1[2,3:9]<-z2[9:15]
     v1[3,4:9]<-z2[16:21]
     v1[4,5:9]<-z2[22:26]
     v1[5,6:9]<-z2[27:30]
     v1[6,7:9]<-z2[31:33]
     v1[7,8:9]<-z2[34:35]
     v1[8,9]<-z2[36]  }
  if(n==4)
  {  v1[1,2:16]<-z2[1:15]
     v1[2,3:16]<-z2[16:29]
     v1[3,4:16]<-z2[30:42]
     v1[4,5:16]<-z2[43:54]
     v1[5,6:16]<-z2[55:65]
     v1[6,7:16]<-z2[66:75]
     v1[7,8:16]<-z2[76:84]
     v1[8,9:16]<-z2[85:92]
     v1[9,10:16]<-z2[93:99]
     v1[10,11:16]<-z2[100:105]
     v1[11,12:16]<-z2[106:110]
     v1[12,13:16]<-z2[111:114]
     v1[13,14:16]<-z2[115:117]
     v1[14,15:16]<-z2[118:119]
     v1[15,16]<-z2[120]   }
  for(i in 1:n2) v1[i,i]<-0.5
  v<-v1+t(v1)
  v
}
#########################################################################


#########################################################################
res.gee<-function(formula,family=gaussian,data,n,clusz=NA,zcor=NA,a=NA,b,
                  R=NA)  {
  #########################################################################
  # A function to calculate fitted values and residuals
  # for Generalized Estimating Equation Models 
  # for gaussian or binary data (with logit link) or Poisson data (log link)
  # Arguments
  # formula     a formula expression
  # family      "gaussian", "binomial", "poisson" are allowed
  #             "binomial" = binary
  # data        a data frame
  # n           for maximal cluster size n*n
  # clusz       an object of class "clus.sz"
  # zcor        an object of class "genZcor" 
  # a           a vector of correlation parameters 
  #             for clusters only
  #             as an object of class "a.gee"
  # b           a vector of regression parameters beta
  # R           a square matrix of correlation parameters
  #             for full dimension (=number of observations)  only
  #          
  #########################################################################
  
  l<-dim(data)[2]
  ieo<-data[,l-1]
  if(n!=dim(data)[1]) {
    n2<-n*n
    n3<-n2*(n2-1)/2
    n4<-n2-1
    n5<-n2-2
    for(i in 1:dim(zcor)[1]){
      for(k in 1:n3){
        if(zcor[i,k]==1) zcor[i,k]<-a[k]  }}
    lc<-length(clusz)
    z2<-matrix(0,lc,n3)
    for( j in 1:n3) {
      k3<-0
      k2<-0
      for(i in 1:lc) {
        if(clusz[i]!=1) {
          k2<-k3+1
          k3<-clusz[i]*(clusz[i]-1)/2+k3
          for(k in k2:k3)
            z2[i,j]<-zcor[k,j]+z2[i,j] }}}
    if(n==2)
      iod<-c(1,1,2)
    if(n==3)
      iod<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,4,4,4,4,5,5,5,6,6,7)
    if(n==4)
      iod<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,
             3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,
             6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,10,10,
             11,11,11,11,12,12,12,13,13,14)
    cs<-0
    v<-matrix(0,length(ieo),length(ieo))
    vgl<-rep(0,n2)
    for(i in 1:lc) {clu<-clusz[i]
                    if(clu!=1) {
                      v1<-matrix(0,n2,n2)
                      if(n==2)
                      {  v1[1,2:4]<-z2[i,1:3]
                         v1[2,3:4]<-z2[i,4:5]
                         v1[3,4]<-z2[i,6]  }
                      if(n==3)
                      {  v1[1,2:9]<-z2[i,1:8]
                         v1[2,3:9]<-z2[i,9:15]
                         v1[3,4:9]<-z2[i,16:21]
                         v1[4,5:9]<-z2[i,22:26]
                         v1[5,6:9]<-z2[i,27:30]
                         v1[6,7:9]<-z2[i,31:33]
                         v1[7,8:9]<-z2[i,34:35]
                         v1[8,9]<-z2[i,36]  }
                      if(n==4)
                      {  v1[1,2:16]<-z2[i,1:15]
                         v1[2,3:16]<-z2[i,16:29]
                         v1[3,4:16]<-z2[i,30:42]
                         v1[4,5:16]<-z2[i,43:54]
                         v1[5,6:16]<-z2[i,55:65]
                         v1[6,7:16]<-z2[i,66:75]
                         v1[7,8:16]<-z2[i,76:84]
                         v1[8,9:16]<-z2[i,85:92]
                         v1[9,10:16]<-z2[i,93:99]
                         v1[10,11:16]<-z2[i,100:105]
                         v1[11,12:16]<-z2[i,106:110]
                         v1[12,13:16]<-z2[i,111:114]
                         v1[13,14:16]<-z2[i,115:117]
                         v1[14,15:16]<-z2[i,118:119]
                         v1[15,16]<-z2[i,120]   }
                      for(i1 in 1:length(iod)) {
                        i2<-iod[i1]
                        if(var(v1[i2,1:n2])==0) {for(k in i2:n5) {k1<-k+1
                                                                  v1[k,]<-v1[k1,]
                                                                  v1[k1,]<-vgl[]}}}
                      for(i1 in 1:length(iod)){
                        i3<-iod[i1]+1
                        if(var(v1[1:n2,i3])==0) {for(k in i3:n4) {k1<-k+1
                                                                  v1[,k]<-v1[,k1]
                                                                  v1[,k1]<-vgl[]}}}
                      
                      clu1<-clu-1
                      for(k in 1:clu1) {csk<-cs+k
                                        f1<-2
                                        for(k1 in f1:clu) {k2<-cs+f1
                                                           v[csk,k2]<-v1[k,k1]
                                                           f1<-f1+1 }}
                      for(k in 1:clu) {csk<-cs+k
                                       v[csk,csk]<- 0.5 } }
                    if(clu==1) {cs1<-cs+1
                                v[cs1,cs1]<-0.5 }
                    cs<- cumsum(clusz)[i]  }
    v<-v+t(v)
  }
  if(n==dim(data)[1]) v<-R
  ww<-solve(v)
  
  s.geese<-svd(ww,LINPACK=T)
  d.geese<-diag(sqrt(s.geese$d))
  w<-s.geese$u%*%d.geese%*%t(s.geese$u)
  
  x.matrix<-model.matrix(formula,data)
  fitted<-x.matrix%*%b
  fitted<-fitted[1:length(ieo)] 
  if(family=="poisson") fitted<-exp(fitted)    
  if(family=="binomial") fitted<-exp(fitted)/(1+exp(fitted))
  
  if(family=="gaussian") rgeese<- model.frame(formula,data)[[1]]-fitted
  if(family=="poisson") rgeese<-( model.frame(formula,data)[[1]]-fitted)/sqrt(fitted)
  if(family=="binomial") rgeese<-( model.frame(formula,data)[[1]]-fitted)/sqrt(fitted*(1-fitted))  
  rsgeese<-w%*%rgeese
  resgeeseo<-rsgeese[1:length(ieo)]
  
  list(fitted=fitted,resid=resgeeseo)                           
}


#########################################################################
# Function for autocorrelation of residuals (reslm)
acfft<-function(x,y,reslm,lim1=1,lim2=2,dmax=10){
  #########################################################################
  reslm<-reslm-mean(reslm)
  mi<-max(x)-min(x)+1
  mk<-max(y)-min(y)+1
  n<-max(mi,mk)
  n2<-n*n
  Ares<-matrix(0,n,n)
  mask<-matrix(0,n,n)
  for(i in 1:length(x)){
    kx<-x[i]-min(x)+1
    ky<-y[i]-min(y)+1
    Ares[ky,kx]<-reslm[i]
    mask[ky,kx]<-1}
  filter1<-matrix(0,n,n)
  filter1[1,1]<-1
  leng<-length(reslm)
  ne<-convolve(convolve(Ares,filter1),Ares)[1,1]/leng
  n3<-3*n
  Ares0<-matrix(0,n3,n3)
  Ares1<-matrix(0,n3,n3)
  n1<-n+1
  nn<-n+n
  Ares0[n1:nn,n1:nn]<-Ares[1:n,1:n]
  Ares1[1:n,1:n]<-Ares[1:n,1:n]
  maske0<-matrix(0,n3,n3)
  maske1<-matrix(0,n3,n3)
  maske0[n1:nn,n1:nn]<-mask[1:n,1:n]
  maske1[1:n,1:n]<-mask[1:n,1:n]
  nx<-rep(1:n3,n3)
  ny<-as.numeric(gl(n3,n3))
  
  gr<-lim1
  gr1<-lim2
  h<-n*n3+n+1
  corr<-rep(0,dmax)
  kk<-0
  while(kk<dmax){
    kk<-kk+1
    filter<-matrix(0,n3,n3)
    for(i in 1:(n3*n3)) {
      d<-sqrt((nx[h]-nx[i])^2+(ny[h]-ny[i])^2) 
      
      if(d>=gr & d<gr1) filter[ny[i],nx[i]]<-1} 
    
    sum<-convolve(convolve(maske0,filter),maske1)[1,1]
    za<-convolve(convolve(Ares0,filter),Ares1)[1,1]/sum
    corr[kk]<-za/ne
    gr<-gr+(lim2-lim1)
    gr1<-gr1+(lim2-lim1)
  }
  corr<-as.vector(corr)
} 



#########################################################################
#########################################################################

##########################################################
#Two predictors -- 2d plots

logistic<-function(x,x2,a,b,c){
  exp(a+(b*x)+(c*x2))/(1 + (exp(a+(b*x)+(c*x2))))
}

#strong x1 and x2
curve(logistic(x,x2=-100,a=10,b=0.05,c=0.05),-200,0)
points(-100,0.5,col='red',cex=2,pch=19)
curve(logistic(x,x2=-150,a=10,b=0.05,c=0.05),-200,0,add=TRUE,lty=2)
curve(logistic(x,x2=-50,a=10,b=0.05,c=0.051),-200,0,add=TRUE,lty=2)

#mod x1 and x2
curve(logistic(x,x2=-100,a=5,b=0.025,c=0.025),-200,0)
points(-100,0.5,col='red',cex=2,pch=19)
curve(logistic(x,x2=-150,a=5,b=0.025,c=0.025),-200,0,add=TRUE,lty=2)
curve(logistic(x,x2=-50,a=5,b=0.025,c=0.025),-200,0,add=TRUE,lty=2)

#weak x1 and x2
curve(logistic(x,x2=-100,a=2,b=0.01,c=0.01),-200,0)
points(-100,0.5,col='red',cex=2,pch=19)
curve(logistic(x,x2=-150,a=2,b=0.01,c=0.01),-200,0,add=TRUE,lty=2)
curve(logistic(x,x2=-50,a=2,b=0.01,c=0.01),-200,0,add=TRUE,lty=2)

#### Mod X1|Vary X2
#mod x1 and weak x2
curve(logistic(x,x2=-100,a=3,b=0.025,c=0.005),-200,0)
points(-100,0.5,col='red',cex=2,pch=19)
curve(logistic(x,x2=-150,a=3,b=0.025,c=0.005),-200,0,add=TRUE,lty=2)
curve(logistic(x,x2=-50,a=3,b=0.025,c=0.005),-200,0,add=TRUE,lty=2)

#mod x1 and mod x2
curve(logistic(x,x2=-100,a=5,b=0.025,c=0.025),-200,0)
points(-100,0.5,col='red',cex=2,pch=19)
curve(logistic(x,x2=-150,a=5,b=0.025,c=0.025),-200,0,add=TRUE,lty=2)
curve(logistic(x,x2=-50,a=5,b=0.025,c=0.025),-200,0,add=TRUE,lty=2)

#mod x1 and strong x2
curve(logistic(x,x2=-100,a=7.5,b=0.025,c=0.05),-200,0,ylim=c(0,1))
points(-100,0.5,col='red',cex=2,pch=19)
curve(logistic(x,x2=-150,a=7.5,b=0.025,c=0.05),-200,0,add=TRUE,lty=2)
curve(logistic(x,x2=-50,a=7.5,b=0.025,c=0.05),-200,0,add=TRUE,lty=2)

##########################################################33
#two predictors -- 3d plots

logistic<-function(x1,x2,a,b,c){
  exp(a+ b*x1 + c*x2)/(1 + (exp(a+ b*x1 + c*x2)))
}

logit.surf<-function(a,b,c){
  pred.mat<-matrix(NA,nrow=200,ncol=200)
  x1vec<-seq(-200,-1)
  x2vec<-seq(-200,-1)
  for(i in 1:200){
    for(j in 1:200){
      pred.mat[i,j]<-logistic(x1vec[i],x2vec[j],a=a,b=b,c=c) 
      }
    }
  res<-persp(x=x1vec,y=x2vec,pred.mat,ticktype='detailed',phi=30,
        col='lightblue',shade=.1,zlim=c(0,1))   
  points(trans3d(-100,-100,0.5,pmat=res),col='red',cex=2,pch=19)
  }

#strong x1 and x2
logit.surf(a=10,b=0.05,c=0.05)

#weak x1 and x2
logit.surf(a=2,b=0.01,c=0.01)

#### Mod X1|Vary X2
#mod x1 and weak x2
logit.surf(a=3,b=0.025,c=0.005)

#mod x1 and x2
logit.surf(a=5,b=0.025,c=0.025)

#mod x1 and strong x2
logit.surf(a=7.5,b=0.025,c=0.05)



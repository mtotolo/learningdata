theta<-function(s) exp(s)/(1+exp(s))
#require(dplyr)
error<-function(df,w) {
#  df<- mutate(df, err=log(1+exp(-y*w%*%select(df,-y))))
  df$err<-log(1+exp(-df$y*(w[1]*df$x0+w[2]*df$x1+w[3]*df$x2)))
  return(sum(df$err)/nrow(df))
}

logreg<-function(N) {
  eta<-0.01
  x1<-runif(2,-1,1)
  x2<-runif(2,-1,1)
  myTh<-lm(x1~x2)
  train<-data.frame(x1=runif(N,-1,1),x2=runif(N,-1,1))
  train$y<-sign(train$x2-(myTh$coeff[1]+train$x1*myTh$coeff[2]))
  #ggplot(train,aes(x=x1,y=x2,col=as.factor(y)))+geom_point()
  w<-c(0,0,0)
  old_w<-c(1,1)
  j<-0
  while(sqrt((old_w[1]-w[1])^2+(old_w[2]-w[2])^2)>0.01){
    j<-j+1
    old_w<-w
    for(i in sample(N)) {
      w<-as.numeric(w+eta*train[i,]$y*train[i,c("x1","x2")]/(1+exp(train[i,]$y*
                                                  (w[1]*train[i,]$x1+w[2]*train[i,]$x2))))
    }
#    print(j)
#    print(error(train,w))
#    print(w)
#    print(sqrt((old_w[1]-w[1])^2+(old_w[2]-w[2])^2))
  }
  M=10000
  test<-data.frame(x1=runif(M,-1,1),x2=runif(M,-1,1))
  test$y<-sign(test$x2-(myTh$coeff[1]+test$x1*myTh$coeff[2]))
  return(error(test,w))
#  return(j)
}

euDist<-function(x,y) {
  dist<-0
  for (i in 1:length(x)) {
    dist<-dist+(x[i]-y[i])^2
  }
  return(sqrt(dist))
}

logreg2<-function(N) {
  eta<-0.01
  x1<-runif(2,-1,1)
  x2<-runif(2,-1,1)
  myTh<-lm(x1~x2)
  train<-data.frame(x0=rep(1,N),x1=runif(N,-1,1),x2=runif(N,-1,1))
  train$y<-sign(train$x2-(myTh$coeff[1]+train$x1*myTh$coeff[2]))
  #ggplot(train,aes(x=x1,y=x2,col=as.factor(y)))+geom_point()
  w<-c(0,0,0)
  old_w<-c(1,1,1)
  j<-0
  while(euDist(w,old_w)>0.01){
    j<-j+1
    old_w<-w
    for(i in sample(N)) {
      w<-as.numeric(w+eta*train[i,]$y*train[i,c("x0","x1","x2")]/(1+exp(train[i,]$y*
                                  (w[1]*train[i,]$x0+w[2]*train[i,]$x1+w[3]*train[i,]$x2))))
    }
#        print(j)
#        print(error(train,w))
#        print(w)
#        print(euDist(w,old_w))
  }
  M=10000
  test<-data.frame(x0=rep(1,M),x1=runif(M,-1,1),x2=runif(M,-1,1))
  test$y<-sign(test$x2-(myTh$coeff[1]+test$x1*myTh$coeff[2]))
  timestamp()
  #return(error(test,w))
  return(j)
}
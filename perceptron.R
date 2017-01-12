estimate<-function(N,M) {
      f<-data.frame(x1=runif(2,-1,1),x2=runif(2,-1,1))
      target<-lm(data = f,f$x2~f$x1)
      X<-data.frame(x1=1,x2=1,y=1)
      while(abs(sum(X$y))==nrow(X)) {
            X<-data.frame(x1=runif(N,-1,1),x2=runif(N,-1,1))
            X$y<-sign(X$x2-target$coefficients[2]*X$x1-target$coefficients[1])
      }
      w<-c(0,0,0)
      X$z<-sign(X$x1*w[1]+X$x2*w[2]+w[3])
      N=0
      while (sum(X$y!=X$z)>0) {
            N<-N+1
            if (length(which(X$z!=X$y))==1) i<-which(X$z!=X$y)
            else i<-sample(which(X$z!=X$y),1)
            w[1]<-w[1]+X$y[i]*X$x1[i]
            w[2]<-w[2]+X$y[i]*X$x2[i]
            w[3]<-w[3]+X$y[i]
            X$z<-sign(X$x1*w[1]+X$x2*w[2]+w[3])
      }
      
      Y<-data.frame(x1=runif(M,-1,1),x2=runif(M,-1,1))
      Y$y<-sign(Y$x2-target$coefficients[2]*Y$x1-target$coefficients[1])
      Y$z<-sign(Y$x1*w[1]+Y$x2*w[2]+w[3])
      return(sum(Y$y!=Y$z)/M)
}


PLA<-function(N) {
      f<-data.frame(x1=runif(2,-1,1),x2=runif(2,-1,1))
      target<-lm(data = f,f$x2~f$x1)
      X<-data.frame(x1=runif(N,-1,1),x2=runif(N,-1,1))
      X$y<-sign(X$x2-target$coefficients[2]*X$x1-target$coefficients[1])
 #     plot(x = X$x1,y=X$x2)
 #     points(f$x1,f$x2,pch=4)
 #     abline(a=target$coeff[1],b=target$coeff[2])
      
      w<-c(0,0,0)
      X$z<-sign(X$x1*w[1]+X$x2*w[2]+w[3])
      N=0
      while (sum(X$y!=X$z)>0) {
            N<-N+1
            if (length(which(X$z!=X$y))==1) i<-which(X$z!=X$y)
            else i<-sample(which(X$z!=X$y),1)
            w[1]<-w[1]+X$y[i]*X$x1[i]
            w[2]<-w[2]+X$y[i]*X$x2[i]
            w[3]<-w[3]+X$y[i]
            X$z<-sign(X$x1*w[1]+X$x2*w[2]+w[3])
      }
      return(N)
}

#mean(replicate(1000,{PLA(10)}))
#mean(replicate(1000,{PLA(100)}))
#mean(replicate(1000,{estimate(10,10000)}))
#mean(replicate(1000,{estimate(100,10000)}))


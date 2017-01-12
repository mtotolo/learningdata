linearQErrIn<-function(N) {

      X<-data.frame(x1=runif(N,-1,1),x2=runif(N,-1,1))
      X$y<-sign(X$x1^2+X$x2^2-0.6)
      flip<-sample(N,N/10)
      X$y[flip]<--X$y[flip]
      w<-c(0,0,0)
      XM<-matrix(c(X$x1,X$x2),ncol=2,byrow=F)
      XM<-cbind(XM,rep(1,N))
     
      
      w<- solve(t(XM)%*%XM)%*%t(XM)%*%X$y
      X$z<-sign(X$x1*w[1]+X$x2*w[2]+w[3])
      
   
      return(sum(X$y!=X$z)/N)
}
linearQ2ErrIn<-function(N) {
   
      X<-data.frame(x1=runif(N,-1,1),x2=runif(N,-1,1))
      X$y<-sign(X$x1^2+X$x2^2-0.6)
      flip<-sample(N,N/10)
      X$y[flip]<--X$y[flip]
      w<-c(0,0,0,0,0,0)
      XM<-matrix(c(X$x1,X$x2,X$x1*X$x2,X$x1^2,X$x2^2),ncol=5,byrow=F)
      XM<-cbind(XM,rep(1,N))
 
      
      w<- solve(t(XM)%*%XM)%*%t(XM)%*%X$y
      X$z<-sign(X$x1*w[1]+X$x2*w[2]+X$x1*X$x2*w[3]+X$x1^2*w[4]+X$x2^2*w[5]+w[6])
      X$g1<-sign(-X$x1*0.05+X$x2*0.08+X$x1*X$x2*0.13+X$x1^2*1.5+X$x2^2*1.5-1)
      X$g2<-sign(-X$x1*0.05+X$x2*0.08+X$x1*X$x2*0.13+X$x1^2*1.5+X$x2^2*15-1)
      X$g3<-sign(-X$x1*0.05+X$x2*0.08+X$x1*X$x2*0.13+X$x1^2*15+X$x2^2*1.5-1)
      X$g4<-sign(-X$x1*1.5+X$x2*0.08+X$x1*X$x2*0.13+X$x1^2*0.05+X$x2^2*0.05-1)
      X$g5<-sign(-X$x1*0.05+X$x2*0.08+X$x1*X$x2*1.5+X$x1^2*0.15+X$x2^2*0.15-1)
      score<-rep(0,5)
      score[1]<-sum(X$z==X$g1)/N
      score[2]<-sum(X$z==X$g2)/N
      score[3]<-sum(X$z==X$g3)/N
      score[4]<-sum(X$z==X$g4)/N
      score[5]<-sum(X$z==X$g5)/N
   return(which.max(score))
}

linearQ2ErrOut<-function(N,M) {
      X<-data.frame(x1=runif(N,-1,1),x2=runif(N,-1,1))
      X$y<-sign(X$x1^2+X$x2^2-0.6)
      flip<-sample(N,N/10)
      X$y[flip]<--X$y[flip]
      w<-c(0,0,0,0,0,0)
      XM<-matrix(c(X$x1,X$x2,X$x1*X$x2,X$x1^2,X$x2^2),ncol=5,byrow=F)
      XM<-cbind(XM,rep(1,N))
      w<- solve(t(XM)%*%XM)%*%t(XM)%*%X$y
      X$z<-sign(X$x1*w[1]+X$x2*w[2]+X$x1*X$x2*w[3]+X$x1^2*w[4]+X$x2^2*w[5]+w[6])
      Y<-data.frame(x1=runif(M,-1,1),x2=runif(M,-1,1))
      Y$y<-sign(Y$x1^2+Y$x2^2-0.6)
      flip<-sample(M,M/10)
      Y$y[flip]<--Y$y[flip]
      Y$z<-sign(Y$x1*w[1]+Y$x2*w[2]+Y$x1*Y$x2*w[3]+Y$x1^2*w[4]+Y$x2^2*w[5]+w[6])
      return(sum(Y$y!=Y$z)/M)
}



linearErrIn<-function(N) {
      f<-data.frame(x1=runif(2,-1,1),x2=runif(2,-1,1))
      target<-lm(data = f,f$x2~f$x1)
      X<-data.frame(x1=runif(N,-1,1),x2=runif(N,-1,1))
      X$y<--sign(X$x2-target$coefficients[2]*X$x1-target$coefficients[1])
      
      w<-c(0,0,0)
      XM<-matrix(c(X$x1,X$x2),ncol=2,byrow=F)
      XM<-cbind(XM,rep(1,N))
      w<- solve(t(XM)%*%XM)%*%t(XM)%*%X$y
      X$z<-sign(X$x1*w[1]+X$x2*w[2]+w[3])
      return(sum(X$y!=X$z)/N)
}

linearErrOut<-function(N,M) {
      f<-data.frame(x1=runif(2,-1,1),x2=runif(2,-1,1))
      target<-lm(data = f,f$x2~f$x1)
      X<-data.frame(x1=runif(N,-1,1),x2=runif(N,-1,1))
      X$y<--sign(X$x2-target$coefficients[2]*X$x1-target$coefficients[1])
      
      w<-c(0,0,0)
      XM<-matrix(c(X$x1,X$x2),ncol=2,byrow=F)
      XM<-cbind(XM,rep(1,N))
 
      w<- solve(t(XM)%*%XM)%*%t(XM)%*%X$y
      X$z<-sign(X$x1*w[1]+X$x2*w[2]+w[3])
      
      Y<-data.frame(x1=runif(M,-1,1),x2=runif(M,-1,1))
      Y$y<--sign(Y$x2-target$coefficients[2]*Y$x1-target$coefficients[1])
      Y$z<-sign(Y$x1*w[1]+Y$x2*w[2]+w[3])
      return(sum(Y$y!=Y$z)/M)
}



linear.PLA<-function(N) {
      f<-data.frame(x1=runif(2,-1,1),x2=runif(2,-1,1))
      target<-lm(data = f,f$x2~f$x1)
      X<-data.frame(x1=runif(N,-1,1),x2=runif(N,-1,1))
      X$y<-sign(X$x2-target$coefficients[2]*X$x1-target$coefficients[1])
      w<-c(0,0,0)
      XM<-matrix(c(X$x1,X$x2),ncol=2,byrow=F)
      XM<-cbind(XM,rep(1,N))
 
      
      w<- solve(t(XM)%*%XM)%*%t(XM)%*%X$y
      X$z<-sign(X$x1*w[1]+X$x2*w[2]+w[3])

      M=0
      while (sum(X$y!=X$z)>0) {
            M<-M+1
            if (length(which(X$z!=X$y))==1) i<-which(X$z!=X$y)
            else i<-sample(which(X$z!=X$y),1)
            w[1]<-w[1]+X$y[i]*X$x1[i]
            w[2]<-w[2]+X$y[i]*X$x2[i]
            w[3]<-w[3]+X$y[i]
            X$z<-sign(X$x1*w[1]+X$x2*w[2]+w[3])
      }
      return(M)
      }


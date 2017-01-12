require(ggplot2)
require(quadprog)
plotSVM<-function(N) {
      f<-data.frame(x1=runif(2,-1,1),x2=runif(2,-1,1))
      target<-lm(data = f,f$x2~f$x1)
      df<-data.frame(x1=1,x2=1,y=1)
      while(abs(sum(df$y))==nrow(df)) {
            df<-data.frame(x1=runif(N,-1,1),x2=runif(N,-1,1))
            df$y<-sign(df$x2-target$coefficients[2]*df$x1-target$coefficients[1])
      }
      Y<-df$y
      X<-as.matrix(df[,1:2])
      D<-matrix(nrow=N,ncol=N)
      for (i in 1:N) {
            for (j in 1:N) {
                  D[i,j]<-Y[i]*Y[j]*t(X[i,])%*%X[j,]     
            }
      }
      d<-as.matrix(rep(1,N))
      A<-cbind(as.matrix(Y),diag(N))
      D<-D+1e-13*diag(N)
      solved<-solve.QP(D,d,A,meq=1)
      wSVM<-c(0,0)
      for (i in 1:N) {
            wSVM<-wSVM+Y[i]*solved$solution[i]*X[i,]
      }
      index<-which.max(solved$solution)
      b<-1/Y[index]-wSVM%*%X[index,]
      sols<-solved$solution>1
      NofS<-length(solved$solution[solved$solution>1])
      side1<-df[sols & df$y==-1,]
      side2<-df[sols & df$y==1,]
      lin<-function(x) -wSVM[1]/wSVM[2]*x-b/wSVM[2]
      linS1<-function(x) -wSVM[1]/wSVM[2]*x+side1$x2[1]+wSVM[1]/wSVM[2]*side1$x1[1]
      linS2<-function(x) -wSVM[1]/wSVM[2]*x+side2$x2[1]+wSVM[1]/wSVM[2]*side2$x1[1]
      ggplot(df,aes(x1,x2,col=as.factor(y)))+geom_point(size=2) +
            stat_function(fun=lin,col="black") +
            stat_function(fun=linS1,col="red") +
            stat_function(fun=linS2,col="blue") +
            scale_x_continuous(limits=c(-1.1,1.1)) +
            scale_y_continuous(limits=c(-1.1,1.1))
      
      #      geom_segment(aes(x=1,y=-wSVM[1]/wSVM[2]-b/wSVM[1],
      #                       xend=-1, yend = +wSVM[1]/wSVM[2]-b/wSVM[1]))
}

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
      cycles=0
      while (sum(X$y!=X$z)>0) {
            cycles<-cycles+1
            if (length(which(X$z!=X$y))==1) i<-which(X$z!=X$y)
            else i<-sample(which(X$z!=X$y),1)
            w[1]<-w[1]+X$y[i]*X$x1[i]
            w[2]<-w[2]+X$y[i]*X$x2[i]
            w[3]<-w[3]+X$y[i]
            X$z<-sign(X$x1*w[1]+X$x2*w[2]+w[3])
      }
      Y<-X$y
      X<-as.matrix(X[,1:2])
      D<-matrix(nrow=N,ncol=N)
      for (i in 1:N) {
            for (j in 1:N) {
             D[i,j]<-Y[i]*Y[j]*t(X[i,])%*%X[j,]     
            }
      }
      d<-as.matrix(rep(1,N))
 #     A<-cbind(as.matrix(Y),-as.matrix(Y),diag(N))
 #     print(A)
      A<-cbind(as.matrix(Y),diag(N))
      D<-D+1e-13*diag(N)
      solved<-solve.QP(D,d,A,meq=1)
      wSVM<-c(0,0)
      for (i in 1:N) {
            wSVM<-wSVM+Y[i]*solved$solution[i]*X[i,]
      }
      index<-which.max(solved$solution)
      b<-1/Y[index]-wSVM%*%X[index,]
      NofS<-length(solved$solution[solved$solution>1])
      return(NofS)
      Y<-data.frame(x1=runif(M,-1,1),x2=runif(M,-1,1))
      Y$y<-sign(Y$x2-target$coefficients[2]*Y$x1-target$coefficients[1])
      Y$z<-sign(Y$x1*w[1]+Y$x2*w[2]+w[3])
      Y$zSVM<-sign(Y$x1*wSVM[1]+Y$x2*wSVM[2]+b)
      errPLA<-sum(Y$y!=Y$z)/M
      errSVM<-sum(Y$y!=Y$zSVM)/M
      if (errPLA>errSVM) return(1)
      else return(0)
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


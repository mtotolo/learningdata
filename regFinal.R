require(dplyr)
train<-read.fwf("features.train",c(16,16,16),col.names = c("digit","intensity","symmetry"))
test<-read.fwf("features.test",c(16,16,16),col.names = c("digit","intensity","symmetry"))
lambda=1

train1vs1<-function(target1,target2) {
      train<-train[train$digit==target1 | train$digit==target2,]
      test<-test[test$digit==target1 | test$digit==target2,]
      train$label[train$digit==target1]<--1
      train$label[train$digit==target2]<- 1
      test$label[test$digit==target1]<--1
      test$label[test$digit==target2]<- 1
      
      X<-as.matrix(select(train,c(intensity,symmetry)))
      X<-cbind(rep(1,nrow(train)),X)
      
      X<-cbind(X,X[,2]*X[,3],X[,2]^2,X[,3]^2) #transform
      
      w<-solve(t(X)%*%X+lambda*diag(ncol(X)))%*%t(X)%*%train$label
      
      Ein<-sum(sign(X%*%w)!=train$label)/nrow(train)
      
      Xtest<-as.matrix(select(test,c(intensity,symmetry)))
      Xtest<-cbind(rep(1,nrow(test)),Xtest)
      
      Xtest<-cbind(Xtest,Xtest[,2]*Xtest[,3],Xtest[,2]^2,Xtest[,3]^2) #transform
      
      Eout<-sum(sign(Xtest%*%w)!=test$label)/nrow(test)
      
      
      return(c(Ein,Eout))
}

train1vsAll<-function(target1) {
      train$label[train$digit==target1]<- 1
      train$label[train$digit!=target1]<- -1
      test$label[test$digit==target1]<- 1
      test$label[test$digit!=target1]<- -1
      
      X<-as.matrix(select(train,c(intensity,symmetry)))
      X<-cbind(rep(1,nrow(train)),X)
      
#      X<-cbind(X,X[,2]*X[,3],X[,2]^2,X[,3]^2) #transform
      
      w<-solve(t(X)%*%X+lambda*diag(ncol(X)))%*%t(X)%*%train$label
      
      Ein<-sum(sign(X%*%w)!=train$label)/nrow(train)
      
      Xtest<-as.matrix(select(test,c(intensity,symmetry)))
      Xtest<-cbind(rep(1,nrow(test)),Xtest)
      
#      Xtest<-cbind(Xtest,Xtest[,2]*Xtest[,3],Xtest[,2]^2,Xtest[,3]^2) #transform
      
      Eout<-sum(sign(Xtest%*%w)!=test$label)/nrow(test)
      
      return(c(Ein,Eout))
}

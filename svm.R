train<-read.fwf("features.train",c(16,16,16),col.names = c("digit","intensity","symmetry"))
test<-read.fwf("features.test",c(16,16,16),col.names = c("digit","intensity","symmetry"))
require(e1071)
train1vsAll<-function(target) {
  train$label<- -1
  train$label[train$digit==target]<-1
  model<-svm(x=train[,c("intensity","symmetry")],y=train$label,
             type="C",kernel="polynomial",degree=2,gamma=1,coef0=1,cost=0.01,scale=c(F,F))
  Ein<-sum(predict(model)!=train$label)/nrow(train)
  return(model$tot.nSV)
  
}
train1vs1<-function(target1,target2,C,Q) {
  #C<-c(0.0001,0.001,0.01,0.1,1)
  train<-train[train$digit==target1 | train$digit==target2,]
  test<-test[test$digit==target1 | test$digit==target2,]
  train$label[train$digit==target1]<--1
  train$label[train$digit==target2]<- 1
  test$label[test$digit==target1]<--1
  test$label[test$digit==target2]<- 1
  model<-svm(x=train[,c("intensity","symmetry")],y=as.factor(train$label),
             type="C",kernel="radial",degree=Q,gamma=1,coef0=1,cost=C,
             scale=c(F,F))
  train$pred<-predict(model)
  Ein<-sum(predict(model)!=train$label)/nrow(train)
  test$pred<-predict(model,newdata=test[,c("intensity","symmetry")])
  Eout<-sum(test$label!=test$pred)/nrow(test)
  return(c(Ein,Eout))
  
}
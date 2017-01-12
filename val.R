train<-read.fwf("in.dta",c(16,16,16),col.names = c("x1","x2","y"))
test<-read.fwf("out.dta",c(16,16,16),col.names = c("x1","x2","y"))

val<-train[26:35,]
train<-train[1:25,]

X<-matrix(data=c(rep(1,nrow(train)),train$x1,train$x2,train$x1*train$x1,train$x2*train$x2,
                 train$x1*train$x2,abs(train$x1-train$x2),abs(train$x1+train$x2)),ncol=8)
Xtest<-matrix(data=c(rep(1,nrow(test)),test$x1,test$x2,test$x1*test$x1,test$x2*test$x2,
                     test$x1*test$x2,abs(test$x1-test$x2),abs(test$x1+test$x2)),ncol=8)
Xval<-matrix(data=c(rep(1,nrow(val)),val$x1,val$x2,val$x1*val$x1,val$x2*val$x2,
                     val$x1*val$x2,abs(val$x1-val$x2),abs(val$x1+val$x2)),ncol=8)
df.error<-data.frame(k=1:8,Ein=NA,Eout=NA,Eval=NA)
for (i in 1:8) {
  w<-solve(t(X[,1:i])%*%X[,1:i])%*%t(X[,1:i])%*%train$y
  val$pred<-sign(Xval[,1:i]%*%w)
  df.error$Eval[i]<-sum(val$y!=val$pred)/nrow(val)
  test$pred<-sign(Xtest[,1:i]%*%w)
  df.error$Eout[i]<-sum(test$y!=test$pred)/nrow(test)
}
print(df.error)
if (FALSE) {
train$pred<-sign(X%*%w)
Ein<-sum(train$y!=train$pred)/nrow(train)

test$pred<-sign(Xtest%*%w)
Eout<-sum(test$y!=test$pred)/nrow(test)
print(Ein)
print(Eout)
df.error<-data.frame(k=-10:10,Ein=NA,Eout=NA)
for (i in 1:nrow(df.error)) {
  lambda<-10^(df.error$k[i])
  wreg<-solve(t(X)%*%X+lambda*diag(8))%*%t(X)%*%train$y
  train$predR<-sign(X%*%wreg)
  df.error$Ein[i]<-sum(train$y!=train$predR)/nrow(train)
  test$predR<-sign(Xtest%*%wreg)
  df.error$Eout[i]<-sum(test$y!=test$predR)/nrow(test)
}
}
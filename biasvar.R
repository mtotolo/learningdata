x<-seq(-1,1,by=0.001)
N=100000
a<-rep(0,N)
b<-rep(0,N)
err<-rep(0,N)
f<-function(x) sin(pi*x)

#CAMBIA QUI
d<-2 #1->un parametro 2->due parametri
n<-2
#FINO QUI
g<-function(a,b,x) a*x^n+b


for (i in 1:N) {
  trainX<-sample(x,2)
  while(trainX[1]+trainX[2]<1e-8) trainX<-sample(x,2)
  train<- g(1,0,trainX)#^2#as.matrix(cbind(rep(1,2),trainX))
  if(d>1) train<-cbind(train,rep(1,2))
  w<-solve(t(train) %*% (train))%*%t(train)%*%f(trainX)
  #CAMBIA QUI
  a[i]<-w[1]
  b[i]<-w[2]
#  b[i]<-mean(f(trainX))
#FINO QUI
}
#m<-mean(a)
m<-mean(a)
print(m)
#1.43

df<-data.frame(x=x)
df$gbar<-g(mean(a),mean(b),x)
df$f<-f(df$x)
bias<-mean((df$gbar-df$f)^2)
print(bias)
#ax->0.27

for (i in 1:length(x)) {
  df$var[i]<-mean((g(a,b,x[i])-df$gbar[i])^2)
}
var<-mean(df$var)
print(var)
#ax->0.24
print(var+bias)
#ax->0.51  
#b->0.75
#ax+b->1.89
#ax^2->18
#ax^2+b->1173

flipCoins<-function() {
      size<-10
      trial<-rbinom(n=1000,size=size,prob=0.5)/size
      nu1<-trial[1]
      nu2<-sample(trial,1)
      nu3<-min(trial)
      return(c(nu1,nu2,nu3))
}
set.seed(123)
N=100000
X=(replicate(N,{flipCoins()}))
x<-apply(X=X,MARGIN = 1, FUN=mean)
print(x)
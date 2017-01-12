calcP<-function(x) 4*(2*x)^10*exp(-1/8*(0.05)^2*x)

delta=0.05
dvc=50


calcOm<-function(x) sqrt(8/x*log(4*(2*x)^dvc/delta))
RPB <- function(x) sqrt(2*log(2*x*x^dvc)/x)+
  sqrt(2/x*log(1/delta))+1/x
PVB<- function(x) {
  eps <- seq(0,1,by=0.00001)
  out <- eps<=((sqrt(1/x*(2*eps+log(6*(2*x)^dvc/delta)))))
  return(max(eps[out]))
}
Dvr<- function(x) {
  eps <- seq(0,1,by=0.00001)
  out <- eps<=((sqrt(1/(2*x)*(4*eps*(1+eps)+log(4)-
                                log(delta)+dvc*log(x^2)))))
  return(max(eps[out]))
}

#min 10000 -> PVB(0.22)
#min 5 -> Dvr
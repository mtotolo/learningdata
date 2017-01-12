u<-1
v<-1
E<-function(u,v) (u*exp(v)-2*v*exp(-u))^2
N<-0
eta<-0.1
while (N<=15) {
  N<-N+1
  u<-u-eta*2*(u*exp(v)-2*v*exp(-u))*(exp(v)+2*v*exp(-u))
  v<-v-eta*2*(u*exp(v)-2*v*exp(-u))*(u*exp(v)-2*exp(-u))
#  u<-u_t
#  v<-v_t
  print(E(u,v))
  print(N)
  print(u)
  print(v)
}
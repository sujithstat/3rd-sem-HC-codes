#MLE
#1
rm(list = ls())
N0=7
x=c(4,5,5,4,5,5,4,7,5,5,5,5,5,5,4)
L=function(p){-log(prod(dbinom(x,N0,p)))}
#phat=nlm(L,p=0.5,hessian = TRUE)$estimate;phat
phat=optim(0.5,L,lower = 0.00001,upper = 1-0.00001,method = "L-BFGS-B")$par;phat

#2
rm(list=ls())
N0=100
x=55
L=function(p){-log(prod(dbinom(x,N0,p)))}
#phat=nlm(L,p=0.5)$estimate;phat
phat=optim(0.5,L,lower = 0.00001,upper = 1-0.00001,method = "L-BFGS-B")$par;phat

#3
rm(list = ls())
x=c(1,2,4,2)
L=function(l){-log(prod(dpois(x,l)))}
#lhat=nlm(L,p=0.5)$estimate;lhat
lhat=optim(0.5,L,lower = 0.00001,method = "L-BFGS-B")$par;lhat
dpois(4,lhat)

#4
rm(list=ls())
x=c(rep(0,19),rep(1,8),rep(2,10),rep(3,2),rep(4,1))
L=function(p){-log(prod(dgeom(x,p)))}
#phat=nlm(L,p=0.6)$estimate;phat
phat=optim(0.5,L,lower = 0.00001,upper = 1-0.00001,method = "L-BFGS-B")$par;phat

#5
rm(list = ls())
x=c(rep(0,19),rep(1,8),rep(2,10),rep(3,2),rep(4,1))
r0=2
L=function(p){-log(prod(dnbinom(x,r0,p)))}
#phat=nlm(L,p=0.6)$estimate;phat
phat=optim(0.5,L,lower = 0.00001,upper = 1-0.00001,method = "L-BFGS-B")$par;phat

#6
rm(list = ls())
x=c(0.10,0.22,0.54,0.36)
L=function(t){-log(prod(dbeta(x,(1/t),1)))}
#that=nlm(L,p=0.6)$estimate;that
that=optim(0.5,L,lower = 0.00001,method = "L-BFGS-B")$par;that

#7
rm(list = ls())
x=c(0.25,0.75,1.50,2.5,2.0)
L=function(a){-log(prod(dgamma(x,2,1/a)))}
#ahat=nlm(L,p=0.6)$estimate;ahat
ahat=optim(0.5,L,lower = 0.00001,method = "L-BFGS-B")$par;ahat

#8
rm(list = ls())
x=c(2,4,7.50,3)
L=function(b){-log(prod(dgamma(x,3,1/b)))}
#bhat=nlm(L,p=0.6)$estimate;bhat
bhat=optim(0.5,L,lower = 0.00001,method = "L-BFGS-B")$par;bhat

#9
rm(list=ls())
x=c(3,0,2,1,3,2,1,0,2,1)
L=function(t){-(2*log((1-t)/3)+2*log(2*t/3)+3*log(2*(1-t)/3)+3*log(t/3))}
#that=nlm(L,p=0.6)$estimate;that
that=optim(0.5,L,lower = 0.00001,upper = 1-0.00001,method = "L-BFGS-B")$par;that

#mle2

#1
rm(list=ls())
x=c(2,3,1,3,4)
L=function(l){-log(prod(dexp(x,l)))}
#lhat=nlm(L,p=0.6)$estimate;lhat
lhat=optim(0.5,L,lower = 0.00001,method = "L-BFGS-B")$par;lhat
pexp(5,lhat,lower.tail = FALSE)
qexp(0.5,lhat)

#2
rm(list = ls())










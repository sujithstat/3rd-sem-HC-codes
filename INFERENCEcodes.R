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
samp=c(22,23.9,20.9,23.8,25.0,24.0,21.7,23.1,23.1,23.8,22.8,22.5,23.0,23.0)
PDF=function(x,a,b){
  out=c()
  for(i in 1:length(x)){
    if (0<=x[i] & x[i]<b) {
      out[i]=a*(x[i]^(a-1))/(b^a)
    } else out[i]=0
  }
  return(out)
}
L=function(inp){-log(prod(PDF(samp,inp[1],inp[2])))}
seed=c(0.7,max(samp)+0.5)
low=c(0.00001,max(samp)+0.00001)
upp=c(Inf,max(samp)+1)
that=optim(seed,L,lower = low ,upper = upp,method = "L-BFGS-B")$par
round(that,4)

#3
rm(list = ls())
x=c(0.863,0.681,0.746,0.997,0.769,0.845,0.624,0.732,0.765,0.157,0.789,0.879,0.848,0.695,0.608,0.886,0.742,0.718,0.891,0.984)
L=function(t){-log(prod(dbeta(x,t+1,1)))}
#that=nlm(L,p=0.6)$estimate;that
that=optim(0.5,L,lower = 0.00001,method = "L-BFGS-B")$par;that

#4
rm(list = ls())
x=c(rep(2,12),rep(3,4),rep(4,3),rep(5,4),rep(6,4),rep(8,2),9,15,17,22,23,24,24,25,27,32,43)
t=2; n=length(x)
L=function(a){-(n*log(a)+n*a*log(t)-sum((a+1)*log(x)))}
ahat=optim(0.5,L,lower = 0.00001,method = "L-BFGS-B")$par;ahat


#MLE 3
#1
rm(list = ls())
x=c(392,376,401,367,389,362,409,415,358,375)
L=function(inp){-log(prod(dnorm(x,inp[1],inp[2])))}
seed=c(mean(x),sd(x))
that=optim(seed,L,method = "L-BFGS-B",lower = c(-Inf,0.00001))$par
that[1];that[2]^2

#2
rm(list = ls())
x=c(2.1,8.2,7.6,3.4,9.1,3.1)
L=function(t){-log(prod(dunif(x,0,t)))}
that=optim(4*mean(x),L,lower = max(x),method = "L-BFGS-B")$par;that

#3
rm(list = ls())
x=c(2.1,8.2,7.6,3.4,9.1,3.1)
L=function(t){-log(prod(dunif(x,t[1],t[2])))}
seed=c(min(x)-1,max(x)+1)
low=c(-Inf,max(x))
upp=c(min(x),Inf)
that=optim(seed,L,lower = low,upper = upp,method = "L-BFGS-B")$par;that

#4
rm(list = ls())
x=c(1.29,0.36,1.33)
L=function(t){-log(prod(dunif(x,t,2*t)))}
that=optim(mean(x),L,method = "L-BFGS-B")$par
that
##### Solution doesn't exist #####

#5
rm(list = ls())
x=c(rep(1,99),rep(2,104),rep(3,110),rep(4,62),rep(5,25),rep(6,10),rep(7,3))
n=length(x)
L=function(l){-(-n*l+(sum(x)*log(l))-sum(log(factorial(x)))-n*log(1-exp(-l)))}
lhat=optim(mean(x),L,method = "L-BFGS-B",lower = 0.00001)$par
round(lhat,3)



##### CHI-SQUARE TEST ######
#1
rm(list = ls())
A=matrix(c(50,25,25,75,50,75,25,25,50),3,3)
chisq.test(A)

#2
rm(list = ls())
A=matrix(c(12,17,21,25,18,17,32,22,16,11,23,26),3,4)
chisq.test(A)

#3
rm(list = ls())
chisq.test()

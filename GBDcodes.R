#YSD
rm(list = ls())
lambda=2
k=4 #rows
v=7 #columns
row=factor(c(rep(1,v),rep(2,v),rep(3,v),rep(4,v)))
column=factor(rep((1:v),k))
treat=factor(c("A","F","D","E","C","B","G",
               "C","B","A","G","F","E","D",
               "G","C","B","F","D","A","E",
               "D","G","F","A","E","C","B"))
yields=c(30,18,65,72,41,60,35,
         48,53,26,39,20,80,72,
         36,44,58,16,67,24,78,
         74,39,18,30,83,51,59)
d1=data.frame(row,column,treat,yields);d1
ysd=lm(yields~row+column+treat)
anova(ysd)

#LSD
rm(list = ls())
k=v=8 #rows & columns
row=factor(c(rep(1,v),rep(2,v),rep(3,v),rep(4,v),rep(5,v),rep(6,v),rep(7,v),rep(8,v)))
column=factor(rep((1:v),k))
treat=factor(c("D","H","C","B","E","A","G","F",
               "F","E","G","A","H","B","C","D",
               "B","C","H","D","G","F","E","A",
               "A","G","E","F","C","D","H","B",
               "C","B","D","H","A","E","F","G",
               "E","F","A","G","D","C","B","H",
               "G","A","F","E","B","H","D","C",
               "H","D","B","C","F","G","A","E"))
yields=c(16.6,16.9,17.4,17.4,15.8,18.2,15.7,15.8,
         15.9,16.4,15.8,19,17.6,17.8,18.9,17.1,
         17.1,16.8,19.2,16.6,15.8,17.8,18.4,18.3,
         17.7,15.9,16.3,16,17.6,17.8,18.1,16.5,
         17.4,17,16.8,19.2,20.3,18.4,15.9,15.7,
         16.5,16,16.9,15.9,14.1,17.5,17.4,19.6,
         15.8,16.9,15.9,16.5,17.6,19.4,17.1,18.3,
         18.6,17.4,17.4,19.2,16.8,15.7,17.4,18.4)
d1=data.frame(row,column,treat,yields);d1
lsd=lm(yields~row+column+treat)
anova(lsd)



###### MISSING PLOT #####
#1 missing
rm(list = ls())
library(MissingPlotRBD)
p1=matrix(c(20.6,19.6,20.5,16.2,19.5,19,18.5,16.5,18.1,15.6,16.3,15.7,17.9,16.7,0,14.8,16,14.1,13.7,12.7),nrow = 4,ncol = 5);p1
xhat=Missing.RBD(p1,3,4)$x.hat;xhat
p1[3,4]=xhat
y=c(p1[,1],p1[,2],p1[,3],p1[,4],p1[,5]);y
treat=factor(c(rep(c("A","B","C","D"),5)))
block=factor(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4)))
data1=data.frame(y,treat,block);data1
fit=lm(y~treat+block,data = data1)
anova(fit)

#2 missing
rm(list = ls())
p1=matrix(c(0,19.6,20.5,16.2,19.5,19,18.5,16.5,18.1,15.6,16.3,15.7,17.9,16.7,0,14.8,16,14.1,13.7,12.7),nrow = 4,ncol = 5);p1
v=4;b=5
ydd=sum(p1)
rowtot=apply(p1,1,sum)
coltot=apply(p1,2,sum)
yi0d=rowtot[1];yi0d
ydj0=coltot[1];ydj0
yi1d=rowtot[3];yi1d
ydj1=coltot[4];ydj1
xhat=((b-1)*(v-1)*(v*yi0d+b*ydj0-ydd)-(v*yi1d+b*ydj1-ydd))/(((b-1)^2)*((v-1)^2)-1)
yhat=((b-1)*(v-1)*(v*yi1d+b*ydj1-ydd)-(v*yi0d+b*ydj0-ydd))/(((b-1)^2)*((v-1)^2)-1)
xhat;yhat
p1[1,1]=xhat;p1[3,4]=yhat
y=c(p1[,1],p1[,2],p1[,3],p1[,4],p1[,5]);y
treat=factor(c(rep(c("A","B","C","D"),5)))
block=factor(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4)))
data1=data.frame(y,treat,block);data1
fit=lm(y~treat+block,data = data1)
anova(fit)


###### ANCOVA #########
rm(list=ls())
x=c(108,136,138,159,146,99,117,90,141,106,184,198,196,198,210,124,95,116,112,123,140,177,189,142,216)
y=c(73,102,118,104,81,98,74,96,111,95,94,79,96,98,102,107,95,97,80,98,49,82,73,86,81)
block=factor(c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5)))
treat=factor(c(rep(c(1,2,3,4,5),5)))
data1=data.frame(x,y,block,treat)
data1
fit=aov(y~treat+block+x,data=data1)
summary(fit)

###### 2^3 Factorial Experiment #########
b=5
treat=factor(c(rep(1,b),rep(2,b),rep(3,b),rep(4,b),rep(5,b),rep(6,b),rep(7,b),rep(8,b)))
block=factor(c(rep(seq(1:b),8)))
yield=c(13,11,3,20,10,26,13,24,26,21,15,2,7,6,5,20,24,8,18,11,13,10,11,5,15,17,21,17,18,17,10,8,6,10,16,18,15,9,8,8)
data1=data.frame(treat,block,yield);data1
fit=aov(yield~treat+block,data = data1);fit
summary(fit)





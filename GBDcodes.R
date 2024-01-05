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









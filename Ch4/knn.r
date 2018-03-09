library(class)#need this lib to knn
?knn#taking a look into the knn function, useful to see the arguments
attach(Smarket)
Xlag=cbind(Lag1,Lag2)#making a matrix with 2 columns
train=Year<2005 #creates a vector with trues and falses
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])#we get a result of 0.5, which shows that knn is absolutely useless in this example
 
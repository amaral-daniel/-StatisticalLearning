library(glmnet)#does not use model formula language, so we need to define x and y matrices
x=model.matrix(Salary~.-1,data=Hitters)
navector=is.na(Hitters$Salary)
y=Hitters$Salary[!navector]#select only values where salary is not NA

fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)#we plot the behaviour of MSE when we change log(Lambda), the full model seems to be much better

#lasso model

fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
plot(fit.lasso,xvar="dev",label=TRUE)#R squared
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)


set.seed (1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
x.test=x[test,]
y.train=y[train]
x.train=x[train,]

lasso.tr=glmnet(x[train,],y.train)
lasso.tr
pred=predict(lasso.tr,x.test)
dim(pred)
rmse=sqrt(apply((y.test-pred)^2,2,mean))
plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda")
lam.best=lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr,s=lam.best)
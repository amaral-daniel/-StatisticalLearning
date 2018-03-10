#minimum risk investment example
require(ISLR)
require(boot)#lib for bootstrap

alpha=function(x,y){#formula to get optimal value for alpha
  vx = var(x)
  vy = var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

alpha(Portfolio$X,Portfolio$y)

#standard error of alpha

alpha.fn=function(data,index)
{
  with(data[index,],alpha(X,Y))
}

alpha.fn(Portfolio,1:10)

set.seed(1)

alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))

boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
class(boot)
class(boot.out)
plot(boot.out)

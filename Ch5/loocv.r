require(ISLR)
require(boot)#lib for bootstrap

?cv.glm #crossvalidation
plot(mpg~horsepower,data=Auto)#taking a look into the data. Auto is the dataframe


#LOOCV

glm.fit=glm(mpg~horsepower, data=Auto)
cv.glm(Auto,glm.fit)$delta #calculates the cv prediction error. it gives two values, one  with the CV value, and the other with the value corrected to reduce bias

#writing a function to reduce computational effort

loocv=function(fit){
  h=lm.influence(fit)$h#h is the leverage
  mean((residuals(fit)/(1-h))^2)#last value computed will be the returned value
}

cv.error = rep(0,5) #creates a 5 dimensions vector filled with 0s
degree = 1:5 
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)#fits new model
  cv.error[d] = loocv(glm.fit)#stores the error
}

plot(degree,cv.error,type="b")

#10 fold CV

cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}

lines(degree,cv.error10,type="b",col="red")


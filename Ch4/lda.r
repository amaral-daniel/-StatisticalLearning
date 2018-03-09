require(ISLR)
require(MASS) #pack to do discriminant analysis

#linear discriminant Analysis

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket, subset = Year<2005)#direction is the response, Lag1 and Lag2 are the predictors
#subset = Year<2005 is a way of setting a subset
lda.fit
plot(lda.fit)
Smarket.2005=subset(Smarket,Year==2005)#selects every row where Year = 2005
lda.pred=predict(lda.fit,Smarket.2005)
class(lda.pred)#verifies that lda.pred is a list
data.frame(lda.pred)[1:5,]#we take a look into the data
table(lda.pred$class,Smarket.2005$Direction)#checking false trues and false falses
mean(lda.pred$class==Smarket.2005$Direction)#overall score



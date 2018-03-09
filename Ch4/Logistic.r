require(ISLR)
names(Smarket)
summary(Smarket)
pairs(Smarket,col = Smarket$Direction)#draws a scatter plot for each pair of variables
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=Smarket,family=binomial)#glm.fit fits a linear model
summary(glm.fit)#from the z and p results we get, we see that no parameters was useful
glm.probs=predict(glm.fit,type="response")
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Up","Down") #ifelse command takes a vector booleans and transforms it
attach(Smarket)
table(glm.pred,Direction)#creates table to see false trues e false falses
mean(glm.pred==Direction)#checkin the mean to see how often the model works
#we must remember that its the test on the training data, for the test data its performance might be worse
train = Year<2005#train is a vector with the same length as Year, but made only my Trues and Falses
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response")
glm.pred=ifelse(glm.probs > 0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
#Smaller model
glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response")
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
#the performance seems to have been increased

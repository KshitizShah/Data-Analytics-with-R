# logistic regression & classification


getwd()
dir()

###############################################
# load library ISLR and see what the dataframe Default looks like
install.packages("ISLR")
library(ISLR)
View(Default)
dim(Default)
# check the names of the columns in Default
names(Default)
?contrasts
# check the dimensions of Default (how many rows, how many columns)
dim(Default)
# check how many missing values there are in the dataframe Default
sum(is.na(Default))

# run logistic regression with default as the response variable and all others as predictors
logreg.fit <- glm(default~balance+income+student,data=Default,family=binomial)

# see the summary output of logistic regression
summary(logreg.fit)


# given we have the logistic regression results stored in logreg.fit, we call predict()
# with logreg.fit as the first argument, the data for predcitors in the second argument,
# type = "response" to clarify we want to predict the probabilities for the response being 1 
# (in this case, predicting the probability of default)
predict(logreg.fit,newdata=data.frame(balance=c(2000,2000),income=c(8000,40000), student=c("Yes","No")),type="response")


#########
# use examples to answer questions
# Is default more likely if the balance is high?
predict(logreg.fit,newdata=data.frame(balance=c(1000,1000),income=c(2000,80000), student=c("Yes","Yes")),type="response")

# Is default more likely if the income is low?
#  Is default more likely if the user is a student?
?glm


########


###############################################
#### Example: Death Penalty Data
###############################################

DeathPenalty=read.csv("DeathPenalty.csv")
head(DeathPenalty)
tail(DeathPenalty)
View(DeathPenalty)
m1<-glm(Death~Agg + VRace, family=binomial,data=DeathPenalty)

m1<-glm(Death~Agg + VRace, data=DeathPenalty)
m1<-lm(Death~Agg + VRace, data=DeathPenalty)

summary(m1)
coef(m1)

predict(m1,newdata=data.frame(Agg=c(1,1),VRace=c(1,0)),type="response")

predict(m1,newdata=data.frame(Agg=c(5,5),VRace=c(1,0)),type="response")

#### compare to #######
predict(m1,data.frame(Agg=c(1,1),VRace=c(1,0)))
#######################

        
predict(m1,newdata=data.frame(Agg=c(1,6),VRace=c(1,1)), type="response")

predict(m1,newdata=data.frame(Agg=c(1,2),VRace=c(0,0)), type="response")


################################################
#### Stock Market Data

write.csv(Smarket, file="Smarket.csv",row.names=F)
################################################

library(ISLR)
attach(Smarket)
data(Smarket)
View(Smarket)
?Smarket

################ read.csv to load data
read.csv("Smarket.csv") ->my.smarket
View(my.smarket)
#####################################

head(Smarket)
tail(Smarket)
names(Smarket)
summary(Smarket)
?cor
cor(Smarket)
cor(Smarket[,-9])
length(Smarket)
length(Smarket$Year)
Smarket[,-seq(2:10)]->xx
head(xx)
Smarket[,-seq(2:5)]->xx
head(xx)

#### plots
plot(Smarket)
plot(Smarket$Today~Smarket$Lag1)
plot(Smarket$Lag1,Smarket$Today)
hist(Smarket$Today)
hist(Smarket$Today,breaks=5)
hist(Today,breaks=5)
hist(Today,breaks=10)
hist(Today,breaks=100)

import(leaps)
#### logistic regression
class(Smarket$Direction)
levels(Smarket$Direction)
?regsubsets
summary(regsubsets())

glm.fit=glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
            family=binomial,data=Smarket)
summary(glm.fit)

predict(glm.fit,type="response")
predict(glm.fit,type="response")->glm.probs
options("digits"=4)

glm.probs[1:5]
contrasts(Direction) ## check dummy variable: 1 or 0; 
## in this case: Up=1; Down =0

glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"

head(glm.pred)
head(Direction)
table(glm.pred,Direction)
(145+507)/1250
mean(glm.pred==Direction)

levels(Smarket$Year)
class(Smarket$Year)
summary(Smarket$Year)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

head(Direction.2005)
length(Direction.2005)
glm.fit=glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
            family=binomial,data=Smarket,subset=train)

glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.probs


glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
mean(glm.pred==Direction.2005)

glm.fit=glm(Direction~Lag4 + Lag3,family=binomial,data=Smarket,subset=train)

# improve prediction accuracy
glm.fit=glm(Direction ~ Lag1+Lag2, data=Smarket, family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)


##################
##KNN anlaysis####

library(ISLR)
attach(Smarket)
train=(Year<2005)

library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
Direction.2005=Direction[!train]

nearest1=knn(train.X,test.X,train.Direction,k=1)
nearest1

table(nearest1,Direction.2005)
mean(nearest1==Direction.2005)

nearest3=knn(train.X,test.X,train.Direction,k=3)
mean(nearest3==Direction.2005)



############## In-Class exercise: Auto Data Set
Auto=read.csv("Auto.csv",header=T,na.strings="?")
Auto2=na.omit(Auto) # remove missing values
View(Auto2)

mpg.median=median(Auto2$mpg)
mpg.median

mpg01 <- ifelse(Auto2$mpg > mpg.median, 1, 0)
head(mpg01)
mpg01

head(Auto2)


###### alternative
mpg01 <- (Auto2$mpg > mpg.median)
head(mpg01)
mpg01
#################

Auto3=data.frame(mpg01,Auto2) # create a new data frame
head(Auto3)

# explore the data w.r.t. mgp01
boxplot(mpg01~cylinders, data=Auto3)

plot(mpg01~weight, data=Auto3)
plot(mpg01~horsepower, data=Auto3)
plot(mpg01~acceleration, data=Auto3)

## it seems that cylinders,weight,horsepower and acceleration are most associated with mpg01. The answers are not unique.

nrow(Auto3)

train= seq(1,nrow(Auto3)/2) # create indeces for training data set
train

test= seq(nrow(Auto3)/2+1, nrow(Auto3)) 
test

### another way
index=seq(1:392)
train=(index<=196)
train
test=!train
###########

# perform logistic regression
logit.auto=glm(mpg01~ cylinders+weight+horsepower +acceleration, family="binomial",data=Auto3)
summary(logit.auto)

# p-value for acceleration is not significant, re-perform logistic regression without it
logit.auto=glm(mpg01~ cylinders+weight+horsepower, family="binomial",data=Auto3)
summary(logit.auto)

# you may also want to remove cylinders
logit.auto=glm(mpg01~ weight+horsepower, family="binomial",data=Auto3)
summary(logit.auto)

# perform logistic regression with training data, use testing data for accuracy
logit.auto2=glm(mpg01~ weight+horsepower, family="binomial",data=Auto3,subset=train)
logit.auto2

auto.probs=predict(logit.auto2,newdata=Auto3[test,],type="response")
auto.probs

auto.pred=rep(0,length(test))

auto.pred[auto.probs>.5]=1

table(auto.pred,Auto3$mpg01[test]) ->test.table
test.table
prediction.accuracy <- (test.table[1,1]+test.table[2,2])/sum(test.table)
prediction.accuracy

mean(auto.pred==mpg01[test])



######################
######## Poission Regression
data = read.csv( "goals.csv")
head(data,n=10)
summary(data)

pslm = glm(goal~half+home,family=poisson(),data=data)
summary(pslm)

pslm2 = glm(goal~half,family=poisson(),data=data)
summary(pslm2)









##### Sample Code for Session 3: Model Selection

#########################################################

#########################
## Best subsect selection
install.packages("ISLR")
library(ISLR)
?ISLR
?lm

View(Hitters)
?Hitters
names(Hitters)
Hitters[1:5,]
dim(Hitters)

rownames(Hitters)

############### write variables to files
write.table(Hitters, file="Hitters.csv",row.names=T)
dir()
my.hitters <- read.table("Hitters.csv")
head(my.hitters)
#####################################


Hitters$Salary
sum(is.na(Hitters$Salary)) ## check missing values

Hitters2=na.omit(Hitters)  ## remove missing values  
dim(Hitters2)
sum(is.na(Hitters))

sum(is.na(Hitters2))

install.packages("leaps")
library(leaps)
?regsubsets

regfit.full=regsubsets(Salary~.,data=Hitters2)
summary(regfit.full)

regfit.full=regsubsets(Salary~.,data=Hitters2,nvmax=19)
summary(regfit.full)
coef(regfit.full,19)

my.lm.100=lm(Salary~CRBI + Hits,data=Hitters2)
summary(my.lm.100)

reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
#for max value
max(reg.summary$adjr2)
#for index 
which.max(reg.summary$adjr2)

plot(reg.summary$adjr2)
which.max(reg.summary$adjr2) # function which.max() returns the index of maximum value
coef(regfit.full,11)

# we can also save the coefficients as a data frame
frame <- data.frame(coef(regfit.full,5))
frame

### use different approaches: R^2, adjusted R^2, CP, BIC
names(reg.summary)
reg.summary$rsq
which.max(reg.summary$adjr2)
coef(regfit.full,11)

reg.summary$cp
which.min(reg.summary$cp)
coef(regfit.full,10)

which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables")

plot(reg.summary$bic,xlab="Number of Variables",type="l") ## "l" is letter l, not number 1


#### stepwise selection: forward or backward
regfit.fwd=regsubsets(Salary~.,data=Hitters2,nvmax=19, method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters2,nvmax=19, method="backward")
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)


###############################################
#### in-class exercise



#################################################
## Model Assessment
### Auto Data Set
Auto=read.csv("Auto.csv",header=T,na.strings="?")
Auto2=na.omit(Auto)
dim(Auto2)
?sample
set.seed(1) ## generate same random variables
train = sample(392,196) ## devide into two equal subsets
## same as train = sample(1:392,196)

lm.fit = lm(mpg~horsepower, data=Auto2, subset=train)

## subset option: only use the training set to fit model
predict(lm.fit,newdata = Auto2[-train,]) -> pred.auto
Auto2$mpg[-train] -> true.auto

sqrt(sum((pred.auto-true.auto)^2)/196)

attach(Auto2)
mean((mpg - predict(lm.fit, Auto2))[-train]^2) # obtain estimated test MSE
# [-train] only select observations that are not in the training set



#### KNN #########
###### k-nearest neighbor (KNN)

my.ad<-read.csv("Advertising.csv")
### new scenario TV=100, Radio=50, Newspaper =25
head(my.ad)
train.X = my.ad[,2:4]
train.Y = my.ad[,5]

test.X = data.frame(TV=100, Radio=50, Newspaper =25)


###
install.packages("FNN")
library(FNN)
?knn.reg
knn.pred.k1=knn.reg(train.X,test.X,train.Y,k=1)
knn.pred.k1

knn.pred.k3=knn.reg(train.X,test.X,train.Y,k=3)
knn.pred.k3

## use the first 190 rows as training data; 
## use the remaining as testing data

train.X = my.ad[1:190,2:4]
test.X = my.ad[191:200,2:4]
train.Y = my.ad[1:190,5]
test.Y= my.ad[191:200,5]

knn.pred=knn.reg(train.X,test.X,train.Y,k=3)
names(knn.pred)

### change data type

sqrt(mean((knn.pred$pred - test.Y)^2))

### another way to do it
attach(my.ad)
train.X = cbind(TV,Radio,Newspaper)[1:190,]
test.X = cbind(TV,Radio,Newspaper)[191:200,]
train.Y = Sales[1:190]
test.Y= Sales[191:200]
test.Y

library(class)
knn.pred2=knn.reg(train.X,test.X,train.Y,k=3)
names(knn.pred2)

sqrt(mean((knn.pred2$pred - test.Y)^2))

#################


#### in-class exercise: GMAT, GPA
GMAT<-c(560,540,520,580,520,620,660,630,550,550,600,537)
GPA<-c(3.20,3.44,3.70,3.10,3.00,4.00,3.38,3.83,2.67,2.75,2.33,3.75)

library(FNN)
GPA.predict <- knn.reg(data.frame(GMAT), data.frame(GMAT.test=540), GPA, k=1)
GPA.predict$pred

## interesting observation
## why?
## how


### linear model
lm.GPA <- lm(GPA~GMAT)
GPA.predict.lm <- predict(lm.GPA, data.frame(GMAT=540))
GPA.predict.lm
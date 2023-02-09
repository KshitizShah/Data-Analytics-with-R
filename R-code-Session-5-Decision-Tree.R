########################################
#### Decision Tree
########################################
 
#### Regression Trees


install.packages("tree")
library(tree)

library(MASS)
set.seed(1)
nrow(Boston)
train = sample(1:nrow(Boston), nrow(Boston)/2) # split into to equal subsets
tree.boston = tree(medv~., data=Boston, subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston)

#predict(tree.boston, newdata=data.frame(lstat=10,crim))

############## prediction
boston.test= Boston$medv[-train]

yhat = predict(tree.boston, newdata=Boston[-train,])

sqrt(mean((yhat - boston.test)^2))
#################



######## prune tree
prune.boston = prune.tree(tree.boston, best=6)
plot(prune.boston)
text(prune.boston)

yhat = predict(prune.boston, newdata=Boston[-train,])
sqrt(mean((yhat - boston.test)^2))
################


## use function cv.tree() function to see whether prunning the tree will improve performance
cv.boston = cv.tree(tree.boston)
names(cv.boston)
summary(cv.boston)
plot(cv.boston$size, cv.boston$dev, type='b')
cv.boston$size
cv.boston$dev

## prune tree
prune.boston = prune.tree(tree.boston, best=4)
plot(prune.boston)
text(prune.boston)

## make prediction
yhat = predict(tree.boston, newdata=Boston[-train,])
#attach(Boston)
boston.test= Boston[-train,"medv"]

### alternative way
boston.test= Boston$medv[-train]
###


plot(yhat, boston.test)
abline(0,1)
mean((yhat - boston.test)^2)
# the MSE is around 25
sqrt(mean((yhat - boston.test)^2))


###############################################
#### in-class exercise ########################



###########################################
### in-class exercise: tree regression
library(tree)
library(ISLR)
View(Carseats)
set.seed(2)
train = sample(1:nrow(Carseats), 200)
tree.carseats3 = tree(Sales~., data=Carseats,subset=train)
Carseats.test=Carseats[-train,]

plot(tree.carseats3)
text(tree.carseats3,pretty=0)

tree.pred3 = predict(tree.carseats3, newdata = Carseats.test)
sqrt(mean((tree.pred3 - Carseats$Sales[-train])^2))



#### cross validation
cv.carseat = cv.tree(tree.carseats3)
names(cv.carseat)
summary(cv.carseat)
plot(cv.carseat$size, cv.carseat$dev, type='b')
cv.carseat$size
cv.carseat$dev

## prune tree
prune.carseats3 = prune.tree(tree.carseats3, best=8)
plot(prune.carseats3)
text(prune.carseats3, pretty=0)

## make prediction
tree.pred4 = predict(prune.carseats3, newdata = Carseats.test)
sqrt(mean((tree.pred4 - Carseats$Sales[-train])^2))


############################################
# Classification Tree
##############################################
install.packages("tree")
library(tree)
library(ISLR)
attach(Carseats)
?Carseats
View(Carseats)

############
write.csv(Carseats, file="Carseats.csv",row.names=F)
my.carseats <- read.csv("Carseats.csv")
head(my.carseats)
############

High=ifelse(Sales <= 8, "No", "Yes")

Carseats2 = data.frame(Carseats, High)
View(Carseats2)

tree.carseats = tree(High~.-Sales, data=Carseats2)
summary(tree.carseats)

#### plot() display tree structure
plot(tree.carseats)
#### text() display the node labels

## pretty=0 includes the category names for any qualitative predictors
text(tree.carseats)

plot(tree.carseats)
text(tree.carseats,pretty=0)

##
plot(tree.carseats)
text(tree.carseats,pretty=1)

tree.carseats

#### calcuate training error and testing error
set.seed(2)
train = sample(1:nrow(Carseats2), 200)
Carseats2.test = Carseats2[-train,]
High.test = High[-train]
tree.carseats = tree(High~.-Sales, data=Carseats2, subset=train)

plot(tree.carseats)
text(tree.carseats,pretty=1)

## In the case of a classification tree, the argument type="class" instructs R to return
## the actual class prediction.
tree.pred = predict(tree.carseats, Carseats2.test, type="class")

## create a confusion table
table(tree.pred, High.test)
(86+57)/200

###### alternative way
mean(tree.pred == High.test)


#### prune the tree
set.seed(3)

prune.carseats = prune.misclass(tree.carseats,best=7)

plot(prune.carseats)
text(prune.carseats,pretty=0)

tree.pred2 = predict(prune.carseats, Carseats2.test, type="class")
mean(tree.pred2 == High.test)



## optional
## The function cv.tree() performs cross-validation in order to
## determine the optimal level of tree complexity
## We use the argument FUN=prune.misclass in order to indicate that we want the
## classification error rate to guide the cross-validation and pruning process,
## rather than the default, which is deviance.

cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)

## cv.tree() function reports the number of terminal nodes of each tree considered
## (size) as well as the corresponding error rate and the value of the
## cost-complexity parameter used
cv.carseats

cv.carseats$size
cv.carseats$dev
cv.carseats$k

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
          
prune.carseats = prune.misclass(tree.carseats,best=9)
par(mfrow=c(1,1))
plot(prune.carseats)
text(prune.carseats,pretty=0)
## pretty=0 instructs R to include category names for qualitative predictors


## How well does this pruned tree perform on the test data set?
tree.pred2 = predict(prune.carseats, Carseats2.test, type="class")

## create a confusion table
table(tree.pred2, High.test)
mean(tree.pred2 == High.test)


### increase the value of best to have a larger tree: not necessarily better prediction
prune.carseats2 = prune.misclass(tree.carseats,best=15)
tree.pred3 = predict(prune.carseats2, Carseats2.test, type="class")

## create a confusion table for prediction accuracy
table(tree.pred3, High.test)
mean(tree.pred3 == High.test)










########## random forests
library(randomForest)
set.seed(1)
bag.boston= randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=T)
#mtry=13 indicates all 13 predictors should be considered for each tree

bag.boston

yhat = predict(bag.boston, newdata=Boston[-train,])
plot(yhat, boston.test)
abline(0,1)
mean((yhat - boston.test)^2)

### change the number of trees
set.seed(1)
bag.boston= randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat = predict(bag.boston, newdata=Boston[-train,])
mean((yhat - boston.test)^2)

### change the number of predictors
set.seed(1)
bag.boston= randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=T)
yhat = predict(bag.boston, newdata=Boston[-train,])
mean((yhat - boston.test)^2)

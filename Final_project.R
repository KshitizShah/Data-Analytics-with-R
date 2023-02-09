install.packages("leaps")
install.packages("tree")
library(tree)
library(leaps)

# Import dataset and remove country/year column
data <- read.csv("Life Expectancy Data.csv")
data <- na.omit(data)
data <- subset(data, select = -c(Country, Year))

# Create a dummy variable for Status and remove the original Status column
data$Status.Dummy <- ifelse(data$Status== "Developing",1,0)
data <- subset(data, select = -c(Status))

# Performs forward stepwise selection
regfit.fwd=regsubsets(Life.expectancy~.,nvmax=19, data=data, method="forward")
summary(regfit.fwd)

# Gets the coefficients of best N variables from 1 to 19
x <- 1
for (x in 1:19){
  print(" ")
  print(" ")
  print(coef(regfit.fwd,x))
}

reg.summary <- summary(regfit.fwd)
plot(reg.summary$adjr2,xlab="Number of Variables")
plot(reg.summary$bic,xlab="Number of Variables")
plot(reg.summary$cp,xlab="Number of Variables")

which.max(reg.summary$adjr2)
which.min(reg.summary$bic)
which.min(reg.summary$cp)

coef(regfit.fwd,15)
coef(regfit.fwd,9)
coef(regfit.fwd,13)

#linear regression
lm(Life.expectancy~., data=data) ->lm.data
summary(lm.data)

#linear regression using 15 variables according to adjr2 in selection model
lm(Life.expectancy~Adult.Mortality+infant.deaths+Alcohol+percentage.expenditure+Hepatitis.B+ BMI+under.five.deaths + Polio+Total.expenditure+Diphtheria+HIV.AIDS+thinness.5.9.years+Income.composition.of.resources+ Schooling+Status.Dummy, data=data) ->lm.data.adjr2
summary(lm.data.adjr2) 

#linear regression using 9 variables according to bic in selection model
lm(Life.expectancy~Adult.Mortality+infant.deaths+percentage.expenditure+ BMI+under.five.deaths +Diphtheria+HIV.AIDS+Income.composition.of.resources+ Schooling, data=data) ->lm.data.bic
summary(lm.data.bic) 

#linear regression using 13 variables according to cp in selection model
lm(Life.expectancy~Adult.Mortality+infant.deaths+Alcohol+ BMI+under.five.deaths+Total.expenditure+Diphtheria+HIV.AIDS+thinness.5.9.years+Income.composition.of.resources+ Schooling+Status.Dummy, data=data) ->lm.data.cp
summary(lm.data.cp) 

# tree model 
set.seed(1)
nrow(data)
names(data)

# training data:split into to equal subsets
train = sample(1:nrow(data), nrow(data)/2) 

#1. tree with all variables as predictors 
tree = tree(Life.expectancy~., data=data, subset=train)
summary(tree)

plot(tree)
text(tree)

# prediction
tree.test= data$Life.expectancy[-train]
yhat = predict(tree, newdata=data[-train,])

#error - with 9 nodes = 3.399 :
sqrt(mean((yhat - tree.test)^2))

#pruning: cross validation
## use function cv.tree() function to see whether prunning the tree will improve performance

cv.tree = cv.tree(tree)

names(cv.tree)
summary(cv.tree)

plot(cv.tree$size, cv.tree$dev, type='b')

cv.tree$size
cv.tree$dev

#pruning
prune.tree = prune.tree(tree, best=8)
plot(prune.tree)
text(prune.tree)

#error after pruning: 3.744
yhat = predict(prune.tree, newdata=data[-train,])
sqrt(mean((yhat - tree.test)^2))

#as we can see that the error is a bit worse after pruning. We cannot say that
# the data is being overfit with our original model as it performed better on the test
# dataset. Trees tend to overfit data with excessive number of nodes, however, 9 leaves is
# well within the reasonable range for number of nodes. Hence pruning does not better our predictions


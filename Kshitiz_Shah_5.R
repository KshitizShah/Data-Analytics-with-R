#homework
library(tree)
library(ISLR)
head(OJ)
dim(OJ)
summary(OJ)

set.seed(385)
#a)randomly generate a training vector of 800
train_OJ = sample(1:1070, 800)

#testing data

testing_OJ <- OJ[-train_OJ,]
#names in dataset
names(OJ)

#b)tree

tree(Purchase ~ ., data=OJ, subset=train_OJ) -> OJ.tree
summary(OJ.tree)

#c) plotting tree
plot(OJ.tree)
text(OJ.tree)


#d) model
prediction <- predict(OJ.tree, testing_OJ, type ="class")
Purchase.testing <-testing_OJ$Purchase

#confusion matrix
table(prediction,Purchase.testing)

#error 
mean(prediction != Purchase.testing)

#or 
#false positive + false negative / total
(34+10)/270


#e) pruning tree

prune.misclass(OJ.tree, best =4) -> OJ.tree.new
plot(OJ.tree.new)
text(OJ.tree.new)

#f) comparing trees
#new prediction
new_prediction <- predict(OJ.tree.new, testing_OJ, type ="class")
#error 
mean(new_prediction != Purchase.testing)

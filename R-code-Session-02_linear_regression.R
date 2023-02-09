## Session 2: linear regression

#linear reagression -> determines relationship between scalar output and explanatory
#variables, if exp = 1, linear, if exp >1, multiple linear regression

#what-if analysis -> what will happen when you do one thing. how will it affect the output variables?
#is the relationship linear? questions like this are answered by linear regression
getwd()
dir()

###################################
#### advertising example
##################################

my.ad<-read.csv("Advertising.csv")
head(my.ad)
tail(my.ad)

my.ad[1:10,]
my.ad[10:15,]
my.ad[10:15,3:5]

my.ad[c(10,11,13,20),c(2,3,5)]

lm(Sales~TV, data = my.ad) -> lm.TV
abline(lm.TV)
/names(my.ad)
summary(my.ad)

### remove records
my.ad[-(2:5),] -> my.ad2
head(my.ad2)

my.ad[-c(2,5,10),] -> my.ad3
head(my.ad3)

my.ad[,-1] -> my.ad4
head(my.ad4)

par(mfrow=c(1,1)) # par(mfrow=c(x,y)) divides plot space into x rows and y colums
plot(Sales,TV)
plot(my.ad$Sales,my.ad$TV)

attach(my.ad)
plot(Sales,TV)

plot(Sales~TV,Newspaper, col =3:4)

plot(TV,Sales,col=5, pch=8)

plot(Sales~Newspaper,TV data=my.ad)
?plot

?lm
my.first.lr=lm(Sales~TV)
my.first.lr
summary(my.first.lr)

predict(my.first.lr, data.frame(TV=100),interval="confidence", level=.9)

names(my.first.lr)

abline(my.first.lr)
my.first.lr$residuals

sd(my.first.lr$residuals)
mean(my.first.lr$residuals)

boxplot(my.first.lr$residuals)
plot(Sales~TV)
abline(my.first.lr)
sd(my.first.lr$residuals)


### regress Sales w.r.t. TV
my.2nd.lr=lm(Sales~TV,data=my.ad)
summary(my.2nd.lr)
MyFirstLr=lm(Sales~TV)
?attach
?na.omit


ad=read.csv("Advertising.csv")
ad[1:10,]
attach(ad)

plot(Sales~TV)
plot(Sales~Radio)

my.lm.1=lm(Sales~TV)
summary(my.lm.1)

my.lm.2=lm(Sales~Radio)
summary(my.lm.2)

##############################################
########### in-class exercise
##############################################



#############################################


my.lm.4=lm(Sales~TV+Radio+Newspaper)
summary(my.lm.4)

my.lm.4=lm(Sales~.,data=ad)
summary(my.lm.4)


head(ad)
# remove first column
ad2=ad[-1]
head(ad2)

ad3=ad[,-1]
head(ad3)

my.lm.4=lm(Sales~.,data=ad2)
head(ad)
ad[1:10,]
head(ad2)
my.lm.4=lm(Sales~.,data=ad2)
summary(my.lm.4)

my.lm.5=lm(Sales~TV+Radio)
summary(my.lm.5)


########################################
##### Example Fuel efficiency
########################################

FuelEff<-read.csv("FuelEfficiency.csv")
head(FuelEff)
attach(FuelEff)
plot(MPG~GPM)
MPG*GPM

plot(GPM~WT,data=FuelEff)
plot(GPM~DIS,data=FuelEff)
plot(GPM~NC,data=FuelEff)
plot(GPM~HP,data=FuelEff)

m0=lm(GPM~WT)
summary(m0)

m1=lm(GPM~.,data=FuelEff)
summary(m1)

FuelEff2=FuelEff[-1]
head(FuelEff2)

m2 <- lm(GPM~.,data=FuelEff2)
m2 <- lm(GPM~.-MPG,data=FuelEff)
summary(m2)

##############################################
################# in-class exercise
#############################################



#############################################


####################################
#### Boston Data Set
####################################

library(MASS)
library(ISLR)
# utils:::menuInstallPkgs()
library(ISLR)
attach(Boston)
head(Boston)
?Boston
fix(Boston)
names(Boston)
View(Boston)

# detach(Boston)
Boston[1:5,]
lm.fit=lm(medv~lstat,data=Boston)
predict(lm.fit, data.frame(lstat=10))
summary(lm.fit)
coef(lm.fit)

# 95% confidence interval
confint(lm.fit)

# 90% confidence interval
confint(lm.fit,"lstat",level=.9)

predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval="prediction")

plot(medv~lstat,data=Boston)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=5)
abline(lm.fit,lwd=5,col=2)
abline(lm.fit,lwd=5,col=3)
abline(lm.fit,lwd=5,col="red")
plot(medv~lstat,data=Boston,pch=20)
plot(medv~lstat,data=Boston,pch="$")
plot(1:20,1:20,pch=1:20)

lm.fit.1=lm(medv~lstat+age,data=Boston)
predict(lm.fit.1, data.frame(lstat=10,age=20))
summary(lm.fit.1)

lm.fit.1=lm(medv~lstat+age+rm,data=Boston)
summary(lm.fit.1)

lm.fit.2=lm(medv~.,data=Boston)
summary(lm.fit.2)

lm.fit.3=lm(medv~.-age,data=Boston)
summary(lm.fit.3)

lm.fit.4=lm(medv~.-age-indus,data=Boston)
summary(lm.fit.4)


############################################
########### interaction terms
###########################################
fit.lm.2=lm(medv~lstat:age,data=Boston)
head(Boston)
summary(fit.lm.2)

fit.lm.3=lm(medv~lstat*age,data=Boston)
summary(fit.lm.3)

x3=lstat*age
x3=Boston$lstat*Boston$age
x3
fit.lm.10=lm(medv~x3,data=Boston)
summary(fit.lm.10)

fit.lm.11=lm(medv~lstat+age+x3,data=Boston)
summary(fit.lm.11)


######################################
########### non-linear term
#####################################
lm.fit4=lm(medv~lstat+ I(lstat^2),data=Boston)
summary(lm.fit4)

x4=Boston$lstat^2
lm.fit5=lm(medv~lstat+x4,data=Boston)
summary(lm.fit5)

lm.fit4=lm(medv~lstat+ I(lstat^2)+I(lstat^3),data=Boston)
summary(lm.fit4)






#######################################################
############# Example: Toyota Used-Car Prices
#######################################################
dir()
toyota<-read.csv("ToyotaCorolla.csv")
head(toyota)
toyota$FuelType[1:10]
levels(toyota$FuelType)


attach(toyota)
v1=rep(1,length(FuelType))
v2=rep(0,length(FuelType))
toyota$FuelType1=ifelse(FuelType=="CNG",v1,v2)
toyota$FuelType2=ifelse(FuelType=="Diesel",v1,v2)
head(toyota)
auto=toyota[-4]
head(auto)
plot(Price~Weight,data=auto)
plot(Price~KM,data=auto)
plot(Price~Automatic,data=auto)
plot(Price~Age,data=auto)
m11=lm(Price~Age+KM,data=auto)
summary(m11)
names(m11)
plot(m11$res~m11$fitted)


auto[1:5,]
lm.1=lm(Price~KM,data=auto)
summary(lm.1)

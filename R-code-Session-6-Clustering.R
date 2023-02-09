############################################
#### Clustering
#### Example with simulated data: K-mean clustering

##### generate data
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
plot(x)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
plot(x)


write.csv(x,"data-clustering.csv",row.names = F)
#############

## read dataset: data-clustering.csv

x=read.csv("data-clustering.csv")
View(x)
plot(V1~V2,data=x)

## kmeans clustering
?kmeans
set.seed(100)
km.out=kmeans(x,2,nstart=20)
names(km.out)
km.out

km.out$cluster

## output

plot(1:10,1:10,col=1:10,pch=1:10)
km.out$cluster

plot(x,col=km.out$cluster)
     
plot(x,col=km.out$cluster,pch=km.out$cluster,lwd=2)

plot(1:10,1:10,pch=1:10)
km.out$tot.withinss
km.out$withinss

##########################
# change number of centers
set.seed(4)
km.out2=kmeans(x,3,nstart=50)
km.out2
plot(x,col=km.out2$cluster,pch=km.out2$cluster)

km.out2$tot.withinss
km.out2$withinss

###########################################################
#### Sample code for the Eurpoean Protein consumption example
#### K-mean clustering
###########################################################

food<-read.csv( "protein.csv" )
View(food)
set.seed(1)
grpMeat<-kmeans(food[,c("WhiteMeat","RedMeat")],3,nstart=10)

food2<-food[,c(2,3)]

grpMeat<-kmeans(food[,c(2,3)],centers=3,nstart=10)

grpMeat

## print output in order
o=order(grpMeat$cluster)
o
data.frame(food$Country[o],grpMeat$cluster[o])
##
plot(food$RedMeat,food$WhiteMeat,col=grpMeat$cluster) # full name
text(x=food$Red,y=food$White +1,labels=food$Country,col=grpMeat$cluster)

## plot cluster
plot(food$RedMeat,food$WhiteMeat,col=grpMeat$cluster)
plot(food$RedMeat,food$WhiteMeat,pch=grpMeat$cluster)

plot(food$Red,food$White) # partial name
plot(food$RedMeat,food$White) # partial name

plot(food$RedMeat,food$WhiteMeat,col=grpMeat$cluster,pch=grpMeat$cluster) # full name
text(x=food$Red,y=food$White-0.25,labels=food$Country,col=grpMeat$cluster)


######### in-class exercise
# change number of clusters
grpMeat2<-kmeans(food[,-1],centers=7,nstart=10)

o2=order(grpMeat2$cluster)
data.frame(food$Country[o2],grpMeat2$cluster[o2])
plot(food$RedMeat,food$WhiteMeat)
text(x=food$Red,y=food$White-0.5,labels=food$Country,col=grpMeat2$cluster)
########




##################### optional
## Sample code for the US Unemployment Rate Example
raw=read.csv("unempstates.csv")
tail(raw)

plot(raw[,5],type="l",ylim=c(0,12),xlab="month",ylab="unemployment rate")
which(names(raw)=="MD")
points(raw[,20],type="l",col="green")
points(raw[names(raw)=="NY"],type="l",col="red")
legend("topright",c("5","MD","NY"))

rawt=t(raw)
grpunemp3<-kmeans(rawt,centers=3,nstart=10)
rowMeans(rawt)->StateMean
apply(rawt,1,sd)->StateSD
plot(StateMean,StateSD)
text(x=StateMean,y=StateSD,labels=rownames(rawt),col=grpunemp3$cluster)


#################################################
############## Wine Example
#################################################
wine <- read.csv("wine.csv")

## scale
xwine <- scale(wine[,1:11])
apply(xwine,2,sd) # sd=1
apply(xwine,2,mean) # mean=0

## fit two clusters
two <- kmeans(xwine,2,nstart=10)
two$centers # big differences on all accounts
# what is the color distribution in each?
tapply(wine$color,two$cluster,table)
# the two clusters are red v. white!

# randomize order in plot, just so its not all white on top of red
# note that my cluster 1 was red; this could be flipped for you.
i <- sample(1:nrow(xwine))  
plot(wine$fixed.acidity[i], wine$volatile.acidity[i],
     pch=21, cex=1.5, bty="n",
     xlab="fixed acidity",
     ylab="volatile acidity",
     bg=c("maroon","gold")[two$cluster[i]],
     col=c("maroon","gold")[wine$color[i]])


#####################################################
#### Hierarchical Clustering
#####################################################

#################### generate data
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
###############

## read dataset: data-clustering.csv

x=read.csv("data-clustering.csv")
View(x)
plot(V1~V2,data=x)

#### hierauchical clustering
?dist

hc.complete=hclust(dist(x),method="complete")
plot(hc.complete)

hc.average=hclust(dist(x),method="average")
hc.single=hclust(dist(x),method="single")
hc.centroid=hclust(dist(x),method="centroid")


#### plots
par(mfrow=c(1,4))
plot(hc.complete)
plot(hc.average)
plot(hc.single)
plot(hc.centroid)

#### cut tree
cutree(hc.complete,3) -> dc
dc

plot(x,col=dc)

par(mfrow=c(1,1))

#######################################################
#### example: USArrests data
#######################################################
## USArrests is part of the base R package
write.csv(USArrests,"USArrests.csv") ## write data into file


###### read data: row names

View(USArrests)
?USArrests
my.USArrests <- read.csv("USArrests.csv")
View(my.USArrests)

row.names(my.USArrests)<-my.USArrests[,1]
View(my.USArrests)

my.USArrests = my.USArrests[,-1]
View(my.USArrests)
############

states=rownames(USArrests)
states

arrest.reasons=colnames(USArrests)
arrest.reasons

hc.complete=hclust(dist(USArrests),method="complete")
plot(hc.complete)

cutree(hc.complete,3)
summary(USArrests)

attach(USArrests)
summary(USArrests)


USArrests2=scale(USArrests)
summary(USArrests2)

hc.complete.2=hclust(dist(USArrests2),method="complete")
plot(hc.complete.2 ,main="Hierachical Clustering with Standardization")



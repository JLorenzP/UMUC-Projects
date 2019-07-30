## Setting Working Dirctory and Uploading the file
setwd("L:/SanDiskSecureAccessV2.0/UMUC MS 630/Assignments/Assignment 5")

abalone<-read.csv(file="abalone.csv", head=TRUE, sep = "," )

dir()
dim(abalone)


## Looking at the Abalone Data set to see what needs to be preprocessed
View(abalone)

str(abalone)

summary(abalone)

# Converting Factors in Gender to Numeric Variables
aba2=abalone
aba2$Sex.num[aba2$Sex=="M"]<-0
aba2$Sex.num[aba2$Sex=="F"]<-1
aba2$Sex.num[aba2$Sex=="I"]<-2

str(aba2)

## Remove the Sex Factor and Ring Attribute 
aba3=aba2
aba3$Sex=NULL
aba3$Rings=NULL

str(aba3)


## Scaling the attributes in the dataset
scale(aba3)


## Looking for Missing variables and Final Check of database
summary(aba3)

apply(aba3, 2, function(aba3) sum(is.na(aba3)))

dim(aba3)

## Finding the appropriate number of Clusters 
## Analysis 1:  ELBOW OR BEND 

clsaba3=aba3

library("factoextra", lib.loc="~/R/win-library/3.3")
library("ggplot2", lib.loc="~/R/win-library/3.3")

mydata <- clsaba3
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# R Code computes Elbow method for kmeans
fviz_nbclust(clsaba3, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)



## Analysis 2 Calinsky Criterion from 1 to 10 groups
library("permute", lib.loc="~/R/win-library/3.3")
library("lattice", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("vegan", lib.loc="~/R/win-library/3.3")
fit <- cascadeKM(scale(clsaba3, center = TRUE,  scale = TRUE), 1, 15, iter = 100000)
plot(fit, sortclsaba3 = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion  optimal number of clusters:", calinski.best, "\n")

## Analysis 3 NBCLUST

# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.

library("NbClust", lib.loc="~/R/win-library/3.3")
nc <- NbClust(clsaba3,
              min.nc=2, max.nc=15,
              method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


########################### Box Plotting Data set

boxplot(aba3, col="light blue",
        main="Boxplot for Abalone Attributes")




##Loading Programs for Cluster Analysis 
library("cluster", lib.loc="~/R/win-library/3.3")
library("caret", lib.loc="~/R/win-library/3.3")
library("lattice", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("ggplot2", lib.loc="~/R/win-library/3.3")



################################ First KMeans Analysis with #############
## 3 clusters fited by K-Means (Same as above) ###########################
#########################################################################


## Building the KC Model ## Three Cluster is the Base Line
summary(aba3)
set.seed(32)

kc<-kmeans(aba3, 3)

kc

#plotting or Graphing 
clusplot(aba3, kc$cluster, color=TRUE, shade = TRUE, labels=2, line=0)

plot(aba3)
plot(aba3, col =(kc$cluster) , main="K-Means result with 3 clusters", pch=20, cex=2)


## plotcluster function from fpc package to draw discriminant projection plot
library("fpc", lib.loc="~/R/win-library/3.3")

plotcluster(aba3, kc$cluster, main="Discriminant Projection Plot using 3 clusters")

##We can see the data is clustered very well, there are no collapse between 
##clusters. Next, we draw parallel coordinates plot to see how variables 
##contributed in each cluster

library("MASS", lib.loc="C:/Program Files/R/R-3.3.2/library")
parcoord(aba3, kc$cluster)


## Assignment 7 Additional parameters requested by Dr. Firdu Bati
kc$size

kc$betweenss

kc$tot.withinss

kc$withinss

kc$iter

#################### Intiating K Center means for Three Cluster analysis
kc$centers





##Building the Clustering Evaluation Cross Validation
table(abalone$Rings)

confuseTable.kc <- table(abalone$Rings, kc$cluster)
confuseTable.kc

library("stats4", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("modeltools", lib.loc="~/R/win-library/3.3")
library("grid", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("flexclust", lib.loc="~/R/win-library/3.3")
library("cluster", lib.loc="~/R/win-library/3.3")

## Adjusted Rand Index
randIndex(confuseTable.kc)


##########Testing Silhouette Package
sil<-silhouette(kc$cluster, dist(aba3))
head(sil[, 1:3], 10)

plot(sil, main ="Silhouette plot - K-means")

# Summary of silhouette analysis
si.sum <- summary(sil)

# Average silhouette width of each cluster
si.sum$clus.avg.widths

# The total average (mean of all individual silhouette widths)
si.sum$avg.width

# The size of each clusters
si.sum$clus.sizes

# Default plot
fviz_silhouette(kc.res)


#####################################################################
###################Round 1B for Testing 2 Clusters instead of 3##########
#####################################################################
## Testing Two clusters

aba3b=aba3
summary(aba3b)
set.seed(32)

kc1b<-kmeans(aba3b, 2)

kc1b

#plotting or Graphing 
clusplot(aba3b, kc1b$cluster, color=TRUE, shade = TRUE, labels=2, line=0)

plot(aba3b)
plot(aba3b, col =(kc1b$cluster) , main="K-Means result with 2 clusters", pch=20, cex=2)


## plotcluster function from fpc package to draw discriminant projection plot
library("fpc", lib.loc="~/R/win-library/3.3")

plotcluster(aba3b, kc1b$cluster, main="Discriminant Projection Plot using 2 clusters")

##Next, we draw parallel coordinates plot to see how variables 
##contributed in each cluster

library("MASS", lib.loc="C:/Program Files/R/R-3.3.2/library")
parcoord(aba3b, kc1b$cluster)


## Assignment 7 Additional parameters requested by Dr. Firdu Bati
kc1b$size

kc1b$betweenss

kc1b$tot.withinss

kc1b$withinss

kc1b$iter


##Building the Clustering Evaluation Cross Validation
table(abalone$Rings, kc1b$cluster)

table(abalone$Rings)


confuseTable.kc1b <- table(abalone$Rings, kc1b$cluster)
confuseTable.kc1b

library("stats4", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("modeltools", lib.loc="~/R/win-library/3.3")
library("grid", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("flexclust", lib.loc="~/R/win-library/3.3")
library("cluster", lib.loc="~/R/win-library/3.3")

## Adjusted Rand Index
randIndex(confuseTable.kc1b)


################################ Second KMeans Analysis with #############
## 7 clusters fited by K-Means (Same as above) ###########################
#########################################################################

##Loading Programs for Cluster Analysis 
library("cluster", lib.loc="~/R/win-library/3.3")
library("caret", lib.loc="~/R/win-library/3.3")
library("lattice", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("ggplot2", lib.loc="~/R/win-library/3.3")


## Building the KC Model ## Seven Cluster is the Base Line
aba7=aba3
summary(aba7)
set.seed(32)

kc7<-kmeans(aba7, 7)

kc7

#plotting or Graphing 
clusplot(aba7, kc7$cluster, color=TRUE, shade = TRUE, labels=2, line=0)

plot(aba7)
plot(aba7, col =(kc7$cluster) , main="K-Means result with 7 clusters", pch=20, cex=2)


## plotcluster function from fpc package to draw discriminant projection plot
library("fpc", lib.loc="~/R/win-library/3.3")

plotcluster(aba7, kc7$cluster, main="Discriminant Projection Plot using 7 clusters")


library("MASS", lib.loc="C:/Program Files/R/R-3.3.2/library")
parcoord(aba3, kc7$cluster)


## Assignment 7 Additional parameters requested by Dr. Firdu Bati
kc7$size

kc7$betweenss

kc7$tot.withinss

kc7$withinss

kc7$iter

############################### Initiating K Center Means for Seven Cluster Analysis 
kc7$centers


##Building the Clustering Evaluation Cross Validation
table(abalone$Rings, kc7$cluster)

table(abalone$Rings)

## Comparison of the Rings with 3 clusters fited by K-Means (Same as above)
confuseTable.kc7 <- table(abalone$Rings, kc7$cluster)
confuseTable.kc7

library("stats4", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("modeltools", lib.loc="~/R/win-library/3.3")
library("grid", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("flexclust", lib.loc="~/R/win-library/3.3")

## Adjusted Rand Index
randIndex(confuseTable.kc7)

################################ THird KMeans Analysis with #############
## 15  clusters fited by K-Means (Same as above) ###########################
#########################################################################

#########################  Third Analysis 
##Loading Programs for Cluster Analysis 
library("cluster", lib.loc="~/R/win-library/3.3")
library("caret", lib.loc="~/R/win-library/3.3")
library("lattice", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("ggplot2", lib.loc="~/R/win-library/3.3")


## Building the KC Model ## 15 Cluster
aba15=aba3
summary(aba15)
set.seed(32)

kc15<-kmeans(aba15, 15)

kc15

#plotting or Graphing 
clusplot(aba15, kc15$cluster, color=TRUE, shade = TRUE, labels=2, line=0)

plot(aba15)
plot(aba15, col =(kc15$cluster) , main="K-Means result with 15 clusters", pch=20, cex=2)


## plotcluster function from fpc package to draw discriminant projection plot
library("fpc", lib.loc="~/R/win-library/3.3")

plotcluster(aba15, kc15$cluster, main="Discriminant Projection Plot using 15 clusters")


library("MASS", lib.loc="C:/Program Files/R/R-3.3.2/library")
parcoord(aba15, kc15$cluster)


## Assignment 7 Additional parameters requested by Dr. Firdu Bati
kc15$size

kc15$betweenss

kc15$tot.withinss

kc15$withinss

kc15$iter



##Building the Clustering Evaluation Cross Validation
table(abalone$Rings, kc15$cluster)

table(abalone$Rings)

## Comparison of the Rings with 15 clusters fited by K-Means (Same as above)
confuseTable.kc15 <- table(abalone$Rings, kc15$cluster)
confuseTable.kc15

library("stats4", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("modeltools", lib.loc="~/R/win-library/3.3")
library("grid", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("flexclust", lib.loc="~/R/win-library/3.3")

## Adjusted Rand Index
randIndex(confuseTable.kc15)



###########################REMOVAL OF I ATTRIBUTE FOR SEX KMeans Analysis with #############
## 2 clusters WITHOUT SEX ATTRIBUTE I by K-Means (Same as above) ###########################
#########################################################################

## Setting Working Dirctory and Uploading the file
setwd("L:/SanDiskSecureAccessV2.0/UMUC MS 630/Assignments/Assignment 5")

abalonemf<-read.csv(file="abalone_maleanfemale.csv", head=TRUE, sep = "," )

dir()
dim(abalonemf)


## Looking at the Abalone Data set to see what needs to be preprocessed
View(abalonemf)

str(abalonemf)


# Converting Factors in Gender to Numeric Variables and removing Sex and RINGS
abalonemf$Sex.num[abalonemf$Sex=="M"]<-0
abalonemf$Sex.num[abalonemf$Sex=="F"]<-1

aba7ri=abalonemf
str(aba7ri)

aba7ri$Sex=NULL
aba7ri$Rings=NULL
scale(aba7ri)

summary(aba7ri)

##Loading Programs for Cluster Analysis 
library("cluster", lib.loc="~/R/win-library/3.3")
library("caret", lib.loc="~/R/win-library/3.3")
library("lattice", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("ggplot2", lib.loc="~/R/win-library/3.3")


## Building the KC Model ## Seven Cluster without I for Sex is the Base Line
# Converting Factors in Gender to Numeric Variables

set.seed(32)

kc7ri<-kmeans(aba7ri, 2)

kc7ri

#plotting or Graphing 
clusplot(aba7ri, kc7ri$cluster, color=TRUE, shade = TRUE, labels=2, line=0)

plot(aba7ri)
plot(aba7ri, col =(kc7ri$cluster) , main="K-Means result with 2 clusters only Male and Female", pch=20, cex=2)

## Creating a blox plot for Male and Female Attributes only
boxplot(aba7ri, col="light green",
        main="Male and Female Variables only for Sex for unscaled data")



## plotcluster function from fpc package to draw discriminant projection plot
library("fpc", lib.loc="~/R/win-library/3.3")

plotcluster(aba7ri, kc7ri$cluster, main="Discriminant Projection Plot using 2 clusters, only Male and Female")


library("MASS", lib.loc="C:/Program Files/R/R-3.3.2/library")
parcoord(aba7ri, kc7ri$cluster)


## Assignment 7 Additional parameters requested by Dr. Firdu Bati
kc7ri$size

kc7ri$betweenss

kc7ri$tot.withinss

kc7ri$withinss

kc7ri$iter



##Building the Clustering Evaluation Cross Validation
table(abalonemf$Rings, kc7ri$cluster)
table(abalonemf$Rings)

## Comparison of the Rings with 2 clusters fited by K-Means (Same as above)
confuseTable.kc7ri <- table(abalonemf$Rings, kc7ri$cluster)
confuseTable.kc7ri

library("stats4", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("modeltools", lib.loc="~/R/win-library/3.3")
library("grid", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("flexclust", lib.loc="~/R/win-library/3.3")

## Adjusted Rand Index
randIndex(confuseTable.kc7ri)



###########################REMOVAL OF MALE ATTRIBUTE FOR SEX KMeans Analysis with #############
## 2 clusters WITHOUT SEX ATTRIBUTE MALE by K-Means (Same as above) ###########################
#########################################################################

## Setting Working Dirctory and Uploading the file
setwd("L:/SanDiskSecureAccessV2.0/UMUC MS 630/Assignments/Assignment 5")

abalonefi<-read.csv(file="abalone_femaleani.csv", head=TRUE, sep = "," )

dir()
dim(abalonefi)


## Looking at the Abalone Data set to see what needs to be preprocessed
View(abalonefi)

str(abalonefi)


# Converting Factors in Gender to Numeric Variables and removing Sex and RINGS
abalonefi$Sex.num[abalonefi$Sex=="F"]<-0
abalonefi$Sex.num[abalonefi$Sex=="I"]<-1

aba7fi=abalonefi
str(aba7fi)

aba7fi$Sex=NULL
aba7fi$Rings=NULL
scale(aba7fi)

summary(aba7fi)

##Loading Programs for Cluster Analysis 
library("cluster", lib.loc="~/R/win-library/3.3")
library("caret", lib.loc="~/R/win-library/3.3")
library("lattice", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("ggplot2", lib.loc="~/R/win-library/3.3")


## Building the KC Model ## Seven Cluster without Male for Sex is the Base Line
# Converting Factors in Gender to Numeric Variables

set.seed(32)

kc7fi<-kmeans(aba7fi, 2)

kc7fi

#plotting or Graphing 
clusplot(aba7fi, kc7fi$cluster, color=TRUE, shade = TRUE, labels=2, line=0)

plot(aba7fi)
plot(aba7fi, col =(kc7fi$cluster) , main="K-Means result with 2 clusters only Female & Infant", pch=20, cex=2)


## plotcluster function from fpc package to draw discriminant projection plot
library("fpc", lib.loc="~/R/win-library/3.3")

plotcluster(aba7fi, kc7fi$cluster, main="Discriminant Projection Plot using 2 clusters, only Female & Infant")

##Next, we draw parallel coordinates plot to see how variables 
##contributed in each cluster

library("MASS", lib.loc="C:/Program Files/R/R-3.3.2/library")
parcoord(aba7fi, kc7fi$cluster)


## Assignment 7 Additional parameters requested by Dr. Firdu Bati
kc7fi$size

kc7fi$betweenss

kc7fi$tot.withinss

kc7fi$withinss

kc7fi$iter



##Building the Clustering Evaluation Cross Validation
table(abalonefi$Rings, kc7fi$cluster)
table(abalonefi$Rings)

## Comparison of the Rings with 2 clusters fited by K-Means (Same as above)
confuseTable.kc7fi <- table(abalonefi$Rings, kc7fi$cluster)
confuseTable.kc7fi

library("stats4", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("modeltools", lib.loc="~/R/win-library/3.3")
library("grid", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("flexclust", lib.loc="~/R/win-library/3.3")

## Adjusted Rand Index
randIndex(confuseTable.kc7fi)




###########################REMOVAL OF I and M ATTRIBUTE FOR SEX KMeans Analysis with #############
## 7 clusters WITHOUT SEX ATTRIBUTE I and M by K-Means (Same as above) ###########################
#########################################################################

## Setting Working Dirctory and Uploading the file
setwd("L:/SanDiskSecureAccessV2.0/UMUC MS 630/Assignments/Assignment 5")

abalonefem<-read.csv(file="abalone_female.csv", head=TRUE, sep = "," )

dir()
dim(abalonefem)


## Looking at the Abalone Data set to see what needs to be preprocessed
View(abalonefem)

str(abalonefem)

summary(abalonefem)

# Converting Factors in Gender to Numeric Variables and removing Sex and RINGS

abalonefem$Sex.num[abalonefem$Sex=="Fem"]<-1

scale(abalonefem)
aba7fem=abalonefem
str(aba7fem)

aba7fem$Sex=NULL
aba7fem$Rings=NULL
summary(aba7fem)

##Loading Programs for Cluster Analysis 
library("cluster", lib.loc="~/R/win-library/3.3")
library("caret", lib.loc="~/R/win-library/3.3")
library("lattice", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("ggplot2", lib.loc="~/R/win-library/3.3")


## Building the KC Model ## Seven Cluster without I for Sex is the Base Line
# Converting Factors in Gender to Numeric Variables



set.seed(32)

kc7fem<-kmeans(aba7fem, 7)

kc7fem

##plotting or Graphing NULL
##clusplot(aba7fem, kc7fem$cluster, color=TRUE, shade = TRUE, labels=2, line=0)

plot(aba7fem)
plot(aba7fem, col =(kc7fem$cluster) , main="K-Means result with 7 clusters, Females only", pch=20, cex=2)


## plotcluster function from fpc package to draw discriminant projection plot
library("fpc", lib.loc="~/R/win-library/3.3")

plotcluster(aba7fem, kc7fem$cluster, main="Discriminant Projection Plot using 7 clusters, females only")

##We can see the data is clustered very well, there are no collapse between 
##clusters. Next, we draw parallel coordinates plot to see how variables 
##contributed in each cluster

library("MASS", lib.loc="C:/Program Files/R/R-3.3.2/library")
parcoord(aba7fem, kc7fem$cluster)


## Assignment 7 Additional parameters requested by Dr. Firdu 
kc7fem$size

kc7fem$betweenss

kc7fem$tot.withinss

kc7fem$withinss

kc7fem$iter



######################################  TESTING THE PAM ALGORITHM 

## Method 2:Partition around Medoids to estimate number of clusters with pamk function 
#### Not very descriptive 

aba3pam=aba3 
summary(aba3)
set.seed(32)


library("fpc", lib.loc="~/R/win-library/3.3")
pamk.best <- pamk(aba3pam)


kcpam<-pam(aba3pam, 2)
plot(kcpam)






##Building the Clustering Evaluation Cross Validation
table(abalonefem$Rings, kc7fem$cluster)
table(abalonefem$Rings)

## Comparison of the Rings with 3 clusters fited by K-Means (Same as above)
confuseTable.kc7fem <- table(abalonefem$Rings, kc7fem$cluster)
confuseTable.kc7fem

library("stats4", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("modeltools", lib.loc="~/R/win-library/3.3")
library("grid", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("flexclust", lib.loc="~/R/win-library/3.3")

## Adjusted Rand Index
randIndex(confuseTable.kc7fem)



#################################################
#############  APPENDIX 1
####### Experiments to finding the best cluster size
##http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters/15376462#15376462

## Reproducible data 
## copying the preprocessed aba3 data to new file 
clsaba3=aba3

## Method 1:  ELBOW OR BEND 
## WORKDS
mydata <- clsaba3
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## reference is http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning

# R Code computes Elbow method for kmeans
fviz_nbclust(clsaba3, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)


# Elbow method for Pam Clustering 
fviz_nbclust(iris.scaled, pam, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)




## Method 2:Partition around Medoids to estimate number of clusters with pamk function 
#### Not very descriptive 

library("fpc", lib.loc="~/R/win-library/3.3")
pamk.best <- pamk(clsaba3)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(clsaba3, pamk.best$nc))


## Method 3 Calinsky Criterion from 1 to 10 groups
library("vegan", lib.loc="~/R/win-library/3.3")
fit <- cascadeKM(scale(clsaba3, center = TRUE,  scale = TRUE), 1, 15, iter = 10000)
plot(fit, sortclsaba3 = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion  optimal number of clusters:", calinski.best, "\n")



### Method 4:  Using Bayesian Information Criterion to determine
## Optimal number of clusters
# See http://www.jstatsoft.org/v18/i06/paper
# http://www.stat.washington.edu/research/reports/2006/tr504.pdf
#
library("mclust", lib.loc="~/R/win-library/3.3")
# Run the function to see how many clusters
# it finds to be optimal, set it to search for
# at least 1 model and up 20.
clsaba3_clust <- Mclust(as.matrix(clsaba3), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")

plot(clsaba3_clust)


nb <- NbClust(clsaba3, distance = "euclidean", min.nc = 2,
              max.nc = 15, method = "complete", index ="all")

nb

fviz_nbclust(nb) + theme_minimal()

nc <- NbClust(clsaba3,
              min.nc=2, max.nc=15,
              method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")



## Method 5 Affinity Propagation clustering 
library("apcluster", lib.loc="~/R/win-library/3.3")


d.apclus <- apcluster(negDistMat(r=2), clsaba3)
cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")
# 4
heatmap(d.apclus)
plot(d.apclus, clsaba3)



## Method 6:Gap Clusters

library(cluster)
clusGap(clsaba3, kmeans, 20, B = 100, verbose = interactive())

## R code for Silhouette method for K-means clustering
library(cluster)
k.max <- 15
data <- clsaba3
sil <- rep(0, k.max)
# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(data, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)





####################################### APPENDIX 2
#################### DENDROGRAMS
############################## Dendrograms or Heirarchical Algorithms
##############################
#### References http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
#### References http://www.phytools.org/eqg/Exercise_3.2/

haba1=aba7ri
summary(haba1)

#haba1$Sex=NULL
haba2=haba1
summary(haba2)

dst <-dist(scale(haba2), method = "euclidean")
hc<- hclust(dst, method = "ward.D2")



## Default Plot and adjustments
plot(hc)

################ cluster analysis for Berkely 
## Reference  https://www.stat.berkeley.edu/~s133/Cluster2a.html
plot(hc, hang = -1, cex = 0.6)

groups.3 = cutree(hc,3)

table(groups.3)

counts = sapply(2:6,function(ncl)table(cutree(hc,ncl)))
names(counts)=2:6
counts

abalone$Rings[groups.3 == 1]

table(groups.3,abalonemaleanfemle$Rings)
table(groups.3,abalonemaleanfemale$Sex)
table(groups.3,abalonemaleanfemale$Shell_weight)
table(groups.3,abalonemaleanfemale$Length)

plot(abalone$Shell_weight, col="dark red")

sapply(unique(groups.3),function(g)abalone$Rings[groups.3 == g])




#### In order to visualize the result of a hierarchical clustering analysis using the 
####function plot.dendrogram(), we must firstly convert it as a dendrogram.

##The format of the function plot.dendrogram() is:
### plot(x, type = c("rectangle", "triangle"), horiz = FALSE)

hc1 <- as.dendrogram(hc)
plot(hc1, type = "rectangle", ylab = "Height")



# Define nodePar
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")

# Customized plot; remove labels
plot(hc1, ylab = "Height", nodePar = nodePar, leaflab = "none")


# Change edge color
plot(hc1,  xlab = "Height", nodePar = nodePar, 
     edgePar = list(col = 2:3, lwd = 2:1))

### Phylogentic Trees NOT WORKING 
library("ape", lib.loc="~/R/win-library/3.3")
library("ctv", lib.loc="~/R/win-library/3.3")
library("phytools", lib.loc="~/R/win-library/3.3")
library("maps", lib.loc="~/R/win-library/3.3")
library("rgl", lib.loc="~/R/win-library/3.3")

plot.phylo(hc1)
plot(as.phylo, cex = 0.6, label.offset = 0.5)

plot(as.phylo, type = "fan")

## Visualization using the default theme named theme_dendro()
## ggdendrogram(hc)

library("ggdendro", lib.loc="~/R/win-library/3.3")
ggdendrogram(hc1, rotate = TRUE, theme_dendro = FALSE)




###################################### Appendix 3
#############  PLOTTING Functions
length = abalone$Length     
whole_weight = abalone$Whole_weight  
plot(length, whole_weight,        # plot the variables 
     xlab="Length",       # x???axis label 
     ylab="Whole_weight",
     col="dark red") 


###################### Appendix 4 used for PAM CLustering
## Building the KC Model ## Three Cluster is the Base Line

aba3pam=aba3 
summary(aba3)
set.seed(32)

kcpam<-pam(aba3pam, 2)
plot(kcpam)

##https://www.stat.berkeley.edu/~s133/Cluster2a.html
table(abalone$Rings, kcpam$clustering)


#plotting or Graphing 
clusplot(aba3, kc$cluster, color=TRUE, shade = TRUE, labels=2, line=0)

plot(aba3)
plot(aba3, col =(kc$cluster) , main="K-Means result with 3 clusters", pch=20, cex=2)


## plotcluster function from fpc package to draw discriminant projection plot
library("fpc", lib.loc="~/R/win-library/3.3")

plotcluster(aba3, kc$cluster, main="Discriminant Projection Plot using 3 clusters")

##We can see the data is clustered very well, there are no collapse between 
##clusters. Next, we draw parallel coordinates plot to see how variables 
##contributed in each cluster

library("MASS", lib.loc="C:/Program Files/R/R-3.3.2/library")
parcoord(aba3, kc$cluster)


## Assignment 7 Additional parameters requested by Dr. Firdu Bati
kc$size

kc$betweenss

kc$tot.withinss

kc$withinss

kc$iter

#################### Intiating K Center means for Three Cluster analysis
kc$centers





##Building the Clustering Evaluation Cross Validation
table(abalone$Rings)

confuseTable.kc <- table(abalone$Rings, kc$cluster)
confuseTable.kc

library("stats4", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("modeltools", lib.loc="~/R/win-library/3.3")
library("grid", lib.loc="C:/Program Files/R/R-3.3.2/library")
library("flexclust", lib.loc="~/R/win-library/3.3")
library("cluster", lib.loc="~/R/win-library/3.3")

## Adjusted Rand Index
randIndex(confuseTable.kc)


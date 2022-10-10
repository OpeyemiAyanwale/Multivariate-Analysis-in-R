# PROJECT TITLE: Cluster Analysis of the Chatterjee-Price Attitude Dataset
# DATE CREATED: 06 October 2022 #
# SCRIPT BY: Opeyemi Ayanwale
# MATRIC NO: 236783

#############################
# R SETUP
#############################

#load the necessary library
library(tidyverse)
library(cluster)
library(factoextra)  #cluster algorithms & visualization
library(datasets)  #to load the attitude dataset from R###

# to read more about the dataset
?attitude

#
df <- attitude

df <- na.omit(df)

df <- scale(df)

#
head(df)

###Using the non-hierarchical k-means method####

####Distance Matrix ####
dist_data <-dist(attitude, method = 'euclidean')
dist_data

hdata <- hclust(dist_data)
hdata

###dissimilaririty matrix
get_dist(df)


#clustering for k=2
k2 <- kmeans(df, centers=2, nstart = 25)
k2
fviz_cluster(k2, df)

##clustering for k=3
k3 <- kmeans(df, centers=3, nstart=25)
k3
fviz_cluster(k3, df)

##clustering for k=4
k4 <- kmeans(df, centers=4, nstart=25)
k4
fviz_cluster(k4, df)

##clustering for k=5
k5 <- kmeans(df, centers=5, nstart=25)
k5
fviz_cluster(k5, df)


###calculatee gap statistics based on number of clusters###
gap_stat <- clusGap(df, FUN = kmeans, nstart =25, K.max = 10, B=30)
fviz_gap_stat(gap_stat)

#from the plot the gap statistics is highest at k = 2 clusters
#Perform K-means clustering with optimal k (k=2)

#make this reproducible
set.seed(1)

km <-kmeans(df, centers = 2, nstart = 25)
km

#visualize
fviz_cluster(km, data = df)

##### Another method is Agglomerative Clustering In R#######
df <- attitude

#remove any missing value that might be present in the data
df <- na.omit(df)

#dissimilarity matrix
d <- dist(df, method = 'euclidean')


#hierarchical clustering using complete linkage
hc1 <- hclust(d, method = 'complete')
hc1

#plot the obtained dendrogram
plot(hc1, cex=0.6, hang = -1)


###using agnes (cluster) function in R####
hc2 <- agnes(df, method = 'complete')
hc2

#agglomerative coefficient ##
hc2$ac


##plot dendrogram ###
pltree(hc2, cex=0.6, hang = -1)


###using ward method####
hc3 <- agnes(df, method = 'ward')


##plot dendrogram ###
pltree(hc3, cex=0.6, hang = -1, main ="Dendrogram of agnes")

##In order to identify clusters we can cut the dendrogram with cuttree function
#ward's method
hc4 <- hclust(d, method = "ward.D2")

sub_grp <- cutree(hc4, k=2)  #

table(sub_grp)     #no of members in each cluster

plot(hc4, cex=0.6)

fviz_cluster(list(data = df, cluster =sub_grp))

##summary of the analysis##
## for the purpose of the final report, the non-hierarchical k-means method will be used ####

##### for citation in report releted to R####
citation("cluster")
citation("factoextra")
citation("datasets")

###URL for the attitude dataset###
#https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/attitude.html#

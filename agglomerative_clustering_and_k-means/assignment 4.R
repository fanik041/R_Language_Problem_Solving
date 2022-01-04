library(cluster)
library(distances)

cl = c(10, 20, 40, 80, 85, 121, 160, 168, 195)

datas <-data.frame(cl)
d<- dist(datas,method="euclidean")
H.fit<- hclust(d,method="single")
plot(H.fit)

h.fit.2 <- hclust(d, method = "complete")
plot(h.fit.2)


utilities.df.norm <- sapply(cl.df, scale)
row.names(utilities.df.norm) <- row.names(c)
# run kmeans algorithm
km <- kmeans(cl.df, centers.selected)
# show cluster membership
km$cluster
km$centers
km$withinss
km$size
km$mean

k.m <- kmeans(c, centers =centers.selected, iter.max =1)

kmeans(c, centers.selected, iter.max = 1, nstart = 1)

plotcluster(cl, km$cluster, iter.max =0)

library(stats)
library(factoextra)
library(ggplot2)
library(cluster)
library(fpc)

data  = c(10, 20, 40, 80, 85, 121, 160, 168, 195 )

centers.selected = c(160, 168, 195)

res.km <- kmeans(data, centers=centers.selected, iter.max = 2)

plotcluster(data, res.km$cluster)

res.km$centers

res.km$withinss

#text(x=data, y=res.km$cluster)

centers.selected.2 = c(40,121,195)

res.km.2 <- kmeans(data, centers=centers.selected.2, iter.max = 1)
plotcluster(data, res.km.2$cluster)

res.km.2$centers
#text(x=data, y=res.km$cluster)
str(res.km)
str(res.km.2)
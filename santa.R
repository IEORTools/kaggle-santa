# load package
library(TSP)
library(doMC)

registerDoMC(cores=3)

# get data
setwd("./Documents/R-work/Kaggle/santa/")
x <- read.csv("santa_cities.csv")
bnch <- read.csv("random_paths_benchmark.csv")
  
  
set.seed(3)

# create clusters
cl <- kmeans(x[,2:3],80)

# example tour
SET <- cl$cluster==1
d <- dist(x[SET,2:3])
plot(x[SET,2:3], type="p", pch=20, cex=.05, col=cl$cluster[1])

tspdat <- TSP(d) 
ex.tour <- solve_TSP(tspdat, method="nn")
ex.tour



# mutli-core tsp solve

CLUS <- 1:80
tours <- foreach(clus=CLUS, .packages=c('TSP'), .inorder=FALSE) %dopar% {
    SET <- cl$cluster==clus
    d <- dist(x[SET,2:3])
    tspdat <- TSP(d)
    solve_TSP(tspdat, method="nn")
  }

# sum of tour lengths.  Note! does not link clusters together
tour.len <- sapply(tours, FUN = attr, "tour_length")
sum(tour.len)

# complete santa tour
tour.cmpl <- as.numeric(unlist(lapply(tours, labels)))

# total distance
pairs <- matrix(c(tour.cmpl[1:149999],tour.cmpl[2:150000]), ncol=2)
seg.dist <- sapply(1:149999, function(i) dist(x[pairs[i,],2:3]))
sum(seg.dist)




#### USED FOR ANALYSIS ####

# plot cluster with tour
CLUS <- 4
plot(x[cl$cluster==CLUS,2:3], type="p", pch=20, cex=.01, col="blue")
for(i in 1:(cl$size[CLUS]-1)){
  lab1 <- as.numeric(labels(tours[[CLUS]])[i])
  lab2 <- as.numeric(labels(tours[[CLUS]])[i+1])
  pt1 <- x[lab1,2:3]
  pt2 <- x[lab2,2:3]
  lines(x=c(pt1[1],pt2[1]), y=c(pt1[2],pt2[2]), col="red")
}

# distances of each tour segment
TSET <- as.numeric(labels(tours[[CLUS]]))
test <- sapply(1:(length(TSET)-1), function(i) dist(x[c(TSET[i],TSET[i+1]),2:3]))

# max segment
max(test)
maxindx <- match(max(test), test)
TSET[c(maxindx,maxindx+1)]


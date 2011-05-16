KMeans <-
function (x, centers, iter.max = 10, num.seeds = 10){
    if (mode(x) == "numeric") 
        x <- data.frame(new.x = x)
    KM <- kmeans(x = x, centers = centers, iter.max = iter.max)
    for (i in 2:num.seeds) {
        newKM <- kmeans(x = x, centers = centers, iter.max = iter.max)
        if (sum(newKM$withinss) < sum(KM$withinss)) {
            KM <- newKM
        }
    }
    KM$tot.withinss <- sum(KM$withinss)
    xmean <- apply(x, 2, mean)
    centers <- rbind(KM$centers, xmean)
    bss1 <- as.matrix(dist(centers)^2)
    KM$betweenss <- sum(as.vector(bss1[nrow(bss1), ]) * c(KM$size, 
        0))
    return(KM)
}


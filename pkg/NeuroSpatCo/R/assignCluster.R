assignCluster <-
function (clusterData, origData, clusterVec){
    rowsDX <- row.names(clusterData)
    rowsX <- row.names(origData)
    clustAssign <- rep(NA, length(rowsX))
    validData <- rowsX %in% rowsDX
    clustAssign[validData] <- clusterVec
    return(as.factor(clustAssign))
}


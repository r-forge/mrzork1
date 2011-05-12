distClust <-
function(Carpeta,Grafica){
  #Grafica 0 no graficar nada; 1 graficar cada cluster, 2 graficar y grabar la imagen en workspace
 require(cluster)
 grups<-names(Carpeta)
  for(k in 1:length(Carpeta)){
    distancias<-new.env() ; comp<-as.numeric()
    for(i in 1:(length(Carpeta[[paste(grups[k])]])-2)){
      dates<-Carpeta[[paste(grups[k])]][[i]]
      setwd(Carpeta[[paste(grups[k])]][[length(Carpeta[[paste(grups[k])]])]])
      contador <- 0 ;s<-0 ;ms<-0
      contador<-length(dates$area[dates$area>quantile(dates$area,0.89)]) ; prop <- contador/length(dates$area)
#     paste("proporcion = ",prop*100,"%")
      clusters<-round(length(dates$area)*prop, digits = 0)
      if(clusters==1){clusters<-3}
      if(clusters==2){clusters<-3}
      ms[i]<-clusters
      .cluster <- Rcmdr::KMeans(model.matrix(~-1 + dates$centroidex, dates), centers = clusters,iter.max = 10, num.seeds = 100)
      dates$KMeans <- Rcmdr::assignCluster(model.matrix(~-1 + dates$centroidex, dates), dates, .cluster$cluster)
      centroidesclust<-.cluster$centers
      remove(.cluster,clusters)
      a<-sort(centroidesclust)
      r<-length(a); dis<-0
      for(p in 1:(r-1)){
	dis[p]<-a[p]-a[p+1]
      }
      dis<-(dis*-1) ; dmicras<-(dis/3.6)
      assign(paste("d",Carpeta[[paste(grups[k])]]$archivos[i],sep=""),dmicras,env=distancias)
      if(Grafica==1){clusplot((1-dates[1:2]),dates$KMeans ,diss = FALSE,span=TRUE, cor = T, stand = F,lines = 0,shade=T,col.p="pink",col.clus="darkblue",main=paste("Clusters Imagen",Carpeta[[paste(grups[k])]]$archivos[i],sep = ""))}
      if(Grafica==2){clusplot((1-dates[1:2]),dates$KMeans ,diss = FALSE,span=TRUE, cor = T, stand = F,lines = 0,shade=T,col.p="pink",col.clus="darkblue",main=paste("Clusters Imagen",Carpeta[[paste(grups[k])]]$archivos[i],sep = ""))
      savePlot(filename = paste(Carpeta[[paste(grups[k])]]$archivos[i],".png",sep = ""),type =  "png",device = dev.cur())}
      comp<-c(comp,dmicras) ; rm(dates,a,r)
    }
    if(k==1){dista1<-as.list(distancias);comp1<-comp}
    if(k==2){dista2<-as.list(distancias);comp2<-comp}
    if(k==3){dista3<-as.list(distancias);comp3<-comp}
    if(k==4){dista4<-as.list(distancias);comp4<-comp}
    if(k==5){dista5<-as.list(distancias);comp5<-comp}
    if(k==6){dista6<-as.list(distancias);comp6<-comp}
    if(k==7){dista7<-as.list(distancias);comp7<-comp}
    if(k==8){dista8<-as.list(distancias);comp8<-comp}
  }
  ns<-length(Carpeta)
  if(ns==1){
    salida<-list(distCarp1=dista1,allCarp1=comp1)
  }
  if(ns==2){
    salida<-list(distCarp1=dista1,distCarp2=dista2,allCarp1=comp1,allCarp2=comp2)
  }
  if(ns==3){
    salida<-list(distCarp1=dista1,distCarp2=dista2,distCarp3=dista3,allCarp1=comp1,allCarp2=comp2,allCarp3=comp3)
  }
  if(ns==4){
    salida<-list(distCarp1=dista1,distCarp2=dista2,distCarp3=dista3,distCarp4=dista4,allCarp1=comp1,allCarp2=comp2,allCarp3=comp3,allCarp4=comp4)
  }
  if(ns==5){
    salida<-list(distCarp1=dista1,distCarp2=dista2,distCarp3=dista3,distCarp4=dista4,distCarp5=dista5,allCarp1=comp1,allCarp2=comp2,allCarp3=comp3,allCarp4=comp4,allCarp5=comp5)
  }
  if(ns==6){
    salida<-list(distCarp1=dista1,distCarp2=dista2,distCarp3=dista3,distCarp4=dista4,distCarp5=dista5,distCarp6=dista6,allCarp1=comp1,allCarp2=comp2,allCarp3=comp3,allCarp4=comp4,allCarp5=comp5,allCarp6=comp6)
  }
  if(ns==7){
    salida<-list(distCarp1=dista1,distCarp2=dista2,distCarp3=dista3,distCarp4=dista4,distCarp5=dista5,distCarp6=dista6,distCarp7=dista7,allCarp1=comp1,allCarp2=comp2,allCarp3=comp3,allCarp4=comp4,allCarp5=comp5,allCarp6=comp6,allCarp7=comp7)
  }
  if(ns==8){
    salida<-list(distCarp1=dista1,distCarp2=dista2,distCarp3=dista3,distCarp4=dista4,distCarp5=dista5,distCarp6=dista6,distCarp7=dista7,distCarp8=dista8,allCarp1=comp1,allCarp2=comp2,allCarp3=comp3,allCarp4=comp4,allCarp5=comp5,allCarp6=comp6,allCarp7=comp7,allCarp8=comp8)
  }
  return(salida)
}


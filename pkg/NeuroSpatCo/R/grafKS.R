grafKS <-
function(Carpeta,Kest=TRUE,Lest=FALSE,density=FALSE){
kestgr = function(x){
a<-ppp(x$centroidex,x$centroidey,c(0,max(x$centroidex)),c(0,max(x$centroidey)),check=F)
  ks<-Kest(a)
  if(Kest==TRUE){
    plot(ks,main=paste('kest_',Carpeta[[paste(grups[k])]][[length(Carpeta[[paste(grups[k])]])-1]][length(Carpeta[[paste(grups[k])]])-(1+i)],sep = ""))
    savePlot(filename = paste(paste(Carpeta[[paste(grups[k])]][[length(Carpeta[[paste(grups[k])]])-1]][length(Carpeta[[paste(grups[k])]])-(1+i)]),"Kest.png",sep = ""),type =  "png")
  }
  if(Lest==TRUE){
    e <- envelope(a, Kest)
    plot(e, sqrt(./pi) ~ r, main = ("Lf unction + Envelops") )
    savePlot(filename = paste('l_',paste(Carpeta[[paste(grups[k])]][[length(Carpeta[[paste(grups[k])]])-1]][length(Carpeta[[paste(grups[k])]])-(1+i)]),".png",sep = ""),type =  "png")
  }
  if(density==TRUE){
    plot(density(a, 20),main=paste('Densidad ',Carpeta[[paste(grups[k])]][[length(Carpeta[[paste(grups[k])]])-1]][length(Carpeta[[paste(grups[k])]])-(1+i)],sep = ""))
    savePlot(filename = paste(paste(Carpeta[[paste(grups[k])]][[length(Carpeta[[paste(grups[k])]])-1]][length(Carpeta[[paste(grups[k])]])-(1+i)]),sep = "",".png"),type = "png")
  }
}
  grups<-names(Carpeta)
  dat<-0
  q<-1
  for(k in 1:length(Carpeta)){
    for(i in 1:(length(Carpeta[[paste(grups[k])]])-2)){
      setwd(Carpeta[[paste(grups[k])]][[length(Carpeta[[paste(grups[k])]])]])
      dates<-Carpeta[[paste(grups[k])]][[i]]
      kestgr(dates)
      q<-q+1
    }
  }
}


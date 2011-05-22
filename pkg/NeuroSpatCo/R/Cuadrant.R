Cuadrant <-
function(Carpeta,graf=FALSE){
  pest = function(x,graf=FALSE){
    a<-ppp(x$centroidex,x$centroidey,c(0,1024),c(0,768),check=F)
    m<-quadrat.test(a,nx=3,ny=3)
      if(graf==TRUE){
	x11()
	plot(a)
	plot(m,add=T,cex=1)
      }
    s<-m$p.value
    return(s)
  }
  grups<-names(Carpeta)
  dat<-0
  q<-1
  for(k in 1:length(Carpeta)){
    for(i in 1:(length(Carpeta[[paste(grups[k])]])-2)){
      dates<-Carpeta[[paste(grups[k])]][[i]]
      dat[q]<-pest(dates,graf)
      q<-q+1
    }
  }
  salida<-list(ppp=dat)
  return(salida)
}


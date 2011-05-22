reconsBeta <-
function(recortados,horizontales,zonaA=1,zonaB=1,grabar=FALSE){
  if(zonaA==1){dato1<-recortados$carp1}; if(zonaB==1){puntos<-horizontales$Carpeta1}
  if(zonaA==2){dato1<-recortados$carp2}; if(zonaB==2){puntos<-horizontales$Carpeta2}
  if(zonaA==3){dato1<-recortados$carp3}; if(zonaB==3){puntos<-horizontales$Carpeta3}
  if(zonaA==4){dato1<-recortados$carp4}; if(zonaB==4){puntos<-horizontales$Carpeta4}
  if(zonaA==5){dato1<-recortados$carp5}; if(zonaB==5){puntos<-horizontales$Carpeta5}
  if(zonaA==6){dato1<-recortados$carp6}; if(zonaB==6){puntos<-horizontales$Carpeta6}
  if(zonaA==7){dato1<-recortados$carp7}; if(zonaB==7){puntos<-horizontales$Carpeta7}
  if(zonaA==8){dato1<-recortados$carp8}; if(zonaB==8){puntos<-horizontales$Carpeta8}
  dato1inv<-1/dato1
  fb<-fitdist(dato1inv,"beta")
  a<-fb$estimate[1]
  b<-fb$estimate[2]
  x<-c(puntos[[1]][[1]],puntos[[2]][[1]],puntos[[3]][[1]])
  y<-c(puntos[[1]][[2]],puntos[[2]][[2]],puntos[[3]][[2]])
  z<-c(runif((length(x)),0,20))
  gen<-function(){
    distcol<-(1/rbeta(10,a,b))*3.6
    distcol2<-(1/rbeta(10,a,b))*3.6
    xp<-c(distcol[1],sum(distcol[1:2]),sum(distcol[1:3]),sum(distcol[1:4]),sum(distcol[1:5]))
    yp<-c(distcol2[1],distcol2[2],distcol2[3],distcol2[4],distcol2[5])
    w<-cbind(xp,yp)
    return(w)
  }
  genu<-gen()
  l1<-genu
  genu<-gen()
  l2<-cbind(genu[,1],(genu[,2]*2))
  genu<-gen()
  l3<-cbind(genu[,1],(genu[,2]*3))
  genu<-gen()
  l4<-cbind(genu[,1],(genu[,2]*4))
  genu<-gen()
  l5<-cbind(genu[,1],(genu[,2]*4))
  final<-rbind(l1,l2,l3,l4,l5)
  q<-0 ;es<-1;final2<-0;x2<-0;y2<-0
  for(i in 1:(length(final)/2)){
    if(final[i,][1]<max(x)){
      if(final[i,][2]<max(y)){
	x2[es]<-final[i,][[1]]
	y2[es]<-final[i,][[2]]
	es<-es+1
      }
    }
  }
  pointx<-x2
  pointy<-y2
  if(length(x)>750){
    xa<-runif((length(x)*5), min=0, max=(max(x)))
    ya<-runif((length(y)*5), min=0, max=(max(y)))
  }
  if(length(x)<=750){
    xa<-runif((length(x)*5), min=0, max=(max(x)))
    ya<-runif((length(y)*5), min=0, max=(max(y)))
  }
  open3d()
  plot3d(xa,ya,z,box=FALSE,axes = FALSE,col="darkslateblue", alpha=0.08,type="s",size=0.4,main="Reconstruccion")
  for(i in 1:length(pointx)){
    Mean <- c(pointx[i],pointy[i],10)
    Sigma2 <- matrix(c(400,9,4,9,350,12,4,12,10), 3,3)
    plot3d( ellipse3d(Sigma2, centre=Mean), col="indianred", alpha=0.15, add = TRUE,smooth = TRUE)
    x2 <- mvrnorm(45, Mean, Sigma2)
    plot3d(x2,box=FALSE,axes = FALSE,col="red", alpha=0.06,type="l",size=0.5, add = TRUE)
  }
  grid3d(c("x", "y+", "z"))
  if(grabar==TRUE){
    movie3d( spin3d(axis=c(0,0,1), rpm=2), duration=10 ,fps=20)
  }
}


imprDist <-
function(recortados){
  dim<-length(recortados)
  if(dim>8){print= "imposible que este objeto lo halla conseguido con este paquete!!"}
  if(dim<=8){
  if(dim==1){
    dat1<-descdist(recortados$carp1,boot=1000)
    ima1 <- recordPlot ()
    salida<-list(imagen=ima1,dist1=dat1)
  }
  if(dim==2){
    par(mfrow = c(1, 2))
    dat1<-descdist(recortados$carp1,boot=1000)
    dat2<-descdist(recortados$carp2,boot=1000)
    ima1 <- recordPlot ()
    salida<-list(imagen=ima1,dist1=dat1,dist2=dat2)
  }
  if(dim==3){
    par(mfrow = c(1, 3))
    dat1<-descdist(recortados$carp1,boot=1000)
    dat2<-descdist(recortados$carp2,boot=1000)
    dat3<-descdist(recortados$carp3,boot=1000)
    ima1 <- recordPlot ()
    salida<-list(imagen=ima1,dist1=dat1,dist2=dat2,dist3=dat3)
  }
  if(dim==4){
    par(mfrow = c(2, 2))
    dat1<-descdist(recortados$carp1,boot=1000)
    dat2<-descdist(recortados$carp2,boot=1000)
    dat3<-descdist(recortados$carp3,boot=1000)
    dat4<-descdist(recortados$carp4,boot=1000)
    ima1 <- recordPlot ()
    salida<-list(imagen=ima1,dist1=dat1,dist2=dat2,dist3=dat3,dist4=dat4)
  }
  if(dim==5){
    par(mfrow = c(3, 2))
    dat1<-descdist(recortados$carp1,boot=1000)
    dat2<-descdist(recortados$carp2,boot=1000)
    dat3<-descdist(recortados$carp3,boot=1000)
    dat4<-descdist(recortados$carp4,boot=1000)
    dat5<-descdist(recortados$carp5,boot=1000)
    ima1 <- recordPlot ()
    salida<-list(imagen=ima1,dist1=dat1,dist2=dat2,dist3=dat3,dist4=dat4,dist5=dat5)
  }
  if(dim==6){
    par(mfrow = c(3, 2))
    dat1<-descdist(recortados$carp1,boot=1000)
    dat2<-descdist(recortados$carp2,boot=1000)
    dat3<-descdist(recortados$carp3,boot=1000)
    dat4<-descdist(recortados$carp4,boot=1000)
    dat5<-descdist(recortados$carp5,boot=1000)
    dat6<-descdist(recortados$carp6,boot=1000)
    ima1 <- recordPlot ()
    salida<-list(imagen=ima1,dist1=dat1,dist2=dat2,dist3=dat3,dist4=dat4,dist5=dat5,dist6=dat6)
  }
  if(dim==7){
    par(mfrow = c(3, 3))
    dat1<-descdist(recortados$carp1,boot=1000)
    dat2<-descdist(recortados$carp2,boot=1000)
    dat3<-descdist(recortados$carp3,boot=1000)
    dat4<-descdist(recortados$carp4,boot=1000)
    dat5<-descdist(recortados$carp5,boot=1000)
    dat6<-descdist(recortados$carp6,boot=1000)
    dat7<-descdist(recortados$carp7,boot=1000)
    ima1 <- recordPlot ()
    salida<-list(imagen=ima1,dist1=dat1,dist2=dat2,dist3=dat3,dist4=dat4,dist5=dat5,dist6=dat6,dist7=dat7)
  }
  if(dim==8){
    par(mfrow = c(3, 3))
    dat1<-descdist(recortados$carp1,boot=1000)
    dat2<-descdist(recortados$carp2,boot=1000)
    dat3<-descdist(recortados$carp3,boot=1000)
    dat4<-descdist(recortados$carp4,boot=1000)
    dat5<-descdist(recortados$carp5,boot=1000)
    dat6<-descdist(recortados$carp6,boot=1000)
    dat7<-descdist(recortados$carp7,boot=1000)
    dat8<-descdist(recortados$carp8,boot=1000)
    ima1 <- recordPlot ()
    salida<-list(imagen=ima1,dist1=dat1,dist2=dat2,dist3=dat3,dist4=dat4,dist5=dat5,dist6=dat6,dist7=dat7,dist8=dat8)
  }
  }
  
  return(salida)
}


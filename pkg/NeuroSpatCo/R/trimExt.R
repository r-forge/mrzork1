trimExt <-
function(distancias,porcent){
comp<-1-porcent
ncarp<-(length(distancias)/2)
  if(ncarp>0){
  carp1<-distancias$allCarp1[distancias$allCarp1<quantile(distancias$allCarp1,comp)&distancias$allCarp1>quantile(distancias$allCarp1,porcent)]
  salida<-list(carp1=carp1)
    if(ncarp>1){
    carp2<-distancias$allCarp2[distancias$allCarp2<quantile(distancias$allCarp2,comp)&distancias$allCarp2>quantile(distancias$allCarp2,porcent)]
    salida<-list(carp1=carp1,carp2=carp2)
      if(ncarp>2){
      carp3<-distancias$allCarp3[distancias$allCarp3<quantile(distancias$allCarp3,comp)&distancias$allCarp3>quantile(distancias$allCarp3,porcent)]
      salida<-list(carp1=carp1,carp2=carp2,carp3=carp3)
  	if(ncarp>3){
	carp4<-distancias$allCarp4[distancias$allCarp4<quantile(distancias$allCarp4,comp)&distancias$allCarp4>quantile(distancias$allCarp4,porcent)]
	salida<-list(carp1=carp1,carp2=carp2,carp3=carp3,carp4=carp4)
	  if(ncarp>4){
	  carp5<-distancias$allCarp5[distancias$allCarp5<quantile(distancias$allCarp5,comp)&distancias$allCarp4>quantile(distancias$allCarp5,porcent)]
	  salida<-list(carp1=carp1,carp2=carp2,carp3=carp3,carp4=carp4,carp5=carp5)
	    if(ncarp>5){
	    carp6<-distancias$allCarp6[distancias$allCarp6<quantile(distancias$allCarp6,comp)&distancias$allCarp6>quantile(distancias$allCarp6,porcent)]
	    salida<-list(carp1=carp1,carp2=carp2,carp3=carp3,carp4=carp4,carp5=carp5,carp6=carp6)
	      if(ncarp>6){
	      carp7<-distancias$allCarp7[distancias$allCarp7<quantile(distancias$allCarp7,comp)&distancias$allCarp7>quantile(distancias$allCarp7,porcent)]
	      salida<-list(carp1=carp1,carp2=carp2,carp3=carp3,carp4=carp4,carp5=carp5,carp6=carp6,carp7=carp7)
		if(ncarp>7){
		carp8<-distancias$allCarp8[distancias$allCarp8<quantile(distancias$allCarp8,comp)&distancias$allCarp8>quantile(distancias$allCarp8,porcent)]
		salida<-list(carp1=carp1,carp2=carp2,carp3=carp3,carp4=carp4,carp5=carp5,carp6=carp6,carp7=carp7,carp8=carp8)
		}
	      }
	    }
	  }
	}
      }
    }
  }
return(salida)
}


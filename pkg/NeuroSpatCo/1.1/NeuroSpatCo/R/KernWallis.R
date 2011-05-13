KernWallis <-
function(recortados,Kern=FALSE){
require(sm)
require(stats)
require(pgirmess)
  ind<-length(recortados)
  if(ind>=2){
    if(ind==2){
      dato1<-recortados$carp1;dato2<-recortados$carp2
      x <- c(dato1,dato2)
      g <- factor(rep(1:2, c(length(dato1),length(dato2))),labels = c("Zona 1","Zona 2"))
      if(Kern==TRUE){
	dat<-c(rep(1,length(dato1)),rep(2,length(dato2)))
	dat2.l <- factor(dat, levels= c(1,2),labels = c("Zona 1", "Zona 2"))
	q<-list()
	q$dat2<-x
	q$dat<-dat
	sm.density.compare(q$dat2, q$dat, xlab="Distribucion para las Distintas Zonas")
	colfill<-c(2:(2+length(levels(dat2.l))))
	legend(locator(1), levels(dat2.l), fill=colfill)
	abline(v=mean(dato1),col="red")
	abline(v=mean(dato2),col="green")
      }
    }
    if(ind==3){
      dato1<-recortados$carp1;dato2<-recortados$carp2;dato3<-recortados$carp3
      x <- c(dato1,dato2,dato3)
      g <- factor(rep(1:3, c(length(dato1),length(dato2), length(dato3))),labels = c("Zona 1","Zona 2","Zona 3"))
      if(Kern==TRUE){
	dat<-c(rep(1,length(dato1)),rep(2,length(dato2)),rep(3,length(dato3)))
	dat2.l <- factor(dat, levels= c(1,2,3),labels = c("Zona 1", "Zona 2","Zona3"))
	q<-list()
	q$dat2<-x
	q$dat<-dat
	sm.density.compare(q$dat2, q$dat, xlab="Distribucion para las Distintas Zonas")
	colfill<-c(2:(2+length(levels(dat2.l))))
	legend(locator(1), levels(dat2.l), fill=colfill)
	abline(v=mean(dato1),col="red")
	abline(v=mean(dato2),col="green")
	abline(v=mean(dato3),col="blue")
      }
    }
    if(ind==4){
      dato1<-recortados$carp1;dato2<-recortados$carp2;dato3<-recortados$carp3;dato4<-recortados$carp4
      x <- c(dato1,dato2,dato3,dato4)
      g <- factor(rep(1:4, c(length(dato1),length(dato2), length(dato3),length(dato4))),labels = c("Zona 1","Zona 2","Zona 3","Zona 4"))
      if(Kern==TRUE){
	dat<-c(rep(1,length(dato1)),rep(2,length(dato2)),rep(3,length(dato3)),rep(4,length(dato4)))
	dat2.l <- factor(dat, levels= c(1,2,3,4),labels = c("Zona 1", "Zona 2","Zona3","Zona 4"))
	q<-list()
	q$dat2<-x
	q$dat<-dat
	sm.density.compare(q$dat2, q$dat, xlab="Distribucion para las Distintas Zonas")
	colfill<-c(2:(2+length(levels(dat2.l))))
	legend(locator(1), levels(dat2.l), fill=colfill)
	abline(v=mean(dato1),col="red")
	abline(v=mean(dato2),col="green")
	abline(v=mean(dato3),col="blue")
	abline(v=mean(dato4),col="5")
      }
    }
    if(ind==5){
    dato1<-recortados$carp1;dato2<-recortados$carp2;dato3<-recortados$carp3;dato4<-recortados$carp4;dato5<-recortados$carp5
    x <- c(dato1,dato2,dato3,dato4,dato5)
    g <- factor(rep(1:5, c(length(dato1),length(dato2), length(dato3),length(dato4),length(dato5))),labels = c("Zona 1","Zona 2","Zona 3","Zona 4","Zona 5"))
    if(Kern==TRUE){
	dat<-c(rep(1,length(dato1)),rep(2,length(dato2)),rep(3,length(dato3)),rep(4,length(dato4)),rep(5,length(dato5)))
	dat2.l <- factor(dat, levels= c(1,2,3,4,5),labels = c("Zona 1", "Zona 2","Zona3","Zona 4","Zona 5"))
	q<-list()
	q$dat2<-x
	q$dat<-dat
	sm.density.compare(q$dat2, q$dat, xlab="Distribucion para las Distintas Zonas")
	colfill<-c(2:(2+length(levels(dat2.l))))
	legend(locator(1), levels(dat2.l), fill=colfill)
	abline(v=mean(dato1),col="red")
	abline(v=mean(dato2),col="green")
	abline(v=mean(dato3),col="blue")
	abline(v=mean(dato4),col="5")
	abline(v=mean(dato5),col="6")
      }
    }
    if(ind==6){
      dato1<-recortados$carp1;dato2<-recortados$carp2;dato3<-recortados$carp3;dato4<-recortados$carp4;dato5<-recortados$carp5;dato6<-recortados$carp6
      x <- c(dato1,dato2,dato3,dato4,dato5,dato6)
      g <- factor(rep(1:6, c(length(dato1),length(dato2), length(dato3),length(dato4),length(dato5),length(dato6))),labels = c("Zona 1","Zona 2","Zona 3","Zona 4","Zona 5","Zona 6"))
      if(Kern==TRUE){
	dat<-c(rep(1,length(dato1)),rep(2,length(dato2)),rep(3,length(dato3)),rep(4,length(dato4)),rep(5,length(dato5)),rep(6,length(dato6)))
	dat2.l <- factor(dat, levels= c(1,2,3,4,5,6),labels = c("Zona 1", "Zona 2","Zona3","Zona 4","Zona 5","Zona 6"))
	q<-list()
	q$dat2<-x
	q$dat<-dat
	sm.density.compare(q$dat2, q$dat, xlab="Distribucion para las Distintas Zonas")
	colfill<-c(2:(2+length(levels(dat2.l))))
	legend(locator(1), levels(dat2.l), fill=colfill)
	abline(v=mean(dato1),col="red")
	abline(v=mean(dato2),col="green")
	abline(v=mean(dato3),col="blue")
	abline(v=mean(dato4),col="5")
	abline(v=mean(dato5),col="6")
	abline(v=mean(dato5),col="7")
      }
    }
    if(ind==7){
      dato1<-recortados$carp1;dato2<-recortados$carp2;dato3<-recortados$carp3;dato4<-recortados$carp4
      dato5<-recortados$carp5;dato6<-recortados$carp6;dato7<-recortados$carp7
      x <- c(dato1,dato2,dato3,dato4,dato5,dato6,dato7)
      g <- factor(rep(1:7, c(length(dato1),length(dato2), length(dato3),length(dato4),length(dato5),length(dato6), length(dato7))),labels = c("Zona 1","Zona 2","Zona 3","Zona 4","Zona 5","Zona 6","Zona 7"))
      if(Kern==TRUE){
	dat<-c(rep(1,length(dato1)),rep(2,length(dato2)),rep(3,length(dato3)),rep(4,length(dato4)),rep(5,length(dato5)),rep(6,length(dato6)),rep(7,length(dato7)))
	dat2.l <- factor(dat, levels= c(1,2,3,4,5,6,7),labels = c("Zona 1", "Zona 2","Zona3","Zona 4","Zona 5","Zona 6","Zona 7"))
	q<-list()
	q$dat2<-x
	q$dat<-dat
	sm.density.compare(q$dat2, q$dat, xlab="Distribucion para las Distintas Zonas")
	colfill<-c(2:(2+length(levels(dat2.l))))
	legend(locator(1), levels(dat2.l), fill=colfill)
	abline(v=mean(dato1),col="red")
	abline(v=mean(dato2),col="green")
	abline(v=mean(dato3),col="blue")
	abline(v=mean(dato4),col="5")
	abline(v=mean(dato5),col="6")
	abline(v=mean(dato5),col="7")
	abline(v=mean(dato5),col="8")
      }
    }
    if(ind==8){
      dato1<-recortados$carp1;dato2<-recortados$carp2;dato3<-recortados$carp3;dato4<-recortados$carp4
      dato5<-recortados$carp5;dato6<-recortados$carp6;dato7<-recortados$carp7;dato8<-recortados$carp8
      x <- c(dato1,dato2,dato3,dato4,dato5,dato6,dato7,dato8)
      g <- factor(rep(1:8, c(length(dato1),length(dato2), length(dato3),length(dato4),length(dato5),length(dato6), length(dato7),length(dato8))),labels = c("Zona 1","Zona 2","Zona 3","Zona 4","Zona 5","Zona 6","Zona 7","Zona 8"))
      if(Kern==TRUE){
	dat<-c(rep(1,length(dato1)),rep(2,length(dato2)),rep(3,length(dato3)),rep(4,length(dato4)),rep(5,length(dato5)),rep(6,length(dato6)),rep(7,length(dato7)),rep(8,length(dato8)))
	dat2.l <- factor(dat, levels= c(1,2,3,4,5,6,7,8),labels = c("Zona 1", "Zona 2","Zona3","Zona 4","Zona 5","Zona 6","Zona 7","Zona 8"))
	q<-list()
	q$dat2<-x
	q$dat<-dat
	sm.density.compare(q$dat2, q$dat, xlab="Distribucion para las Distintas Zonas")
	colfill<-c(2:(2+length(levels(dat2.l))))
	legend(locator(1), levels(dat2.l), fill=colfill)
	abline(v=mean(dato1),col="red")
	abline(v=mean(dato2),col="green")
	abline(v=mean(dato3),col="blue")
	abline(v=mean(dato4),col="5")
	abline(v=mean(dato5),col="6")
	abline(v=mean(dato5),col="7")
	abline(v=mean(dato5),col="8")
	abline(v=mean(dato5),col="9")
      }
    }
    restest<-kruskal.test(x,g)
    post<-kruskalmc(x,g)
    salida<-list(restest,post)
    return(salida)
  }
  if(ind<2){
    print("Necesita almenos 2 grupos para poder realizar comparaciones")
  }
}


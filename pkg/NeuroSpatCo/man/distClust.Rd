\name{distClust}
\alias{distClust}
\title{
distClust
}
\description{
Esta funcion lee objetos tipo NeuroSpatCo y genera agrupaciones (clusters) de puntos en sentido vertical. finalmente entrega como salida una lista con todos las distancias entre clusters hallados y objetos con el condensado de cada carpeta en evaluacion.
}
\usage{
distClust(Carpeta, Grafica)
}
\arguments{
  \item{Carpeta}{ Objeto de la Clase (NeuroSpatCo) que contiene la informacion de  las carpetas a ser evaluadas. 
}
  \item{Grafica}{
Grafica = 0:  solo entrega los objetos de vuelta. \cr
Grafica = 1:  entrega los objetos y se pueden visualizar los clusters encontrados \cr
Grafica = 2:  entrega los objetos y ademas guarda la imagen de los clusters (en ejes de componentes principales) en las carpetas donde se encuentran los archivos iniciales (.txt) \cr
}
}
\author{
Camilo Alberto Herrera Rozo  \email{ hr.camilo@hrglobalideas.com}
}

\examples{
## se genera un objetos de la clase (NeuroSpatCo)
Carpetas<-read.folder()
##se hace uso de la funcion distClust
distancias<-distClust(Carpetas,0)
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

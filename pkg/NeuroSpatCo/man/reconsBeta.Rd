\name{reconsBeta}
\alias{reconsBeta}
\title{
reconstruccion distribucion Beta y patrones Horizontales
}
\description{
En esta funcion se puede realizar la recontruccion en 3D, para los patrones tanto horizontales como verticales conjuntamente. 
}
\usage{
reconsBeta(recortados, horizontales, zonaA = 1, zonaB = 1, grabar = FALSE)
}
\arguments{
  \item{recortados}{
variable de entrada de datos, generalmente es una variable que contiene objetos que ha sido filtrados de errores (eje: vertical).
}
  \item{horizontales}{
variable de entrada de objetos del eje horizotal.
}
  \item{zonaA}{
En esta variable se ingresa la asignacion en al cual se encuentra el patron vertical de distribucion beta a ser graficado.
}
  \item{zonaB}{
En esta variable se ingresa la asignacion en al cual se encuentra el patron horizontal de distribucion poisson homogenea a ser graficado.
}
  \item{grabar}{
si grabar=TRUE se genarara el grafico y posteriromente se grabara una animacion  de 360º del patron conjunto encontrado.
}
}
\details{
se debe tener en cuenta que las asignaciones de zonaA y zonaB debes ser congruentes para realmente describir un patron de organizacion existente.
}

\references{
Cullen AC and Frey HC (1999) Probabilistic techniques in exposure assessment. Plenum Press, USA, pp. 81-155. 
Venables WN and Ripley BD (2002) Modern applied statistics with S. Springer, New York, pp. 435-446. 
Vose D (2000) Risk analysis, a quantitative guide. John Wiley & Sons Ltd, Chischester, England, pp. 99-143.
}
\author{
Camilo Alberto Herrera Rozo  \email{ hr.camilo@hrglobalideas.com}
}
\examples{
%% nada
## se genera un objetos de la clase (NeuroSpatCo) que contiene los datos (Verticales)
# Carpetas<-read.folder()
## se hace uso de la funcion distClust
# distancias<-distClust(Carpetas,0)
## se recortan los datos segun criterio del experto
# recortados<-trimExt(distancias,0.10)
## se genera un objetos de la clase (NeuroSpatCo) que contiene los datos (horizontales)
# hori<-read.folder()
## se genera la recontruccion en 3D
# reconsBeta(recortados, horizontales, zonaA = 1, zonaB = 1, grabar = FALSE)
%% nada
}
\keyword{ ~recontruction }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

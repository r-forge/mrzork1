\name{reconsBeta}
\alias{reconsBeta}
\title{
reconstrucción distribución Beta y patrones Horizontales
}
\description{
En esta función se puede realizar la reconstrucción en 3D, para los patrones tanto horizontales como verticales conjuntamente. 
}
\usage{
reconsBeta(recortados, horizontales, zonaA = 1, zonaB = 1, grabar = FALSE)
}
\arguments{
  \item{recortados}{
variable de entrada de datos, generalmente es una variable que contiene objetos que ha sido filtrados de errores (eje: vertical).
}
  \item{horizontales}{
variable de entrada de objetos del eje horizontal.
}
  \item{zonaA}{
En esta variable se ingresa la asignación en al cual se encuentra el patrón vertical de distribución beta a ser graficado.
}
  \item{zonaB}{
En esta variable se ingresa la asignación en al cual se encuentra el patrón horizontal de distribución poisson homogénea a ser graficado.
}
  \item{grabar}{
si grabar=TRUE se generara el gráfico y posteriormente se grabara una animación  de 360º del patrón conjunto encontrado.
}
}
\details{
se debe tener en cuenta que las asignaciones de zonaA y zonaB debes ser congruentes para realmente describir un patrón de organización existente.
}

\references{
Cullen AC and Frey HC (1999) Probabilistic techniques in exposure assessment. Plenum Press, USA, pp. 81-155. 
 \cr
Venables WN and Ripley BD (2002) Modern applied statistics with S. Springer, New York, pp. 435-446. 
\cr
Vose D (2000) Risk analysis, a quantitative guide. John Wiley & Sons Ltd, Chischester, England, pp. 99-143.
}
\author{
Camilo Alberto Herrera Rozo  \email{ hr.camilo@hrglobalideas.com}
}
\examples{
## Se genera un objetos de la clase (NeuroSpatCo) que contiene los datos (Verticales)
# Carpetas<-read.folder()
## Se hace uso de la funcion distClust
# distancias<-distClust(Carpetas,0)
## Se recortan los datos segun criterio del experto
# recortados<-trimExt(distancias,0.10)
## Se genera un objetos de la clase (NeuroSpatCo) que contiene los datos (horizontales)
# hori<-read.folder()
## Se genera la recontruccion en 3D
# reconsBeta(recortados, horizontales, zonaA = 1, zonaB = 1, grabar = FALSE)

}
\keyword{ ~recontruction }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

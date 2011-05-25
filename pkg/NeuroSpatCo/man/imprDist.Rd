\name{imprDist}
\alias{imprDist}
\title{
imprDist 
}
\description{
se realiza la búsqueda gráfica de la distribución de una serie de variables contenidas en un listado list(), generalmente filtrados previamente.
}
\usage{
imprDist(recortados)
}
\arguments{
  \item{recortados}{ parámetro de entrada de datos contenidos en un listado list(), generalmente filtrados previamente.
}
}
\value{
  \item{imagen }{contiene la grafica resultante de la aplicación del gráfico de cullen and frey}
  \item{dist# }{se guardaran cada uno de los parámetros de cada una de las distribuciones de los datos evaluados}
}
\references{
Cullen AC and Frey HC (1999) Probabilistic techniques in exposure assessment. Plenum Press, USA, pp. 81-159. Evans M, Hastings N and Peacock B (2000) Statistical distributions. John Wiley and Sons Inc. 
\cr
Fisher RA (1930) The moments of the distribution for normal samples of measures of departures from normality. Proc. R. Soc. London, Series A 130, 16-28.
}
\author{
Camilo Alberto Herrera Rozo  \email{ hr.camilo@hrglobalideas.com}
}
\examples{
## Se genera un objetos de la clase (NeuroSpatCo)
#Carpetas<-read.folder()
## Se hace uso de la funcion distClust
#distancias<-distClust(Carpetas,0)
## Se recortan los datos segun criterio del experto
#recortados<-trimExt(distancias,0.10)
## Se realiza el proceso de descricion de los datos
#distribuciones<-imprDist(recortados)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

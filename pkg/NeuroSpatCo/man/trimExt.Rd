\name{trimExt}
\alias{trimExt}
\title{
trimExt
}
\description{
recorta datos extremos superiores e inferiores con el porcentaje de datos indicado.
}
\usage{
trimExt(distancias, porcent)
}
\arguments{
  \item{distancias}{
nombre del objeto que contiene el conjunto de datos con las distancias encontradas entre las minicolumnas.
}
  \item{porcent}{
porcentaje de recorte tanto superior como inferior.
}
}

\author{
Camilo Alberto Herrera Rozo  \email{ hr.camilo@hrglobalideas.com}
}
\examples{
# Se genera un objetos de la clase (NeuroSpatCo)
Carpetas<-read.folder()
# Se hace uso de la funcion distClust
distancias<-distClust(Carpetas,0)
# Se recortan los datos segun criterio del experto
recortados<-trimExt(distancias,0.10)
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{grafKS}
\alias{grafKS}
\title{
Graficas test de aleatoriedad completa grafKS(Carpeta, Kest = TRUE, Lest = FALSE, density = FALSE)
}
\description{
Esta funcion realiza las graficas del test de aleatoriedad completa las cuales son la guia para el analisis de patrones de organizacion en casos en los que se evalua la existencia de patrones puntuales.
}
\usage{
grafKS(Carpeta, Kest = TRUE, Lest = FALSE, density = FALSE)
}
\arguments{
  \item{Carpeta}{
Entrada de datos, variables de la clase NeuroSpatCo.
}
  \item{Kest}{
si Kest=TRUE se grafican las funciones de aleatoriedad completa K de ripley.
}
  \item{Lest}{
si Lest=TRUE se grafican las funciones de aleatoriedad completa L que es una transformacion de las funcion K de ripley con el fin de evaluar la aleatoriedad teniendo en cuenta variaciones estocasticas mediante el metodo de montecarlo.
}
  \item{density}{
si density=TRUE se grafican imagenes guia que seran de gran ayuda al momento de evaluar si realmente los patrones encontrados pertenecen a patrones de agrupacion o si es debido a algun otro factor externo.
}
}
\references{
Baddeley, A.J. Spatial sampling and censoring. In O.E. Barndorff-Nielsen, W.S. Kendall and M.N.M. van Lieshout (eds) Stochastic Geometry: Likelihood and Computation. Chapman and Hall, 1998. Chapter 2, pages 37–78. 
 Cressie, N.A.C. Statistics for spatial data. John Wiley and Sons, 1991. 
 Diggle, P.J. Statistical analysis of spatial point patterns. Academic Press, 1983. 
 Ohser, J. (1983) On estimators for the reduced second moment measure of point processes. Mathematische Operationsforschung und Statistik, series Statistics, 14, 63 – 71. 
 Ripley, B.D. (1977) Modelling spatial patterns (with discussion). Journal of the Royal Statistical Society, Series B, 39, 172 – 212. 
 Ripley, B.D. Statistical inference for spatial processes. Cambridge University Press, 1988. 
 Stoyan, D, Kendall, W.S. and Mecke, J. (1995) Stochastic geometry and its applications. 2nd edition. Springer Verlag. 
 Stoyan, D. and Stoyan, H. (1994) Fractals, random shapes and point fields: methods of geometrical statistics. John Wiley and Sons.
}
\author{
Camilo Alberto Herrera Rozo  \email{ hr.camilo@hrglobalideas.com}
}
\examples{
## se genera un objetos de la clase (NeuroSpatCo)
#Carpetas<-read.folder()
##se realiza una de las graficas (K_ripley) para cada patron puntual
#grafKS(Carpetas, Kest = TRUE, Lest = FALSE, density = FALSE)
##el resultado queda consignado en la carpeta de lectura de datos inicial.
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

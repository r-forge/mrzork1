\name{grafKS}
\alias{grafKS}
\title{
Graficas test de aleatoriedad completa grafKS(Carpeta, Kest = TRUE, Lest = FALSE, density = FALSE)
}
\description{
Esta función realiza las gráficas del test de aleatoriedad completa las cuales son la guía para el análisis de patrones de organización en casos en los que se evalúa la existencia de patrones puntuales.
}
\usage{
grafKS(Carpeta, Kest = TRUE, Lest = FALSE, density = FALSE)
}
\arguments{
  \item{Carpeta}{
Entrada de datos, variables de la clase NeuroSpatCo.
}
  \item{Kest}{
si Kest=TRUE se grafícan las funciones de aleatoriedad completa K de ripley.
}
  \item{Lest}{
si Lest=TRUE se grafícan las funciones de aleatoriedad completa L que es una transformación de las función K de ripley con el fin de evaluar la aleatoriedad teniendo en cuenta variaciones estocásticas mediante el método de montecarlo.
}
  \item{density}{
si density=TRUE se grafícan imágenes guía que serán de gran ayuda al momento de evaluar si realmente los patrones encontrados pertenecen a patrones de agrupación o si es debido a algún otro factor externo.
}
}
\references{
Baddeley, A.J. Spatial sampling and censoring. In O.E. Barndorff-Nielsen, W.S. Kendall and M.N.M. van Lieshout (eds) Stochastic Geometry: Likelihood and Computation. Chapman and Hall, 1998. Chapter 2, pages 37–78. 
\cr
 Cressie, N.A.C. Statistics for spatial data. John Wiley and Sons, 1991. 
\cr
 Diggle, P.J. Statistical analysis of spatial point patterns. Academic Press, 1983. 
 \cr
 Ohser, J. (1983) On estimators for the reduced second moment measure of point processes. Mathematische Operationsforschung und Statistik, series Statistics, 14, 63 – 71. 
\cr
 Ripley, B.D. (1977) Modelling spatial patterns (with discussion). Journal of the Royal Statistical Society, Series B, 39, 172 – 212. 
\cr
 Ripley, B.D. Statistical inference for spatial processes. Cambridge University Press, 1988. 
\cr
 Stoyan, D, Kendall, W.S. and Mecke, J. (1995) Stochastic geometry and its applications. 2nd edition. Springer Verlag. 
\cr
 Stoyan, D. and Stoyan, H. (1994) Fractals, random shapes and point fields: methods of geometrical statistics. John Wiley and Sons.
}
\author{
Camilo Alberto Herrera Rozo  \email{ hr.camilo@hrglobalideas.com}
}
\examples{
## se genera un objetos de la clase (NeuroSpatCo)
#Carpetas<-read.folder()
##se realiza una de las gráficas (K_ripley) para cada patrón puntual
#grafKS(Carpetas, Kest = TRUE, Lest = FALSE, density = FALSE)
##el resultado queda consignado en la carpeta de lectura de datos inicial.
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line


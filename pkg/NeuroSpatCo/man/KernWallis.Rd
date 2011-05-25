\name{KernWallis}
\alias{KernWallis}
\title{
KernWallis
}
\description{
Realiza el test de Kruskal-Wallis y su prueba post, además genera gráficas de Kernels si Kern=TRUE. 
}
\usage{
KernWallis(recortados, Kern = FALSE)
}
\arguments{
  \item{recortados}{
Variable que contiene los datos a comparar preferiblemente después de haber sido recortados.
}
  \item{Kern}{
default=FALSE, si Kern=TRUE se realizara la gráfica de Kernels conjunta de todas las distribuciones de datos a evaluar.
}
}

\value{
\item{[1]}{contiene el resultado del test de Kruskal-Wallis}
\item{[2]}{contiene el resultado del test post Kruskal-wallis que sera la guia para saber entre que grupos existen diferencias}
}
\references{
Bowman, A.W. and Azzalini, A. (1997). Applied Smoothing Techniques for Data Analysis: the Kernel Approach with S-Plus Illustrations. Oxford University Press, Oxford.\cr
Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120. \cr
Siegel and Castellan (1988) Non parametric statistics for the behavioural sciences. MacGraw Hill Int., New York. pp 213-214
}
\author{
Camilo Alberto Herrera Rozo  \email{ hr.camilo@hrglobalideas.com}
}

\examples{
%% nada
# Se genera un objetos de la clase (NeuroSpatCo)
Carpetas<-read.folder()
# Se hace uso de la funcion distClust
distancias<-distClust(Carpetas,0)
# Se recortan los datos segun criterio del experto
recortados<-trimExt(distancias,0.10)
# No se realizan las graficas de kernels y el test se guarda en una variable Resultados
Resultados<-KernWallis(recortados, Kern = FALSE)
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

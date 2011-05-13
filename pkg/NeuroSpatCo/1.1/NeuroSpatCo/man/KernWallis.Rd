\name{KernWallis}
\alias{KernWallis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
KernWallis
}
\description{
Realiza el test de Kruskal-Wallis y su prueba post, ademas genera graficas de Kernels si Kern=TRUE. 
}
\usage{
KernWallis(recortados, Kern = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recortados}{
Variable que contiene los datos a comparar preferiblemente despues de haber sido recortados.
}
  \item{Kern}{
default=FALSE, si Kern=TRUE se realizara la grafica de Kernels conjunta de todas las distribuciones de datos a evaluar.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{[1]}{contiene el resultado del test de Kruskal-Wallis}
\item{[2]}{contiene el resultado del test post Kruskal-wallis que sera la guia para saber entre que grupos existen diferencias}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Camilo Alberto Herrera Rozo  \email{ hr.camilo@hrglobalideas.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# se genera un objetos de la clase (NeuroSpatCo)
Carpetas<-read.folder()
# se hace uso de la funcion distClust
distancias<-distClust(Carpetas,0)
#se recortan los datos segun criterio del experto
recortados<-trimExt(distancias,0.10)
#No se realizan las graficas de kernels y el test se guarda en una variable Resultados
Resultados<-KernWallis(recortados, Kern = FALSE)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

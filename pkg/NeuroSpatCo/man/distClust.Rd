\name{distClust}
\alias{distClust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
distClust
}
\description{
Esta funcion lee objetos tipo NeuroSpatCo y genera agrupaciones (clusters) de puntos en sentido vertical. finalmente entrega como salida una lista con todos las distancias entre clusters hallados y objetos con el condensado de cada carpeta en evaluacion.
}
\usage{
distClust(Carpeta, Grafica)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Carpeta}{ Objeto de la Clase (NeuroSpatCo) que contiene la informacion de  las carpetas a ser evaluadas. 
%%     ~~Describe \code{Carpeta} here~~
}
  \item{Grafica}{
Grafica = 0:  solo entrega los objetos de vuelta. \cr
Grafica = 1:  entrega los objetos y se pueden visualizar los clusters encontrados \cr
Grafica = 2:  entrega los objetos y ademas guarda la imagen de los clusters (en ejes de componentes principales) en las carpetas donde se encuentran los archivos iniciales (.txt) \cr
%%     ~~Describe \code{Grafica} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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
% se genera un objetos de la clase (NeuroSpatCo)
Carpetas<-read.folder()
% se hace uso de la funcion distClust
distancias<-distClust(Carpetas,0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

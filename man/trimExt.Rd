\name{trimExt}
\alias{trimExt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
trimExt
}
\description{
recorta datos extremos superiores e inferiores con el porcentaje de datos indicado.
}
\usage{
trimExt(distancias, porcent)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distancias}{
nombre del objeto que contiene el conjunto de datos con las distancias encontradas entre las minicolumnas.
}
  \item{porcent}{
porcentaje de recorte tanto superior como inferior.
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
% se genera un objetos de la clase (NeuroSpatCo)
Carpetas<-read.folder()
% se hace uso de la funcion distClust
distancias<-distClust(Carpetas,0)
%se recortan los datos segun criterio del experto
recortados<-trimExt(distancias,0.10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

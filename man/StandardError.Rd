\name{StandardError}
\alias{StandardError}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of the standard error of the modules}
\description{
   This function calculates the standard error of the arithmetic mean of a set of input modules.
}
\usage{
StandardError(modules)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modules}{Vector containing the values of the modules}
}
\details{
   One way to obtain a set of modules from X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function.
}
\value{
   The standard error of all input modules.
}
\references{
 Website \url{http://gim.unex.es/VecStatGraphs2D/}
} 
\author{Ruiz-Cuetos, J.C., \email{jcruizcue@gmail.com}, Felicisimo, A.M., \email{amfeli@unex.es}, 
Cuartero, A., \email{acuartero@unex.es}, Polo, M.E., \email{mepolo@unex.es}, 
Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{ArithmeticMean}}, \code{\link{ModuleStandardDeviation}}, \code{\link{ModuleVariance}}, 
   
   \code{\link{ModulePopulationVariance}}, \code{\link{ModulePopulationStandardDeviation}}, 

   \code{\link{SkewnessModuleCoefficient}}, \code{\link{KurtosisModuleCoefficient}}, \code{\link{AllModuleStatistics}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2)
   modules<-dat[,1]
   StandardError(modules)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{module}
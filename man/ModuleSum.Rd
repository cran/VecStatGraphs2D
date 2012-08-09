\name{ModuleSum}
\alias{ModuleSum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of the sum of the modules}
\description{
   This function calculates the sum of all modules.
}
\usage{
ModuleSum(modules)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modules}{Vector containing the values of the module}
}
\details{
   One way to obtain a set of modules from X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function.
}
\value{
  The sum of all input modules.
}
\references{
   Website \url{http://gim.unex.es/VecStatGraphs2D/}
}
\author{Ruiz-Cuetos, J.C., \email{bilba_t@hotmail.com}, Polo, M.E., \email{mepolo@unex.es}, Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{NumberOfElements}}, \code{\link{MaxValue}}, \code{\link{MinValue}}, \code{\link{Range}},
   \code{\link{ArithmeticMean}}, \code{\link{StandardError}},
   \code{\link{ModuleStandardDeviation}}, \code{\link{ModuleVariance}}, \code{\link{ModulePopulationVariance}},
   \code{\link{ModulePopulationStandardDeviation}}, \code{\link{SkewnessModuleCoefficient}}, \code{\link{KurtosisModuleCoefficient}},
   \code{\link{AllModuleStatistics}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2, Direction=2)
   modules<-dat[,1]
   ModuleSum(modules)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{module}
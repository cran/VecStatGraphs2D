\name{AllModuleStatistics}
\alias{AllModuleStatistics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of All Statistics of the Modules.}
\description{
   This function calculates several statistics from a set of input modules.
}
\usage{
AllModuleStatistics(modules)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modules}{Vector containing the values of the modules}
}
\details{
   One way to get a set of azimuths from X and Y coordinates of the origin position and end position 
   (coordinates X and Y of the vector), it is using the \code{\link{LoadData}} function.
   
   The statistics calculated are: \code{Number Of Elements, Max Value, Min Value, Range, Module Sum, Mean Arithmetic, Standard Error, Standard Deviation Module, Variance Module, Variance Module Population, Standard Deviation Module Population, Skewness Module Coefficient, Kurtosis Module Coefficient.}
}
\value{
   The value All Module Statistics of all input modules.
}
\references{
   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{NumberOfElements}}, \code{\link{MaxValue}}, \code{\link{MinValue}}, \code{\link{Range}},
   \code{\link{ModuleSum}}, \code{\link{ArithmeticMean}}, \code{\link{StandardError}},
   \code{\link{ModuleStandardDeviation}}, \code{\link{ModuleVariance}}, \code{\link{ModulePopulationVariance}},
   \code{\link{ModulePopulationStandardDeviation}}, \code{\link{SkewnessModuleCoefficient}}, \code{\link{KurtosisModuleCoefficient}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   modules<-dat[,1]
   AllModuleStatistics(modules)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{module}

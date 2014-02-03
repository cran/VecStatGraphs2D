\name{AllModuleStatistics}
\alias{AllModuleStatistics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of all statistics of the modules.}
\description{
   This function calculates the basic linear statistics of a sample of linear magnitudes.
}
\usage{
AllModuleStatistics(modules, ndig = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modules}{Vector containing the values of the modules}
  \item{ndig}{Integer value that indicates the decimal places. The default value is 4.}
}
\details{
   One way to obtain a set of modules from X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function.
   
   The statistics calculated are: \code{Number Of Elements, Max Value, Min Value, Range, 
Mean Arithmetic, Standard Error, Standard Deviation Module, Variance Module, 
Variance Module Population, Standard Deviation Module Population, Skewness Module Coefficient, 
Kurtosis Module Coefficient.}
}
\value{
   The value all module statistics of all input modules.
}
\references{
   Website \url{http://gim.unex.es/VecStatGraphs2D/}
}
\author{Ruiz-Cuetos, J.C., \email{jcruizcue@gmail.com}, Felicisimo, A.M., \email{amfeli@unex.es}, 
Cuartero, A., \email{acuartero@unex.es}, Polo, M.E., \email{mepolo@unex.es}, 
Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{StandardError}}, \code{\link{ModuleStandardDeviation}}, \code{\link{ModuleVariance}}, \code{\link{ModulePopulationVariance}},
   
  \code{\link{ModulePopulationStandardDeviation}}, \code{\link{SkewnessModuleCoefficient}}, \code{\link{KurtosisModuleCoefficient}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2)
   modules<-dat[,1]
   AllModuleStatistics(modules, ndig = 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{module}
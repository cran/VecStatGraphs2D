\name{CircularStandardDeviation}
\alias{CircularStandardDeviation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of the Circular Standard Deviation of the azimuths}
\description{
   This function calculates the circular standard deviation from a set of input azimuths.
}
\usage{
CircularStandardDeviation(azimuths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
}
\details{
   One way to get a set of azimuths from X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function.
}
\value{
   The value of the circular standard deviation of all input azimuths.
}
\references{
   Fisher N.I. (1995) Statistical analysis of circular data. Cambridge. Cambridge University Press.
   
   Website \url{http://gim.unex.es/VecStatGraphs2D/} 
}
\author{Ruiz-Cuetos, J.C., \email{bilba_t@hotmail.com}, Polo, M.E., \email{mepolo@unex.es}, Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{MeanModule}}, \code{\link{MeanAzimuth}}, \code{\link{CircularVariance}},
   \code{\link{CircularDispersal}}, \code{\link{KurtosisAzimuthCoefficient}}, \code{\link{VonMisesParameter}},
   \code{\link{SkewnessAzimuthCoefficient}}, \code{\link{AllAzimuthStatistics}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2, Direction=2)
   azimuths<-dat[,2]
   CircularStandardDeviation(azimuths)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{azimuth}
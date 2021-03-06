\name{CircularDispersal}
\alias{CircularDispersal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of the Circular Dispersal of the Azimuths}
\description{
   This function calculates the circular dispersal from a set of input azimuths.
}
\usage{
CircularDispersal(azimuths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
}
\details{
   One way to get a set of azimuths from X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function.
   
   In the case of uniform distribution, this value is infinite, and with values close to 0, a highly concentrated data distribution is expected.
}
\value{
    The value circular dispersal of all input azimuths.
}
\references{
   Fisher N.I. (1995) Statistical analysis of circular data. Cambridge. Cambridge University Press.
   
   Website \url{http://gim.unex.es/VecStatGraphs2D/}
}
\author{Ruiz-Cuetos, J.C., \email{jcruizcue@gmail.com}, Felicisimo, A.M., \email{amfeli@unex.es}, 
Cuartero, A., \email{acuartero@unex.es}, Polo, M.E., \email{mepolo@unex.es}, 
Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{MeanModule}}, \code{\link{MeanAzimuth}}, \code{\link{CircularVariance}}, \code{\link{CircularStandardDeviation}},
   
\code{\link{KurtosisAzimuthCoefficient}}, \code{\link{VonMisesParameter}},
   
\code{\link{SkewnessAzimuthCoefficient}}, \code{\link{AllAzimuthStatistics}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2)
   azimuths<-dat[,2]
   CircularDispersal(azimuths)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{azimuth}
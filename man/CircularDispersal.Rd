\name{CircularDispersal}
\alias{CircularDispersal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of the Circular Dispersal of the Azimuths}
\description{
   This function calculates the Circular Dispersal from a set of input azimuths.
}
\usage{
CircularDispersal(azimuths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
}
\details{
   One way to get a set of azimuths from X and Y coordinates of the origin position and end position 
   (coordinates X and Y of the vector), it is using the \code{\link{LoadData}} function.
   
   In the case of uniform distribution this value is infinite, and with values close to 0, a highly 
   concentrated data distribution is expected.
}
\value{
    The value cirular Dispersal of all input azimuths.
}
\references{
   Fisher N.I. (1995) Statistical analysis of circular data. Cambridge. Cambridge University Press.
   
   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{MeanModule}}, \code{\link{MeanAzimuth}}, \code{\link{CircularVariance}}, \code{\link{CircularStandardDeviation}},
   \code{\link{KurtosisAzimuthCoefficient}}, \code{\link{VonMisesParameter}},
   \code{\link{SkewnessAzimuthCoefficient}}, \code{\link{AllAzimuthStatistics}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   azimuths<-dat[,2]
   CircularDispersal(azimuths)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{azimuth}
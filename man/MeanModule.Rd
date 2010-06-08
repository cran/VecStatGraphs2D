\name{MeanModule}
\alias{MeanModule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of the Mean Module of the azimuths}
\description{
   This function calculates the Mean Module from a set of input azimuths.
}
\usage{
MeanModule(azimuths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
}
\details{
   One way to get a set of azimuths from X and Y coordinates of the origin position and end position 
   (coordinates X and Y of the vector), it is using the \code{\link{LoadData}} function.
}
\value{
  The value Mean module of all input azimuths.
}
\references{
   Fisher N.I. (1995) Statistical analysis of circular data. Cambridge. Cambridge University Press.
   
   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{MeanAzimuth}}, \code{\link{CircularVariance}}, \code{\link{CircularStandardDeviation}},
   \code{\link{CircularDispersal}}, \code{\link{KurtosisAzimuthCoefficient}}, \code{\link{VonMisesParameter}},
   \code{\link{SkewnessAzimuthCoefficient}}, \code{\link{AllAzimuthStatistics}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   azimuths<-dat[,2]
   MeanModule(azimuths)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{azimuth}

\name{AllAzimuthStatistics}
\alias{AllAzimuthStatistics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of All Statistics of the Azimuths}
\description{
   This function calculates several circular data statistics from a set of input azimuths.
}
\usage{
AllAzimuthStatistics(azimuths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
}
\details{
   One way to get a set of azimuths from X and Y coordinates of the origin position and end position 
   (coordinates X and Y of the vector), it is using the \code{\link{LoadData}} function.
   
   The statistics calculated are: \code{Mean Azimuth, Mean Module, Circular Variance, Circular Standard Deviation, Circular Dispersal, Von Mises Parameter, Kurtosis Coefficient, Skewness Coefficient.}
}
\value{
   The value All Azimuth Statistics of all input azimuths.
}
\references{
   Bachelet E. (1981) Circular statistics in biology. London. London Academic Press.

   Dixon K. R., Chapman J. A. (1980) Harmonic mean measure of animal activity areas. Ecology 61, 1040-1044.

   Fisher N.I. (1995) Statistical analysis of circular data. Cambridge. Cambridge University Press.

   Jammalamadaka S.R., Sengupta A. (2001) Topic in circular statistics. Singapore. World Scientific Publihing.

   Mardia K.V. , Jupp, P.E. (2000) Directional statistics. Chichester. Wiley.

   Russell G.S. , Levitin D.J. (1995) An expanded table of probability values for Rao spacing test. Communications in Statistics: Simulation and Computation, 24(4), 879-888.

   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{MeanModule}}, \code{\link{MeanAzimuth}}, \code{\link{CircularVariance}}, \code{\link{CircularStandardDeviation}},
   \code{\link{CircularDispersal}}, \code{\link{KurtosisAzimuthCoefficient}}, \code{\link{VonMisesParameter}},
   \code{\link{SkewnessAzimuthCoefficient}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   azimuths<-dat[,2]
   AllAzimuthStatistics(azimuths)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{azimuth}

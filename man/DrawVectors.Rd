\name{DrawVectors}
\alias{DrawVectors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of the vectors}
\description{
   This function creates a graph that shows an arrow for each vector. The graph represents the situation of every vector by its coordinates. To represent this graph, the input data must be of the measured and real type (Type=3).
}
\usage{
DrawVectors(measured_data_, real_data_)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measured_data_}{Matrix containing the measured coordinates}
  \item{real_data_}{Matrix containing the real coordinates}
}
\details{
   The measured values and real values cannot be calculated. They must be introduced as input data, although it is advisable to use \code{\link{LoadData}} for obtain the vectors of the X and Y coordinates as well as the azimuths of the measured values.
}
\value{
   This function returns no value and creates a graph that represents the vectors of the input data.
}
\references{
   Website \url{http://gim.unex.es/VecStatGraphs2D/}
}
\author{Ruiz-Cuetos, J.C., \email{bilba_t@hotmail.com}, Polo, M.E., \email{mepolo@unex.es}, Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawHistogram}}, \code{\link{DrawDistribution}}, \code{\link{DrawPoints}}, \code{\link{DrawModuleAndAzimuthDistribution}},
   \code{\link{DrawDensityMap}}.
}
\examples{
   FileName<-system.file("data/MeasuredRealData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   measured_data_<-dat[,6:7]
   real_data_<-dat[,8:9]
   DrawVectors(measured_data_, real_data_)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
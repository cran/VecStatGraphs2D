\name{DrawVectors}
\alias{DrawVectors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of the Vectors}
\description{
   This function creates a graph, which shows an arrow for each sample. The graph represents the situation of 
   the origin and end nodes of the every vector. In order to represent this graph, the input data must be of 
   the measured and real type (type=3).
}
\usage{
DrawVectors(measured_data_, real_data_)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measured_data_}{Matrix containing the coordinates measured}
  \item{real_data_}{Matrix containing the coordinates real}
}
\details{
   The measured values and real values can not be calculated, must be introduced as input data, although it is advisable to use 
   \code{\link{LoadData}} for obtain vector of X coordinate and Y coordinate and azimuths of measured values.
}
\value{
   This function returns no value, creates a graph that represents the vectors of the input data.
}
\references{
   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawHistogram}}, \code{\link{DrawDistribution}}, \code{\link{DrawPoints}}, \code{\link{DrawModuleAndAzimuthDistribution}},
   \code{\link{DrawDensityMap}}.
}
\examples{
   FileName<-system.file("data/MeasuredRealData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=3, Direction=2)
   measured_data_<-dat[,6:7]
   real_data_<-dat[,8:9]
   DrawVectors(measured_data_, real_data_)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}

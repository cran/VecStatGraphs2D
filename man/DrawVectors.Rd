\name{DrawVectors}
\alias{DrawVectors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of the vectors}
\description{
   This function creates a graph that shows an arrow for each vector. The graph represents the situation of every vector by its coordinates. To represent this graph, the input data must be the cartesian coordinates (Type = 1).
}
\usage{
DrawVectors(data, SVGf=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Matrix containing the coordinates of the final and initial data}
  \item{SVGf}{Integer value. If SVGf = 0, the plot is showed only in the graphic window. If SVGf = 1, then the plot is saved as SVG graphic. The default value is 0.} 
} 

\details{
   The cartesian coordinates cannot be calculated. They must be introduced as input data, although it is advisable to use \code{\link{LoadData}} for obtain the vectors of the X and Y coordinates as well as the azimuths of the final values.
}
\value{
   This function returns no value and creates a graph that represents the vectors of the input data.
}
\references{
   Website \url{http://gim.unex.es/VecStatGraphs2D/}
}
\author{Ruiz-Cuetos, J.C., \email{jcruizcue@gmail.com}, Felicisimo, A.M., \email{amfeli@unex.es}, 
Cuartero, A., \email{acuartero@unex.es}, Polo, M.E., \email{mepolo@unex.es}, 
Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawAzimuthDensity}}, \code{\link{DrawHistogram}}, \code{\link{DrawDistribution}}, 
\code{\link{DrawPoints}}, \code{\link{DrawModuleAndAzimuthDistribution}}, \code{\link{DrawDensityMap}}.
}

\examples{
   FileName<-system.file("data/MeasuredRealData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1)
   data<-dat[,1:8]
   DrawVectors(data, SVGf=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}

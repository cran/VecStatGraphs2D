\name{DrawModuleAndAzimuthDistribution}
\alias{DrawModuleAndAzimuthDistribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of module and azimuth distributions}
\description{
   This function creates a graph that allows us to check the angular and linear magnitudes simultaneously. The mean azimuth is plotted by a red arrow.
}
\usage{
DrawModuleAndAzimuthDistribution(data_x, data_y, Direction = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_x}{Vector containing the values of the X coordinates}
  \item{data_y}{Vector containing the values of the Y coordinates}
  \item{Direction}{Integer value that indicates the direction of the data. Direction  1 = the mathematical direction (0 grades E counterclockwise). Direction 2 = the topographical direction (0 grades N clockwise). The default value is 2.}
}
\details{
   The graph draws a line for each vector. Each vector is moved to a common origin (0, 0) without changing its azimuth and module. The concentric circumferences allow us to identify linear magnitudes, and the legend of the angles allows us to determine the angular magnitude values.

It is very important to work always with the same direction. If the data are loaded in topographical direction and the graphic representation of the module and azimuth distribution in the mathematical direction, then the resulting graph will be wrong.

One way to obtain a set of X and Y coordinates of the vectors is to use the 
\code{\link{LoadData}} function.
   
   Typical usages are \preformatted{
    DrawModuleAndAzimuthDistribution(data_x, data_y, \dots)
    ......
   }
   
}
\value{
   This function returns no value and creates a graph that represents module and azimuth distributions.
}
\references{
    Website \url{http://gim.unex.es/VecStatGraphs2D/}
} 
\author{Ruiz-Cuetos, J.C., \email{bilba_t@hotmail.com}, Polo, M.E., \email{mepolo@unex.es}, Rodriguez, P.G. \email{pablogr@unex.es}} 


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawHistogram}}, \code{\link{DrawDistribution}}, \code{\link{DrawPoints}},
   \code{\link{DrawDensityMap}}, \code{\link{DrawVectors}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   X_coordinate<-dat[,3]
   Y_coordinate<-dat[,4]
   DrawModuleAndAzimuthDistribution(X_coordinate, Y_coordinate, Direction=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}

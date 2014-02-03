\name{DrawModuleAndAzimuthDistribution}
\alias{DrawModuleAndAzimuthDistribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of module and azimuth distributions}
\description{
   This function creates a graph that allows us to check the angular and linear magnitudes simultaneously. The mean vector is plotted by a red arrow.
}
\usage{
DrawModuleAndAzimuthDistribution(data_x, data_y, SVGf=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_x}{Vector containing the values of the X coordinates}
  \item{data_y}{Vector containing the values of the Y coordinates}
  \item{SVGf}{Integer value. If SVGf = 0, the plot is showed only in the graphic window. If SVGf = 1, then the plot is saved as SVG graphic. The default value is 0.} 
}
\details{
   The graph draws a line for each vector. Each vector is moved to a common origin (0, 0) without changing its azimuth and module. The concentric circumferences allow us to identify linear magnitudes, and the legend of the angles allows us to determine the angular magnitude values.

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
\author{Ruiz-Cuetos, J.C., \email{jcruizcue@gmail.com}, Felicisimo, A.M., \email{amfeli@unex.es}, 
Cuartero, A., \email{acuartero@unex.es}, Polo, M.E., \email{mepolo@unex.es}, 
Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawAzimuthDensity}}, \code{\link{DrawHistogram}}, \code{\link{DrawDistribution}}, 
\code{\link{DrawPoints}}, \code{\link{DrawDensityMap}}, \code{\link{DrawVectors}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2)
   X_coordinate<-dat[,3]
   Y_coordinate<-dat[,4]
   DrawModuleAndAzimuthDistribution(X_coordinate, Y_coordinate, SVGf=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
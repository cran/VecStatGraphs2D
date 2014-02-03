\name{DrawHistogram}
\alias{DrawHistogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of the histogram of the azimuth distribution}
\description{
   This function creates a graph that represents a circumference divided into sectors with different radii. Each radius represents the percentage of azimuth data that belong to each sector. Each portion represents the azimuths that exist between the angles formed by the sides of the portion.
}
\usage{
DrawHistogram(azimuths, ClassSize = 15, SVGf = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
  \item{ClassSize}{Integer value that represents the size of each portion. The default value is 15.}
  \item{SVGf}{Integer value. If SVGf = 0, the plot is showed only in the graphic window. If SVGf = 1, then the plot is saved as SVG graphic. The default value is 0.} 
}
\details{
This function shows where the highest concentration of points is located.

One way to obtain a set of azimuths from X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function.
   
   Typical usages are \preformatted{
    DrawHistogram(azimuths, \dots)
    DrawHistogram(azimuths, ClassSize = 20, \dots)
    ......
   }
   
}
\value{
   This function returns no value and creates a graph that represents a circular histogram of the input data.
}
\references{
    Website \url{http://gim.unex.es/VecStatGraphs2D/}
} 
\author{Ruiz-Cuetos, J.C., \email{jcruizcue@gmail.com}, Felicisimo, A.M., \email{amfeli@unex.es}, 
Cuartero, A., \email{acuartero@unex.es}, Polo, M.E., \email{mepolo@unex.es}, 
Rodriguez, P.G. \email{pablogr@unex.es}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawDistribution}}, \code{\link{DrawPoints}}, \code{\link{DrawModuleAndAzimuthDistribution}},
   
\code{\link{DrawDensityMap}}, \code{\link{DrawVectors}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2)
   azimuths<-dat[,2]
   DrawHistogram(azimuths, ClassSize=15, SVGf=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
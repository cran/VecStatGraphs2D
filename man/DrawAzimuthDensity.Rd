\name{DrawAzimuthDensity}
\alias{DrawAzimuthDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical representation of the azimuth density}
\description{
This function creates a graph that represents a circular graphic when the data azimuths densities are represented by color bands. A vector is drawn representing the mode azimuth.
}
\usage{
DrawAzimuthDensity(azimuths, Period = 15, SVGf = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
  \item{Period}{Integer value which represents the number of moving average terms} 
  \item{SVGf}{Integer value. If SVGf = 0, the plot is showed only in the graphic window. If SVGf = 1, then the plot is saved as SVG graphic. The default value is 0.} 
}
\details{
   With this function, a circular graphic is obtained, representing azimuths densities by color bands.
   
   Typical usages are \preformatted{
    DrawAzimuthDensity(azimuths, ...)
    DrawAzimuthDensity(azimuths, Period = 15, ...)
    DrawAzimuthDensity(azimuths, Period = 15, SVGf = 0)
    ......
   }
   
}
\value{
   This function returns a no value and creates a graph that represents the density of the azimuths.
}
\references{
    Website \url{http://gim.unex.es/VecStatGraphs2D/}
}
\author{Ruiz-Cuetos, J.C., \email{jcruizcue@gmail.com}, Felicisimo, A.M., \email{amfeli@unex.es}, 
Cuartero, A., \email{acuartero@unex.es}, Polo, M.E., \email{mepolo@unex.es}, 
Rodriguez, P.G. \email{pablogr@unex.es}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{DrawDistribution}}, \code{\link{DrawHistogram}}, \code{\link{DrawPoints}}, 

\code{\link{DrawModuleAndAzimuthDistribution}}, \code{\link{DrawDensityMap}}, \code{\link{DrawVectors}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2)
   azimuths<-dat[,2]
   DrawAzimuthDensity(azimuths, Period = 15, SVGf=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
\name{DrawDistribution}
\alias{DrawDistribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of the distribution of the azimuths}
\description{
This function creates a graph that represents the distribution of the azimuths. Every azimuth is plotted as a blue point in a unit circle. The mean azimuth is represented by a red arrow. A 95 percentage confidence interval for the mean azimuth is plotted in red. When the data concentration is too low (von Mises parameter below 0.9) the mean azimuth is not plotted. This confidence interval depends on the mean module, the number of azimuths and the von Mises parameter.
}
\usage{
DrawDistribution(azimuths, SVGf = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
  \item{SVGf}{Integer value. If SVGf = 0, the plot is showed only in the graphic window. If SVGf = 1, then the plot is saved as SVG graphic. The default value is 0.} 
}
\details{
   With this function, the positions of the largest and smallest concentrations of points can be observed. 
   
   One way to obtain a set of azimuths from X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function.
   
   Typical usages are \preformatted{
    DrawDistribution(azimuths, \dots)
    ......
   }
   
}
\value{
   This function returns no value and creates a graph that represents the distribution of the input data.
}
\references{
    Website \url{http://gim.unex.es/VecStatGraphs2D/}
}
\author{Ruiz-Cuetos, J.C., \email{jcruizcue@gmail.com}, Felicisimo, A.M., \email{amfeli@unex.es}, 
Cuartero, A., \email{acuartero@unex.es}, Polo, M.E., \email{mepolo@unex.es}, 
Rodriguez, P.G. \email{pablogr@unex.es}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawAzimuthDensity}}, \code{\link{DrawHistogram}}, \code{\link{DrawPoints}}, 

\code{\link{DrawModuleAndAzimuthDistribution}}, \code{\link{DrawDensityMap}}, \code{\link{DrawVectors}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2)
   azimuths<-dat[,2]
   DrawDistribution(azimuths, SVGf=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
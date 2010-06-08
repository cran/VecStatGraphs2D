\name{DrawDensityMap}
\alias{DrawDensityMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of a point Density Map}
\description{
   The density map was built as follows: a) vectors are moved to a common origin without changing its azimuth and module, 
   b) end position perform a point cloud of varying density that depends on the accumulation of vectors with similar properties, 
   c) end positions are used for to calculate the density map as a surface with values depending on the end-position 
   accumulation per area unit. The density map gives us information about the joint distribution of modules and azimuths 
   and allows the detection of errors or outliers. Areas where there is a greater number of points (end position of the vectors) 
   will have a deeper color (red), while the areas a lesser density will have a less intense color (white).
   The outliers can be calculated by the higher value of the Modules or the Harmonic Mean.
}
\usage{
DrawDensityMap(data_x, data_y, PercentajeOutliers = 5, PaintPoint = FALSE, Div = 250, HarmonicMean = FALSE, PaintAxis = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_x}{Vector containing the values of the X coordinate}
  \item{data_y}{Vector containing the values of the Y coordinate}
  \item{PercentajeOutliers}{Integer value that indicates the percentage of outliers. The default value is 5}
  \item{PaintPoint}{Logical value, if PaintPoint=TRUE samples represented by points, if PaintPoint=FALSE
					samples not represented by points. The defaul is FALSE}
  \item{Div}{IntegerValue that indicates the number of divisions that will have the density map. The default value is 250}
  \item{HarmonicMean}{Logical value, if HarmonicMean=FALSE the module is used for the calculation of outliers, 
                     if HarmonicMean=TRUE the Harmonic Mean is used for the calculation of outliers. The default value is FALSE}
  \item{PaintAxis}{Logical value, if PaintAxis=TRUE draw axis, if PaintAxis=FALSE draw not axis. The default value is FALSE}
}
\details{
   To create the density map, are used Kernel descriptors, to perform these calculations is required the \code{MASS} package.
   
   The parameter Div is very important because a very large value will cause the creation of the slow density map, 
   and a very small value would create a ineffective density map. 
   
   One way to get a vector of X and Y coordinate from modules and azimuths of the origin position and end position 
   (coordinates X and Y of the error), it is using the \code{\link{LoadData}} function.
   
   Typical usages are \preformatted{
    DrawDensityMap(data_x, data_y, \dots)
    DrawDensityMap(data_x, data_y, Div = 100, \dots)
    DrawDensityMap(data_x, data_y, PaintPoint = TRUE, HarmonicMean = TRUE, PaintAxis = TRUE, \dots)
    ......
	}
}
\value{
   This function returns no value, creates a graph that represents a density map of the input values.
}
\references{
   Dixon K. R., Chapman J. A. (1980) Harmonic mean measure of animal activity areas. Ecology 61, 1040-1044.
   
   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawHistogram}}, \code{\link{DrawDistribution}}, \code{\link{DrawPoints}}, \code{\link{DrawModuleAndAzimuthDistribution}},
   \code{\link{DrawVectors}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   X_coordinate<-dat[,3]
   Y_coordinate<-dat[,4]
   DrawDensityMap(X_coordinate, Y_coordinate, PaintPoint = TRUE, Div=200)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}

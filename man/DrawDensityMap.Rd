\name{DrawDensityMap} 
\alias{DrawDensityMap} 
%- Also NEED an '\alias' for EACH other topic documented here. 
\title{Graphic representation of a point density map} 
\description{ 
  The density map will be built as follows: a) vectors are moved to a common origin without changing its azimuth and module; b) end positions perform a point cloud of varying density that depends on the accumulation of vectors with similar properties; and c) end positions are used to calculate the density map as a surface with values depending on the end-position accumulation per area unit. The density map provides information about the joint distribution of modules and azimuths and allows the detection of errors or outliers. Areas in which there is a greater number of points (end position of the vectors) will have a deeper color (red), whereas the areas with lower density will have a less intense color (white). The user can define how many points are classified as outliers. The outliers can be calculated using the higher value of the modules or the harmonic mean. For that, the module value or harmonic mean value is calculated for each vector. Points with the largest module value or harmonic mean value will be considered as outliers.  
} 
\usage{ 
DrawDensityMap(data_x, data_y, PercentageOutliers = 5, PaintPoint = FALSE, Div = 250, HarmonicMean = FALSE, PaintAxis = FALSE) 
} 
%- maybe also 'usage' for other objects documented here. 
\arguments{ 
 \item{data_x}{Vector containing the values of the X coordinates} 
 \item{data_y}{Vector containing the values of the Y coordinates} 
 \item{PercentageOutliers}{Integer value that indicates the percentage of points that are considered as outliers. The default value is 5} 
 \item{PaintPoint}{Logical value. If PaintPoint=TRUE, then samples are represented by points. If PaintPoint=FALSE, then samples are not represented by points. The default value is FALSE} 
 \item{Div}{IntegerValue that indicates the number of divisions that will have the density map. The default value is 250} 
 \item{HarmonicMean}{Logical value. If HarmonicMean=FALSE, then the module is used for the calculation of outliers. If HarmonicMean=TRUE, then the Harmonic Mean is used for the calculation of outliers. The default value is FALSE.} 
 \item{PaintAxis}{Logical value. If PaintAxis=TRUE, then the axis will be drawn. If PaintAxis=FALSE, then the axis will not be drawn. The default value is FALSE.} 
} 
\details{ 
  Kernel descriptors are used to create the density map. To perform these calculations, the MASS package is required.

The parameter Div is very important because a very large value will lead to the creation of a slow density map, whereas a very small value will create an ineffective density map.
  
  One way to obtain a set of X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function. 
   
  Typical usages are \preformatted{ 
   DrawDensityMap(data_x, data_y, \dots) 
   DrawDensityMap(data_x, data_y, Div = 100, \dots) 
   DrawDensityMap(data_x, data_y, PaintPoint = TRUE, HarmonicMean = TRUE, PaintAxis = TRUE, \dots) 
   ...... 
	} 
} 
\value{ 
  This function returns no value and creates a graph that represents a density map from the input values. 
} 
\references{ 
  Dixon K. R., Chapman J. A. (1980) Harmonic mean measure of animal activity areas. Ecology 61, 1040-1044. 
   
 Website \url{http://gim.unex.es/VecStatGraphs2D/}
} 
\author{Ruiz-Cuetos, J.C., \email{bilba_t@hotmail.com}, Polo, M.E., \email{mepolo@unex.es}, Rodriguez, P.G. \email{pablogr@unex.es}} 


%% ~Make other sections like Warning with \section{Warning }{....} ~ 

\seealso{ 
  \code{\link{DrawHistogram}}, \code{\link{DrawDistribution}}, \code{\link{DrawPoints}}, \code{\link{DrawModuleAndAzimuthDistribution}}, 
  \code{\link{DrawVectors}}. 
} 
\examples{ 
  FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D") 
  dat<-LoadData(FileName, Type=2, Direction=2) 
  X_coordinate<-dat[,3] 
  Y_coordinate<-dat[,4] 
  DrawDensityMap(X_coordinate, Y_coordinate, PaintPoint = TRUE, Div=200) 
} 
% Add one or more standard keywords, see file 'KEYWORDS' in the 
% R documentation directory. 
\keyword{graphics}
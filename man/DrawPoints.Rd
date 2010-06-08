\name{DrawPoints}
\alias{DrawPoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of Points}
\description{
   This function creates a graph, that represents one point for each sample in the final coordinates of each 
   vector assuming that initials are at the origin (0,0), also shows a percentage of discordant points (red color), 
   selected from the module or Harmonic Mean, called "outliers".
}
\usage{
DrawPoints(data_x, data_y, PercentajeOutliers = 5, HarmonicMean = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_x}{Vector containing the values of the X coordinate}
  \item{data_y}{Vector containing the values of the Y coordinate}
  \item{PercentajeOutliers}{Integer value that indicates the percentage of outliers. The default value is 5}
  \item{HarmonicMean}{Logical value, if HarmonicMean=FALSE the module is used for the calculation of outliers, 
                     if HarmonicMean=TRUE the HarmonicMean is used for the calculation of outliers. The default value is FALSE}
}
\details{
	All vectors are moved to a common origin (0,0) without changing its azimuth and module 
	The graph represents the situation of the end nodes of the vectors.
	
    One way to get a vector of X and Y coordinate from modules and azimuths of the origin position and end position 
   (coordinates X and Y of the error), it is using the \code{\link{LoadData}} function.
    
   Typical usages are \preformatted{
    DrawPoints(data_x, data_y, \dots)
    DrawPoints(data_x, data_y, PercentajeOutliers = 3, \dots)
    DrawPoints(data_x, data_y, HarmonicMean = TRUE, \dots)
    DrawPoints(data_x, data_y, PercentajeOutliers = 3, HarmonicMean = TRUE)
    ......
   }
}
\value{
   This function returns no value, creates a graph that represents  the end nodes of the vectors. 
}
\references{
   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawHistogram}}, \code{\link{DrawDistribution}}, \code{\link{DrawModuleAndAzimuthDistribution}},
   \code{\link{DrawDensityMap}}, \code{\link{DrawVectors}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   X_coordinate<-dat[,3]
   Y_coordinate<-dat[,4]
   DrawPoints(X_coordinate, Y_coordinate, PercentajeOutliers=8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}

\name{DrawPoints}
\alias{DrawPoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of points}
\description{
   This function creates a graph that represents one point for each vector. Each vector is moved to a common origin (0, 0) without changing its azimuth and module. The user can define how many points are classified as outliers (in red color). The outliers can be calculated by the higher value of the modules or the harmonic mean. For that, the module value or the harmonic mean value is calculated for each vector. Points with the largest module value or harmonic mean value will be considered as outliers.
}
\usage{
DrawPoints(data_x, data_y, PercentageOutliers = 5, HarmonicMean = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_x}{Vector containing the values of the X coordinates}
  \item{data_y}{Vector containing the values of the Y coordinates}
  \item{PercentageOutliers}{Integer value that indicates the percentage of outliers. The default value is 5.}
  \item{HarmonicMean}{Logical value. If HarmonicMean=FALSE, then the module is used for the calculation of outliers. if HarmonicMean=TRUE, then the HarmonicMean is used for the calculation of outliers. The default value is FALSE.}
}
\details{
	Each vector is moved to a common origin (0, 0) without changing its azimuth and module. The graph represents the situation of the end nodes of the vectors.

One way to obtain a set of X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function.
    
   Typical usages are \preformatted{
    DrawPoints(data_x, data_y, \dots)
    DrawPoints(data_x, data_y, PercentageOutliers = 3, \dots)
    DrawPoints(data_x, data_y, HarmonicMean = TRUE, \dots)
    DrawPoints(data_x, data_y, PercentageOutliers = 3, HarmonicMean = TRUE)
    ......
   }
}
\value{
   This function returns no value and creates a graph that represents the end nodes of the vectors. 
}
\references{
   Website \url{http://gim.unex.es/VecStatGraphs2D/}
}
\author{Ruiz-Cuetos, J.C., \email{bilba_t@hotmail.com}, Polo, M.E., \email{mepolo@unex.es}, Rodriguez, P.G. \email{pablogr@unex.es}}


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
   DrawPoints(X_coordinate, Y_coordinate, PercentageOutliers=8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}

\name{DrawModuleAndAzimuthDistribution}
\alias{DrawModuleAndAzimuthDistribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of Module and Azimuth Distribution}
\description{
   This function creates a graph, that allows us to check the angular and linear magnitudes simultaneously,
   also indicated the Mean Azimuth (red color).
}
\usage{
DrawModuleAndAzimuthDistribution(data_x, data_y, Direction = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_x}{Vector containing the values of the X coordinate}
  \item{data_y}{Vector containing the values of the Y coordinate}
  \item{Direction}{Integer value that indicates the direction of the data.
			Direction  1 = Mathematical direction (0 grades E anticlockwise),
			direction 2 = Topographical direction (0 grades N clockwise). The default value is 2}
}
\details{
   The graph draws a line for each of the samples. The lines will have origin in (0,0) and end at coordinates (X, Y).
   The concentric circumferences allow us to identify linear magnitudes, and the legend of the angles allows to know the angular magnitudes value.
   
   It is very important to work always with the same direction, because if the data are loaded in Topographical direction, 
   and Graphic representation of the module and azimuth distribution in Mathematical direction, the resulting graph is wrong.
   
   One way to get a vector of X and Y coordinate from modules and azimuths of the origin position and end position 
   (coordinates X and Y of the error), it is using the \code{\link{LoadData}} function.
   
   Typical usages are \preformatted{
    DrawModuleAndAzimuthDistribution(data_x, data_y, \dots)
    ......
   }
   
}
\value{
   This function returns no value, creates a graph that represents module and azimuth distribution
}
\references{
   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


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

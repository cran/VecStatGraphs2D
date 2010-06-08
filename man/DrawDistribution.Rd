\name{DrawDistribution}
\alias{DrawDistribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of the Distribution of the azimuths}
\description{
   This function creates a graph that represents the azimuths distribution. 
   Every azimuth is plotted as a blue point in a unit circle. The color red is 
   representation of the Mean Azimuth that is in the center of the confidence interval
}
\usage{
DrawDistribution(azimuths, Direction = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
  \item{Direction}{Integer value that indicates the direction of the data.
			Direction  1 = Mathematical direction (0 grades E anticlockwise),
			direction 2 = Topographical direction (0 grades N clockwise). The default value is 2}
}
\details{
   It is very important to work always with the same direction, because if the data are loaded in Topographical direction, 
   and Graphic representation of the distribution in Mathematical direction, the resulting graph is wrong.
   
   With this function you can see in detail where the largest and smallest concentrations of points are. 
   
   One way to get a set of azimuths from X and Y coordinates of the origin position and end position 
   (coordinates X and Y of the vector), it is using the \code{\link{LoadData}} function.
   
   Typical usages are \preformatted{
    DrawDistribution(azimuths, \dots)
    ......
   }
   
}
\value{
   This function returns no value, creates a graph that represents the distribution of the input data.
}
\references{
   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawHistogram}}, \code{\link{DrawPoints}}, \code{\link{DrawModuleAndAzimuthDistribution}},
   \code{\link{DrawDensityMap}}, \code{\link{DrawVectors}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   azimuths<-dat[,2]
   DrawDistribution(azimuths, Direction = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}

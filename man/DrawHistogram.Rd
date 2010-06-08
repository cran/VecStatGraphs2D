\name{DrawHistogram}
\alias{DrawHistogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphic representation of the histogram of the azimuth distribution}
\description{
   This function creates a graph, that represents a circumference divided in sectors of different radius.
   The radius represents the percentage of azimuth data which belongs to each sector. Each portion represents
   the azimuths that exist between the angles formed by the sides of the portion.
}
\usage{
DrawHistogram(azimuths, TamClasses = 15, Direction = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
  \item{TamClasses}{Integer value that represents the size of each portion. The default value is 15.}
  \item{Direction}{Integer value that indicates the direction of the data.
			Direction  1 = Mathematical direction (0 grades E anticlockwise),
			direction 2 = Topographical direction (0 grades N clockwise). The default value is 2}
}
\details{
   It is very important to work always with the same direction, because if the data are loaded in Topographical direction, 
   and Graphic representation of the Histogram in Mathematical direction, the resulting graph is wrong.

   This function allows seeing where the more concentrated points are.
   
   One way to get a set of azimuths from X and Y coordinates of the origin position and end position 
   (coordinates X and Y of the vector), it is using the \code{\link{LoadData}} function.
   
   Typical usages are \preformatted{
    DrawHistogram(azimuths, \dots)
    DrawHistogram(azimuths, TamClasses = 20, \dots)
    ......
   }
   
}
\value{
   This function returns no value, creates a graph that represents a circular histogram of the input data.
}
\references{
   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawDistribution}}, \code{\link{DrawPoints}}, \code{\link{DrawModuleAndAzimuthDistribution}},
   \code{\link{DrawDensityMap}}, \code{\link{DrawVectors}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   azimuths<-dat[,2]
   DrawHistogram(azimuths, TamClasses=15)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}

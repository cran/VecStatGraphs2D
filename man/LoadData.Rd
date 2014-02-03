\name{LoadData}
\alias{LoadData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data loading and conversion of cartesian coordinates, incremental data, and polar coordinates}
\description{
  This function reads data from a text file containing data of coordinates. Data can be read as cartesian coordinates, incremental data or polar coordinates. This functions returns a matrix with all possible data conversions according to topographical direction (0 grades N clockwise).
}
\usage{
LoadData(FileName, Type = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{File name to load data}
  \item{Type}{Integer value that indicates the type of data contained in the file.	Type 1 = Cartesian, Type 2= Incremental, Type 3 = Polar}
}
\details{
Input files must be in a certain format for the data type. By default, the type is incremental data (Type = 2).
   
The cartesian coordinate type contains four columns, each pair defining the coordinates of a vector node. The columns 1 and 2 are the values of the initial node, while columns 3 and 4 are the coordinates of the final node. If necessary, we can used the inverse order to adapt the direction to our data. The calculations are performed subtracting the coordinates of the final node to the initial node; each row corresponds to one set of data.

The incremental data contains two columns, the first column is the increase of X coordinate, and the second is the increase of Y coordinate; each row corresponds to one set of data. 
   
The polar coordinates contains two columns. The first column represents the module, and the second column represents the azimuth; each row corresponds to one set of data.
 
  
  
   Typical usages are \preformatted{
    LoadData(FileName, \dots)
    LoadData(FileName, Type=2, \dots)
    ......
   }
}
\value{
   The function returns a nx9 matrix size, where n is the number of data. Column 1 represents the module; column 2 represents the azimuth; column 3 represents the increase of X coordinate; the column 4 represents the increase of Y coordinate; columns 5 and 6 represent the X and Y coordinates, respectively, of the initial node, and columns 7 and 8 represent the X and Y coordinates, respectively, of the final node.
   
   Columns 5, 6, 7 and 8 will have value only if the input data are of type 1.
}
\references{
   Website \url{http://gim.unex.es/VecStatGraphs2D/}
}
\author{Ruiz-Cuetos, J.C., \email{jcruizcue@gmail.com}, Felicisimo, A.M., \email{amfeli@unex.es}, 
Cuartero, A., \email{acuartero@unex.es}, Polo, M.E., \email{mepolo@unex.es}, 
Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{DrawAzimuthDensity}}, \code{\link{DrawHistogram}}, \code{\link{DrawModuleAndAzimuthDistribution}},
   \code{\link{DrawDensityMap}}, \code{\link{AllAzimuthStatistics}}, \code{\link{AllModuleStatistics}}, 
   \code{\link{DrawQQPlot}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   LoadData(FileName, Type=2) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file}
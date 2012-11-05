\name{LoadData}
\alias{LoadData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data loading and conversion of cartesian coordinates, incremental data, and polar coordinates}
\description{
  This function reads data from a text file containing data of coordinates. Data can be read as cartesian coordinates, incremental data or polar coordinates. This functions returns a matrix with all possible data conversions according to mathematical direction (0 grades E counterclockwise) or topographical direction (0 grades N clockwise).
}
\usage{
LoadData(FileName, Type = 2, Direction = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{File name to load data}
  \item{Type}{Integer value that indicates the type of data contained in the file.	Type 1 = Cartesian, Type 2= Incremental, Type 3 = Polar}
  \item{Direction}{Integer value that indicates the direction of the data. Direction  1 = the mathematical direction (0 grades E counterclockwise). Direction 2 = the topographical direction (0 grades N clockwise)}
}
\details{
Input files must be in a certain format for the data type. By default, the type is incremental data with topographical direction (Type = 2 and Direction = 2).
   
The cartesian coordinate type contains four columns, each pair defining the coordinates of a vector node. The columns 1 and 2 are the values of the final node, while columns 3 and 4 are the coordinates of the initial node. If necessary, we can used the inverse order to adapt the direction to our data. The calculations are performed subtracting the coordinates of the final node to the initial node; each row corresponds to one set of data.

The incremental data contains two columns, the first column is the increase of X coordinate, and the second is the increase of Y coordinate; each row corresponds to one set of data. 
   
The polar coordinates contains two columns. The first column represents the module, and the second column represents the azimuth; each row corresponds to one set of data.
 
  
In the cases of Type = 1 and Type =2 are very important to choose the correct sense of direction, because the output data depend on this variable. The mathematical direction chooses the origin of the degrees in the X coordinate axis, rotating counter-clockwise. The topographical direction chooses the origin of the degrees in the Y coordinate axis, rotating clockwise. Mathematical direction and topographical direction follow a sexagesimal angular system.
   
   Typical usages are \preformatted{
    LoadData(FileName, \dots)
    LoadData(FileName, Type=2, \dots)
    ......
   }
}
\value{
   The function returns a nx9 matrix size, where n is the number of data. Column 1 represents the module; column 2 represents the azimuth; column 3 represents the increase of X coordinate; the column 4 represents the increase of Y coordinate; the column 5 represents the direction and type of input data; columns 6 and 7 represent the X and Y coordinates, respectively, of the final node, and columns 8 and 9 represent the X and Y coordinates, respectively, of the initial node.
   
   Columns 6, 7, 8 and 9 will have value only if the input data are of type 1.
}
\references{
   Website \url{http://gim.unex.es/VecStatGraphs2D/}
}
\author{Ruiz-Cuetos, J.C., \email{bilba_t@hotmail.com}, Polo, M.E., \email{mepolo@unex.es}, Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawHistogram}}, \code{\link{DrawModuleAndAzimuthDistribution}},
   \code{\link{DrawDensityMap}}, \code{\link{AllAzimuthStatistics}}, \code{\link{AllModuleStatistics}}, 
   \code{\link{ValuationOfUniformity}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   LoadData(FileName, Type=2, Direction=2) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file}
\name{LoadData}
\alias{LoadData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data loading and conversion of measured and real coordinates, rectangular coordinates, and polar coordinates}
\description{
  This function reads data from a text file containing data of coordinates. Data can be read as measured and real coordinates, rectangular coordinates or polar coordinates. This functions returns a matrix with all possible data conversions according to mathematical direction (0 grades E counterclockwise) or topographical direction (0 grades N clockwise).
}
\usage{
LoadData(FileName, Type = 2, Direction = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{File name to load data}
  \item{Type}{Integer value that indicates the type of data contained in the file.	Type 1 = Measured and Real, Type 2= Rectangular, Type 3 = Polar}
  \item{Direction}{Integer value that indicates the direction of the data. Direction  1 = the mathematical direction (0 grades E counterclockwise). Direction 2 = the topographical direction (0 grades N clockwise)}
}
\details{
   Input files must be in a certain format for the data type. By default, the type is rectangular with topographical direction (Type = 2 and Direction = 2).
   
  The measured and real type contains four columns. The first two columns represent the X and Y coordinates of the measured data, and the last two columns represent the coordinates of the real data; each row corresponds to one set of data.

   The rectangular type contains two columns, the first column is the X coordinate, and the second column is the Y coordinate; each row corresponds to one set of data. The X coordinates and Y coordinates are obtained from the difference between the measured coordinates and the real coordinates.
   
   The polar type contains two columns. The first column represents the module, and the second column represents the azimuth; each row corresponds to one set of data.
 
  
   It is very important to choose the correct sense of direction, because the output data depend on this variable. The mathematical direction chooses the origin of the degrees in the X coordinate axis, rotating counter-clockwise. The topographical direction chooses the origin of the degrees in the Y coordinate axis, rotating clockwise. Mathematical direction and topographical direction follow a sexagesimal angular system.
   
   Typical usages are \preformatted{
    LoadData(FileName, \dots)
    LoadData(FileName, Type=2, \dots)
    ......
   }
}
\value{
   The function returns a nx9 matrix size, where n is the number of data. Column 1 represents the module; column 2 represents the azimuth; column 3 represents the X coordinate (the difference between the X measured coordinate and the X real coordinate); the column 4 represents the Y coordinate (the difference between the Y measured coordinate and the Y real coordinate); the column 5 represents the direction and type of input data; columns 6 and 7 represent the X and Y coordinates, respectively, of the measured data, and columns 8 and 9 represent the X and Y coordinates, respectively, of the real data.
   
   Columns 6, 7, 8 and 9 will have value only if the input data are of type 1 (measured and real).
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
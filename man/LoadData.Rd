\name{LoadData}
\alias{LoadData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data loading and conversion of polar coordinates, rectangular coordinates, and measured and real coordinates}
\description{
  This function reads data from a txt file containing data of coordinates. Data can be read as polar coordinates, 
  rectangular coordinates or measured and real coordinates. This functions returns a matrix with all possible data 
  conversions according to Mathematical direction (0 grades E anticlockwise) or Topographical direction (0 grades N clockwise).
}
\usage{
LoadData(FileName, Type = 1, Direction = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{File name to load data}
  \item{Type}{Integer value that indicates the type of data contained in the file.
			Type 1 = Rectangular, Type 2= Polar, Type 3 = Measured and Real}
  \item{Direction}{Integer value that indicates the direction of the data.
			Direction  1 = Mathematical direction (0 grades E anticlockwise),
			Direction 2 = Topographical direction (0 grades N clockwise)}
}
\details{
   Input files must be in a certain format for the data type. By default the type is Rectangular with Topographical direction.
   
   The rectangular type contains two columns, the first column is the X coordinate and the second column is 
   the Y coordinate, and each row corresponds to one of the data. The X coordinate and Y coordinate are obtained 
   from the difference of the coordinates measured coordinates with respect to the real coordinates.
   
   The polar type contains two columns, the first column represents the module and the second column represents 
   the azimuth, and each row corresponds to one of the data.
   
   The measured and real type contains four columns, the first two columns represent the X and Y coordinates 
   of the measured data and the last two columns represents the coordinates of the actual data, and each row corresponds to one of the data.
   
   It is very important to choose the correct sense of direction, because the output data depend on this variable.
   The Mathematical direction chooses the origin of the degrees in the X coordinate axis, growing anticlockwise.
   The Topographical direction chooses the origin of the degrees in the Y coordinate axis, growing clockwise.
   Mathematical direction and Topographical direction follow sexagesimal angular system.
   
   Typical usages are \preformatted{
    LoadData(FileName, \dots)
    LoadData(FileName, Type=2, \dots)
    ......
   }
}
\value{
   The function returns a nx9 matrix size , where n is the number of data. The column 1 represents the module,the  column 2 represents the azimuth, 
   the column 3 represents the X coordinate, the column 4 represents the Y coordinate, the column 5 represents the direction and type of input data, 
   the column 6 and 7 represent the X and Y coordinates of the measured data, the column 8 and 9 represent the X and Y coordinates of the real data.
   
   The columns 6, 7, 8 and 9 will only have value if the input data are of type 3 (measured and real).
}
\references{
   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawHistogram}}, \code{\link{DrawModuleAndAzimuthDistribution}},
   \code{\link{DrawDensityMap}}, \code{\link{AllAzimuthStatistics}}, \code{\link{AllModuleStatistics}}, 
   \code{\link{ValuationOfUniformity}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   LoadData(FileName, Type=1, Direction=2) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file}

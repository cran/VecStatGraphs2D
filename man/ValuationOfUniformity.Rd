\name{ValuationOfUniformity}
\alias{ValuationOfUniformity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical assessment of uniformity}
\description{
   This function generates a graph that allows you to see quickly if the data distribution can be considered uniform
   or if it departs from the uniformity.
}
\usage{
ValuationOfUniformity(azimuths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
}
\details{
   The graphic is created by placing in increasing order n pairs of points from the calculation of the linear order statistics of the azimuths.
   
   If the data fit to the uniform distribution, the points should lie along 45 grades line passing through the origin. 
   
   The arbitrary choice of the origin of the azimuths may induces a misunderstanding in the interpretation. We can solve this problem
   adding the unit to each element of the pair of values of the first 20 percent of points, and subtracting the unit to each element 
   of the pair of values of the last 20 percent.
   
   One way to get a set of azimuths from X and Y coordinates of the origin position and end position 
   (coordinates X and Y of the vector), it is using the \code{\link{LoadData}} function.
}
\value{
  This function returns no value.  A graph is displayed to show if the hypothesis of uniform distribution can be accepted or rejected.
}
\references{
   Fisher N.I. (1995) Statistical analysis of circular data. Cambridge. Cambridge University Press.
   
   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{RayleighTest}}, \code{\link{RaoTest}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   azimuths<-dat[,2]
   ValuationOfUniformity(azimuths)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{test}

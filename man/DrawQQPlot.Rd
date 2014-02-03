\name{DrawQQPlot}
\alias{DrawQQPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical assessment of uniformity}
\description{
   This function generates a graph that allows one to determine quickly the azimuths distribution can be considered uniform.
}
\usage{
DrawQQPlot(azimuths, SVGf=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
  \item{SVGf}{Integer value. If SVGf = 0, the plot is showed only in the graphic window. If SVGf = 1, then the plot is saved as SVG graphic. The default value is 0.} 
} 
\details{
   The graphic is created by placing in increasing order of n pairs of points from the calculation of the linear order statistics of the azimuths.
   
   If the data fit to the uniform distribution, the points should lie along a 45 grades line passing through the origin. 
   
   The arbitrary choice of the origin of the azimuths may lead to a misunderstanding in the interpretation. We can solve this problem by adding the unit to each element of the pair of values of the first 20 percent of points and by subtracting the unit to each element of the pair of values of the last 20 percent. All these new points are plotted.
   
   One way to obtain a set of azimuths from X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function.
}
\value{
  This function returns no value. A graph is displayed to show whether the hypothesis of uniform distribution can be accepted or rejected.
}
\references{
   Fisher N.I. (1995) Statistical analysis of circular data. Cambridge. Cambridge University Press.
   
    Website \url{http://gim.unex.es/VecStatGraphs2D/}
} 
\author{Ruiz-Cuetos, J.C., \email{jcruizcue@gmail.com}, Felicisimo, A.M., \email{amfeli@unex.es}, 
Cuartero, A., \email{acuartero@unex.es}, Polo, M.E., \email{mepolo@unex.es}, 
Rodriguez, P.G. \email{pablogr@unex.es}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{RayleighTest}}, \code{\link{RaoTest}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2)
   azimuths<-dat[,2]
   DrawQQPlot(azimuths, SVGf=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{test}
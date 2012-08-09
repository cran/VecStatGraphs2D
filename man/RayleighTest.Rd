\name{RayleighTest}
\alias{RayleighTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rayleigh test. Formal test of uniformity}
\description{
   This function performs the Rayleigh test of uniformity.
}
\usage{
RayleighTest(azimuths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
}
\details{
   This test detects a single modal direction in a sample of azimuths when the mean azimuth is unspecified. The hypothesis of uniformity is rejected if the mean module is very large. This test assumes that a large mean module implies a high concentration around the mean and, therefore, less probability that the data are uniformly distributed.

   One way to obtain a set of azimuths from X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function.
}
\value{
   Returns the value of the probability of accepting the hypothesis of uniformity.
}
\references{
   Fisher N.I. (1995) Statistical analysis of circular data. Cambridge. Cambridge University Press.
   
 Website \url{http://gim.unex.es/VecStatGraphs2D/}
} 
\author{Ruiz-Cuetos, J.C., \email{bilba_t@hotmail.com}, Polo, M.E., \email{mepolo@unex.es}, Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{ValuationOfUniformity}}, \code{\link{RaoTest}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2, Direction=2)
   azimuths<-dat[,2]
   RayleighTest(azimuths)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{test}
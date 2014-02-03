\name{RaoTest}
\alias{RaoTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rao test. Formal test of uniformity}
\description{
   This function performs the Rao test of uniformity.
}
\usage{
RaoTest(azimuths, Alpha = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
  \item{Alpha}{Significance level. Value used to obtain the Rao Value of the Rao Table. The values can be 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, or 0.9. The default is 0.01.}
}
\details{
   For a uniform distribution, the space between points should be approximately \code{360/n}. If the space deviates much from this value, then the probability that the data belong to a uniform distribution is reduced.

   One way to obtain a set of azimuths from X and Y coordinates of the vectors is to use the \code{\link{LoadData}} function.
   
   Typical usages are \preformatted{
    RaoTest(azimuths, \dots)
    ......
   }
}
\value{
  Returns the p-value and indicates whether the hypothesis of uniformity can be accepted.
}
\references{
   Bachelet E. (1981) Circular statistics in biology. London. London Academic Press.

   Jammalamadaka S.R., Sengupta A. (2001) Topic in circular statistics. Singapore. World Scientific Publishing.

   Mardia K.V. , Jupp, P.E. (2000) Directional statistics. Chichester. Wiley.

   Russell G.S. , Levitin D.J. (1995) An expanded table of probability values for Rao spacing test. Communications in Statistics: Simulation and Computation, 24(4), 879-888.

 Website \url{http://gim.unex.es/VecStatGraphs2D/}
} 
\author{Ruiz-Cuetos, J.C., \email{jcruizcue@gmail.com}, Felicisimo, A.M., \email{amfeli@unex.es}, 
Cuartero, A., \email{acuartero@unex.es}, Polo, M.E., \email{mepolo@unex.es}, 
Rodriguez, P.G. \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{DrawQQPlot}}, \code{\link{RayleighTest}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=2)
   azimuths<-dat[,2]
   RaoTest(azimuths, Alpha = 0.01)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{test}
\name{RaoTest}
\alias{RaoTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rao Test. Formal test of uniformity}
\description{
   This function performs the Rao test of uniformity.
}
\usage{
RaoTest(azimuths, Alpha = 0.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{azimuths}{Vector containing the values of the azimuths}
  \item{Alpha}{Value used to obtain the Rao Value of the Rao Table. 
			The values can be 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 0.9.
			The default is 0.01.}
}
\details{
   For a uniform distribution, the space between points should be about \code{360/n}. If the space 
   deviates much from this value, the probability that the data belong to a uniform distribution is reduced.

   One way to get a set of azimuths from X and Y coordinates of the origin position and end position 
   (coordinates X and Y of the vector), it is using the \code{\link{LoadData}} function.
   
   Typical usages are \preformatted{
    RaoTest(azimuths, \dots)
    ......
   }
}
\value{
  Returns the probability value, and indicates whether or not to accept the hypothesis of uniformity.
}
\references{
   Bachelet E. (1981) Circular statistics in biology. London. London Academic Press.

   Jammalamadaka S.R., Sengupta A. (2001) Topic in circular statistics. Singapore. World Scientific Publihing.

   Mardia K.V. , Jupp, P.E. (2000) Directional statistics. Chichester. Wiley.

   Russell G.S. , Levitin D.J. (1995) An expanded table of probability values for Rao spacing test. Communications in Statistics: Simulation and Computation, 24(4), 879-888.

   Website \url{http://fegtepinta.byethost11.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{ValuationOfUniformity}}, \code{\link{RayleighTest}}.
}
\examples{
   FileName<-system.file("data/RectangularData.txt", package="VecStatGraphs2D")
   dat<-LoadData(FileName, Type=1, Direction=2)
   azimuths<-dat[,2]
   RaoTest(azimuths, Alpha = 0.01)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{test}

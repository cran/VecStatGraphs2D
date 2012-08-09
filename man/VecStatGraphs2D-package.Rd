\name{VecStatGraphs2D-package}
\alias{VecStatGraphs2D-package}
\alias{VecStatGraphs2D}
\docType{package}
\title{
Vector analysis using graphical and analytical methods in 2D
}
\description{
    This package performs a 2D statistical analysis, both numerical and graphic, of a set of vectors. Because a vector has two components, module and azimuth, vector analysis is performed in three stages. First, modules are analyzed using linear statistics. Second, azimuths are analyzed by circular statistics. Third a joint analysis of modules and azimuths is performed using density maps that allow detection of other distribution properties (i.e. anisotropy) and outliers. Tests and circular statistic parameters are accompanied by a full range of graphing: histograms, maps of distributions, point maps, vector maps, density maps, distribution modules and azimuths.
}
\details{
\tabular{ll}{
Package: \tab VecStatGraphs2D\cr
Type: \tab Package\cr
Version: \tab 1.4\cr
Date: \tab 2012-08-09\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}

}
\author{
   Juan Carlos Ruiz Cuetos, Maria Eugenia Polo Garcia, Pablo Garcia Rodriguez.

   Maintainer: Pablo Garcia Rodriguez <pablogr@unex.es>
}
\references{
   Bachelet E. (1981) Circular statistics in biology. London. London Academic Press.

   Dixon K. R., Chapman J. A. (1980) Harmonic mean measure of animal activity areas. Ecology 61, 1040-1044.

   Fisher N.I. (1995) Statistical analysis of circular data. Cambridge. Cambridge University Press.

   Jammalamadaka S.R., Sengupta A. (2001) Topic in circular statistics. Singapore. World Scientific Publihing.

   Mardia K.V. , Jupp, P.E. (2000) Directional statistics. Chichester. Wiley.

   Russell G.S. , Levitin D.J. (1995) An expanded table of probability values for Rao spacing test. Communications in Statistics: Simulation and Computation, 24(4), 879-888.

   Website \url{http://gim.unex.es/VecStatGraphs2D/}
}

\keyword{ package }
\seealso{
   \code{\link{AllAzimuthStatistics}}, \code{\link{AllModuleStatistics}}, \code{\link{DrawHistogram}}, \code{\link{DrawPoints}}, 
   \code{\link{DrawModuleAndAzimuthDistribution}}, \code{\link{DrawDensityMap}}, \code{\link{DrawVectors}}, \code{\link{DrawDistribution}}, 
   \code{\link{RaoTest}}, \code{\link{RayleighTest}}, \code{\link{ValuationOfUniformity}}.
}


\name{VecStatGraphs2D-package}
\alias{VecStatGraphs2D-package}
\alias{VecStatGraphs2D}
\docType{package}
\title{
Vectorial analysis using graphical and analytical methods in 2D
}
\description{
    This package performs a 2D statistical analysis, both numerical and graphical, of a set of vectors. Because a vector has two components, module and azimuth, vector analysis is performed in three stages. First, modules are analyzed using linear statistics. Second, azimuths are analyzed by circular statistics. Third a joint analysis of modules and azimuths is performed using density maps that allow detection of other distribution properties (i.e. anisotropy) and outliers. Tests and circular statistic parameters are accompanied by a full range of graphing: histograms, maps of distributions, point maps, vector maps, density maps, distribution modules and azimuths.
}
\details{
\tabular{ll}{
Package: \tab VecStatGraphs2D\cr
Type: \tab Package\cr
Version: \tab 1.7\cr
Date: \tab 2014-02-03\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}

}
\author{
   Juan Carlos Ruiz Cuetos, Angel Manuel Felicisimo, Aurora Cuartero, Maria Eugenia Polo Garcia, Pablo Garcia Rodriguez.

   Maintainer: Pablo Garcia Rodriguez <pablogr@unex.es>
}
\references{
   Bachelet E. (1981) Circular statistics in biology. London. London Academic Press.

   Dixon K. R., Chapman J. A. (1980) Harmonic mean measure of animal activity areas. Ecology 61, 1040-1044.

   Cuartero A., Felicisimo A.M., Polo M.E., Caro A., Rodriguez P.G. (2010) Positional Accuracy Analysis of Satellite Imagery by Circular Statistics.  Photogrammetric Engineering & Remote Sensing (JCR-2010 = 0,926; Category: Imaging Science & Photographic Technology 9/19; second quartile), 76 (11), 1275-1286.

   Cuartero A., Armesto J., Rodriguez P.G., Arias P. (2010) Error Analysis of Terrestrial Laser Scanning Data by Means of Spherical Statistics and 3D Graphs. Sensors (JCR-2010 = 1,771; Category: Instruments & Instrumentation 14/61; first quartile), 10, 10128-10145.
   
   Fisher N.I. (1995) Statistical analysis of circular data. Cambridge. Cambridge University Press.

   Jammalamadaka S.R., Sengupta A. (2001) Topic in circular statistics. Singapore. World Scientific Publishing.

   Mardia K.V. , Jupp, P.E. (2000) Directional statistics. Chichester. Wiley.

   Pewsey, A., Neuhauser, M., Ruxton, G. D. (2013). Circular statistics in R. Oxford University Press.
   
   Rodriguez P.G., Polo M.E., Cuartero A., Felicisimo A.M., Ruiz-Cuetos J.C. (2014) VecStatGraphs2D, A Tool for the Analysis of Two-Dimensional Vector Data: An Example Using QuikSCAT Ocean Winds. IEEE Geoscience and Remote Sensing Letters (JCR-2012 = 1,823; Category: Remote Sensing 8/27; first quartile), 11(5), 921-925.
   
   Russell G.S. , Levitin D.J. (1995) An expanded table of probability values for Rao spacing test. Communications in Statistics: Simulation and Computation, 24(4), 879-888.
   
   Website \url{http://gim.unex.es/VecStatGraphs2D/}
}

\keyword{ package }
\seealso{
   \code{\link{AllAzimuthStatistics}}, \code{\link{AllModuleStatistics}}, \code{\link{DrawHistogram}}, \code{\link{DrawPoints}},
 
   \code{\link{DrawModuleAndAzimuthDistribution}}, \code{\link{DrawDensityMap}}, \code{\link{DrawVectors}}, 

   \code{\link{DrawAzimuthDensity}}, \code{\link{DrawDistribution}}, \code{\link{RaoTest}}, \code{\link{RayleighTest}}, 

   \code{\link{DrawQQPlot}}.
}

